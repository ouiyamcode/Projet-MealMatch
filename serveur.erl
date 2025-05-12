-module(serveur).
-export([start/0, charger_recettes/0]).
-include("bdd.hrl").

% État initial
initial_state() ->
  #{id => user1,
    allergies => [],
    scores_pos => [],
    scores_neg => [],
    scores_neutres => [], % <-- NOUVEAU
    nb_interactions => 0,
    deja_connecte => false,
    plats_tries => [],
    alerte_faite => false,
    recettes_aimees => []}.

% Lancement
start() ->
  mnesia:start(),
  {ok, ListenSocket} = gen_tcp:listen(4040, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
  io:format("✅ Serveur TCP en écoute sur le port 4040...~n"),
  accept(ListenSocket).


accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("👤 Client connecté~n"),
    spawn(fun() -> handle_client(Socket) end),
    accept(ListenSocket). % continuer à écouter

 handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
            UserId = binary_to_term(Bin),
            io:format("🔐 Utilisateur reçu: ~p~n", [UserId]),
            State = charger_ou_initialiser_utilisateur(UserId),
            IsKnown = maps:get(deja_connecte, State),

            % 1. Envoyer profil si déjà connu
            % 2. Sinon, demander allergies
            case IsKnown of
                true ->
                    gen_tcp:send(Socket, term_to_binary({profil, State})),
                    FinalState = boucle_interactive(Socket, State);
                false ->
                    gen_tcp:send(Socket, term_to_binary({demande_allergies})),
                    FinalState = boucle_interactive(Socket, State)
            end,

            sauvegarder_utilisateur(FinalState),
            gen_tcp:close(Socket);

        {error, closed} ->
            io:format("❌ Client déconnecté.~n")
    end.


boucle_interactive(Socket, State) ->
    case maps:get(deja_connecte, State) of
        false ->
            case gen_tcp:recv(Socket, 0) of
                {ok, Bin} ->
                    {allergies, Liste} = binary_to_term(Bin),
                    NewState = State#{allergies => Liste, deja_connecte => true},
                    io:format("✅ Allergies reçues du client : ~p~n", [Liste]),
                    gen_tcp:send(Socket, term_to_binary({profil, NewState})),
                    NewState;
                {error, closed} ->
                    io:format("❌ Client déconnecté.~n"),
                    State
            end;
        true ->
            % tu pourras ici plus tard envoyer un plat
            State
    end.



charger_ou_initialiser_utilisateur(UserId) ->
    F = fun() ->
        case mnesia:read({users, UserId}) of
            [User] ->
                io:format("ℹ️ Utilisateur existant trouvé, chargement...~n"),
                #{
                    id => User#users.user_id,
                    allergies => User#users.restrictions_alimentaires,
                    scores_pos => [],
                    scores_neg => [],
                    scores_neutres => [],
                    nb_interactions => 0,
                    deja_connecte => true,
                    plats_tries => [],
                    alerte_faite => false,
                    recettes_aimees => User#users.recettes_aimees
                };
            [] ->
                io:format("👤 Nouvel utilisateur, état initialisé.~n"),
                maps:put(id, UserId, initial_state())

        end
    end,
    {atomic, Result} = mnesia:transaction(F),
    Result.


% Boucle principale
loop(State) ->
  case maps:get(nb_interactions, State) of
    10 ->
      io:format("10 interactions terminées. Voici une recommandation pour vous :~n"),
      Recommande = trouver_recommandation(State),
      afficher_recommandation(Recommande),
      sauvegarder_utilisateur(State),
      io:format("Souhaitez-vous continuer ? (o pour oui, autre chose pour quitter)~n"),
      Continue = string:trim(io:get_line("Votre choix: ")),
      case Continue of
        "o" ->
          % Redémarre avec nb_interactions = 0 mais conserve les préférences
          loop(State#{nb_interactions => 0});
        _ ->
          io:format("Session terminée.~n"),
          ok
      end;
    _ ->
      case maps:get(deja_connecte, State) of
        false ->
          io:format("Bienvenue ! Avez-vous des allergies ? (ex: gluten, arachides)~n"),
          Allergies = io:get_line("Allergies: "),
          ParsedAllergies = parse_allergies(Allergies),
          NewState = State#{allergies => ParsedAllergies, deja_connecte => true},
          envoyer_plat(NewState);
        true ->
          envoyer_plat(State)
      end
  end.

% Envoi d’un plat
envoyer_plat(State) ->
  TriedIds = [R#recipes.recipe_id || R <- maps:get(plats_tries, State)],
  AllRecettes = charger_recettes(),
  Allergies = maps:get(allergies, State),
  Remaining = [R || R <- AllRecettes,
    not lists:member(R#recipes.recipe_id, TriedIds),
    not contient_allergene(R, Allergies)],
  case Remaining of
    [] ->
      io:format("Aucun plat ne correspond à vos préférences ou à vos restrictions alimentaires.~n"),
      case maps:get(scores_pos, State) of
        [] ->
          io:format("Aucune préférence enregistrée, pas de recommandation possible.~n");
        _ ->
          Recommande = trouver_recommandation(State),
          afficher_recommandation(Recommande)
      end,
      sauvegarder_utilisateur(State),
      io:format("Session terminée : plus aucun plat ne peut être proposé.~n"),
      ok;

    _ ->
      AlerteDejaFaite = maps:get(alerte_faite, State),
      case {length(Remaining), AlerteDejaFaite} of
        {N, false} when N =< 5 ->
          io:format("⚠️  Il ne reste que ~p plats disponibles.~n", [N]),
          io:format("Voici une recommandation anticipée :~n"),
          Recommande = trouver_recommandation(State),
          afficher_recommandation(Recommande),
          envoyer_et_reagir(Remaining, State#{alerte_faite => true});
        _ ->
          envoyer_et_reagir(Remaining, State)
      end
  end.


envoyer_et_reagir(Remaining, State) ->
  rand:seed(exsplus, {erlang:monotonic_time(), erlang:unique_integer(), erlang:phash2(self())}),
  Index = rand:uniform(length(Remaining)),
  Plat = lists:nth(Index, Remaining),
  afficher_plat(Plat),
  Reponse = demander_reaction(),
  NouveauState = maj_etat(State, Plat, Reponse),
  loop(NouveauState).


% Affiche une recommandation si trouvée
afficher_recommandation(none) ->
  io:format("Aucune recette restante à recommander.~n");
afficher_recommandation(Plat) ->
  io:format("Voici une recommandation pour vous :~n"),
  afficher_plat(Plat).

% Sauvegarde le profil utilisateur et affiche les plats gardés
sauvegarder_utilisateur(State) ->
  UserId = maps:get(id, State),
  User = #users{
    user_id = UserId,
    mdp = <<"">>,
    choix_repas = 1,
    restrictions_alimentaires = maps:get(allergies, State),
    recettes_aimees = maps:get(recettes_aimees, State),
    historique_recherches = [],
    score_global = calculer_score_global(State),
    norme_utilisateur = calculer_norme_utilisateur(State)
  },
  bdd:insert_user(User),
  io:format("💾 Profil sauvegardé pour ~p avec ~p plats gardés !~n", [
    UserId, length(User#users.recettes_aimees)
  ]),
  afficher_recettes_aimees(User#users.recettes_aimees).


% Affiche les plats gardés
afficher_recettes_aimees([]) ->
  io:format("Vous n'avez gardé aucun plat.~n");
afficher_recettes_aimees(Ids) ->
  UniqueIds = lists:usort(Ids),
  io:format("Voici les plats que vous avez gardés :~n"),
  lists:foreach(
    fun(Id) ->
      case bdd:get_recipe(Id) of
        {atomic, {ok, #recipes{nom = Nom}}} ->
          io:format("- ~s~n", [Nom]);
        _ -> ok
      end
    end,
    UniqueIds
  ).

% Lecture utilisateur
parse_allergies(Input) ->
  Trimmed = string:trim(Input),
  List = string:tokens(Trimmed, ", "),
  [list_to_atom(string:trim(S)) || S <- List].

% Lecture de la réaction utilisateur
demander_reaction() ->
  io:format("Que pensez-vous de ce plat ?~n"),
  io:format(" [Entrée] = aime | 'p' = aime_pas | 'g' = garder~n"),
  Raw = io:get_line("Réponse: "),
  Clean = string:trim(string:lowercase(Raw)),
  case Clean of
    "" -> "aime";
    "a" -> "aime";
    "p" -> "aime_pas";
    "g" -> "garder";
    "aime" -> "aime";
    "aime_pas" -> "aime_pas";
    "garder" -> "garder";
    _ -> Clean
  end.

% Mise à jour de l’état
maj_etat(State, Plat, Reponse) ->
  ScorePlat = Plat#recipes.score,
  IdPlat = Plat#recipes.recipe_id,
  NouvellesInteractions = maps:get(nb_interactions, State) + 1,
  NouveauxPlatsTries = [Plat | maps:get(plats_tries, State)],
  case Reponse of
    "aime" ->
      State#{
        scores_pos => [ScorePlat | maps:get(scores_pos, State)],
        scores_neg => maps:get(scores_neg, State),
        nb_interactions => NouvellesInteractions,
        plats_tries => NouveauxPlatsTries
      };
    "aime_pas" ->
      State#{
        scores_pos => maps:get(scores_pos, State),
        scores_neg => [ScorePlat | maps:get(scores_neg, State)],
        nb_interactions => NouvellesInteractions,
        plats_tries => NouveauxPlatsTries
      };
    "garder" ->
      State#{
        scores_pos => maps:get(scores_pos, State),
        scores_neg => maps:get(scores_neg, State),
        scores_neutres => [ScorePlat | maps:get(scores_neutres, State)],
        nb_interactions => NouvellesInteractions,
        plats_tries => NouveauxPlatsTries,
        recettes_aimees => [IdPlat | maps:get(recettes_aimees, State)]
      };
    _ ->
      State
  end.

% Chargement des recettes
charger_recettes() ->
  {atomic, Recettes} = mnesia:transaction(
    fun() ->
      mnesia:match_object(#recipes{recipe_id = '_', nom = '_', ingredients = '_',
        prix = '_', site_source = '_', score = '_',
        alternatives = '_', avis_utilisateurs = '_',
        norme_recette = '_'})
    end),
  Recettes.


% Affichage d’un plat
afficher_plat(#recipes{nom = Nom, score = Score}) ->
  io:format("Plat proposé : ~s~n", [Nom]),
  io:format("Score : ~p~n", [Score]).

% Moyennes et distances
additionner_vecteurs([], []) -> [];
additionner_vecteurs([A|As], [B|Bs]) -> [A + B | additionner_vecteurs(As, Bs)].

moyenne_vecteurs(Listes) ->
  case Listes of
    [] -> lists:duplicate(15, 0);
    _ ->
      N = length(Listes),
      Total = lists:foldl(fun additionner_vecteurs/2, lists:duplicate(15, 0), Listes),
      [X div N || X <- Total]
  end.

moyenne_ponderee(Pos, Neutre) ->
  TotalListes = Pos ++ Neutre,
  TotalPoids = length(Pos) + (length(Neutre) div 2), % pondération 0.5
  case TotalListes of
    [] -> lists:duplicate(15, 0);
    _ ->
      Total = lists:foldl(fun additionner_vecteurs/2, lists:duplicate(15, 0), Pos ++ Neutre),
      [X div TotalPoids || X <- Total]
  end.

% Recommandation (plats non vus)
trouver_recommandation(State) ->
  Pos = moyenne_ponderee(maps:get(scores_pos, State), maps:get(scores_neutres, State)),
  Neg = moyenne_vecteurs(maps:get(scores_neg, State)),
  TriedIds = [P#recipes.recipe_id || P <- maps:get(plats_tries, State)],
  AimeesIds = maps:get(recettes_aimees, State),
  Recettes = [R || R <- charger_recettes(),
    not lists:member(R#recipes.recipe_id, TriedIds),
    not lists:member(R#recipes.recipe_id, AimeesIds)],
  case Recettes of
    [] -> none;
    _ ->
      min_by(
        fun(Plat) ->
          Score = Plat#recipes.score,
          SimPos = similarite_cosinus(Score, Pos),
          SimNeg = similarite_cosinus(Score, Neg),
          SimNeg - SimPos
        end,
        Recettes
      )
  end.

min_by(Fun, [H|T]) ->
  min_by(Fun, T, H, Fun(H)).

min_by(_, [], Best, _) ->
  Best;
min_by(Fun, [H|T], Best, BestVal) ->
  Val = Fun(H),
  case Val < BestVal of
    true -> min_by(Fun, T, H, Val);
    false -> min_by(Fun, T, Best, BestVal)
  end.

contient_allergene(#recipes{ingredients = Ingredients}, Allergies) ->
  lists:any(fun(A) -> lists:member(A, Ingredients) end, Allergies).

similarite_cosinus(V1, V2) ->
  ProduitScalaire = lists:sum([A * B || {A, B} <- lists:zip(V1, V2)]),
  NormeV1 = math:sqrt(lists:sum([A * A || A <- V1])),
  NormeV2 = math:sqrt(lists:sum([B * B || B <- V2])),
  case {NormeV1, NormeV2} of
    {0.0, _} -> 0.0;
    {_, 0.0} -> 0.0;
    _ -> ProduitScalaire / (NormeV1 * NormeV2)
  end.
calculer_score_global(State) ->
  length(maps:get(recettes_aimees, State)).

calculer_norme_utilisateur(State) ->
  Scores = moyenne_ponderee(
    maps:get(scores_pos, State),
    maps:get(scores_neutres, State)
  ),
  math:sqrt(lists:sum([X * X || X <- Scores])).

%% afficher_profil_utilisateur(State) ->
%%   Id = maps:get(id, State),
%%   Allergies = maps:get(allergies, State),
%%   NbLikes = length(maps:get(recettes_aimees, State)),
%%   Score = calculer_score_global(State),
%%   Norme = calculer_norme_utilisateur(State),
%%   io:format("~n===== Profil utilisateur (~p) =====~n", [Id]),
%%   io:format("🥗 Allergies         : ~p~n", [Allergies]),
%%   io:format("❤️ Recettes aimées  : ~p~n", [NbLikes]),
%%   io:format("📊 Score global     : ~p~n", [Score]),
%%   io:format("📐 Norme utilisateur: ~.2f~n", [Norme]),
%%   io:format("==================================~n~n").
