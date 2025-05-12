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
    recettes_aimees => []}.

% Lancement
start() ->
  mnesia:start(),
  State = initial_state(),
  loop(State).

% Boucle principale
loop(State) ->
  case maps:get(nb_interactions, State) of
    10 ->
      io:format("10 interactions terminées. Voici une recommandation pour vous :~n"),
      Recommande = trouver_recommandation(State),
      afficher_recommandation(Recommande),
      sauvegarder_utilisateur(State),
      ok;
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
      ok;
    _ ->
      rand:seed(exsplus, {erlang:monotonic_time(), erlang:unique_integer(), erlang:phash2(self())}),
      Index = rand:uniform(length(Remaining)),
      Plat = lists:nth(Index, Remaining),
      afficher_plat(Plat),
      Reponse = demander_reaction(),
      NouveauState = maj_etat(State, Plat, Reponse),
      loop(NouveauState)
  end.

% Affiche une recommandation si trouvée
afficher_recommandation(none) ->
  io:format("Aucune recette restante à recommander.~n");
afficher_recommandation(Plat) ->
  io:format("Voici une recommandation pour vous :~n"),
  afficher_plat(Plat).

% Sauvegarde le profil utilisateur et affiche les plats gardés
sauvegarder_utilisateur(State) ->
  User = #users{
    user_id = <<"user1">>,
    mdp = <<"">>,
    choix_repas = 1,
    restrictions_alimentaires = maps:get(allergies, State),
    recettes_aimees = maps:get(recettes_aimees, State),
    historique_recherches = [],
    score_global = 0,
    norme_utilisateur = 0.0
  },
  bdd:insert_user(User),
  io:format("Profil sauvegardé avec ~p plats gardés !~n", [length(User#users.recettes_aimees)]),
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
