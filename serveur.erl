-module(serveur).
-export([start/0]).
-include("bdd.hrl").

%% === État initial ===
initial_state(UserId) ->
  #{id => UserId,
    allergies => [],
    scores_pos => [],
    scores_neg => [],
    scores_neutres => [],
    nb_interactions => 0,
    deja_connecte => false,
    plats_tries => [],
    alerte_faite => false,
    recettes_aimees => []}.

start() ->
  mnesia:start(),
  %% Lancer le serveur UDP de découverte
  spawn(fun serveur_discovery:start/0),
  %% Lancer le serveur TCP
  {ok, ListenSocket} = gen_tcp:listen(4040, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
  io:format("✅ Serveur TCP en écoute sur le port 4040...~n"),
  accept(ListenSocket).

accept(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> handle_client(Socket) end),
  accept(ListenSocket).

handle_client(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Bin} ->
      UserId = binary_to_term(Bin),
      State = charger_ou_initialiser_utilisateur(UserId),
      case maps:get(deja_connecte, State) of
        true ->
          gen_tcp:send(Socket, term_to_binary({profil, State})),
          boucle(Socket, State);
        false ->
          gen_tcp:send(Socket, term_to_binary({demande_allergies})),
          boucle(Socket, State)
      end;
    {error, closed} ->
      ok
  end.

boucle(Socket, State) ->
  case maps:get(deja_connecte, State) of
    false ->
      case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
          {allergies, Liste} = binary_to_term(Bin),
          NewState = State#{allergies => Liste, deja_connecte => true},
          gen_tcp:send(Socket, term_to_binary({profil, NewState})),
          boucle(Socket, NewState);
        {error, closed} ->
          State
      end;
    true ->
      case maps:get(nb_interactions, State) of
        10 ->
          Reco = trouver_recommandation(State),
          gen_tcp:send(Socket, term_to_binary({recommandation, Reco})),
          sauvegarder_utilisateur(State),
          gen_tcp:send(Socket, term_to_binary({continuer_choix})),
          case gen_tcp:recv(Socket, 0) of
            {ok, Bin2} ->
              case binary_to_term(Bin2) of
                continuer ->
                  boucle(Socket, State#{nb_interactions => 0});
                _ ->
                  gen_tcp:send(Socket, term_to_binary({fin})),
                  ok
              end;
            {error, closed} -> ok
          end;
        _ ->
          envoyer_plat(Socket, State)
      end
  end.

envoyer_plat(Socket, State) ->
  Tried = [R#recipes.recipe_id || R <- maps:get(plats_tries, State)],
  Allergies = maps:get(allergies, State),
  Recettes = [R || R <- charger_recettes(), not lists:member(R#recipes.recipe_id, Tried), not contient_allergene(R, Allergies)],
  case Recettes of
    [] ->
      gen_tcp:send(Socket, term_to_binary({fin})),
      sauvegarder_utilisateur(State);
    _ ->
      AlerteFaite = maps:get(alerte_faite, State),
      if
        length(Recettes) =< 5 andalso AlerteFaite =:= false ->
          gen_tcp:send(Socket, term_to_binary({alerte_5, length(Recettes)})),
          Reco = trouver_recommandation(State),
          gen_tcp:send(Socket, term_to_binary({recommandation, Reco})),
          envoyer_plat(Socket, State#{alerte_faite => true});
        true ->
          rand:seed(exsplus, {erlang:monotonic_time(), erlang:unique_integer(), erlang:phash2(self())}),
          Index = rand:uniform(length(Recettes)),
          Plat = lists:nth(Index, Recettes),
          gen_tcp:send(Socket, term_to_binary({plat, Plat})),
          recevoir_reaction(Socket, State, Plat)
      end
  end.


recevoir_reaction(Socket, State, Plat) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Bin} ->
      {reaction, Rep} = binary_to_term(Bin),
      Nouveau = maj_etat(State, Plat, Rep),
      boucle(Socket, Nouveau);
    {error, closed} ->
      ok
  end.

%% === Chargement utilisateur ===
charger_ou_initialiser_utilisateur(UserId) ->
  F = fun() ->
    case mnesia:read({users, UserId}) of
      [User] ->
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
        initial_state(UserId)
    end
      end,
  {atomic, Res} = mnesia:transaction(F),
  Res.

%% === BDD recettes ===
charger_recettes() ->
  {atomic, Recs} = mnesia:transaction(
    fun() ->
      mnesia:match_object(#recipes{recipe_id = '_', nom = '_', ingredients = '_',
        prix = '_', site_source = '_', score = '_',
        alternatives = '_', avis_utilisateurs = '_',
        norme_recette = '_'})
    end),
  Recs.

contient_allergene(#recipes{ingredients = I}, Allergies) ->
  lists:any(fun(A) -> lists:member(A, I) end, Allergies).

%% === Réaction ===
maj_etat(State, Plat, Reponse) ->
  Score = Plat#recipes.score,
  Id = Plat#recipes.recipe_id,
  N = maps:get(nb_interactions, State) + 1,
  Tried = [Plat | maps:get(plats_tries, State)],
  case Reponse of
    "aime" ->
      State#{scores_pos => [Score | maps:get(scores_pos, State)],
        nb_interactions => N,
        plats_tries => Tried};
    "aime_pas" ->
      State#{scores_neg => [Score | maps:get(scores_neg, State)],
        nb_interactions => N,
        plats_tries => Tried};
    "garder" ->
      State#{scores_neutres => [Score | maps:get(scores_neutres, State)],
        recettes_aimees => [Id | maps:get(recettes_aimees, State)],
        nb_interactions => N,
        plats_tries => Tried};
    _ -> State
  end.

%% === Sauvegarde utilisateur ===
sauvegarder_utilisateur(State) ->
  Id = maps:get(id, State),
  User = #users{
    user_id = Id,
    mdp = <<"">>,
    choix_repas = 1,
    restrictions_alimentaires = maps:get(allergies, State),
    recettes_aimees = maps:get(recettes_aimees, State),
    historique_recherches = [],
    score_global = calculer_score_global(State),
    norme_utilisateur = calculer_norme_utilisateur(State)
  },
  bdd:insert_user(User).

%% === Recommandation ===
trouver_recommandation(State) ->
  Pos = moyenne_ponderee(maps:get(scores_pos, State), maps:get(scores_neutres, State)),
  Neg = moyenne_vecteurs(maps:get(scores_neg, State)),
  Tried = [P#recipes.recipe_id || P <- maps:get(plats_tries, State)],
  Aimees = maps:get(recettes_aimees, State),
  Recs = [R || R <- charger_recettes(), not lists:member(R#recipes.recipe_id, Tried), not lists:member(R#recipes.recipe_id, Aimees)],
  case Recs of
    [] -> none;
    _ ->
      min_by(fun(Plat) ->
        Score = Plat#recipes.score,
        SimPos = similarite_cosinus(Score, Pos),
        SimNeg = similarite_cosinus(Score, Neg),
        SimNeg - SimPos
             end, Recs)
  end.

%% === Utilitaires math ===
calculer_score_global(State) ->
  length(maps:get(recettes_aimees, State)).

calculer_norme_utilisateur(State) ->
  V = moyenne_ponderee(maps:get(scores_pos, State), maps:get(scores_neutres, State)),
  math:sqrt(lists:sum([X*X || X <- V])).

moyenne_vecteurs(Listes) ->
  case Listes of
    [] -> lists:duplicate(15, 0);
    _ ->
      N = length(Listes),
      Total = lists:foldl(fun additionner_vecteurs/2, lists:duplicate(15,0), Listes),
      [X div N || X <- Total]
  end.

moyenne_ponderee(Pos, Neutre) ->
  TotalPoids = length(Pos) + (length(Neutre) div 2),
  case TotalPoids of
    0 -> lists:duplicate(15, 0);
    _ ->
      Total = lists:foldl(fun additionner_vecteurs/2, lists:duplicate(15, 0), Pos ++ Neutre),
      [X div TotalPoids || X <- Total]
  end.

additionner_vecteurs([], []) -> [];
additionner_vecteurs([A|As], [B|Bs]) -> [A + B | additionner_vecteurs(As, Bs)].

similarite_cosinus(V1, V2) ->
  Produit = lists:sum([A * B || {A, B} <- lists:zip(V1, V2)]),
  Norm1 = math:sqrt(lists:sum([A * A || A <- V1])),
  Norm2 = math:sqrt(lists:sum([B * B || B <- V2])),
  case {Norm1, Norm2} of
    {0.0, _} -> 0.0;
    {_, 0.0} -> 0.0;
    _ -> Produit / (Norm1 * Norm2)
  end.

min_by(F, [H|T]) ->
  min_by(F, T, H, F(H)).

min_by(_, [], Best, _) -> Best;
min_by(F, [H|T], Best, ValBest) ->
  Val = F(H),
  case Val < ValBest of
    true -> min_by(F, T, H, Val);
    false -> min_by(F, T, Best, ValBest)
  end.
