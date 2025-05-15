-module(bdd_tools).
-export([init_bdd/0, reset_bdd/0]).

init_bdd() ->
  mnesia:start(),
  io:format("📦 Mnesia démarrée~n"),

  %% Créer les tables si elles n'existent pas
  bdd:create_tables(),
  io:format("📁 Tables créées si besoin~n"),

  %% Insérer les données recettes
  bdd_data:init(),
  io:format("🍽️  Recettes insérées depuis bdd_data~n").


reset_bdd() ->
  mnesia:start(),

  %% Créer les tables si elles n'existent pas
  bdd:create_tables(),

  %% ⚠️ Attendre qu'elles soient bien actives (important !)
  Tables = [users, recipes, reviews, user_scores, searches],
  lists:foreach(fun(T) ->
    case mnesia:wait_for_tables([T], 5000) of
      ok -> ok;
      Error -> io:format("⚠️  Timeout table ~p: ~p~n", [T, Error])
    end
                end, Tables),

  %% Maintenant vider les tables
  lists:foreach(fun(T) ->
    case mnesia:clear_table(T) of
      {aborted, Reason} ->
        io:format("⚠️  Erreur clear ~p: ~p~n", [T, Reason]);
      _ ->
        io:format("✅ Table ~p vidée~n", [T])
    end
                end, Tables),

  %% Réinsertion des recettes
  bdd_data:init(),
  io:format("🔁 Recettes rechargées~n").


