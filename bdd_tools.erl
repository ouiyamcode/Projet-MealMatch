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

  %% Tables à vider
  Tables = [users, recipes, reviews, user_scores, searches],
  lists:foreach(fun(T) ->
    case mnesia:clear_table(T) of
      {aborted, {no_exists, _}} ->
        io:format("⚠️  Table ~p n'existe pas, création en attente~n", [T]);
      {aborted, Reason} ->
        io:format("⚠️  Erreur clear ~p: ~p~n", [T, Reason]);
      _ ->
        io:format("✅ Table ~p vidée~n", [T])
    end
                end, Tables),

  %% Réinsertion des recettes
  bdd_data:init(),
  io:format("🔁 Recettes rechargées~n").

