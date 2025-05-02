-module(bdd).
-compile([export_all]).

%% Records
-record(users, {
  user_id,
  mdp,
  choix_repas,
  restrictions_alimentaires,
  recettes_aimees,
  historique_recherches,
  score_global,
  norme_utilisateur
}).

-record(recipes, {
  recipe_id,
  nom,
  ingredients,
  prix,
  site_source,
  score,
  alternatives,
  avis_utilisateurs,
  norme_recette
}).

-record(critere, {
  critere_id,
  nom_critere,
  type
}).

-record(user_scores, {
  user_id,
  critere_id,
  score_utilisateur
}).

-record(searches, {
  search_id,
  user_id,
  filters,
  resultats,
  scores_recherche
}).

-record(reviews, {
  review_id,
  user_id,
  recipe_id,
  score,
  commentaire
}).

%% Création des tables individuellement (pas dynamiquement)
create_tables() ->
  mnesia:create_table(users, [
    {attributes, record_info(fields, users)},
    {type, set},
    {disc_copies, [node()]}
  ]),
  mnesia:create_table(recipes, [
    {attributes, record_info(fields, recipes)},
    {type, set},
    {disc_copies, [node()]}
  ]),
  mnesia:create_table(critere, [
    {attributes, record_info(fields, critere)},
    {type, set},
    {disc_copies, [node()]}
  ]),
  mnesia:create_table(user_scores, [
    {attributes, record_info(fields, user_scores)},
    {type, set},
    {disc_copies, [node()]}
  ]),
  mnesia:create_table(searches, [
    {attributes, record_info(fields, searches)},
    {type, set},
    {disc_copies, [node()]}
  ]),
  mnesia:create_table(reviews, [
    {attributes, record_info(fields, reviews)},
    {type, set},
    {disc_copies, [node()]}
  ]).

%% Fonctions utilitaires
insert(Record) ->
  mnesia:transaction(fun() -> mnesia:write(Record) end).

get(Tab, Id) ->
  mnesia:transaction(fun() ->
    case mnesia:read({Tab, Id}) of
      [] -> {error, not_found};
      [Res] -> {ok, Res}
    end
                     end).

insert_user(User) -> insert(User).
get_user(UserId) -> get(users, UserId).

insert_recipe(Recipe) -> insert(Recipe).
get_recipe(RecipeId) -> get(recipes, RecipeId).

insert_critere(Critere) -> insert(Critere).
get_critere(CritereId) -> get(critere, CritereId).

insert_user_score(Score) -> insert(Score).
insert_search(Search) -> insert(Search).
get_search(Id) -> get(searches, Id).

insert_review(Review) -> insert(Review).
get_review(Id) -> get(reviews, Id).
