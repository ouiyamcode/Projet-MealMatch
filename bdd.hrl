%% === Définition fixe des critères utilisés dans les vecteurs de score ===
%% Ordre : [Sucré, Salé, Épicé, Gras, Bœuf, Poulet, Porc, Agneau, Veau, Poisson, Fruits de mer, Végétarien, Vegan, Sans gluten, Sans lactose]
-define(CRITERES_SCORE, [
  sucre, sale, epice, gras,
  viande_boeuf, viande_poulet, viande_porc, viande_agneau, viande_veau,
  poisson, fruits_de_mer, vegetarien, vegan,
  sans_gluten, sans_lactose
]).

%% === Record pour la table des utilisateurs ===
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

%% === Record pour la table des recettes ===
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

%% === Record pour les critères ===
-record(critere, {
  critere_id,
  nom_critere,
  type
}).

%% === Record pour les scores utilisateurs par critère ===
-record(user_scores, {
  user_id,
  critere_id,
  score_utilisateur
}).

%% === Record pour les recherches ===
-record(searches, {
  search_id,
  user_id,
  filters,
  resultats,
  scores_recherche
}).

%% === Record pour les avis ===
-record(reviews, {
  review_id,
  user_id,
  recipe_id,
  score,
  commentaire
}).
