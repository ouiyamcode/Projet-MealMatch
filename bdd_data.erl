-module(bdd_data).
-export([init/0]).
-include("bdd.hrl").

init() ->
  bdd:insert_recipe(#recipes{
    recipe_id = 1,
    nom = <<"Pizza Margherita">>,
    ingredients = [tomate, mozzarella, basilic, farine],
    prix = 6.0,
    site_source = <<"https://pizza.example.com">>,
    %% [sucre, sale, epice, gras, boeuf, poulet, porc, agneau, veau, poisson, fruits_mer, végétarien, vegan, sans_gluten, sans_lactose]
    score = [1, 3, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 2,
    nom = <<"Tarte aux pommes">>,
    ingredients = [pomme, sucre, pate_feuilletee],
    prix = 4.5,
    site_source = <<"https://dessert.example.com">>,
    score = [4, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 3,
    nom = <<"Pad Thaï">>,
    ingredients = [nouilles, soja, cacahuete, crevette],
    prix = 7.0,
    site_source = <<"https://thai.example.com">>,
    score = [1, 2, 4, 2, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 4,
    nom = <<"Salade César">>,
    ingredients = [laitue, poulet, parmesan, croutons],
    prix = 5.0,
    site_source = <<"https://salade.example.com">>,
    score = [0, 2, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 5,
    nom = <<"Curry de légumes">>,
    ingredients = [carotte, pomme_de_terre, lait_coco, curry],
    prix = 5.5,
    site_source = <<"https://curry.example.com">>,
    score = [1, 1, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 6,
    nom = <<"Fondant au chocolat">>,
    ingredients = [chocolat, beurre, sucre, oeuf],
    prix = 3.5,
    site_source = <<"https://choco.example.com">>,
    score = [5, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 7,
    nom = <<"Spaghetti Bolognaise">>,
    ingredients = [pates, boeuf, tomate],
    prix = 5.03,
    site_source = <<"https://recette7.example.com">>,
    score = [1, 2, 1, 5, 3, 2, 5, 0, 0, 2, 1, 5, 5, 2, 3],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 8,
    nom = <<"Quiche Lorraine">>,
    ingredients = [oeuf, lardon, creme, pate],
    prix = 8.33,
    site_source = <<"https://recette8.example.com">>,
    score = [5, 1, 1, 3, 3, 3, 2, 4, 1, 4, 4, 3, 3, 4, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 9,
    nom = <<"Soupe Miso">>,
    ingredients = [tofu, algue, miso],
    prix = 7.33,
    site_source = <<"https://recette9.example.com">>,
    score = [0, 0, 1, 3, 2, 1, 3, 0, 2, 2, 2, 3, 1, 2, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 10,
    nom = <<"Falafel">>,
    ingredients = [pois_chiches, ail, coriandre],
    prix = 9.85,
    site_source = <<"https://recette10.example.com">>,
    score = [5, 1, 4, 0, 5, 4, 5, 0, 3, 2, 4, 0, 0, 0, 3],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 11,
    nom = <<"Burger Végétarien">>,
    ingredients = [galette_vegetale, pain, salade],
    prix = 8.04,
    site_source = <<"https://recette11.example.com">>,
    score = [4, 5, 1, 4, 2, 2, 5, 1, 2, 0, 1, 0, 2, 2, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 12,
    nom = <<"Gratin Dauphinois">>,
    ingredients = [pomme_de_terre, creme, fromage],
    prix = 4.17,
    site_source = <<"https://recette12.example.com">>,
    score = [2, 3, 0, 5, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 13,
    nom = <<"Ramen">>,
    ingredients = [nouilles, bouillon, porc],
    prix = 6.79,
    site_source = <<"https://recette13.example.com">>,
    score = [3, 4, 5, 4, 0, 0, 5, 0, 0, 1, 0, 0, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 14,
    nom = <<"Biryani">>,
    ingredients = [riz, poulet, epices],
    prix = 9.34,
    site_source = <<"https://recette14.example.com">>,
    score = [1, 3, 5, 3, 0, 5, 0, 0, 0, 0, 0, 0, 0, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 15,
    nom = <<"Tajine d’agneau">>,
    ingredients = [agneau, abricot, amande],
    prix = 9.69,
    site_source = <<"https://recette15.example.com">>,
    score = [3, 3, 2, 4, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 16,
    nom = <<"Poke Bowl">>,
    ingredients = [riz, thon, avocat],
    prix = 6.55,
    site_source = <<"https://recette16.example.com">>,
    score = [2, 3, 1, 2, 0, 0, 0, 0, 0, 5, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 17,
    nom = <<"Lasagnes Végétariennes">>,
    ingredients = [pates, legumes, bechamel],
    prix = 5.78,
    site_source = <<"https://recette17.example.com">>,
    score = [2, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 18,
    nom = <<"Tofu sauté">>,
    ingredients = [tofu, soja, gingembre],
    prix = 7.61,
    site_source = <<"https://recette18.example.com">>,
    score = [1, 1, 3, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 19,
    nom = <<"Paëlla">>,
    ingredients = [riz, fruits_de_mer, poivron],
    prix = 8.92,
    site_source = <<"https://recette19.example.com">>,
    score = [1, 4, 3, 3, 0, 0, 0, 0, 0, 1, 5, 0, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 20,
    nom = <<"Pizza Végétarienne">>,
    ingredients = [tomate, mozzarella, champignon],
    prix = 6.22,
    site_source = <<"https://recette20.example.com">>,
    score = [2, 3, 1, 3, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 21,
    nom = <<"Tartiflette">>,
    ingredients = [pomme_de_terre, lardon, reblochon],
    prix = 7.85,
    site_source = <<"https://recette21.example.com">>,
    score = [0, 4, 0, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 22,
    nom = <<"Salade de quinoa">>,
    ingredients = [quinoa, feta, concombre],
    prix = 4.74,
    site_source = <<"https://recette22.example.com">>,
    score = [1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 23,
    nom = <<"Risotto au champignon">>,
    ingredients = [riz, champignon, parmesan],
    prix = 6.96,
    site_source = <<"https://recette23.example.com">>,
    score = [2, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 24,
    nom = <<"Wrap au poulet">>,
    ingredients = [poulet, tortilla, salade],
    prix = 5.48,
    site_source = <<"https://recette24.example.com">>,
    score = [1, 3, 1, 2, 0, 5, 0, 0, 0, 0, 0, 0, 0, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 25,
    nom = <<"Kebab">>,
    ingredients = [agneau, pain, oignon],
    prix = 6.89,
    site_source = <<"https://recette25.example.com">>,
    score = [1, 4, 4, 4, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 26,
    nom = <<"Couscous">>,
    ingredients = [semoule, legumes, merguez],
    prix = 7.21,
    site_source = <<"https://recette26.example.com">>,
    score = [1, 3, 3, 3, 0, 0, 4, 0, 0, 0, 0, 0, 0, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  io:format("Recettes insérées avec succès !~n").
