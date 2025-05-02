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
    score = [1,3,0],
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
    score = [4,0,1],
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
    score = [2,2,3],
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
    score = [0,2,1],
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
    score = [1,1,4],
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
    score = [5,0,0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  io:format("Recettes insérées avec succès !~n").
