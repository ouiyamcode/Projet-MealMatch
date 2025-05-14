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
  bdd:insert_recipe(#recipes{
    recipe_id = 27,
    nom = <<"Soupe Tom Yum">>,
    ingredients = [citronnelle, crevette, champignon, piment],
    prix = 7.90,
    site_source = <<"https://recette27.example.com">>,
    score = [0, 1, 5, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 28,
    nom = <<"Gaufres au caramel">>,
    ingredients = [oeuf, lait, caramel, farine],
    prix = 4.80,
    site_source = <<"https://recette28.example.com">>,
    score = [5, 1, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 29,
    nom = <<"Buddha Bowl">>,
    ingredients = [quinoa, avocat, pois_chiches, carotte],
    prix = 6.75,
    site_source = <<"https://recette29.example.com">>,
    score = [2, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 30,
    nom = <<"Poulet Tikka Masala">>,
    ingredients = [poulet, yaourt, tomate, epices],
    prix = 9.10,
    site_source = <<"https://recette30.example.com">>,
    score = [1, 4, 4, 3, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 31,
    nom = <<"Salade grecque">>,
    ingredients = [tomate, concombre, feta, olive],
    prix = 4.90,
    site_source = <<"https://recette31.example.com">>,
    score = [1, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 32,
    nom = <<"Porridge aux fruits rouges">>,
    ingredients = [flocons_avoine, lait, fruits_rouges],
    prix = 3.75,
    site_source = <<"https://recette32.example.com">>,
    score = [4, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 33,
    nom = <<"Croque-Monsieur">>,
    ingredients = [pain, jambon, fromage, beurre],
    prix = 5.20,
    site_source = <<"https://recette33.example.com">>,
    score = [1, 3, 0, 4, 1, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 34,
    nom = <<"Smoothie banane-kiwi">>,
    ingredients = [banane, kiwi, yaourt, miel],
    prix = 3.20,
    site_source = <<"https://recette34.example.com">>,
    score = [5, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 35,
    nom = <<"Tacos végétarien">>,
    ingredients = [tortilla, haricot_noir, poivron, mais],
    prix = 6.60,
    site_source = <<"https://recette35.example.com">>,
    score = [2, 3, 2, 2, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 36,
    nom = <<"Poulet rôti au thym">>,
    ingredients = [poulet, pomme_de_terre, thym],
    prix = 8.40,
    site_source = <<"https://recette36.example.com">>,
    score = [0, 4, 1, 3, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 37,
    nom = <<"Chili sin carne">>,
    ingredients = [haricots_rouges, tomate, poivron, epices],
    prix = 6.30,
    site_source = <<"https://recette37.example.com">>,
    score = [1, 2, 4, 2, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 38,
    nom = <<"Crêpes au miel">>,
    ingredients = [oeuf, lait, farine, miel],
    prix = 3.60,
    site_source = <<"https://recette38.example.com">>,
    score = [4, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 39,
    nom = <<"Banh Mi végétarien">>,
    ingredients = [baguette, tofu, carotte, coriandre],
    prix = 5.90,
    site_source = <<"https://recette39.example.com">>,
    score = [2, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 40,
    nom = <<"Tiramisu classique">>,
    ingredients = [mascarpone, cafe, biscuit, cacao],
    prix = 4.80,
    site_source = <<"https://recette40.example.com">>,
    score = [5, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 41,
    nom = <<"Bibimbap">>,
    ingredients = [riz, boeuf, oeuf, legumes],
    prix = 8.70,
    site_source = <<"https://recette41.example.com">>,
    score = [1, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 42,
    nom = <<"Gratin de courgettes">>,
    ingredients = [courgette, creme, fromage],
    prix = 4.95,
    site_source = <<"https://recette42.example.com">>,
    score = [1, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 43,
    nom = <<"Poké bowl saumon">>,
    ingredients = [riz, saumon, edamame, avocat],
    prix = 9.10,
    site_source = <<"https://recette43.example.com">>,
    score = [2, 3, 1, 2, 0, 0, 0, 0, 0, 4, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 44,
    nom = <<"Crumble aux fruits">>,
    ingredients = [pomme, poire, farine, beurre],
    prix = 4.60,
    site_source = <<"https://recette44.example.com">>,
    score = [4, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 45,
    nom = <<"Riz cantonais">>,
    ingredients = [riz, oeuf, petit_pois, jambon],
    prix = 5.30,
    site_source = <<"https://recette45.example.com">>,
    score = [1, 2, 0, 2, 1, 0, 2, 0, 0, 0, 0, 0, 0, 1, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 46,
    nom = <<"Bowl énergie au sarrasin">>,
    ingredients = [sarrasin, avocat, oeuf, betterave],
    prix = 6.90,
    site_source = <<"https://recette46.example.com">>,
    score = [2, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 47,
    nom = <<"Pizza 4 fromages">>,
    ingredients = [mozzarella, bleu, emmental, parmesan],
    prix = 7.20,
    site_source = <<"https://recette47.example.com">>,
    score = [1, 4, 0, 5, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 48,
    nom = <<"Soupe froide de concombre">>,
    ingredients = [concombre, yaourt, menthe],
    prix = 3.90,
    site_source = <<"https://recette48.example.com">>,
    score = [0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 49,
    nom = <<"Wok de légumes croquants">>,
    ingredients = [brocoli, carotte, soja, sésame],
    prix = 6.10,
    site_source = <<"https://recette49.example.com">>,
    score = [1, 2, 3, 2, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 50,
    nom = <<"Muffins chocolat-noisette">>,
    ingredients = [farine, chocolat, noisette, beurre],
    prix = 4.10,
    site_source = <<"https://recette50.example.com">>,
    score = [5, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 51,
    nom = <<"Wrap végétalien houmous-avocat">>,
    ingredients = [tortilla, houmous, avocat, salade],
    prix = 6.40,
    site_source = <<"https://recette51.example.com">>,
    score = [2, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 52,
    nom = <<"Omelette aux fines herbes">>,
    ingredients = [oeuf, persil, ciboulette],
    prix = 3.10,
    site_source = <<"https://recette52.example.com">>,
    score = [0, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 53,
    nom = <<"Tarte salée poireaux-chèvre">>,
    ingredients = [pate, poireau, chevre],
    prix = 6.70,
    site_source = <<"https://recette53.example.com">>,
    score = [1, 3, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 54,
    nom = <<"Salade de lentilles corail">>,
    ingredients = [lentilles, tomate, oignon, citron],
    prix = 4.80,
    site_source = <<"https://recette54.example.com">>,
    score = [2, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 55,
    nom = <<"Poulet coco curry">>,
    ingredients = [poulet, lait_coco, curry, riz],
    prix = 8.25,
    site_source = <<"https://recette55.example.com">>,
    score = [1, 3, 4, 3, 0, 5, 0, 0, 0, 0, 0, 0, 0, 1, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 56,
    nom = <<"Gratin vegan de patate douce">>,
    ingredients = [patate_douce, lait_vegetal, levure_nutritionnelle],
    prix = 5.95,
    site_source = <<"https://recette56.example.com">>,
    score = [3, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),
  bdd:insert_recipe(#recipes{
    recipe_id = 57,
    nom = <<"Gnocchis sauce tomate">>,
    ingredients = [gnocchis, tomate, basilic],
    prix = 5.30,
    site_source = <<"https://recette57.example.com">>,
    score = [2, 3, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 58,
    nom = <<"Clafoutis cerise">>,
    ingredients = [cerise, lait, oeuf, farine],
    prix = 4.00,
    site_source = <<"https://recette58.example.com">>,
    score = [5, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 59,
    nom = <<"Steak de lentilles">>,
    ingredients = [lentilles, oignon, ail, farine],
    prix = 5.50,
    site_source = <<"https://recette59.example.com">>,
    score = [1, 3, 1, 2, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 60,
    nom = <<"Galette bretonne complète">>,
    ingredients = [galette_sarrasin, oeuf, jambon, fromage],
    prix = 6.20,
    site_source = <<"https://recette60.example.com">>,
    score = [1, 4, 0, 3, 1, 0, 3, 0, 0, 0, 0, 0, 0, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 61,
    nom = <<"Cake aux olives et feta">>,
    ingredients = [oeuf, farine, olive, feta],
    prix = 4.90,
    site_source = <<"https://recette61.example.com">>,
    score = [1, 3, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 62,
    nom = <<"Panini tomate-mozzarella">>,
    ingredients = [pain, tomate, mozzarella],
    prix = 5.00,
    site_source = <<"https://recette62.example.com">>,
    score = [1, 3, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 63,
    nom = <<"Riz au lait vanillé">>,
    ingredients = [riz, lait, sucre, vanille],
    prix = 3.60,
    site_source = <<"https://recette63.example.com">>,
    score = [4, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 64,
    nom = <<"Nouilles sautées aux légumes">>,
    ingredients = [nouilles, poivron, oignon, soja],
    prix = 6.30,
    site_source = <<"https://recette64.example.com">>,
    score = [2, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 65,
    nom = <<"Velouté de potimarron">>,
    ingredients = [potimarron, creme, oignon],
    prix = 4.70,
    site_source = <<"https://recette65.example.com">>,
    score = [1, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),

  bdd:insert_recipe(#recipes{
    recipe_id = 66,
    nom = <<"Grilled cheese végétarien">>,
    ingredients = [pain, fromage, beurre],
    prix = 4.20,
    site_source = <<"https://recette66.example.com">>,
    score = [1, 4, 0, 4, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    alternatives = [],
    avis_utilisateurs = [],
    norme_recette = 0.0
  }),


  io:format("Recettes insérées avec succès !~n").
