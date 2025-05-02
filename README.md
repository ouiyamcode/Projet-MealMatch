# 🍽️ MealMatch

**MealMatch** est une application interactive écrite en **Erlang** qui propose des plats à un utilisateur, apprend de ses préférences, et lui recommande un plat personnalisé à la fin de la session.

---

## 🚀 Fonctionnalités

* Interaction en ligne de commande
* Gestion des préférences utilisateur (`aime`, `aime_pas`, `garder`)
* Système de recommandation basé sur des scores gustatifs (vecteurs)
* Persistance des données via **Mnesia**
* Affichage final des plats gardés

---

## ✅ Prérequis

* **Erlang/OTP ≥ 22**

    * Recommandé : OTP 24+
    * Testé sur Erlang 24 et 25
* Git
* Terminal

---

## 📦 Installation

### 1. Cloner le projet

```bash
git clone <url_du_repo>
cd MealMatch
```

### 2. Lancer Erlang

Dans le dossier du projet :

```bash
erl
```

---

## 💪 Première initialisation (une seule fois)

### 1. Créer le schéma Mnesia

```erlang
mnesia:create_schema([node()]).
```

### 2. Démarrer Mnesia

```erlang
mnesia:start().
```

### 3. Compiler les modules

```erlang
c(bdd).
c(bdd_data).
c(serveur).
```

### 4. Créer les tables Mnesia

```erlang
bdd:create_tables().
```

### 5. Insérer les recettes de base

```erlang
bdd_data:init().
```

Tu verras :

```
Recettes insérées avec succès !
```

---

## 👤 Utilisation

### Démarrer l'application :

```erlang
serveur:start().
```

### Fonctionnement :

* On te demande tes allergies (`gluten, arachides`, etc.)
* Tu reçois un plat à chaque itération
* Tu peux répondre par :

    * `"aime"` → ajoute ce plat aux scores positifs
    * `"aime_pas"` → ajoute ce plat aux scores négatifs
    * `"garder"` → ajoute ce plat à la liste des favoris

L’application s’arrête automatiquement :

* Après **10 interactions**
* Ou lorsque **tous les plats ont été vus**

---

## 📌 Fin de session

* Le serveur calcule une recommandation (en comparant les scores)
* Il affiche le plat recommandé (si possible)
* Il sauvegarde l'utilisateur dans la base
* Il affiche les plats que tu as **gardés** (`garder`)

---

## 💡 Tests manuels

### Voir un utilisateur enregistré :

```erlang
bdd:get_user(<<"user1">>).
```

### Voir ses recettes aimées :

```erlang
{atomic, {ok, U}} = bdd:get_user(<<"user1">>),
U#users.recettes_aimees.
```

---

## 🔄 Réinitialisation (optionnel)

### Réinitialiser la base :

```erlang
mnesia:stop().
```

Puis en ligne de commande Linux/macOS :

```bash
rm -rf Mnesia*
```

Et relancer :

```erlang
mnesia:create_schema([node()]).
mnesia:start().
bdd:create_tables().
bdd_data:init().
```

---

## 📚 Structure du projet

```
MealMatch/
│
├── bdd.erl         % Déclaration des tables et accès Mnesia
├── bdd_data.erl    % Données de test : recettes à insérer
├── serveur.erl     % Logique principale (interaction, état, recommandation)
├── bdd.hrl         % Records partagés (si utilisé)
└── README.md       % Ce fichier
```

---

## 🙋 Support

En cas de problème, bug ou suggestion :

* Ouvrir une issue si le projet est sur GitHub/GitLab
* Contacter le développeur via le canal de communication du projet

---

Bon appétit avec MealMatch ! 😋
