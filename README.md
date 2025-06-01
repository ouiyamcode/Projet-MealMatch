# 🍽️ MealMatch – Guide utilisateur final

**MealMatch** est une application interactive écrite en **Erlang**, qui recommande des plats en fonction de vos goûts. Elle apprend de vos réactions (j'aime, j'aime pas, garder) et propose un plat personnalisé à la fin.

---

## ✅ Prérequis

- **Erlang/OTP ≥ 24** (testé avec OTP 24 et 25)
- Terminal sur Linux/macOS ou Windows (avec WSL conseillé)
- Tous les fichiers du projet fournis (pas besoin de Git)

---

## 📦 Installation (serveur)

### 1. Démarrer Erlang dans le dossier du projet

```bash
cd MealMatch
erl
```

### 2. Initialiser Mnesia (à faire une seule fois)

```erlang
mnesia:create_schema([node()]).
mnesia:start().
c(bdd).
c(bdd_data).
c(bdd_tools).
c(serveur).
c(serveur_discovery).
c(client).
```

### 3. Créer les tables et insérer les recettes

Option 1 : à la main
```erlang
bdd:create_tables().
bdd_data:init().
```

Option 2 : tout en un avec `bdd_tools`
```erlang
bdd_tools:init_bdd().
```

Tu devrais voir : 📁 Tables créées et 🍽️  Recettes insérées.

### 4. Lancer le serveur

```erlang
serveur:start().
```

Cela démarre :
- un serveur TCP (port 4040)
- un serveur UDP (port 5050) pour découverte automatique

Le serveur est prêt à recevoir des clients.

---

## 👤 Utilisation (client)

Chaque utilisateur lance **le client sur sa propre machine** (si possible dans un terminal séparé).

### 1. Compiler et lancer le client

Dans le dossier du projet, lancer Erlang puis :

```erlang
c(client).
client:start().
```

### 2. Navigation utilisateur

Le client propose :
- Entrer un identifiant (ex: `user42`)
- Répondre à une éventuelle **demande d’allergies** (ex: `gluten, lactose`)
- Accéder à un **menu principal** avec options :
  - Lancer la recommandation
  - Voir son profil
  - Voir ses plats enregistrés
  - Réinitialiser son profil
  - Quitter

---

## 🔄 Session de recommandation

1. Tu reçois des plats proposés un à un
2. À chaque plat, tu peux répondre :
   - **Entrée** → j'aime
   - **p** → j'aime pas
   - **g** → garder

Après **10 interactions**, une recommandation finale te sera proposée.

Tu peux ensuite choisir de continuer ou arrêter.

---

## 📌 Sauvegarde automatique

Chaque utilisateur est enregistré automatiquement :
- Allergies
- Recettes aimées ou gardées
- Score global et préférences

À la prochaine connexion, tu retrouves ton profil.

---

## 💥 Problèmes fréquents

- 🔐 **Connexion échoue ?** Vérifie que le **pare-feu** autorise les ports `4040 (TCP)` et `5050 (UDP)`
- 🌐 **Pas de découverte automatique ?** Entrez l'IP du serveur manuellement dans le code si nécessaire
- ❗ **Plats épuisés ?** Tu verras un message d'alerte et la recommandation se lancera automatiquement

---

## 🧪 Réinitialiser la base (serveur uniquement)

### Option simple avec `bdd_tools`
```erlang
bdd_tools:reset_bdd().
```

### Option manuelle :
```erlang
mnesia:stop().
```
Puis en terminal :
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

## 📁 Structure des fichiers

```
MealMatch/
│
├── bdd.erl              % Gestion base de données Mnesia
├── bdd_data.erl         % Insertion recettes
├── bdd_tools.erl        % Utilitaires pour init/reset BDD
├── serveur.erl          % Logique serveur + matching
├── serveur_discovery.erl % Découverte UDP
├── client.erl           % Interface ligne de commande utilisateur
├── bdd.hrl              % Définition des records
```

---

## 🌿 Bon appétit !

MealMatch vous aide à décider quoi manger intelligemment 😋
