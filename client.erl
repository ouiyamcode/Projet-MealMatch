-module(client).
-export([start/0]).
-include("bdd.hrl").

start() ->
    {ok, Socket} = gen_tcp:connect("localhost", 4040, [binary, {packet, 0}, {active, false}]),
    io:format("✅ Connecté au serveur TCP~n"),
    UserId = demander_identifiant(),
    gen_tcp:send(Socket, term_to_binary(UserId)),
    boucle(Socket),
    gen_tcp:close(Socket).

demander_identifiant() ->
    Input = string:trim(io:get_line("Entrez votre identifiant (ex: user1): ")),
    list_to_atom(Input).

boucle(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
            case catch binary_to_term(Bin) of
                {'EXIT', Reason} ->
                    io:format("⚠️  Erreur de décodage : ~p~n", [Reason]),
                    boucle(Socket);
                Msg ->
                    traiter_msg(Socket, Msg),
                    boucle(Socket)
            end;
        {error, closed} ->
            io:format("🔌 Connexion fermée par le serveur.~n"),
            ok;
        {error, Reason} ->
            io:format("❗ Erreur inattendue : ~p~n", [Reason]),
            ok
    end.

traiter_msg(Socket, {profil, State}) ->
    afficher_profil(State);

traiter_msg(Socket, {demande_allergies}) ->
    Allergies = demander_allergies(),
    gen_tcp:send(Socket, term_to_binary({allergies, Allergies}));

traiter_msg(Socket, {plat, Plat}) ->
    afficher_plat(Plat),
    Reponse = demander_reaction(),
    gen_tcp:send(Socket, term_to_binary({reaction, Reponse}));

traiter_msg(_, {recommandation, none}) ->
    io:format("🔍 Aucune recommandation disponible.~n");

traiter_msg(_, {recommandation, #recipes{nom = Nom, score = Score}}) ->
    io:format("🎯 Recommandation finale : ~s avec score ~p~n", [Nom, Score]);

traiter_msg(_, {alerte_5, N}) ->
    io:format("⚠️  Attention : il ne reste que ~p plats disponibles.~n", [N]);

traiter_msg(Socket, {continuer_choix}) ->
    Reponse = io:get_line("Souhaitez-vous continuer ? (o/n): "),
    Clean = string:trim(string:lowercase(Reponse)),
    Msg = case Clean of
              "o" -> continuer;
              _ -> stop
          end,
    gen_tcp:send(Socket, term_to_binary(Msg));

traiter_msg(_, {fin}) ->
    io:format("🛑 Session terminée.~n"),
    halt();

traiter_msg(_, Autre) ->
    io:format("❓ Message inconnu : ~p~n", [Autre]).



demander_allergies() ->
    Ligne = io:get_line("Avez-vous des allergies ? (ex: gluten, arachides): "),
    Trim = string:trim(Ligne),
    Tokens = string:tokens(Trim, ", "),
    [list_to_atom(string:trim(T)) || T <- Tokens].

afficher_plat(#recipes{nom = Nom, score = Score}) ->
    io:format("🍽️  Plat proposé : ~s~n", [Nom]),
    io:format("📊 Score : ~p~n", [Score]).

demander_reaction() ->
    io:format("Que pensez-vous de ce plat ? [Entrée=aime | p=aime_pas | g=garder]~n"),
    Ligne = string:trim(string:lowercase(io:get_line("Réponse: "))),
    case Ligne of
        "" -> "aime";
        "p" -> "aime_pas";
        "g" -> "garder";
        _ -> Ligne
    end.

afficher_profil(State) ->
    Id = maps:get(id, State),
    Allergies = maps:get(allergies, State),
    Aimees = maps:get(recettes_aimees, State),
    io:format("~n===== Profil utilisateur (~p) =====~n", [Id]),
    io:format("🥗 Allergies         : ~p~n", [Allergies]),
    io:format("❤️ Recettes aimées  : ~p~n", [Aimees]),
    io:format("==================================~n~n").
