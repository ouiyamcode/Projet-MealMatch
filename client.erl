-module(client).
-export([start/0]).
-include("bdd.hrl").

start() ->
    %% Découverte du serveur
    IP = case decouvrir_ip_serveur() of
             {ok, DiscoveredIP} ->
                 io:format("✅ Connexion à l'IP détectée : ~p~n", [DiscoveredIP]),
                 DiscoveredIP;
             error ->
                 io:format("🔁 Aucun serveur trouvé, utilisation de localhost~n"),
                 {127,0,0,1}
         end,

    %% Connexion TCP
    case gen_tcp:connect(IP, 4040, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} ->
            io:format("✅ Connecté au serveur TCP à ~p:4040~n", [IP]),
            UserId = demander_identifiant(),
            gen_tcp:send(Socket, term_to_binary(UserId)),
            boucle(Socket),
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format("❌ Échec de la connexion au serveur : ~p~n", [Reason])
    end.

decouvrir_ip_serveur() ->
    %% Ouvrir un socket UDP temporaire
    {ok, Socket} = gen_udp:open(0, [binary, {broadcast, true}, {active, false}]),

    %% Message de découverte
    Message = <<"mealmatch_discover">>,
    BroadcastAddr = {255,255,255,255},
    Port = 5050,

    %% Envoyer le message en broadcast
    ok = gen_udp:send(Socket, BroadcastAddr, Port, Message),

    %% Attendre une réponse (1 seconde max)
    case gen_udp:recv(Socket, 0, 1000) of
        {ok, {_Host, _Port, <<"server:", IPBin/binary>>}} ->
            io:format("🔍 Serveur détecté à l’adresse : ~s~n", [IPBin]),
            IPStr = binary_to_list(IPBin),
            {ok, parse_ip(IPStr)};
        {error, timeout} ->
            io:format("⏱️  Découverte du serveur échouée (timeout)~n"),
            error
    end.

%% Convertit "172.16.200.201" → {172,16,200,201}
parse_ip(Str) ->
    [A,B,C,D] = [list_to_integer(S) || S <- string:tokens(Str, ".")],
    {A,B,C,D}.


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


traiter_msg(_Socket, {profil, State}) ->
    io:format("📥 Reçu : profil utilisateur~n"),
    afficher_profil(State);

traiter_msg(Socket, {demande_allergies}) ->
    io:format("📥 Reçu : demande_allergies~n"),
    Allergies = demander_allergies(),
    io:format("📤 Envoi : allergies = ~p~n", [Allergies]),
    gen_tcp:send(Socket, term_to_binary({allergies, Allergies}));

traiter_msg(Socket, {plat, Plat}) ->
    io:format("📥 Reçu : plat = ~p~n", [Plat#recipes.nom]),
    afficher_plat(Plat),
    Reponse = demander_reaction(),
    io:format("📤 Envoi : reaction = ~p~n", [Reponse]),
    gen_tcp:send(Socket, term_to_binary({reaction, Reponse}));

traiter_msg(_, {recommandation, none}) ->
    io:format("📥 Reçu : recommandation = none~n"),
    io:format("🔍 Aucune recommandation disponible.~n");

traiter_msg(_, {recommandation, #recipes{nom = Nom, score = Score}}) ->
    io:format("📥 Reçu : recommandation = ~s~n", [Nom]),
    io:format("🎯 Recommandation finale : ~s avec score ~p~n", [Nom, Score]);

traiter_msg(_, {alerte_5, N}) ->
    io:format("📥 Reçu : alerte_5 = ~p plats restants~n", [N]),
    io:format("⚠️  Attention : il ne reste que ~p plats disponibles.~n", [N]);

traiter_msg(Socket, {continuer_choix}) ->
    io:format("📥 Reçu : continuer_choix~n"),
    Reponse = io:get_line("Souhaitez-vous continuer ? (o/n): "),
    Clean = string:trim(string:lowercase(Reponse)),
    Msg = case Clean of
              "o" -> continuer;
              _ -> stop
          end,
    io:format("📤 Envoi : réponse continuer = ~p~n", [Msg]),
    gen_tcp:send(Socket, term_to_binary(Msg));

traiter_msg(_, {fin}) ->
    io:format("📥 Reçu : fin~n"),
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
    AimeesIds = maps:get(recettes_aimees, State),
    Noms = maps:get(noms_recettes_aimees, State, []),

    io:format("~n===== Profil utilisateur (~p) =====~n", [Id]),
    io:format("🥗 Allergies         : ~p~n", [Allergies]),
    io:format("🧾 Recettes aimées (IDs) : ~p~n", [AimeesIds]),
    io:format("❤️ Recettes aimées  : ~p~n", [Noms]),
    io:format("==================================~n~n").



