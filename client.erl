-module(client).
-export([start/0]).

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
                    io:format("⚠️  Erreur de décodage (binary_to_term) : ~p~n", [Reason]),
                    boucle(Socket);
                Msg ->
                    io:format("📩 Message reçu : ~p~n", [Msg]),
                    traiter_msg(Socket, Msg),
                    boucle(Socket)
            end;

        {error, einval} ->
            io:format("❌ Erreur : mauvais format binaire reçu (einval).~n"),
            boucle(Socket);

        {error, closed} ->
            io:format("🔌 Connexion fermée par le serveur.~n");

        Error ->
            io:format("❗ Erreur inattendue : ~p~n", [Error]),
            boucle(Socket)
    end.


afficher_profil(State) ->
    Id = maps:get(id, State),
    Allergies = maps:get(allergies, State),
    NbLikes = length(maps:get(recettes_aimees, State)),
    Score = length(maps:get(recettes_aimees, State)),
    Norme = calculer_norme(maps:get(scores_pos, State), maps:get(scores_neutres, State)),
    io:format("~n===== Profil utilisateur (~p) =====~n", [Id]),
    io:format("🥗 Allergies         : ~p~n", [Allergies]),
    io:format("❤️ Recettes aimées  : ~p~n", [NbLikes]),
    io:format("📊 Score global     : ~p~n", [Score]),
    io:format("📐 Norme utilisateur: ~.2f~n", [Norme]),
    io:format("==================================~n~n").

demander_allergies() ->
    Ligne = io:get_line("Avez-vous des allergies ? (ex: gluten, arachides): "),
    Trim = string:trim(Ligne),
    Tokens = string:tokens(Trim, ", "),
    [list_to_atom(string:trim(T)) || T <- Tokens].

calculer_norme(Pos, Neutre) ->
    Scores = moyenne_ponderee(Pos, Neutre),
    math:sqrt(lists:sum([X * X || X <- Scores])).

moyenne_ponderee(Pos, Neutre) ->
    TotalPoids = length(Pos) + (length(Neutre) div 2),
    Total = lists:foldl(fun additionner_vecteurs/2, lists:duplicate(15, 0), Pos ++ Neutre),
    case TotalPoids of
        0 -> lists:duplicate(15, 0);
        _ -> [X div TotalPoids || X <- Total]
    end.

additionner_vecteurs([], []) -> [];
additionner_vecteurs([A|As], [B|Bs]) -> [A + B | additionner_vecteurs(As, Bs)].

traiter_msg(Socket, {profil, State}) ->
    afficher_profil(State);
traiter_msg(Socket, {demande_allergies}) ->
    Allergies = demander_allergies(),
    gen_tcp:send(Socket, term_to_binary({allergies, Allergies}));
traiter_msg(_, {fin}) ->
    io:format("🛑 Session terminée.~n"),
    ok;
traiter_msg(_, Autre) ->
    io:format("❓ Message inconnu : ~p~n", [Autre]).
