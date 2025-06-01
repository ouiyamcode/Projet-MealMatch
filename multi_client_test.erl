-module(multi_client_test).
-export([start/1, simulate_client/1]).

start(N) when N > 0 ->
  lists:foreach(fun(I) -> spawn(?MODULE, simulate_client, [I]) end, lists:seq(1, N)).

simulate_client(Index) ->
  Id = list_to_atom("test_user_" ++ integer_to_list(Index)),
  case gen_tcp:connect({127,0,0,1}, 4040, [binary, {packet, 4}, {active, false}]) of
    {ok, Socket} ->
      gen_tcp:send(Socket, term_to_binary(Id)),
      handle_init(Socket),
      loop_reco(Socket, 0),
      gen_tcp:close(Socket),
      io:format("✅ Client ~p terminé~n", [Id]);
    {error, Reason} ->
      io:format("❌ Client ~p : erreur connexion - ~p~n", [Id, Reason])
  end.

handle_init(Socket) ->
  case gen_tcp:recv(Socket, 0, 2000) of
    {ok, Bin} ->
      case binary_to_term(Bin) of
        {demande_allergies} ->
          Allergies = [],
          gen_tcp:send(Socket, term_to_binary({allergies, Allergies})),
          handle_init(Socket);
        {profil, _State} ->
          ok;
        Other ->
          io:format("❓ Réponse inattendue : ~p~n", [Other])
      end;
    _ -> io:format("⚠️ Aucun message reçu à l'init~n")
  end.

loop_reco(Socket, 10) ->
  gen_tcp:send(Socket, term_to_binary({demarrer_reco})),
  recevoir(Socket);
loop_reco(Socket, N) ->
  gen_tcp:send(Socket, term_to_binary({demarrer_reco})),
  attendre_plat(Socket, N).

attendre_plat(Socket, N) ->
  case gen_tcp:recv(Socket, 0, 3000) of
    {ok, Bin} ->
      case catch binary_to_term(Bin) of
        {plat, Plat} ->
          Reactions = ["aime", "aime_pas", "garder"],
          RandomReaction = lists:nth(rand:uniform(length(Reactions)), Reactions),
          timer:sleep(rand:uniform(200)),
          gen_tcp:send(Socket, term_to_binary({reaction, RandomReaction})),
          attendre_plat(Socket, N + 1);
        {recommandation, _} ->
          gen_tcp:send(Socket, term_to_binary(continuer)),
          attendre_plat(Socket, N);
        {fin} ->
          ok;
        {continuer_choix} ->
          gen_tcp:send(Socket, term_to_binary(continuer)),
          attendre_plat(Socket, N);
        _ -> attendre_plat(Socket, N)
      end;
    _ -> ok
  end.

recevoir(Socket) ->
  case gen_tcp:recv(Socket, 0, 3000) of
    {ok, Bin} ->
      case catch binary_to_term(Bin) of
        {fin} -> ok;
        {continuer_choix} ->
          gen_tcp:send(Socket, term_to_binary(stop));
        _ -> recevoir(Socket)
      end;
    _ -> ok
  end.

