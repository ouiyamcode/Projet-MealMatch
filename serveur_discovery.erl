-module(serveur_discovery).
-export([start/0]).

start() ->
  {ok, Socket} = gen_udp:open(5050, [
    binary, {active, true}, {ip, {0,0,0,0}}, {reuseaddr, true}
  ]),
  io:format("📡 Serveur UDP de découverte lancé sur le port 5050 (en attente de clients)~n"),
  loop(Socket).

loop(Socket) ->
  receive
    {udp, Socket, Host, Port, <<"mealmatch_discover">>} ->
      IPString = inet_parse:ntoa(Host),
      Response = <<"server:", IPString/binary>>,
      io:format("📨 Requête de découverte reçue de ~p:~p~n", [IPString, Port]),
      ok = gen_udp:send(Socket, Host, Port, Response),
      io:format("✅ Réponse envoyée au client : ~p~n", [Response]),
      loop(Socket);
    Other ->
      io:format("⚠️  Reçu un message inconnu : ~p~n", [Other]),
      loop(Socket)
  end.
