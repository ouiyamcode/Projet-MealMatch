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
      IP = get_local_ip(),
      IPString = inet_parse:ntoa(IP),
      io:format("📨 Requête de découverte reçue, IP du serveur : ~s~n", [IPString]),
      Response = <<"server:", (list_to_binary(IPString))/binary>>,
      gen_udp:send(Socket, Host, Port, Response),
      io:format("✅ Réponse envoyée au client : ~p~n", [Response]),
      loop(Socket);
    Other ->
      io:format("⚠️  Message inconnu reçu : ~p~n", [Other]),
      loop(Socket)
  end.

get_local_ip() ->
  {ok, Addrs} = inet:getifaddrs(),
  IPs = [Addr || {_Iface, Props} <- Addrs,
    {addr, Addr = {A,B,C,D}} <- Props,
    A =/= 127],
  case IPs of
    [First | _] -> First;
    [] -> {127,0,0,1}
  end.


