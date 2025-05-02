-module(client).
-export([]).

-define(INITIAL_CLIENT, #{
  id => user1,
  allergies => [],
  scores => [0, 0, 0],
  nb_interactions => 0,
  connected => false,
  recettes_gardees => []
}).

