-module(simulation_test).

-export([start/0]).

start() ->
    simulation_carrot_sup:start_link(),
    simulation_rabbit_sup:start_link(),
    simulation_wolf:start_link().