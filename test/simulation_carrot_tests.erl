-module(simulation_carrot_tests).
-include_lib("eunit/include/eunit.hrl").
-export([start_link/0, kill_carrot/1]).

carrot_test_() ->
    [{setup, fun start_link/0, fun kill_carrot/1, fun carrot_init/0},
    {setup, fun start_link/0, fun kill_carrot/1, fun test_two/0}].

start_link() ->
    simulation_carrot_sup:start_link(1),
    [{_, Pid, _, _}| _Tail] = supervisor:which_children(carrot_sup),
    Pid.


kill_carrot(Pid) ->
    supervisor:which_children(carrot_sup),
    simulation_carrot_sup:kill_carrot(Pid),
    ok.
    

carrot_init() ->
    %simulation_carrot_sup:start_link(1),
    [_, _, _, {workers,ChildNum1}] = supervisor:count_children(carrot_sup),
    ?assertEqual(ChildNum1, 1),
    [{_, Pid, _, _}| _Tail] = supervisor:which_children(carrot_sup),
    io:format("~p", [Pid]),
    simulation_carrot_sup:kill_carrot(Pid),
    [_, _, _, {workers,ChildNum2}] = supervisor:count_children(carrot_sup),
    ?assertEqual(ChildNum2, 0),
    Pid.

test_two() ->
    ?_assertEqual(1, 1).