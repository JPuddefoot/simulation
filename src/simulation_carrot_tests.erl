-module(simulation_carrot_tests).
-include_lib("eunit/include/eunit.hrl").

%full_carrot_test_() ->
%    {setup,
%    fun start/0,
%    fun stop/0,
%   fun carrot_test_two/0}.
%
%start() ->
%    simulation_carrot_sup:start_link(1).
%stop() ->
%    exit(carrot_sup, shutdown).

carrot_test_two() ->
    [_, _, _, {workers,ChildNum1}] = supervisor:count_children(carrot_sup),
    [?_assertEqual(1, ChildNum1)].


carrot_test() ->
    simulation_carrot_sup:start_link(1),
    [_, _, _, {workers,ChildNum1}] = supervisor:count_children(carrot_sup),
    ?assertEqual(ChildNum1, 1),
    [{_, Pid, _, _}| _Tail] = supervisor:which_children(carrot_sup),
    io:format("~p", [Pid]),
    simulation_carrot_sup:kill_carrot(Pid),
    [_, _, _, {workers,ChildNum2}] = supervisor:count_children(carrot_sup),
    ?assertEqual(ChildNum2, 0).
    


    