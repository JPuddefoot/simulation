-module(simulation_rabbit_tests).
-include_lib("eunit/include/eunit.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_test_() ->
    [{"rabbit can be started and stopped",
    {setup, fun start_one/0, fun stop/1, fun is_child/1}}].


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start_one() ->
    simulation_carrot_sup:start_link(0),
    simulation_rabbit_sup:start_link(1),
    [{_, Pid, _, _}| _Tail] = supervisor:which_children(rabbit_sup),
    Pid.

stop(Pid) ->
    supervisor:terminate_child(rabbit_sup, Pid),
    supervisor:delete_child(rabbit_sup, Pid).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
is_child(_Pid) ->
    [_, _, _, {workers, N}] = supervisor:count_children(rabbit_sup),
    [?_assertEqual(N, 1),
     ?_assertEqual(1, 1)].
    
%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
