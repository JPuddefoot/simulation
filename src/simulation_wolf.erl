
%%%     Wolf    %%%
%% A wolf will randomly roam the world eating rabbits
%% until it splits in two

-module(simulation_wolf).
-behaviour(gen_statem).

-include("../include/simulation_records.hrl").

%%% API Calls
-export([start_link/0, check_rabbit/1]).

%%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%%% gen_statem stateNames
-export([roaming/3]).

%%% Wolf API

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

%%% gen_statem callbacks

callback_mode() ->
    state_functions.

init([]) ->
    Wolf = #wolf{position=simulation_move:rand_coords()},
    io:format("Wolf born: ~p~n", [Wolf]),
    {ok, roaming, Wolf, ?WOLF_SPEED}.            

terminate(_Reason, _State, _Data) ->
    io:format("Wolf: Wolf died~n"),
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%% gen_statem states

roaming(timeout, _EventContent, #wolf{position=[X,Y], speed=Speed, rabbits=Rabbits}) ->
    % move wolf by one
    [X2, Y2] = simulation_move:rand_move([X,Y], Speed),
    Wolf = #wolf{position=[X2, Y2], rabbits=Rabbits},
    io:format("Wolf ~p: ~p~n", [Wolf#wolf.pid, Wolf#wolf.position]),
    case check_rabbit([X2, Y2]) of
        none ->
            {next_state, roaming, Wolf, ?WOLF_SPEED};
        
        _Pid ->
            {next_state, eating, Wolf, ?WOLFSPEED}
    end,
    {next_state, roaming, Wolf, ?WOLF_SPEED}.



%%% internal functions

check_rabbit([X,Y]) ->
    RabbitList = supervisor:which_children(rabbit_sup),
    Pid = check_rabbit(RabbitList, [X,Y]),
    Pid.

%% base condition
check_rabbit([], _Position) ->
    none;
%% loop through rabbit gen_statems, checking if wolf co-ords match with rabbit coords
check_rabbit(RabbitList, [X,Y]) ->
    [{_, Pid, _, _}|Tail] = RabbitList,
    % try...catch if message errors out
    try gen_statem:call(Pid, {are_you_here, [X,Y]}) of 
        yes ->
            Pid;
        no ->
            check_rabbit(Tail, [X,Y])
    catch
        exit:_Reason ->
            check_rabbit(Tail, [X,Y])
    end.
