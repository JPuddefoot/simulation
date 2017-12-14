-module(simulation_carrot).
-behaviour(gen_server).

-include("../include/simulation_records.hrl").

%%% Carrot %%%

%   Takes calls from rabbits:
%       - are_you_here - if carrot is at same position as rabbit, then return true, false if not
%       - eaten - reduces carrot patch by one - if goes to 0 then remove carrot server

%%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% Public API
-export([start_link/0]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%%% Callbacks

init([]) ->
    process_flag(trap_exit, true),
    Carrot = #carrot{position=simulation_move:rand_coords()},
    io:format("Carrot: Carrot spawned: ~p~n", [Carrot#carrot.pid]),
    {ok, Carrot}.

%% returns true if rabbit is in same position, false if not
handle_call({are_you_here, [X,Y]}, _From, Carrot = #carrot{}) ->
    case [X,Y] =:= Carrot#carrot.position of
        true ->
            {reply, yes, Carrot};
        false ->
            {reply, no, Carrot}
    end;

handle_call(terminate, _From, Carrot = #carrot{}) ->
    {stop, normal, ok, Carrot}.

%% Removes a carrot when a rabbit sends an eaten cast
handle_cast(eaten, #carrot{quantity=N, position=Position}) when N > 1 ->
    %io:format("Carrot ~p: Carrot eaten~n", [Carrot#carrot.pid]),
    {noreply, #carrot{quantity=N-1, position=Position}};
handle_cast(eaten, #carrot{quantity=N, position=Position}) when N =:= 1 ->
    %io:format("Carrot ~p: Carrot eaten~n", [Carrot#carrot.pid]),
    {stop, normal, #carrot{quantity=0, position=Position}};
handle_cast(eaten, #carrot{quantity=N}) when N < 1 ->
    {stop, error, #carrot{}}.


handle_info(Msg, Carrot = #carrot{}) ->
    io:format("Carrot: Unexpected message ~p~n", [Msg]),
    {noreply, Carrot}.

%% carrot terminates when no more carrots left
terminate(shutdown, Carrot) ->
    io:format("Carrot ~p fully eaten~n", [Carrot#carrot.pid]);
%% error 
terminate(_Reason, Carrot) ->
    io:format("Error: ~p~n", [Carrot#carrot.pid]),
    ok.
    
code_change(_Vsn, State, _Extra) ->
    {ok, State}.
