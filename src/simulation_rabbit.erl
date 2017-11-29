
%%% 
% A rabbit will randomly travel the world eating carrots that spawn in random places
% when they eat enough carrots, they will split in two
% if a rabbit finds a carrot patch, other rabbits quickly join him
% tries to run away from wolf?

% A rabbit is a fsm with many states:
%       State: roaming - 
%           A rabbit will randomly roam the world, and will head toward a carrot broadcast
%       State: running -
%           If a rabbit gets within a certain distance of a wolf, starts broadcasting wolf position to all other rabbits, runs from wolf 
%           If a rabbit gets a broadcast, starts running in opposite direction
%       State: eating -
%           If a rabbit hits a carrot, eats it and signals he has found a carrot

-module(simulation_rabbit).
-behaviour(gen_statem).

-include("../include/simulation_records.hrl").

%%% api calls
-export([start_link/0]).

%%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%%% gen_statem stateNames
-export([roaming/3, eating/3, splitting/3, check_carrot/1]).


%%% Rabbit API

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

%%% gen_statem callbacks

callback_mode() ->
    state_functions.

init([]) ->
    Rabbit = #rabbit{position=simulation_move:rand_coords()},
    io:format("Rabbit born: ~p~n", [Rabbit]),
    {ok, roaming, Rabbit, ?TIMEOUT}.            

terminate(_Reason, _State, _Data) ->
    io:format("Rabbit: Rabbit died~n"),
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%% gen_statem states

%% roaming state - after timeout, rabbit moves by one and check if on same coord as a carrot

roaming(timeout, _EventContent, #rabbit{position=[X,Y], speed=Speed, carrots=Carrots}) ->
    % move rabbit by one
    [X2, Y2] = simulation_move:rand_move([X,Y], Speed),
    Rabbit = #rabbit{position=[X2, Y2], carrots=Carrots},
    io:format("Rabbit ~p: ~p~n", [Rabbit#rabbit.pid, Rabbit#rabbit.position]),
    % check if new position has a carrot on it
    case check_carrot([X,Y]) of
        
        none ->
            % if no carrot, then move to new space
            {next_state, roaming, Rabbit, ?TIMEOUT};
        
        Pid -> 
            % if carrot, move to eating state
            {next_state, eating, [Rabbit,Pid], ?TIMEOUT}
    end.
    
%% eating state - after timeout rabbit will increase carrot count by one and send message to carrot server

eating(timeout, _StateContent, [#rabbit{carrots=Carrots, position=[X,Y], speed=Speed}, Pid]) ->
    NewCarrots = Carrots+1,
    Rabbit = #rabbit{carrots=NewCarrots, position=[X,Y], speed=Speed},
    gen_server:cast(Pid, eaten),
    % check if CARROT_MAX reached
    case NewCarrots of
        ?CARROT_MAX  ->
            % if max carrot, move to splitting
            io:format("Rabbit ~p: Splitting...~n", [Rabbit#rabbit.pid]),
            {next_state, splitting, #rabbit{carrots=Carrots, position=[X,Y]}, ?FAST_TIMEOUT};
        
        _ -> 
            % if not max carrot, move to roaming
            io:format("Rabbit ~p: Eaten Carrot~n", [Rabbit#rabbit.pid]),  
            {next_state, roaming, Rabbit, ?TIMEOUT}
    end.
    
%% splitting state - rabbit dynamically adds a new rabbit to the supervisor, return current rabbit to 0 carrots
splitting(timeout, _EventContent, Rabbit = #rabbit{}) ->
    simulation_rabbit_sup:start_rabbit(),
    {next_state, roaming, Rabbit#rabbit{carrots=0}, ?TIMEOUT}.
    
    
%%% internal functions

%% get list of carrot servers
check_carrot([X,Y]) ->
    ChildList = supervisor:which_children(carrot_sup),
    Pid = check_carrot(ChildList, [X,Y]),
    Pid.

%% base condition
check_carrot([], _Position) ->
    none;
%% loop through carrot servers, checking if carrot coords match with rabbit coords
check_carrot(ChildList, [X,Y]) ->
    [{_,Pid, _,_}|Tail] = ChildList,
    % try...catch for race condition when two rabbits call server at same time
    try gen_server:call(Pid, {are_you_here, [X,Y]}) of
        yes ->
            Pid;
        no ->
            check_carrot(Tail, [X,Y])
    catch
    % catch any errors with communicating with particular carrot server, and move onto next
        exit:_Reason ->
            check_carrot(Tail, [X,Y])
    end.

   










    
    



