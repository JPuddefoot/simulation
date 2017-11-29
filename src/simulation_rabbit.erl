-module(simulation_rabbit).
-behaviour(gen_statem).

-include("../include/simulation_records.hrl").

%%% api calls


%%% gen_statem callbacks
-export([start_link/0, init/1, callback_mode/0, terminate/3, code_change/4]).

%%% gen_statem stateNames
-export([roaming/3, eating/3, splitting/3, check_carrot/1]).


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

%%% Rabbit API

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

%%% gen_statem callbacks

callback_mode() ->
    state_functions.

init([]) ->
    Rabbit = #rabbit{position=simulation_move:rand_coords()},
    io:format("Rabbit: ~p~n", [Rabbit]),
    {ok, roaming, Rabbit, ?TIMEOUT}.            %% need to randomly set position/speed

terminate(_Reason, _State, _Data) ->
    io:format("Rabbit: Rabbit died~n"),
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%% gen_statem states


roaming(cast, found_carrot, Rabbit = #rabbit{}) ->
    {next_state, eating, Rabbit, ?TIMEOUT};
roaming(timeout, _EventContent, #rabbit{position=[X,Y], speed=Speed, carrots=Carrots}) ->
    % move rabbit by one
    [X2, Y2] = simulation_move:rand_move([X,Y], Speed),
    Rabbit = #rabbit{position=[X2, Y2], carrots=Carrots},
    io:format("Rabbit: ~p~n", [Rabbit]),
    % check if new position has a carrot on it
    case check_carrot([X,Y]) of
        
        none ->
            % if no carrot, then move to new space
            {next_state, roaming, Rabbit, ?TIMEOUT};
        
        _Pid -> 
            % if carrot, move to eating state
            {next_state, eating, Rabbit, ?TIMEOUT}
    end.
    

eating(timeout, _EventContent, #rabbit{carrots=Carrots, position=[X,Y], speed=Speed}) ->
    NewCarrots = Carrots+1,
    Rabbit = #rabbit{carrots=NewCarrots, position=[X,Y], speed=Speed},
    % check if Carrot_MAX reached
    case NewCarrots of
        ?CARROT_MAX  ->
            % if max carrot, move to splitting
            io:format("Rabbit: Splitting...~n"),
            {next_state, splitting, #rabbit{carrots=Carrots, position=[X,Y]}, ?FAST_TIMEOUT};
        
        _ -> 
            % if not max carrot, move to roaming
            io:format("Rabbit: Eaten Carrot~n"),  
            {next_state, roaming, Rabbit, ?TIMEOUT}
    end.
    

splitting(timeout, _EventContent, Rabbit = #rabbit{}) ->
    %%% dynamically add a new rabbit to the supervisor, return current rabbit to 0 carrots
    simulation_rabbit_sup:start_rabbit(),
    {next_state, roaming, Rabbit#rabbit{carrots=0}, ?TIMEOUT}.
    
    
%%% internal functions

check_carrot([X,Y]) ->
    ChildList = supervisor:which_children(carrot_sup),
    Pid = check_carrot(ChildList, [X,Y]),
    Pid.


check_carrot([], _Position) ->
    none;
check_carrot(ChildList, [X,Y]) ->
    [{_,Pid, _,_}|Tail] = ChildList,
    case gen_server:call(Pid, {are_you_here, [X,Y]}) of
        yes ->
            Pid;
        no ->
            check_carrot(Tail, [X,Y])
    end.

   










    
    



