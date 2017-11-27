-module(simulation_rabbit).
-behaviour(gen_statem).

%%% api calls
-export([rabbit_rand_move/1, rabbit_eat/1]).

%%% gen_statem callbacks
-export([start_link/1, init/1, callback_mode/0, terminate/3, code_change/4]).

%%% gen_statem stateNames
-export([roaming/3, eating/3]).

%% for moving the rabbit
%-export([move_rabbit/1]).

-record(rabbit, {position=[0,0],
                speed = 0.5,
                carrots=0}).

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

start_link(Name) ->
    gen_statem:start_link({local, Name}, ?MODULE, [], []).

rabbit_rand_move(Name) ->
    gen_statem:cast(Name, rand_move).

rabbit_eat(Name) ->
    gen_statem:call(Name, test).

%%% gen_statem callbacks

callback_mode() ->
    state_functions.

init([]) ->
    {ok, roaming, #rabbit{}}.            %% need to randomly set position/speed

terminate(_Reason, _State, _Data) ->
    io:format("Rabbit died~n"),
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%% gen_statem states

roaming(cast, rand_move, #rabbit{position=[X,Y], speed=Speed}) ->
    [X2, Y2] = simulation_move:rand_move([X,Y], Speed),
    Rabbit = #rabbit{position=[X2, Y2]},
    io:format("~p~n", [Rabbit]),
    {next_state, roaming, Rabbit};
roaming(cast, found_carrot, #rabbit{}) ->
    {next_state, eating, #rabbit{}};
roaming(cast, _EventContent, #rabbit{}) ->
    io:format("unknown message"),
    {next_state, roaming, #rabbit{}}.

eating({call, From}, eating, #rabbit{carrots=Carrots}) ->
    io:format("Rabbit eating...~n"),
    {next_state, roaming, #rabbit{carrots=Carrots+1}, [{reply, From, ok}]}.








    
    



