-module(simulation_rabbit).
-behaviour(gen_statem).

-define(TIMEOUT, 1000).
-define(FAST_TIMEOUT, 10).
-define(CARROT_MAX, 5).

%%% api calls


%%% gen_statem callbacks
-export([start_link/1, init/1, callback_mode/0, terminate/3, code_change/4]).

%%% gen_statem stateNames
-export([roaming/3, eating/3, splitting/3]).

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

%%% gen_statem callbacks

callback_mode() ->
    state_functions.

init([]) ->
    {ok, roaming, #rabbit{}, ?TIMEOUT}.            %% need to randomly set position/speed

terminate(_Reason, _State, _Data) ->
    io:format("Rabbit died~n"),
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%% gen_statem states


roaming(cast, found_carrot, Rabbit = #rabbit{}) ->
    {next_state, eating, Rabbit, ?TIMEOUT};
roaming(cast, _EventContent, Rabbit=#rabbit{}) ->
    io:format("unknown message"),
    {next_state, roaming, Rabbit, ?TIMEOUT};
roaming(timeout, _EventContent, #rabbit{position=[X,Y], speed=Speed, carrots=Carrots}) ->
    [X2, Y2] = simulation_move:rand_move([X,Y], Speed),
    Rabbit = #rabbit{position=[X2, Y2], carrots=Carrots},
    io:format("~p~n", [Rabbit]),
    {next_state, roaming, Rabbit, ?TIMEOUT}.


eating({call, _From}, eating, #rabbit{}) ->
    io:format("Rabbit eating...~n"),
    {next_state, eating, #rabbit{}, ?TIMEOUT};
eating(timeout, _EventContent, #rabbit{carrots=Carrots}) ->
    case Carrots of
        ?CARROT_MAX  ->
            io:format("Splitting...~n"),
            {next_state, splitting, #rabbit{carrots=Carrots}, ?FAST_TIMEOUT};
        
        _ -> 
            io:format("Eaten~n"),    
            {next_state, roaming, #rabbit{carrots=Carrots+1}, ?TIMEOUT}
    end.
    
splitting(timeout, _EventContent, #rabbit{}) ->
    %%% dynamically add a new rabbit to the supervisor, return current rabbit to 0 carrots
    {next_state, roaming, #rabbit{carrots=0}, ?TIMEOUT}.
    









    
    



