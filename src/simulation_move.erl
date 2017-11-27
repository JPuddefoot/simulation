
-module(simulation_move).
-export([rand_move/1]).

%%% define area of simulation
-define(MAX_X, 2).
-define(MAX_Y, 2).

%% simulate 2-d random walk
%       takes [x,y] coords, and adjusts x/y by 1 
rand_move([X,Y]) ->
    Change = rand:uniform(4),
    [X2, Y2] = rand_move([X,Y], Change),
    [X2, Y2].


rand_move([X,Y], Change) when X<?MAX_X, Change =:= 1 -> 
    [X2, Y2] = [X+1, Y],
    [X2, Y2];
rand_move([X,Y], Change) when X>-?MAX_X, Change =:= 2 -> 
    [X2, Y2] = [X-1, Y],
    [X2, Y2];
rand_move([X,Y], Change) when Y<?MAX_Y, Change =:= 3 -> 
    [X2, Y2] = [X, Y+1],
    [X2, Y2];
rand_move([X,Y], Change) when Y>-?MAX_Y, Change =:= 4 ->
    [X2, Y2] = [X, Y-1],
    [X2, Y2];
rand_move(_Position, _Change) ->
    rand_move(_Position).




