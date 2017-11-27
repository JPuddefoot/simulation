
-module(simulation_move).
-export([rand_move/2]).

%%% define area of simulation
-define(MAX_X, 5).
-define(MAX_Y, 5).

%% simulate 2-d random walk
%       takes [x,y] coords, and adjusts x/y by 1 
rand_move([X,Y], Speed) ->
    Change = rand:uniform(4),
    [X2, Y2] = rand_move([X,Y], Speed, Change),
    [X2, Y2].


rand_move([X,Y], Speed, Change) when X<?MAX_X, Change =:= 1 -> 
    [X2, Y2] = [X+Speed, Y],
    [X2, Y2];
rand_move([X,Y], Speed, Change) when X>-?MAX_X, Change =:= 2 -> 
    [X2, Y2] = [X-Speed, Y],
    [X2, Y2];
rand_move([X,Y], Speed, Change) when Y<?MAX_Y, Change =:= 3 -> 
    [X2, Y2] = [X, Y+Speed],
    [X2, Y2];
rand_move([X,Y], Speed, Change) when Y>-?MAX_Y, Change =:= 4 ->
    [X2, Y2] = [X, Y-Speed],
    [X2, Y2];
rand_move(Position, Speed, _Change) ->
    rand_move(Position, Speed).




