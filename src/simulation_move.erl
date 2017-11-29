
-module(simulation_move).

-include("../include/simulation_records.hrl").

-export([rand_move/2, rand_coords/0]).




%% simulate 2-d random walk
%       takes [x,y] coords, and adjusts x/y by 1 
rand_move([X,Y], Speed) ->
    Change = rand:uniform(4),
    [X2, Y2] = rand_move([X,Y], Speed, Change),
    [X2, Y2].


rand_move([X,Y], Speed, Change) when X<?MAX_X, Change =:= 1 -> 
    [X2, Y2] = [X+Speed, Y],
    [X2, Y2];
rand_move([X,Y], Speed, Change) when X>0, Change =:= 2 -> 
    [X2, Y2] = [X-Speed, Y],
    [X2, Y2];
rand_move([X,Y], Speed, Change) when Y<?MAX_Y, Change =:= 3 -> 
    [X2, Y2] = [X, Y+Speed],
    [X2, Y2];
rand_move([X,Y], Speed, Change) when Y>0, Change =:= 4 ->
    [X2, Y2] = [X, Y-Speed],
    [X2, Y2];
rand_move(Position, Speed, _Change) ->
    rand_move(Position, Speed).


rand_coords() ->
    [X,Y] = [float(rand:uniform(?MAX_X)), float(rand:uniform(?MAX_Y))],
    [X,Y].

