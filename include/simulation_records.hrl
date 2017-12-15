

%% world parameters
-define(MAX_X, 5).
-define(MAX_Y, 5).
-define(MAX_RES, 0.5).
-define(MAX_WOLF, 1).
-define(MAX_RABBITS, 10).
-define(MAX_CARROTS, 30).
-define(SPLIT_TIME, 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Wolf parameters         %%
-record(wolf, {pid=self(),
                position=[0,0],
                speed=?MAX_RES,
                rabbits=0}).
% time between wolf movements
-define(WOLF_SPEED, 500).

% number of rabbits before splitting
-define(RABBIT_MAX, 3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Rabbit parameters       %%       
-record(rabbit, {pid=self(),
                position=[0,0],
                speed = ?MAX_RES,
                carrots=0}).
% time between rabbit movements
-define(RABBIT_SPEED, 500).
%time taken to split

% number of carrots before splitting
-define(CARROT_MAX, 5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Carrot parameter        %%
-define(CARROT_NUM, 3).

-record(carrot, {pid=self(),
                position=[0,0],
                quantity=?CARROT_NUM}). 





