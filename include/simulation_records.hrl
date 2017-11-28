

%% world parameters
-define(MAX_X, 5).
-define(MAX_Y, 5).
-define(MAX_RES, 0.5).
-define(MAX_RABBITS, 2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Rabbit parameters       %%       
-record(rabbit, {pid=self(),
                position=[0,0],
                speed = ?MAX_RES,
                carrots=0}).
% time between rabbit movements
-define(TIMEOUT, 1000).
%time taken to split
-define(FAST_TIMEOUT, 10).
% number of carrots before splitting
-define(CARROT_MAX, 5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Carrot parameter        %%
-record(carrot, {pid=self(),
                position=[0,0]}).     



