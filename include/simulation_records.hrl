

%% world parameters
-define(MAX_X, 5).
-define(MAX_Y, 5).


%% rabbit parameters
-record(rabbit, {position=[0,0],
                speed = 0.5,
                carrots=0}).
% time between rabbit movements
-define(TIMEOUT, 1000).
%time taken to split
-define(FAST_TIMEOUT, 10).
% number of carrots before splitting
-define(CARROT_MAX, 5).