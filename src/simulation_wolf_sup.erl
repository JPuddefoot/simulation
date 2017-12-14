-module(simulation_wolf_sup).
-behaviour(supervisor).

-include("../include/simulation_records.hrl").

%%% supervisor api
-export([start_link/0, start_wolf/0, kill_wolf/1]).

%%% supervisor callbacks
-export([init/1]).

%%% API
start_link() ->
    supervisor:start_link({local, wolf_sup}, ?MODULE, []),
    breed_wolf(?MAX_WOLF).

start_wolf() ->
    supervisor:start_child(wolf_sup, []).

kill_wolf(Pid) ->
    supervisor:terminate_child(wolf_sup, Pid).

breed_wolf(Total) ->
    breed_wolf(0, Total).

%%% Callbacks

init([]) ->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpec = #{id => wolf,
                start => {simulation_wolf, start_link, []},
                restart => permanent,
                shutdown => 1000,
                type => worker},
    {ok, {SupFlags, [ChildSpec]}}.

%%% internal functions
breed_wolf(Created, Total) when Created < Total ->
    start_wolf(),
    breed_wolf(Created+1, Total);
breed_wolf(_Created, _Total) ->
    done.