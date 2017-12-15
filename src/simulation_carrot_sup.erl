-module(simulation_carrot_sup).
-behaviour(supervisor).

-include("../include/simulation_records.hrl").

%%% supervisor api
-export([start_link/0, start_link/1, kill_carrot/1]).

%%% supervisor callbacks
-export([init/1]).

%%% API
start_link() ->
    supervisor:start_link({local, carrot_sup}, ?MODULE, []),
    spawn_carrots(?MAX_CARROTS).
start_link(CarrotNum) ->
    supervisor:start_link({local, carrot_sup}, ?MODULE, []),
    spawn_carrots(CarrotNum).

start_carrot() ->
    supervisor:start_child(carrot_sup, []).

kill_carrot(Pid) ->
    supervisor:terminate_child(carrot_sup, Pid).

%%% Callbacks
init([]) ->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpec = #{id => carrot,
                start => {simulation_carrot, start_link, []},
                restart => temporary,
                shutdown => 1000,
                type => worker},
    {ok, {SupFlags, [ChildSpec]}}.


%%% internal functions
spawn_carrots(Total) ->
    spawn_carrots(0, Total).

spawn_carrots(Created, Total) when Created < Total ->
    start_carrot(),
    spawn_carrots(Created+1, Total);
spawn_carrots(_Created, _Total) ->
    done.