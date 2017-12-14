-module(simulation_carrot_sup).
-behaviour(supervisor).

-include("../include/simulation_records.hrl").

%%% supervisor api
-export([start_link/0]).

%%% supervisor callbacks
-export([init/1]).

%%% API
start_link() ->
    supervisor:start_link({local, carrot_sup}, ?MODULE, []),
    spawn_carrots(?MAX_CARROTS).

start_carrot() ->
    supervisor:start_child(carrot_sup, []).


%%% Callbacks
init([]) ->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpec = #{id => carrot,
                start => {simulation_carrot, start_link, []},
                restart => temporary,
                shutdown => infinity,
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