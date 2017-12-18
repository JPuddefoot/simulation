-module(simulation_rabbit_sup).
-behaviour(supervisor).

-include("../include/simulation_records.hrl").

%%% supervisor api
-export([start_link/0, start_link/1, start_rabbit/0, kill_rabbit/1]).

%%% supervisor callbacks
-export([init/1]).

%%% API
start_link() ->
    supervisor:start_link({local, rabbit_sup}, ?MODULE, []),
    breed_rabbits(?MAX_RABBITS).

start_link(RabbitNum) ->
    supervisor:start_link({local, rabbit_sup}, ?MODULE, []),
    breed_rabbits(RabbitNum).

start_rabbit() ->
    supervisor:start_child(rabbit_sup, []).

kill_rabbit(Pid) ->
    supervisor:terminate_child(rabbit_sup, Pid).

breed_rabbits(Total) ->
    breed_rabbits(0, Total).

%%% Callbacks
init([]) ->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpec = #{id => rabbit,
                start => {simulation_rabbit, start_link, []},
                restart => permanent,
                shutdown => 1000,
                type => worker},
    {ok, {SupFlags, [ChildSpec]}}.

  
%%% internal functions
breed_rabbits(Created, Total) when Created < Total ->
    start_rabbit(),
    breed_rabbits(Created+1, Total);
breed_rabbits(_Created, _Total) ->
    done.

