-module(simulation_rabbit_sup).
-behaviour(supervisor).

%%% supervisor api
-export([start_link/0, start_rabbit/1]).

%%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, rabbit_sup}, ?MODULE, []).


init([]) ->
    Sup_flags = #{strategy => simple_one_for_one},
    ChildSpec = #{id => rabbit,
                start => {simulation_rabbit, start_link, []},
                restart => permanent,
                shutdown => 1000,
                type => worker},
    {ok, {Sup_flags, [ChildSpec]}}.

start_rabbit(Name) ->
    supervisor:start_child(rabbit_sup, [Name]).
   