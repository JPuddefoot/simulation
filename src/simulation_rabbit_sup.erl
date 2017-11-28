-module(simulation_rabbit_sup).
-behaviour(supervisor).

-include("../include/simulation_records.hrl").

%%% supervisor api
-export([start_link/0, start_rabbit/1, kill_rabbit/1]).

%%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, rabbit_sup}, ?MODULE, []),
    breed_rabbits(?MAX_RABBITS).


init([]) ->
    Sup_flags = #{strategy => simple_one_for_one},
    ChildSpec = #{id => rabbit,
                start => {simulation_rabbit, start_link, []},
                restart => permanent,
                shutdown => 1000,
                type => worker},
    {ok, {Sup_flags, [ChildSpec]}}.

start_rabbit(Name) ->
    io:format("Rabbit ~p born~n", [Name]),
    supervisor:start_child(rabbit_sup, [Name]).

kill_rabbit(Pid) ->
    supervisor:terminate_child(rabbit_sup, Pid).

breed_rabbits(Total) ->
    breed_rabbits(0, Total).  

breed_rabbits(Created, Total) when Created < Total ->
    Name = list_to_atom(integer_to_list(Created)),
    start_rabbit(Name),
    breed_rabbits(Created+1, Total);
breed_rabbits(_Created, _Total) ->
    done.

