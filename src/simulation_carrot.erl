-module(simulation_carrot).
-behaviour(gen_server).

-include("../include/simulation_records.hrl").

%%% Carrot %%%

%   Takes calls from rabbits:
%       - are_you_here - if carrot is at same position as rabbit, then return true, false if not
%       - eaten - reduces carrot patch by one - if goes to 0 then remove carrot server

%%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% Public API
-export([start_link/0]).



start_link() ->
    gen_server:start_link(?MODULE, [], []).



terminate(_Reason, _State, _Data) ->
    io:format("Carrot Eaten~n"),
    ok.
    
code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.
