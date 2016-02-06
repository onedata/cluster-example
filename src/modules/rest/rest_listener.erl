%%%--------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc gui listener starting & stopping
%%% @end
%%%--------------------------------------------------------------------
-module(rest_listener).
-author("Michal Zmuda").

-include("global_definitions.hrl").
-include_lib("ctool/include/logging.hrl").

% Cowboy listener references
-define(REST_LISTENER, rest).

-behaviour(listener_behaviour).

%% listener_starter_behaviour callbacks
-export([start/0, stop/0, port/0, healthcheck/0]).

%%%===================================================================
%%% listener_starter_behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link listener_starter_behaviour} callback start/1.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, Reason :: term()}.
start() ->
    Dispatch = [
        {'_', [
            {"/example/[...]", rest_handler, []}
        ]}
    ],

    % Start the listener for nagios handler
    Result = cowboy:start_http(?REST_LISTENER, 5,
        [
            {ip, {0, 0, 0, 0}},
            {port, port()}
        ], [
            {env, [{dispatch, cowboy_router:compile(Dispatch)}]}
        ]),
    case Result of
        {ok, _} -> ok;
        _ -> Result
    end.

%%--------------------------------------------------------------------
%% @doc
%% {@link listener_starter_behaviour} callback stop/1.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    case catch cowboy:stop_listener(?REST_LISTENER) of
        (ok) ->
            ok;
        (Error) ->
            ?error("Error on stopping listener ~p: ~p", [?REST_LISTENER, Error]),
            {error, rest_stop_error}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns the status of a listener.
%% @end
%%--------------------------------------------------------------------
-spec healthcheck() -> ok | {error, server_not_responding}.
healthcheck() ->
    case http_client:get("http://127.0.0.1:" ++ integer_to_list(port()), [], <<>>, [insecure]) of
        {ok, _, _, _} ->
            ok;
        _ ->
            {error, server_not_responding}
    end.


%%--------------------------------------------------------------------
%% @doc
%% {@link listener_starter_behaviour} callback port/0.
%% @end
%%--------------------------------------------------------------------
-spec port() -> integer().
port() ->
    8080.

%%%===================================================================
%%% Internal functions
%%%===================================================================