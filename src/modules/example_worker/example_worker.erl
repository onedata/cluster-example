%%%--------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2014 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module implements worker_plugin_behaviour callbacks.
%%% It is example of added worker.
%%% @end
%%%--------------------------------------------------------------------
-module(example_worker).
-author("Michal Zmuda").

-behaviour(worker_plugin_behaviour).

-include("datastore/example_model_def.hrl").
-include_lib("cluster_worker/include/modules/datastore/datastore.hrl").
-include_lib("global_definitions.hrl").
-include_lib("ctool/include/logging.hrl").

%% worker_plugin_behaviour callbacks
-export([init/1, handle/1, cleanup/0]).

%%%===================================================================
%%% worker_plugin_behaviour callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Initialize module
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
  {ok, State :: worker_host:plugin_state()} | {error, Reason :: term()}.
init(_) ->
  {ok, #{}}.


%%--------------------------------------------------------------------
%% @doc
%% Do your work.
%% @end
%%--------------------------------------------------------------------
-spec handle(Request :: term()) ->
  {ok, Id :: term()} | {error, Reason :: term()}.

handle({set, Value}) ->
  case example_model:create(#document{value = #example_model{value = Value}}) of
    {ok, Id} ->
      {ok, Id};
    {error, Reason} ->
      {error, Reason}
  end;

handle({get, Id}) ->
  case example_model:get(Id) of
    {ok, #document{value = #example_model{value = Value}}} ->
      {ok, Value};
    _ ->
      {error, unexpected}
  end;

handle({set_global, Value}) ->
  case example_shared_model:create(#document{value = #example_shared_model{value = Value}}) of
    {ok, Id} ->
      {ok, Id};
    {error, Reason} ->
      {error, Reason}
  end;

handle({get_global, Id}) ->
  case example_shared_model:get(Id) of
    {ok, #document{value = #example_shared_model{value = Value}}} ->
      {ok, Value};
    _ ->
      {error, unexpected}
  end;

handle(healthcheck) ->
  ok;

handle(_) ->
  {error, unexpected_call}.

%%--------------------------------------------------------------------
%% @doc
%% The module will not be used anymore. Clean up!
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok | {error, Reason :: term()}.
cleanup() ->
  ok.

