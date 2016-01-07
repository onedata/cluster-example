%%%--------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module handles Nagios monitoring requests.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_handler).
-author("Michal Zmuda").

-behaviour(cowboy_http_handler).

-include("global_definitions.hrl").
-include_lib("ctool/include/logging.hrl").

-export([init/3, handle/2, terminate/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Cowboy handler callback, no state is required
%% @end
%%--------------------------------------------------------------------
-spec init(term(), term(), term()) -> {ok, cowboy_req:req(), term()}.
init(_Type, Req, _Opts) ->
  {ok, Req, []}.


%%--------------------------------------------------------------------
%% @doc
%% Handles a request producing an text response.
%% @end
%%--------------------------------------------------------------------
-spec handle(term(), term()) -> {ok, cowboy_req:req(), term()}.
handle(Req, State) ->
  {Value, _} = cowboy_req:qs_val(<<"value">>, Req),
  {Id, _} = cowboy_req:qs_val(<<"id">>, Req),

  case {Value, Id} of
    {undefined, undefined} ->
      Response = cowboy_req:reply(204, Req);
    {undefined, _} ->
      {ok, Result} = example_worker:handle({get, Id}),
      Response = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], Result, Req);
    {_, undefined} ->
      {ok, Result} = example_worker:handle({set, Value}),
      Response = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], Result, Req);
    _ ->
      Response = cowboy_req:reply(204, Req)
  end,

  {ok, Response, State}.


%%--------------------------------------------------------------------
%% @doc
%% Cowboy handler callback, no cleanup needed.
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), term(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
  ok.
