%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module defines datastore config related to the example.
%%% @end
%%%-------------------------------------------------------------------
-module(datastore_config_plugin).
-author("Michal Zmuda").

-behaviour(datastore_config_behaviour).

%% datastore_config_behaviour callbacks
-export([models/0]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% {@link datastore_config_behaviour} callback models/0.
%% @end
%%--------------------------------------------------------------------
-spec models() -> Models :: [model_behaviour:model_type()].
models() -> [
  example_model,
  example_shared_model
].