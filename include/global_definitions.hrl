%%%-------------------------------------------------------------------
%%% @author Michal Wrzeszcz
%%% @copyright (C) 2013 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains global definitions of component names, macros and types
%%% used accross the application.
%%% @end
%%%-------------------------------------------------------------------
-ifndef(GLOBAL_DEFINITIONS_HRL).
-define(GLOBAL_DEFINITIONS_HRL, 1).

-include_lib("cluster_worker/include/modules/datastore/datastore.hrl").

%%%===================================================================
%%% Global names
%%%===================================================================

%% Name of the application.
-define(APP_NAME, example).

%% Local name (name and node is used to identify it) of supervisor that
%% coordinates application at each node (one supervisor per node).
-define(APPLICATION_SUPERVISOR_NAME, example_sup).

%% Local name (name and node is used to identify it) of gen_server that
%% works as a dispatcher.
-define(DISPATCHER_NAME, request_dispatcher).

-endif.
