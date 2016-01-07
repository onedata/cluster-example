%%%-------------------------------------------------------------------
%%% @author Michal Zmuda
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Contains definitions of models.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(EXAMPLE_MODEL_DEF_HRL).
-define(EXAMPLE_MODEL_DEF_HRL, 1).

%% sample model with example fields
-record(example_model, {
    value :: term()
}).

%% sample model with example fields
-record(example_shared_model, {
    value :: term()
}).

-endif.