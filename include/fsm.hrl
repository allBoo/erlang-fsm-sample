%%%-------------------------------------------------------------------
%%% @author alboo
%%%-------------------------------------------------------------------
-author("alboo").

%%% ====================================================================
%%% Main include file
%%% ====================================================================
-include_lib("log.hrl").
-include_lib("error.hrl").


%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(CHILD(I, A), {I, {I, start_link, A}, permanent, 5000, worker, [I]}).
-define(CHILD_TEMP(I), #{id => I, start => {I, start_link, []}, restart => transient, shutdown => 5000, type => worker, modules => [I]}).
-define(CHILD_TEMP(I, A), #{id => I, start => {I, start_link, A}, restart => transient, shutdown => 5000, type => worker, modules => [I]}).
-define(CHILD_TEMP_SIG(I), #{id => I, start => {I, start_link, []}, restart => transient, significant => true, shutdown => 5000, type => worker, modules => [I]}).
-define(CHILD_TEMP_SIG(I, A), #{id => I, start => {I, start_link, A}, restart => transient, significant => true, shutdown => 5000, type => worker, modules => [I]}).
-define(CHILD(N, I, A), {N, {I, start_link, A}, permanent, 5000, worker, [I]}).
-define(CHILD_SUP(I), {I, {I, start_link, []}, permanent, infinity, supervisor, [I]}).
-define(CHILD_SUP_T(I), {I, {I, start_link, []}, transient, infinity, supervisor, [I]}).

%%% ====================================================================
%%% Env spec
%%% ====================================================================
-record(env, {
  key,
  value
}).

%%% ====================================================================
%%% Data types
%%% ====================================================================
-record(options, {infile, outfile}).
