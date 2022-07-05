%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

%% @doc A parse-transform that converts {@link khepri_path:unix_pattern()}s
%% into {@link khepri_path:native_pattern()}s at compilation-time.
%%
%% Add this parse-transform to a module with the `compile' attribute:
%%
%% ```
%% -compile({parse_transform, khepri_path_transform}).
%% '''
%%
%% or with the `erlc' option `` +'{parse_transform, khepri_path_transform}' ''.
%%
%% Calls to {@link khepri_path:compile/1} and {@link khepri_path:from_string/1}
%% with a string {@link khepri_path:unix_pattern()} are expanded at
%% compile-time.

-module(khepri_path_transform).
-export([parse_transform/2]).

-spec parse_transform(Forms, Options) -> Forms when
    Forms :: erl_parse:abstract_form(),
    Options :: term().
parse_transform(Forms, _Options) ->
    walk_forms(Forms, fun rewrite_path_call/1).

walk_forms(Forms, Fun) when is_list(Forms) ->
    [walk_forms(Form, Fun) || Form <- Forms];
walk_forms({function, _Location, _Name, _Arity, Clauses} = Function0, Fun) ->
    Function = setelement(5, Function0, walk_forms(Clauses, Fun)),
    Fun(Function);
walk_forms({clause, _Location, _Pattern, _Guard, Body} = Clause0, Fun) ->
    Clause = setelement(5, Clause0, walk_forms(Body, Fun)),
    Fun(Clause);
walk_forms({'case', _Location, Clauses} = Case0, Fun) ->
    Case = setelement(3, Case0, walk_forms(Clauses, Fun)),
    Fun(Case);
walk_forms(Form, Fun) ->
    Fun(Form).

rewrite_path_call(
  {call,
   _Location,
   {remote, _, {atom, _, khepri_path},
   {atom, _, Function}},
   [{string, Location, Path}]}) when
       Function =:= from_string orelse Function =:= compile ->
    NativePath = khepri_path:from_string(Path),
    erl_parse:abstract(NativePath, Location);
rewrite_path_call(Form) ->
    Form.
