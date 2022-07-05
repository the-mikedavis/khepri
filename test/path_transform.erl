%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(path_transform).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, khepri_path_transform}).

%% Tests on the result of the `khepri_path_transform' on this file.

unix_path_is_compiled_test() ->
    StringPath = khepri_path:from_string("/:stock/:wood/oak"),
    NativePath = [stock, wood, <<"oak">>],
    ?assertEqual(NativePath, StringPath),
    ok.

%% Tests on form changes directly

simple_replacement_test() ->
    Listing = ""
    "hello_world() ->"
    "    khepri_path:from_string(\"/:stock/:wood/oak\").",

    Path = [function(hello_world, 0), clause(0)],
    Term = evaluate_form(Listing, Path),
    ?assertEqual({ok, [stock, wood, <<"oak">>]}, Term),
    ok.

replacement_within_binding_test() ->
    Listing = ""
    "hello_world() ->"
    "    Path = khepri_path:from_string(\"/:stock/:wood/oak\"),"
    "    Path.",
    Path = [function(hello_world, 0), clause(0), 'match'],
    Term = evaluate_form(Listing, Path),
    ?assertEqual({ok, [stock, wood, <<"oak">>]}, Term),
    ok.

replacement_within_case_expression_test() ->
    Listing = ""
    "hello_world(X) ->"
    "    case X of"
    "        true -> khepri_path:from_string(\"/:stock/:wood/oak\");"
    "        false -> khepri_path:from_string(\"/:emails/alice\")"
    "    end.",

    TrueClausePath = [function(hello_world, 1), clause(0), 'case', clause(0)],
    TrueClauseTerm = evaluate_form(Listing, TrueClausePath),
    ?assertEqual({ok, [stock, wood, <<"oak">>]}, TrueClauseTerm),

    FalseClausePath = [function(hello_world, 1), clause(0), 'case', clause(1)],
    FalseClauseTerm = evaluate_form(Listing, FalseClausePath),
    ?assertEqual({ok, [emails, <<"alice">>]}, FalseClauseTerm),
    ok.

replacement_within_if_expression_test() ->
    Listing = ""
    "hello_world(N) ->"
    "    if"
    "        N =:= node() ->"
    "            khepri_path:from_string(\"/:stock/:wood/oak\");"
    "        true ->"
    "            [stock, wood, <<\"oak\">>]"
    "    end.",
    Path = [function(hello_world, 1), clause(0), 'if', clause(0)],
    TrueClauseTerm = evaluate_form(Listing, Path),
    ?assertEqual({ok, [stock, wood, <<"oak">>]}, TrueClauseTerm),
    ok.

%% Helper functions

evaluate_form(Listing, Path) ->
    Forms = forms(Listing),
    TransformedForms = khepri_path_transform:parse_transform(Forms, []),
    case get_form_in(Path, lists:flatten([TransformedForms])) of
        {ok, Form} ->
            {ok, erl_parse:normalise(Form)};
        error ->
            error
    end.

get_form_in(Path, Forms) ->
    get_form_in(Path, Forms, 0).

get_form_in([], [Form], _Count) ->
    io:format("Selected form: ~p~n", [Form]),
    {ok, Form};
get_form_in([], Form, _Count) ->
    io:format("Selected form: ~p~n", [Form]),
    {ok, Form};
get_form_in(_Path, [], _Count) ->
    error;
get_form_in([Selector | Next] = Path, [Form | Forms], Count) ->
    io:format("{Selector, Form}: ~p~n", [{Selector, Form}]),
    case {Selector, Form} of
        {{function, Name, Arity}, {function, _, Name, Arity, Clauses}} ->
            get_form_in(Next, Clauses, 0);
        {{clause, Count}, {clause, _, _, _, Body}} ->
            get_form_in(Next, Body, 0);
        {'case', {'case', _, _, Clauses}} ->
            get_form_in(Next, Clauses, 0);
        {'if', {'if', _, Clauses}} ->
            get_form_in(Next, Clauses, 0);
        {match, {match, _, Expression}} ->
            get_form_in(Next, Expression, 0);
        {_Selector, _Form} ->
            get_form_in(Path, Forms, Count + 1)
    end.

function(Name, Arity) ->
    {function, Name, Arity}.

clause(ClauseNumber) ->
    {clause, ClauseNumber}.

%% nth(Index) ->
%%     {nth, Index}.

forms(Listing) ->
    {ok, Tokens, _} = erl_scan:string(Listing),
    {ok, Forms} = erl_parse:parse_form(Tokens),
    Forms.
