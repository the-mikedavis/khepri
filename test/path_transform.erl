%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright © 2021-2022 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(path_transform).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, khepri_path_transform}).

simple_replacement_test() ->
    %% Listing = ""
    %% "hello_world() ->\n"
    %% "    khepri_path:from_string(\"/:stock/:wood/oak\").\n",
    ok.

replacement_within_case_expression_test() ->
    Listing = ""
    "hello_world(X) ->\n"
    "    case X of\n"
    "        true -> khepri_path:from_string(\"/:stock/:wood/oak\");\n"
    "        false -> khepri_path:from_string(\"/:emails/alice\")\n"
    "    end.\n",
    throw(forms(Listing)),
    ok.

forms(Listing) ->
    {ok, Tokens, _} = erl_scan:string(Listing),
    {ok, Forms} = erl_parse:parse_form(Tokens),
    Forms.
