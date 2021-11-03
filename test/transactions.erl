%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2021 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(transactions).

-include_lib("eunit/include/eunit.hrl").

-include("include/khepri.hrl").
-include("src/internal.hrl").
-include("src/khepri_machine.hrl").
-include("test/helpers.hrl").

%% Used internally for a testcase.
-export([really_do_get_root_path/0,
         really_do_get_node_name/0]).

fun_extraction_test() ->
    Parent = self(),

    %% We load the `mod_used_for_transactions' and do the function extraction
    %% from a separate process. This is required so that we can unload the
    %% module.
    %%
    %% If we were to do that from the test process, it would have a reference
    %% to the module's code and it would be impossible to unload and purge it.
    Child = spawn(
              fun() ->
                      Fun = mod_used_for_transactions:get_lambda(),
                      ExpectedRet = Fun(Parent),
                      StandaloneFun = khepri_fun:to_standalone_fun(Fun, #{}),
                      Parent ! {standalone_fun,
                                StandaloneFun,
                                [Parent],
                                ExpectedRet}
              end),
    MRef = erlang:monitor(process, Child),
    receive
        {'DOWN', MRef, process, Child, _Reason} ->
            %% At this point, we are sure the child process is gone. We can
            %% unload the code.
            true = code:delete(mod_used_for_transactions),
            false = code:purge(mod_used_for_transactions),
            ?assertEqual(false, code:is_loaded(mod_used_for_transactions)),

            receive
                {standalone_fun, StandaloneFun, Args, ExpectedRet} ->
                    ?assertMatch({ok, _, _}, ExpectedRet),
                    ?assertEqual(
                       ExpectedRet,
                       khepri_fun:exec(StandaloneFun, Args))
            end
    end.

noop_query_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic, ok},
         begin
             Fun = fun() ->
                           ok
                   end,
             khepri_machine:query(?FUNCTION_NAME, Fun)
         end)]}.

noop_tx_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic, ok},
         begin
             Fun = fun() ->
                           ok
                   end,
             khepri_machine:transaction(?FUNCTION_NAME, Fun)
         end)]}.

get_query_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic,
          {ok, #{[foo] => #{data => value1,
                            payload_version => 1,
                            child_list_version => 1,
                            child_list_length => 0}}}},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo], ?DATA_PAYLOAD(value1)),

             Fun = fun() ->
                           khepri_tx:get([foo])
                   end,
             khepri_machine:query(?FUNCTION_NAME, Fun)
         end)]}.

get_tx_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic,
          {ok, #{[foo] => #{data => value1,
                            payload_version => 1,
                            child_list_version => 1,
                            child_list_length => 0}}}},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo], ?DATA_PAYLOAD(value1)),

             Fun = fun() ->
                           khepri_tx:get([foo])
                   end,
             khepri_machine:transaction(?FUNCTION_NAME, Fun)
         end)]}.

put_query_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {aborted, store_update_denied},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo], ?DATA_PAYLOAD(value1)),

             Fun = fun() ->
                           Path = [foo],
                           case khepri_tx:get(Path) of
                               {ok, #{Path := #{data := value1}}} ->
                                   khepri_tx:put(Path, ?DATA_PAYLOAD(value2));
                               Other ->
                                   Other
                           end
                   end,
             khepri_machine:query(?FUNCTION_NAME, Fun)
         end)]}.

put_tx_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic,
          {ok, #{[foo] => #{data => value1,
                            payload_version => 1,
                            child_list_version => 1,
                            child_list_length => 0}}}},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo], ?DATA_PAYLOAD(value1)),

             Fun = fun() ->
                           Path = [foo],
                           case khepri_tx:get(Path) of
                               {ok, #{Path := #{data := value1}}} ->
                                   khepri_tx:put(Path, ?DATA_PAYLOAD(value2));
                               Other ->
                                   Other
                           end
                   end,
             khepri_machine:transaction(?FUNCTION_NAME, Fun)
         end)]}.

delete_query_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {aborted, store_update_denied},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo], ?DATA_PAYLOAD(value1)),

             Fun = fun() ->
                           Path = [foo],
                           case khepri_tx:get(Path) of
                               {ok, #{Path := #{data := value1}}} ->
                                   khepri_tx:delete(Path);
                               Other ->
                                   Other
                           end
                   end,
             khepri_machine:query(?FUNCTION_NAME, Fun)
         end)]}.

delete_tx_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic,
          {ok, #{[foo] => #{data => value1,
                            payload_version => 1,
                            child_list_version => 1,
                            child_list_length => 0}}}},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo], ?DATA_PAYLOAD(value1)),

             Fun = fun() ->
                           Path = [foo],
                           case khepri_tx:get(Path) of
                               {ok, #{Path := #{data := value1}}} ->
                                   khepri_tx:delete(Path);
                               Other ->
                                   Other
                           end
                   end,
             khepri_machine:transaction(?FUNCTION_NAME, Fun)
         end)]}.

exists_api_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic,
          {false, true, false}},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo, bar], ?DATA_PAYLOAD(bar_value)),

             Fun = fun() ->
                           {khepri_tx:has_data([foo]),
                            khepri_tx:has_data([foo, bar]),
                            khepri_tx:has_data([foo, bar, baz])}
                   end,
             khepri_machine:transaction(?FUNCTION_NAME, Fun)
         end)]}.

has_data_api_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic,
          {true, false}},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo], ?DATA_PAYLOAD(foo_value)),

             Fun = fun() ->
                           {khepri_tx:exists([foo]),
                            khepri_tx:exists([bar])}
                   end,
             khepri_machine:transaction(?FUNCTION_NAME, Fun)
         end)]}.

find_api_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic,
          {ok, #{[foo] => #{data => foo_value,
                            payload_version => 1,
                            child_list_version => 1,
                            child_list_length => 0}}}},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo], ?DATA_PAYLOAD(foo_value)),

             Fun = fun() ->
                           khepri_tx:find([], #if_data_matches{pattern = '_'})
                   end,
             khepri_machine:transaction(?FUNCTION_NAME, Fun)
         end)]}.

simple_api_test_() ->
    %% This is in the `khepri` module which provides the "simple API", but in
    %% the case of transactions, the API is the same as the "advanced API"
    %% from `khepri_machine`.
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic,
          {ok, #{[foo] => #{data => value1,
                            payload_version => 1,
                            child_list_version => 1,
                            child_list_length => 0}}}},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo], ?DATA_PAYLOAD(value1)),

             Fun = fun() ->
                           Path = [foo],
                           case khepri_tx:get(Path) of
                               {ok, #{Path := #{data := value1}}} ->
                                   khepri_tx:put(Path, ?DATA_PAYLOAD(value2));
                               Other ->
                                   Other
                           end
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

list_comprehension_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic, [bar_value, foo_value]},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo], ?DATA_PAYLOAD(foo_value)),
             khepri_machine:put(
               ?FUNCTION_NAME, [bar], ?DATA_PAYLOAD(bar_value)),

             Fun = fun() ->
                           {ok, Nodes} = khepri_tx:list([?ROOT_NODE]),
                           [Data ||
                            Path <- lists:sort(maps:keys(Nodes)),
                            #{data := Data} <- [maps:get(Path, Nodes)]]
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

aborted_transaction_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {aborted, abort_transaction},
         begin
             Fun = fun() ->
                           khepri_tx:abort(abort_transaction)
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

fun_taking_args_in_query_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, {requires_args, 1}},
         begin
             Fun = fun(Arg) ->
                           Arg
                   end,
             khepri:query(?FUNCTION_NAME, Fun)
         end)]}.

fun_taking_args_in_transaction_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, {requires_args, 1}},
         begin
             Fun = fun(Arg) ->
                           Arg
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

not_a_function_as_query_test_() ->
    Term = an_atom,
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, Term},
         khepri:query(?FUNCTION_NAME, Term))]}.

not_a_function_as_transaction_test_() ->
    Term = an_atom,
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, Term},
         khepri:transaction(?FUNCTION_NAME, Term))]}.

exception_in_query_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertError(
         {badmatch, {ok, #{[] := #{payload_version := 1,
                                   child_list_version := 1,
                                   child_list_length := 0}}}},
         begin
             Fun = fun() ->
                           bad_return_value = khepri_tx:list([])
                   end,
             khepri:query(?FUNCTION_NAME, Fun)
         end)]}.

exception_in_transaction_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertError(
         {badmatch, {ok, #{[] := #{payload_version := 1,
                                   child_list_version := 1,
                                   child_list_length := 0}}}},
         begin
             Fun = fun() ->
                           bad_return_value = khepri_tx:list([])
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

external_variable_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic, {ok, #{[] => #{payload_version => 1,
                                 child_list_version => 1,
                                 child_list_length => 0}}}},
         begin
             Path = [],
             Fun = fun() ->
                           khepri_tx:get(Path)
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

get_root_path() -> do_get_root_path().
do_get_root_path() -> ?MODULE:really_do_get_root_path().
really_do_get_root_path() -> seriously_do_get_root_path().
seriously_do_get_root_path() -> [].

calling_valid_local_function_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic, {ok, #{[] => #{payload_version => 1,
                                 child_list_version => 1,
                                 child_list_length => 0}}}},
         begin
             Fun = fun() ->
                           Path = get_root_path(),
                           khepri_tx:get(Path)
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

get_node_name() -> do_get_node_name().
do_get_node_name() -> ?MODULE:really_do_get_node_name().
really_do_get_node_name() -> node().

calling_invalid_local_function_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, {call_denied, {node, 0}}},
         begin
             Fun = fun() ->
                           Path = [node, get_node_name()],
                           khepri_tx:get(Path)
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

noop() -> ok.

calling_local_function_as_fun_term_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic, ok},
         begin
             Fun = fun noop/0,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

calling_stdlib_function_as_fun_term_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic, dict:new()},
         begin
             Fun = fun dict:new/0,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

calling_remote_function_as_fun_term_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic, mod_used_for_transactions:exported()},
         begin
             Fun = fun mod_used_for_transactions:exported/0,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

calling_unexported_remote_function_as_fun_term_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun,
          {call_to_unexported_function,
           {mod_used_for_transactions, unexported, 0}}},
         begin
             Fun = fun mod_used_for_transactions:unexported/0,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

fun_requiring_args_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, {requires_args, 1}},
         begin
             Fun = fun(SomeArg) ->
                           {ok, SomeArg}
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

with(Arg, Fun) ->
    fun() -> Fun(Arg) end.

nested_funs_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic, {nested, arg1}},
         begin
             Fun = fun(Arg) ->
                           erlang:list_to_tuple([nested, Arg])
                   end,
             khepri:transaction(?FUNCTION_NAME, with(arg1, Fun))
         end)]}.

trying_to_send_msg_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, sending_message_denied},
         begin
             Pid = self(),
             Fun = fun() ->
                           Pid ! msg
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

trying_to_receive_msg_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, receiving_message_denied},
         begin
             Fun = fun() ->
                           receive
                               Msg ->
                                   Msg
                           after 100 ->
                                     ok
                           end
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

trying_to_use_process_dict_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, {call_denied, {erlang, get, 0}}},
         begin
             Fun = fun() ->
                           get()
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

trying_to_use_persistent_term_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, {call_denied, {persistent_term, put, 2}}},
         begin
             Fun = fun() ->
                           persistent_term:put(key, value)
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

trying_to_use_mnesia_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, {call_denied, {mnesia, read, 2}}},
         begin
             Fun = fun() ->
                           mnesia:read(table, key)
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

trying_to_run_http_request_test_() ->
    %% In this case, Khepri will try to copy the code of the `httpc' module
    %% and will eventually find a forbidden instruction or call.
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, _},
         begin
             Fun = fun() ->
                           httpc:request("url://")
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

trying_to_use_ssl_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, {call_denied, {ssl, connect, 4}}},
         begin
             Fun = fun() ->
                           ssl:connect("localhost", 1234, [], infinity)
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

use_an_invalid_path_in_tx_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {aborted, {invalid_path, not_a_list}},
         begin
             Fun = fun() ->
                           khepri_tx:put(not_a_list, ?NO_PAYLOAD)
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end),
      ?_assertEqual(
         {aborted, {invalid_path, "not_a_component"}},
         begin
             Fun = fun() ->
                           khepri_tx:put(["not_a_component"], ?NO_PAYLOAD)
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

use_an_invalid_payload_in_tx_test_() ->
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {aborted, {invalid_payload, [foo], invalid_payload}},
         begin
             Fun = fun() ->
                           khepri_tx:put([foo], invalid_payload)
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end),
      ?_assertEqual(
         {aborted, {invalid_payload, [foo], {invalid_payload, in_a_tuple}}},
         begin
             Fun = fun() ->
                           khepri_tx:put([foo], {invalid_payload, in_a_tuple})
                   end,
             khepri:transaction(?FUNCTION_NAME, Fun)
         end)]}.

tx_from_the_shell_test_() ->
    %% We simuate the use of a transaction from the Erlang shell by using
    %% `erl_parse' and `erl_eval'. The transaction is the same as
    %% `put_tx_test_()'.
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertEqual(
         {atomic,
          {ok, #{[foo] => #{data => value1,
                            payload_version => 1,
                            child_list_version => 1,
                            child_list_length => 0}}}},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo], ?DATA_PAYLOAD(value1)),

             Code = "
             Fun = fun() ->
                           Path = [foo],
                           case khepri_tx:get(Path) of
                               {ok, #{Path := #{data := value1}}} ->
                                   khepri_tx:put(
                                     Path,
                                     khepri:data_payload(value2));
                               Other ->
                                   Other
                           end
                   end,
             khepri_machine:transaction(
               " ++ atom_to_list(?FUNCTION_NAME) ++ ",
               Fun).
             ",

             Bindings = erl_eval:new_bindings(),
             {ok, Tokens, _EndLocation} = erl_scan:string(Code),
             {ok, Exprs} = erl_parse:parse_exprs(Tokens),
             {value, Value, _NewBindings} = erl_eval:exprs(Exprs, Bindings),
             Value
         end)]}.

local_fun_using_erl_eval() ->
    Code = "
    Fun = fun() ->
                  Path = [foo],
                  case khepri_tx:get(Path) of
                      {ok, #{Path := #{data := value1}}} ->
                          khepri_tx:put(
                            Path,
                            khepri:data_payload(value2));
                      Other ->
                          Other
                  end
          end,
    Fun().
    ",

    Bindings = erl_eval:new_bindings(),
    {ok, Tokens, _EndLocation} = erl_scan:string(Code),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, _NewBindings} = erl_eval:exprs(Exprs, Bindings),
    Value.

tx_using_erl_eval_test_() ->
    %% We simuate the use of a transaction from the Erlang shell by using
    %% `erl_parse' and `erl_eval'. The transaction is the same as
    %% `put_tx_test_()'.
    {setup,
     fun() -> test_ra_server_helpers:setup(?FUNCTION_NAME) end,
     fun(Priv) -> test_ra_server_helpers:cleanup(Priv) end,
     [?_assertThrow(
         {invalid_tx_fun, {call_denied, _}},
         begin
             khepri_machine:put(
               ?FUNCTION_NAME, [foo], ?DATA_PAYLOAD(value1)),

             khepri_machine:transaction(
               ?FUNCTION_NAME,
             fun local_fun_using_erl_eval/0)
         end)]}.
