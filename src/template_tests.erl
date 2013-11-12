-module(template_tests).

-define(PROPER_NO_IMPORTS, true). %% I hate imported functions!
-include_lib("proper/include/proper.hrl").
-compile(export_all).

%% Generators ==========================================================
printable_char() ->
    proper_types:elements(lists:seq($ , $~)).

%% Any printable char but @
valid_char() ->
    ?SUCHTHAT(C, printable_char(), C =/= $@).

string() ->
    ?LET(
       Chars, proper_types:vector(5, valid_char()),
       proper_types:list(proper_types:elements(Chars))).

%% Non-empty string without any @
valid_string() ->
    non_empty_list(valid_char()).

non_empty_list(G) ->
    proper_types:non_empty(proper_types:list(G)).

permutation([]) ->
    [];
permutation(L) ->
    ?LET(E, proper_types:elements(L), [E | permutation(lists:delete(E, L))]).

text() ->
    {text, valid_string()}.

%% Generate a list of variables without duplicated keys
var_list() ->
    ?LET(L, proper_types:list(var()), remove_duplicated_keys(L)).

var() ->
    {var, valid_string(), string()}.

%% Generates the internal representation of a string with substitutions
template() ->
    ?LET(
       {Texts, Vars}, {proper_types:list(text()), var_list()},
          ?LET(
             Template, permutation(Texts ++ Vars),
             fold_text(Template))).

%% Properties ==========================================================
%% Test that no substitutions leave the string intact
prop_string_empty_list() ->
    ?FORALL(S, string(), proper:equals(template:string(S, []), S)).

prop_tokens() ->
    ?FORALL(
       T, template(),
       proper:equals(to_tokens(T), template:tokens(to_string(T)))).

prop_parse() ->
    ?FORALL(
       T, template(),
       proper:equals(
         to_parsed(T),template:parse(template:tokens(to_string(T))))).

prop_string() ->
    ?FORALL(
       T, template(),
       begin
           {Substs, String, Expected} =
               {to_substs(T), to_string(T), to_result(T)},
           Result = template:string(String, Substs),
           ?WHENFAIL(
              io:format(
                "~nTemplate: ~p~n"
                "Substs  : ~p~n"
                "Expected: ~p~n"
                "Got     : ~p~n",
                [String, Substs, Expected, Result]),
              proper:equals(Expected, Result))
       end).

%% Internals ============================================================

%% Change sequences like [{text, "a"}, {text, "b"}] in [{text, "ab"}]
fold_text([{text, A}, {text, B} | T]) ->
    fold_text([{text, A ++ B} | T]);
fold_text([H | T]) ->
    [H | fold_text(T)];
fold_text([]) ->
    [].

%% Returns the expected token list from the internal representation of a
%% template
to_tokens(Template) ->
    lists:concat([to_tokens_acc(X) || X <- Template]).

to_tokens_acc({var, S, _}) ->
    [at, {string, S}, at];
to_tokens_acc({text, S}) ->
    [{string, S}].

%% Returns the string form from the internal representation of a template
to_string(Template) ->
    lists:concat([to_string_acc(X) || X <- Template]).

to_string_acc({var, V, _}) ->
    lists:flatten(io_lib:format("@~s@", [V]));
to_string_acc({text, S}) ->
    S.

to_parsed(Template) ->
    [to_parsed_acc(X) || X <- Template].

to_parsed_acc({var, Name, _}) -> {var, Name};
to_parsed_acc({text, S}) -> {text, S}.

to_substs(T) ->
    to_substs_acc(T).

to_substs_acc([]) ->
    [];
to_substs_acc([{var, Name, Value} | T]) ->
    [{Name, Value} | to_substs_acc(T)];
to_substs_acc([{text, _} | T]) ->
    to_substs_acc(T).

%% Returns the expected result, after substituting variables by their values
to_result(Template) ->
    lists:concat([to_result_acc(X) || X <- Template]).

to_result_acc({var, _, S}) -> S;
to_result_acc({text, S}) -> S.

format_failure(Template, Substs, Expected, Result) ->
    io:format(
      "~nTemplate: ~p~n"
      "Substs  : ~p~n"
      "Expected: ~p~n"
      "Got     : ~p~n",
      [Template, Substs, Expected, Result]).

remove_duplicated_keys(Vars) ->
    lists:foldl(
      fun(Var = {var, Name, _}, Acc) ->
              case lists:keymember(Name, 2, Acc) of
                  true -> Acc;
                  false -> [Var | Acc]
              end
      end,
      [], Vars).
