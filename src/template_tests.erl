-module(template_tests).

-define(PROPER_NO_IMPORTS, true). %% I hate imported functions!
-include_lib("proper/include/proper.hrl").
-compile(export_all).

printable_char() ->
    proper_types:elements(lists:seq($ , $~)).

%% Generates a non empty string
%% Any printable char but @
valid_char() ->
    ?SUCHTHAT(C, printable_char(), C =/= $@).

string() ->
    proper_types:list(printable_char()).

%% Non-empty string without any @
valid_string() ->
    non_empty_list(valid_char()).

non_empty_list(G) ->
    proper_types:non_empty(proper_types:list(G)).

permutation([]) ->
    [];
permutation(L) ->
    ?LET(E, proper_types:elements(L), [E | permutation(lists:delete(E, L))]).

%% Generates the internal representation of a string with substitutions
%% template() -> [{var, Variable::string(), Value::string()}, {text, string()}]
template() ->
    ?LET(
       {Text, VarList}, {proper_types:list(text()), var_list()},
       ?LET(
          RepeatedVars, repeated_vars(VarList),
          ?LET(
             Template, permutation(Text ++ VarList ++ RepeatedVars),
             fold_text(Template)))).

text() ->
    proper_types:frequency(
      [{5, {text, valid_string()}},
       {1, escaped_at}]).

%% Generate a list of variables without duplicated keys
var_list() ->
    ?LET(L, proper_types:list(var()), remove_duplicated_keys(L)).

var() ->
    {var, valid_string(), string()}.

%% In the unlikely event that we generate two substitutions with the same
%% variable name, filter the first out
remove_duplicated_keys([]) ->
    [];
remove_duplicated_keys([H = {var, Name, _} | T]) ->
    case lists:keymember(Name, 2, T) of
        true ->
            remove_duplicated_keys(T);
        false ->
            [H | remove_duplicated_keys(T)]
    end.

repeated_vars([]) ->
    [];
repeated_vars(VarList) ->
    proper_types:list(proper_types:elements(VarList)).

%% Change sequences like [{text, "a"}, {text, "b"}] in [{text, "ab"}]
fold_text([{text, A}, {text, B} | T]) ->
    fold_text([{text, A ++ B} | T]);
fold_text([H | T]) ->
    [H | fold_text(T)];
fold_text([]) ->
    [].


%% Returns the string form from the internal representation of a template
to_string(Template) ->
    lists:concat([to_string_acc(X) || X <- Template]).

to_string_acc(escaped_at) ->
    "@@";
to_string_acc({var, V, _}) ->
    lists:flatten(io_lib:format("@~s@", [V]));
to_string_acc({text, S}) ->
    S.

%% Returns the expected token list from the internal representation of a
%% template
to_tokens(Template) ->
    lists:concat([to_tokens_acc(X) || X <- Template]).

to_tokens_acc({var, S, _}) ->
    [at, {string, S}, at];
to_tokens_acc(escaped_at) ->
    [at, at];
to_tokens_acc({text, S}) ->
    [{string, S}].

to_parsed(Template) ->
    [to_parsed_acc(X) || X <- Template].

to_parsed_acc({var, Name, _Value}) -> {var, Name};
to_parsed_acc(escaped_at) -> {text, "@"};
to_parsed_acc({text, S}) -> {text, S}.

%% Returns the expected result, after substituting variables by their values
to_result(Template) ->
    lists:concat([to_result_acc(X) || X <- Template]).

to_result_acc({var, _, S}) -> S;
to_result_acc(escaped_at) -> "@";
to_result_acc({text, S}) -> S.

to_substs(T) ->
    remove_duplicates(to_substs_acc(T)).

to_substs_acc([]) ->
    [];
to_substs_acc([escaped_at | T]) ->
    to_substs_acc(T);
to_substs_acc([{var, Name, Value} | T]) ->
    [{Name, Value} | to_substs_acc(T)];
to_substs_acc([{text, _} | T]) ->
    to_substs_acc(T).

%% Test that no substitutions leave the string intact
prop_string_empty_list() ->
    ?FORALL(
       S, valid_string(),
       try
           S =:= template:string(S, [])
       catch
          Error:Reason ->
               ?WHENFAIL(io:format("Error ~p:~p~n", [Error, Reason]), false)
       end).

prop_tokens() ->
    ?FORALL(
       T, template(),
       to_tokens(T) == template:tokens(to_string(T))).

prop_parse() ->
    ?FORALL(
       T, template(),
       to_parsed(T) == template:parse(template:tokens(to_string(T)))).

prop_string() ->
    ?FORALL(
       T, template(),
       begin
           {OrderedSubsts, String, Expected} =
               {to_substs(T), to_string(T), to_result(T)},
           ?FORALL(
              Substs, permutation(OrderedSubsts),
              begin
                  Result = template:string(String, Substs),
                  ?WHENFAIL(
                     io:format(
                       "~nTemplate: ~p~n"
                       "Substs  : ~p~n"
                       "Expected: ~p~n"
                       "Got     : ~p~n",
                       [String, Substs, Expected, Result]),
                     Expected =:= Result)
              end)
       end).

remove_duplicates(L) ->
  FoldF = fun(X, {Acc, Seen}) ->
              case gb_sets:is_element(X, Seen) of
                true  -> {Acc , Seen};
                false -> {[X|Acc], gb_sets:add(X, Seen)}
              end
          end,
  {Reversed, _} = lists:foldl(FoldF, {[], gb_sets:new()}, L),
  lists:reverse(Reversed).
