%%% @doc
-module(recursive_generators).

-compile([export_all]).

%%%_* Includes =========================================================
-define(PROPER_NO_IMPORTS, true). %% I hate imported functions!
-include_lib("proper/include/proper.hrl").

%%%_* Warm up generator ================================================

%% Generate a permutation of L. E.g.
%% permutations([1, 2, 3]) ->
%%  * [2, 1, 3]
%%  * [1, 2, 3]
%%  * [3, 2, 1]
%%  * [2, 3, 1]
%%  * ...
permutation([]) -> [];
permutation(L)  ->
  ?LET(E, proper_types:elements(L), [E | permutation(lists:delete(E, L))]).


%%%_* Production Code ==================================================

%%% Assume we want to test this function that folds arithmetic expressions of
%%% the specified types

-type expr() :: integer() | {expr(), 'x', expr()} | {expr(), '+', expr()}.

eval({_, '+', {_, '*', {_, '+', _}}}) -> throw(ouch);
eval({A, '+', B}) -> eval(A) + eval(B);
eval({A, '*', B}) -> eval(A) * eval(B);
eval(A) when is_integer(A) -> A.

expr_type({_, '*', _})          -> times;
expr_type({_, '+', _})          -> sum;
expr_type(N) when is_integer(N) -> constant.

format_expr({A, '+', B}) ->
  io_lib:format("(~s+~s)", [format_expr(A), format_expr(B)]);
format_expr({A, '*', B}) ->
  io_lib:format("(~s*~s)", [format_expr(A), format_expr(B)]);
format_expr(A) when is_integer(A) -> integer_to_list(A).

%%%_* Properties =======================================================

%%% Weak property, we just want to make sure we hit all the expression branches
prop_eval() ->
  ?FORALL(
     E, expr(),
     ?WHENFAIL(
        io:format("~nError with expression: ~s~n", [format_expr(E)]),
        proper:collect(expr_type(E), is_integer(eval(E))))).

%%%_* Generator =======================================================
expr() -> ?SIZED(Size, expr(Size)).

expr(0) -> proper_types:integer();
expr(Size) ->
  proper_types:oneof(
    [proper_types:integer(),
     mult(Size),
     sum(Size)]).

mult(Size) ->
  ?LET({E1, E2}, {expr(Size div 2), expr(Size div 2)}, {E1, '*', E2}).

sum(Size) ->
  ?LET({E1, E2}, {expr(Size div 2), expr(Size div 2)}, {E1, '+', E2}).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
