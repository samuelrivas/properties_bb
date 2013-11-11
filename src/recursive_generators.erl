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

eval({A, '+', B}) -> eval(A) + eval(B);
eval({A, '*', B}) -> eval(A) * eval(B);
eval(A) when is_integer(A) -> A.

expr_type({_, '*', _})          -> times;
expr_type({_, '+', _})          -> sum;
expr_type(N) when is_integer(N) -> constant.

%%%_* Properties =======================================================

%%% Weak property, we just want to make sure we hit all the expression branches
prop_eval() ->
  ?FORALL(E, expr(), proper:collect(expr_type(E), is_integer(eval(E)))).

%%%_* Generator =======================================================
expr() ->
  proper_types:oneof(
    [proper_types:integer(),
     mult(),
     sum()]).

mult() -> ?LET({E1, E2}, {expr(), expr()}, {E1, '*', E2}).

sum() -> ?LET({E1, E2}, {expr(), expr()}, {E1, '+', E2}).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
