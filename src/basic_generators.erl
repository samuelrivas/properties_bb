%%% @doc
-module(basic_generators).

-compile([export_all]).

%%% For this exercise you should use only the types exported by
%%% proper_types. Don't bother about ?LET, ?SIZED, ?SUCHTHAT, etc.

%% A dice throw (i.e. an integer in [1, 6])
die_throw() ->
  false.

%% Two dice throws, e.g. {1, 6}, {2, 4}, ...
two_throws() ->
  false.

%% Generate things like a, b, ..., z
small_letter() ->
  false.

%% Generate things like A, B, ..., Z
big_letter() ->
  false.

%% Either a big or a small letter
letter() ->
  false.

%% Generate strings like "askfjKKEp"
printable_string() ->
  false.

%% A sentence. This should generate a lower case string with words (random lists
%% of letters) of around 5 letters.
%% Don't worry about duplicated spaces (for now :))
sentence() ->
  false.

space() -> $ .

%% A printable string starting with capital letter, like "Askfjkkep"
'PrintableString'() ->
  false.

%% A gb_set (use integers as elements)
%% Hint, this may be hard
gb_set() ->
  false.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
