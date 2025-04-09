:- begin_tests(preprocessing_test).
:- use_module('src/Preprocessing.pl').

test(tokenize_simple):-
    tokenize("Hello world", Tokens),
    assertion(Tokens == ["hello", "world"]).

test(tokenize_punctuation):-
    tokenize("Hello, world!", Tokens),
    assertion(Tokens == ["hello", "world"]).

test(tokenize_multiple_spaces):-
    tokenize("  Hello   world  ", Tokens),
    assertion(Tokens == ["hello", "world"]).

:- end_tests(preprocessing_test).
