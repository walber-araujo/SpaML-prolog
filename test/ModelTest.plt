:- begin_tests(model_test).
:- use_module('src/Model.pl').

test(count_words_empty):-
    count_words([], (HamWords, SpamWords, HamCount, SpamCount)),
    assertion(HamWords == []),
    assertion(SpamWords == []),
    assertion(HamCount == 0),
    assertion(SpamCount == 0).

test(count_words_single_ham):-
    count_words([record('ham', 'hello world')], (HamWords, SpamWords, HamCount, SpamCount)),
    assertion(HamWords == [("hello",1), ("world",1)]),
    assertion(SpamWords == []),
    assertion(HamCount == 1),
    assertion(SpamCount == 0).

test(count_words_single_spam):-
    count_words([record('spam', 'send cash')], (HamWords, SpamWords, HamCount, SpamCount)),
    assertion(HamWords == []),
    assertion(SpamWords == [("send",1), ("cash",1)]),
    assertion(HamCount == 0),
    assertion(SpamCount == 1).

test(count_words_multiple):-
    count_words([
        record('ham', 'hello'),
        record('ham', 'hello world'),
        record('spam', 'buy premium'),
        record('spam', 'hello buy')
    ], (HamWords, SpamWords, HamCount, SpamCount)),
    assertion(HamWords == [("hello",2), ("world",1)]),
    assertion(SpamWords == [("buy",2), ("premium", 1), ("hello",1)]),
    assertion(HamCount == 2),
    assertion(SpamCount == 2).

test(calculate_probabilities):-
    calculate_word_probabilities([("test", 4)], 2, [("test", 1)], 1, Result),
    Div is 5 / 3,
    assertion(Result == [row("test", Div)]).

% test(probabilities_division_by_zero):-
%     calculate_word_probabilities([("test", 4)], 2, [], 0, Result),
%     assertion(Result == [row("test", 0)]).

:- end_tests(model_test).