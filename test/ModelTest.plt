:- begin_tests(model_test).
:- use_module('src/Model.pl').

test(count_words_empty):-
    count_words([], (HamWords, SpamWords, HamCount, SpamCount)),
    assertion(HamWords == []),
    assertion(SpamWords == []),
    assertion(HamCount == 0),
    assertion(SpamCount == 0).

test(count_words_single_ham):-
    count_words([row('ham', 'hello world')], (HamWords, SpamWords, HamCount, SpamCount)),
    assertion(HamWords == [("hello",1), ("world",1)]),
    assertion(SpamWords == []),
    assertion(HamCount == 1),
    assertion(SpamCount == 0).

test(count_words_single_spam):-
    count_words([row('spam', 'send cash')], (HamWords, SpamWords, HamCount, SpamCount)),
    assertion(HamWords == []),
    assertion(SpamWords == [("send",1), ("cash",1)]),
    assertion(HamCount == 0),
    assertion(SpamCount == 1).

test(count_words_multiple):-
    count_words([
        row('ham', 'hello'),
        row('ham', 'hello world'),
        row('spam', 'buy premium'),
        row('spam', 'hello buy')
    ], (HamWords, SpamWords, HamCount, SpamCount)),
    assertion(HamWords == [("hello",2), ("world",1)]),
    assertion(SpamWords == [("buy",2), ("premium", 1), ("hello",1)]),
    assertion(HamCount == 2),
    assertion(SpamCount == 2).

:- end_tests(model_test).