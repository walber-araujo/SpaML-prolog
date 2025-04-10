:- begin_tests(classifier_test).
:- use_module('src/Classifier.pl').

% Test data
ham_probs([row("hello", 0.8), row("world", 0.2)]).
spam_probs([row("buy", 0.9), row("now", 0.7)]).

test(classify_ham):-
    ham_probs(HamProbs),
    spam_probs(SpamProbs),
    classify_message(HamProbs, SpamProbs, "hello world", Value),
    assertion(Value == 0).

test(classify_spam):-
    ham_probs(HamProbs),
    spam_probs(SpamProbs),
    classify_message(HamProbs, SpamProbs, "buy now", Value),
    assertion(Value == 1).

test(classify_neutral):-
    ham_probs(HamProbs),
    spam_probs(SpamProbs),
    classify_message(HamProbs, SpamProbs, "hello buy", Value),
    assertion(Value == 1).

:- end_tests(classifier_test).
