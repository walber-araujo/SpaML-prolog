:- module('Classifier.pl', [classify_message/4, sum_probs/3, find_with_default/3]).

:- use_module(library(lists)).
:- use_module('Preprocessing.pl').

classify_message(Ham_Probs, Spam_Probs, Message, Value):- 
                tokenize(Message, Tokens),
                sum_probs(Tokens, Ham_Probs, Ham_Prob),
                sum_probs(Tokens, Spam_Probs, Spam_Prob),
                (Ham_Prob > Spam_Prob -> Value = 0; Value = 1). %0 = ham, 1 = spam

sum_probs([], _, 0).
sum_probs([Word | Rest], Probs_Map, Sum) :-
    find_with_default(Word, Probs_Map, Prob),
    SafeProb is max(Prob, 1e-10),
    LogProb is log(SafeProb),
    sum_probs(Rest, Probs_Map, Rest_Sum),
    Sum is LogProb + Rest_Sum.

find_with_default(_, [], 0).
find_with_default(Key, [(Key, Value) | _], Value) :- !.
find_with_default(Key, [row(Key, Value) | _], Value) :- !.
find_with_default(Key, [_ | Rest], Value) :-
    find_with_default(Key, Rest, Value).