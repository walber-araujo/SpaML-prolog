%%Module      : Classifier
%Description : Classification logic to determine whether a message is 'spam' or 'ham'.
%Stability   : stable
:- module('Classifier.pl', [classify_message/4, sum_probs/3, find_with_default/3]).

:- use_module(library(lists)).
:- use_module('Preprocessing.pl').

%% classify_message(+Ham_Probs:list, +Spam_Probs:list, +Message:string, -Value:integer) is det.
%
%  Classifies a message as ham or spam based on the sum of logarithmic word probabilities.
%  Uses tokenization and compares total log-probability under both models:
%  - Returns 0 if the message is more likely ham.
%  - Returns 1 if the message is more likely spam.
%
%  @param Ham_Probs   List of row(Word, Probability) for ham messages.
%  @param Spam_Probs  List of row(Word, Probability) for spam messages.
%  @param Message     The message to classify.
%  @param Value       0 for ham, 1 for spam.
classify_message(Ham_Probs, Spam_Probs, Message, Value):- 
                tokenize(Message, Tokens),
                sum_probs(Tokens, Ham_Probs, Ham_Prob),
                sum_probs(Tokens, Spam_Probs, Spam_Prob),
                (Ham_Prob > Spam_Prob -> Value = 0; Value = 1). %0 = ham, 1 = spam

%% sum_probs(+Words:list, +Probs_Map:list, -Sum:float) is det.
%
%  Calculates the sum of the logarithmic probabilities of a list of words based on a given probability map.
%  Ensures numerical stability by setting a minimum probability threshold of 1e-10.
%
%  @param Words      List of words to evaluate.
%  @param Probs_Map  List of row(Word, Probability) terms.
%  @param Sum        The resulting sum of log probabilities.
sum_probs([], _, 0).
sum_probs([Word | Rest], Probs_Map, Sum) :-
    find_with_default(Word, Probs_Map, Prob),
    SafeProb is max(Prob, 1e-10),
    LogProb is log(SafeProb),
    sum_probs(Rest, Probs_Map, Rest_Sum),
    Sum is LogProb + Rest_Sum.

%% find_with_default(+Key:string, +List:list, -Value:any) is det.
%
%  Searches for a key in a list of row(Key, Value) terms.
%  If the key is found, returns the corresponding value.
%  If the key is not found, returns the default value 0.
%
%  @param Key    The key to search for.
%  @param List   The list of row(Key, Value) terms.
%  @param Value  The value associated with the key, or 0 if not found.
find_with_default(_, [], 0).
find_with_default(Key, [(Key, Value) | _], Value) :- !.
find_with_default(Key, [row(Key, Value) | _], Value) :- !.
find_with_default(Key, [_ | Rest], Value) :-
    find_with_default(Key, Rest, Value).