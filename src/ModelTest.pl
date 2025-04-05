:- module('ModelTest.pl', [test_model/4, test_model_recursion/4, classifier_message/3]).

:- use_module(library(lists)).
:- use_module('Classifier.pl').
:- use_module('Utils.pl').
:- consult('Utils.pl').


test_model(Records, Ham_Probs, Spam_Probs, Accuracy):-
                    test_model_recursion(Records, Ham_Probs, Spam_Probs, Score),
                    length(Records, Total),
                    Accuracy is Score / Total.

%Value_Message is 0 or 1
test_model_recursion([record(Label, Message) | T], Ham_Probs, Spam_Probs, Score):-
                            classify_message(Ham_Probs, Spam_Probs, Message, Value_Message),
                            test_model_recursion(T, Ham_Probs, Spam_Probs, Score_Rec),
                            classifier_message(Value_Message, Label, Result_Classification),
                            Score is Result_Classification + Score_Rec.
test_model_recursion([], _, _, 0).


classifier_message(0, 'ham', 1):- !.
classifier_message(1, 'spam', 1):- !.
classifier_message(_, _, 0).