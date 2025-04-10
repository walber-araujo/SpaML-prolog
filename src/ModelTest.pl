%%Module      : ModelTest
%Description : Function to test the model and calculate accuracy.
%Stability   : stable.
:- module('ModelTest.pl', [test_model/4, test_model_recursion/4, classifier_message/3]).

:- use_module(library(lists)).
:- use_module('Classifier.pl').
:- use_module('Utils.pl').
:- consult('Utils.pl').

%% test_model(+Records:list, +Ham_Probs:list, +Spam_Probs:list, -Accuracy:float) is det.
%
%  Evaluates the classifier on a list of records and calculates the overall accuracy.
%
%  @param Records     The test dataset as a list of record(Label, Message).
%  @param Ham_Probs   Word probabilities for ham messages.
%  @param Spam_Probs  Word probabilities for spam messages.
%  @param Accuracy    The ratio of correctly classified messages to total messages.
test_model(Records, Ham_Probs, Spam_Probs, Accuracy):-
                    test_model_recursion(Records, Ham_Probs, Spam_Probs, Score),
                    length(Records, Total),
                    Accuracy is Score / Total.

%% test_model_recursion(+TestSet:list, +Ham_Probs:list, +Spam_Probs:list, -Score:integer) is det.
%
%  Recursively evaluates the classifier on a test set and accumulates a correctness score.
%
%  @param TestSet   A list of records in the form record(Label, Message).
%  @param Ham_Probs Word probabilities for ham messages.
%  @param Spam_Probs Word probabilities for spam messages.
%  @param Score     The total number of correctly classified messages.
test_model_recursion([record(Label, Message) | T], Ham_Probs, Spam_Probs, Score):-
                            classify_message(Ham_Probs, Spam_Probs, Message, Value_Message),
                            test_model_recursion(T, Ham_Probs, Spam_Probs, Score_Rec),
                            classifier_message(Value_Message, Label, Result_Classification),
                            Score is Result_Classification + Score_Rec.
test_model_recursion([], _, _, 0).

%% classifier_message(+Label:integer, -Category:string, -Result:integer) is det.
%
%  Maps numeric labels to category names and marks the classification result.
%  - 0 is mapped to 'ham'
%  - 1 is mapped to 'spam'
%  - Any other value results in a failed classification (Result = 0)
%
%  @param Label     The numeric label (e.g., 0 or 1).
%  @param Category  The corresponding category name ('ham' or 'spam').
%  @param Result    1 if classification succeeded, 0 otherwise.
classifier_message(0, 'ham', 1):- !.
classifier_message(1, 'spam', 1):- !.
classifier_message(_, _, 0).