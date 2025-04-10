%%Module      : Training
%Description : Model training logic.
%Stability   : stable.
:- module('Training.pl', [train_model_csv/3, train_model/3]).

:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module('ModelTest.pl').
:- use_module('Utils.pl').
:- consult('Utils.pl').
:- use_module('Model.pl').

%% train_model_csv(+File_Path:string, -Ham_Probs:list, -Spam_Probs:list) is det.
%
%  Loads a dataset from a CSV file, trains a Naive Bayes model, and evaluates its accuracy on a test set.
%
%  @param File_Path   The path to the CSV file containing labeled messages.
%  @param Ham_Probs   The computed list of word probabilities for ham messages.
%  @param Spam_Probs  The computed list of word probabilities for spam messages.
%
%  Displays the accuracy of the model after testing, and handles cases where the file
%  does not exist or is empty.
train_model_csv(File_Path, Ham_Probs, Spam_Probs):- exists_file(File_Path),
    clear_screen,
    write("Loading model...\n"),
    read_csv(File_Path, Messages),
    (Messages == [] -> write('Error reading the CSV'), Ham_Probs = [], Spam_Probs = [] ;
    divide_csv_training_test(File_Path, Messages, Train_Set, Test_Set),
    train_model(Train_Set, Ham_Probs, Spam_Probs),
    test_model(Test_Set, Ham_Probs, Spam_Probs, Raw_Accuracy),
    Accuracy is Raw_Accuracy * 100,
    format('Model accuracy on the test set: ~2f% \n', [Accuracy])).

%% train_model(+Records:list, -Ham_Probs:list, -Spam_Probs:list) is det.
%
%  Trains a spam classifier model using a list of labeled messages.
%
%  @param Records A list of records in the form record(Label, Message), where Label is 'ham' or 'spam'.
%  @param Ham_Probs The output list of word probabilities for ham messages.
%  @param Spam_Probs The output list of word probabilities for spam messages.
%
train_model(Records, Ham_Probs, Spam_Probs):-
    count_words(Records, (Ham_Words, Spam_Words, HamCount, SpamCount)),
    calculate_word_probabilities(Ham_Words, HamCount, Spam_Words, SpamCount, Ham_Probs),
    calculate_word_probabilities(Spam_Words, SpamCount, Ham_Words, HamCount, Spam_Probs).
