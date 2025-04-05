:- module('Training.pl', [train_model_csv/3, train_model/5]).

:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module('ModelTest.pl').
:- use_module('Utils.pl').
:- consult('Utils.pl').
:- use_module('Model.pl').


train_model_csv(File_Path, Ham_Probs, Spam_Probs):- exists_file(File_Path),
                            clear_screen,
                            format('CSV file loaded from ~w \n', [File_Path]),
                            read_csv(File_Path, Messages),
                            (Messages == [] -> write('Error reading the CSV'), Ham_Probs = [], Spam_Probs = [] ;
                            divide_csv_training_test(File_Path, Messages, Train_Set, Test_Set),
                            train_model(Train_Set, Ham_Probs, Spam_Probs, _, _),
                            test_model(Test_Set, Ham_Probs, Spam_Probs, Raw_Accuracy),
                            Accuracy is Raw_Accuracy * 100,
                            format('Model accuracy on the test set: ~2f% \n', [Accuracy])).

%%Spam_Count e Ham_Count é na forma [(Word, Count)] e Train_Set e Test_Set É NA FOMRA record(Category, Message) = Head
%Ham_Probs e Spam_Probs é formato como [row(Word, Count)]
train_model(Records, Ham_Probs, Spam_Probs, _, _):-
                            count_words(Records, 0, 0, [], [], (Ham_Words, Spam_Words, _, _)),
                            sum_counts(Ham_Words, Ham_Total_Word_Count),
                            sum_counts(Spam_Words, Spam_Total_Word_Count),
                            calculate_word_probabilities(Ham_Words, Ham_Total_Word_Count, Spam_Words, Spam_Total_Word_Count, Ham_Probs),
                            calculate_word_probabilities(Spam_Words, Spam_Total_Word_Count, Ham_Words, Ham_Total_Word_Count, Spam_Probs).

sum_counts([], 0).
sum_counts([(_, Count)|T], Total) :-
    sum_counts(T, Rest),
    Total is Count + Rest.