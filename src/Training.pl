:- module(Training, [train_model_csv/3, clear_screen/0, train_model/5]).

:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module(ModelTest).
:- use_module(Utils).
:- use_module(Model).


train_model_csv(File_Path, Ham_Probs, Spam_Probs):- exists_file(File_Path),
                            clear_screen,
                            write("CSV file loaded from ~w", [File_Path]),
                            csv_read_file(File_Path, Rows, [functor(my_record), arity(_)]),
                            (Rows == [] -> write("Error reading the CSV"), Ham_Probs = [], Spam_Probs = [] ;
                            divide_csv_training_test(File_Path, Rows, Train_Set, Test_Set),
                            train_model(Train_Set, Ham_Probs, Spam_Probs, _, _),
                            test_model(Test_Set, Ham_Probs, Spam_Probs, Raw_Accuracy),
                            Accuracy is Raw_Accuracy * 100,
                            write("Model accuracy on the test set: ~2f%", [Accuracy])).

train_model(Records, Ham_Probs, Spam_Probs, Ham_Count, Spam_Count):-
                            count_words(Records, [], [], 0, 0, Ham_Words, Spam_Words, Ham_Count, Spam_Count),
                            calculate_word_probabilities(Ham_Words, Ham_Count, Spam_Words, Spam_Count, [], Ham_Probs),
                            calculate_word_probabilities(Spam_Words, Spam_Count, Ham_Words, Ham_Count, [], Spam_Probs).