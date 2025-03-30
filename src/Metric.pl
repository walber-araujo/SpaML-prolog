:- module('Metric.pl', [show_accuracy/2, accuracy_recursion/1, accuracy_csvs/1, model_classification/2]).

:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module('ModelTest.pl').
:- use_module('Utils.pl').
:- consult('Utils.pl').
:- use_module('Training.pl').


show_accuracy(File_Path, Accuracy):- exists_file(File_Path),
                                csv_read_file(File_Path, Rows, [functor(my_record), arity(_)]),
                                (Rows == [] -> write("Error reading the CSV: "), Accuracy is 0.0 ;
                                divide_csv_training_test(File_Path, Rows, Train_Set, Test_Set),
                                train_model(Train_Set, Ham_Probs, Spam_Probs, _, _),
                                test_model(Test_Set, Ham_Probs, Spam_Probs, Raw_Accuracy),
                                Accuracy is round(Raw_Accuracy * 10000) / 100), !.
show_accuracy(_, -1.0).

accuracy_recursion([]):- write("The default model accuracy is calculated by training the model,"), 
                    write("where 30% of the data from the file is used for training, and 70% is reserved for testing."), 
                    write("When is used other model criated by the user are used 100% of the new data to training and 100% of default file to testing."),
                    write("First, the messages are counted and categorized as spam or ham (not spam)."),
                    write("Then, the model calculates the probability of a message being spam or ham."),
                    write("Finally, the data set aside for testing is processed by the classifier,"),
                    write("which determines whether each message is spam or ham."),
                    write("The model then evaluates whether the classification was correct or not."),
                    write("The accuracy is calculated as the ratio of correctly classified messages to the total number of test messages.\n"),
                    write("Rating ranges: \n"),
                    write("0% - 65% = Bad\n"),
                    write("65% - 85% = Moderate\n"),
                    write("85% - 100% = Good\n"),
                    write("------------------------------------------------------------------------------\n"),
                    write("| Model Name                     | Accuracy (%)          | Classification     |\n"),
                    write("------------------------------------------------------------------------------").

accuracy_recursion([(Model_Name, File_Path)|T]):- accuracy_recursion(T),
                    show_accuracy(File_Path, Accuracy),
                    (Accuracy =:= (-1.0) -> format("| |-30t~w~ | |-42t~w~ |\n", [Model_Name, "File path not found"]); 
                    model_classification(Accuracy, Classification),
                    format("| |-30t~w~ | |-21.2t~2f~ | ~|-18t~w~ |\n", [Model_Name, Accuracy, Classification])).                                                


accuracy_csvs(File_Path):- load_model_map("./data/models/models.json", Model_Map),
                        dict_pairs(Model_Map, _, Lista_Model),
                        accuracy_recursion(Lista_Model).


model_classification(Accuracy, "Bad"):- Accuracy < 65.0, !.
model_classification(Accuracy, "Moderate"):- Accuracy < 85.0, !.
model_classification(Accuracy, "Good").