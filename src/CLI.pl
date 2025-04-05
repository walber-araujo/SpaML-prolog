:- module('CLI.pl', [menu/0, process_option/1, classification_submenu/2, loop/2, reusing_previous_model_submenu/0, previous_model_submenu_two/2, lookup_model_name/2]).

:- use_module(library(csv)).
:- use_module('Utils.pl').
:- consult('Utils.pl').
:- use_module('Model.pl').
:- use_module('Intro.pl').
:- use_module('Training.pl').
:- use_module('Classifier.pl').
:- use_module('Metric.pl').

menu:-
    clear_screen,
    write('\n====================================================================================\n'),
    write('Menu Options:\n\n'),
    write('[1]. Reuse previous models.\n'),
    write('[2]. Add new model.\n'),
    write('[3]. Remove a model.\n'),
    write('[4]. Train model manually.\n'),
    write('[5]. Classify individual messages using the default model.\n'),
    write('[6]. Show results with accuracy rates.\n'),
    write('[7]. Exit.\n'),
    write("\nChoose an option (1-7): "),
    read_line_to_string(user_input, Option),
    process_option(Option).

process_option("1"):- 
    clear_screen,
    reusing_previous_model_submenu,
    menu, !.

%write('Not implemented'), !.
process_option("2"):- 
    write('Not implemented'), !.
process_option("3"):- write('Not implemented'), !.
process_option("4"):- write('Not implemented'), !.

% exemplo de uso
process_option("5"):-
    read_csv('data/train_data/SMSSpamCollection.csv', Messages),
    %remove_header(Rows, R),
    count_words(Messages, (HamWords, SpamWords, HamCount, SpamCount)),
    format('\nHam message count: ~w\n', HamCount),
    format('Spam message count: ~w\n', SpamCount),
    format('Ham Words: ~w~n', [HamWords]),
    format('Spam Words: ~w~n', [SpamWords]), !.

process_option("6"):- write('Not implemented'), !.
process_option("7"):- show_out, halt.

process_option(_):- write('\nInvalid option. Please try again ok.\n').

classification_submenu(Ham_Probs, Spam_Probs):-
    clear_screen,
    write('\nClassification Submenu:\n'),
    write('1. Classify a message\n'),
    write('2. Return to main menu\n'),
    write('\nChoose an option: '),
    read_line_to_string(user_input, Option),
    classification_submenu_two(Option, Ham_Probs, Spam_Probs), !.

classification_submenu_two("1", Ham_Probs, Spam_Probs):-
    clear_screen,
    loop(Ham_Probs, Spam_Probs), !.
classification_submenu_two("2", _, _):-
    clear_screen,
    write('\nReturning to main menu...\n'), !.
classification_submenu_two(_, Ham_Probs, Spam_Probs):-
    write('Invalid option. Please try again.\n'),
    classification_submenu(Ham_Probs, Spam_Probs).

loop(Ham_Probs, Spam_Probs):-
    write('\nType a message to classify (or "exit" to quit):\n'),
    write('> '),
    read_line_to_string(user_input, Option),
    (Option == "exit" ->
        write('\nReturning to main menu...\n'),
        clear_screen
    ;
    classify_message(Ham_Probs, Spam_Probs, Option, Result),
    (Result == 0 ->  
        write('The message has been classified as: ham\n')
    ;   
    write('The message has been classified as: spam\n')
    ),
    loop(Ham_Probs, Spam_Probs)
    ).


reusing_previous_model_submenu:-
    load_model_map('./data/models/models.json', Dict_Model),
    write('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n'),
    write('       Available Models       \n'),
    write('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n'),
    print_models(Dict_Model),
    write('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n'),
    write('\nEnter the name of the model you want to reuse (or "exit" to quit): '),
    read_line_to_string(user_input, Model),
    string_to_atom(Model,Model_Name),
    dict_pairs(Dict_Model, _, Model_Map),
    previous_model_submenu_two(Model_Name, Model_Map).

previous_model_submenu_two('exit', _):-
    clear_screen,
    menu, !.
previous_model_submenu_two(Model_Name, Model_Map):-
    lookup_model_name(Model_Name, Model_Map).

%Value - path é na forma data/train_data/SMSSpamCollection.csv
lookup_model_name(Key, [Key-Value | _]):-
    exists_file(Value),
    write('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n'),
    format('  Training with model: ~w\n', [Key]),
    write('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n'),
    train_model_csv(Value, Ham_Probs, Spam_Probs),
    classification_submenu(Ham_Probs, Spam_Probs),
    clear_screen,
    menu, !.
lookup_model_name(Key, [Key-Value | _]):-
    clear_screen,
    format('\n⚠️  CSV file ~w not found. Please check the file path.\n', [Value]),
    reusing_previous_model_submenu, !.
lookup_model_name(_, [_ | Tail]):-
    lookup_model_name(_, Tail).
lookup_model_name(Model_Name, []):-
    clear_screen,
    format('\n⚠️  Model ~w not found. Please try again.\n', [Model_Name]),
    reusing_previous_model_submenu.