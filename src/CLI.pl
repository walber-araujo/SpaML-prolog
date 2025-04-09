%%Module      : CLI
%Description : Command-line interface for interacting with the message classifier.
%Stability   : stable
:- module('CLI.pl', [menu/0, process_option/1, classification_submenu/2, loop/2, reusing_previous_model_submenu/0, previous_model_submenu_two/2, lookup_model_name/2]).

:- use_module(library(csv)).
:- use_module('Utils.pl').
:- consult('Utils.pl').
:- use_module('Model.pl').
:- use_module('Intro.pl').
:- use_module('Training.pl').
:- use_module('Classifier.pl').
:- use_module('Metric.pl').
%% menu is det.
%
%  Displays the main menu interface and processes user input.
%
%  - Prompts the user to select an option by entering a number between 1 and 7.
%  - Passes the input string to 'process_option/1' for handling.
%
%  This is the central navigation point for the system, allowing the user to interact
%  with all major functionalities of the application.
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
    write('\nChoose an option (1-7): '),
    read_line_to_string(user_input, Option),
    process_option(Option).

%% process_option(+Option) is det.
%
%  Forwards to the function according to the input option
%
process_option("1"):- 
    clear_screen,
    reusing_previous_model_submenu,
    menu.

process_option("2"):- 
    write('\nAdd a new model by providing a name and selecting a CSV file containing training data.\n'), 
    write('Type a name to your model (or "exit" to quit).\n'),
    add_new_model_submenu,
    menu.
    
process_option("3"):- 
    clear_screen,
    remove_model_submenu,
    menu.

process_option("4") :-
    clear_screen,
    write('Training model manually...\n'),
    write('Enter the file name or type exit to return: '),
    flush_output,
    read_line_to_string(user_input, FileName),
    ( FileName == "exit" ->
        clear_screen,
        menu
    ;  
        string_to_atom(FileName, ModelName),
        ensure_csv_extension(FileName, CSVName),
        clear_screen,
        training_manual_submenu(CSVName, ModelName)
    ).

process_option("5"):-
    clear_screen,
    write('\nClassifying individual messages...\n'),
    train_model_csv("data/train_data/SMSSpamCollection.csv", Ham_Probs, Spam_Probs),
    classification_submenu(Ham_Probs, Spam_Probs),
    menu.

process_option("6"):- 
    clear_screen,
    write('\nShowing results with accuracy rates...\n'),
    accuracy_csvs,
    wait_for_any_key,
    menu.

process_option("7"):- show_out, halt.
process_option(_):-
    write('\nInvalid option. Please try again.\n'),
    wait_for_any_key,
    menu.

%% add_new_model_submenu is det.
%
%  Prompts the user to provide a name and path for a new model and attempts to save it.
%
%  Behavior:
%    - Asks for a model name.
%    - If the input is not "exit", prompts for the model file path.
%    - Validates the file path.
%    - If valid, saves the model name and path to a JSON map using 'save_model_to_json/2'.
%    - If the path is invalid or the user types "exit", the operation is canceled silently.
add_new_model_submenu:-
    write('Enter the new model name: '),
    read_line_to_string(user_input, Model),
    string_to_atom(Model,Model_Name),
    (Model_Name \= 'exit' ->
    ask_path(Path_Model),
    (Path_Model \= "unknown" ->
    save_model_to_json(Model_Name, Path_Model));
    _ = Model_Name
    ).

%% ask_path(-Model_Path) is det.
%
%  Prompts the user to enter a model path and verifies its existence.
%
%  Parameters:
%    - Model_Path: Unifies with the valid file path if it exists, or the atom "unknown" if the user types "exit".
%
%  Behavior:
%    - Re-prompts the user if the entered file does not exist.
%    - Clears the screen and shows a warning for invalid paths.
%    - Accepts "exit" to cancel the operation and returns "unknown".
ask_path(Model_Path):-
    write('Enter the model path (or "exit" to quit): '),
    read_line_to_string(user_input, Path),
    string_to_atom(Path,File_Path),
    (File_Path \= 'exit' ->
    (exists_file(File_Path) -> 
    Model_Path = File_Path ;
    clear_screen,
    format('\n⚠️ Model ~w not found. Please try again.\n', [File_Path]),
    ask_path(Model_Path)) ;
    Model_Path = "unknown").

%% training_manual_submenu(+FilePath, +ModelName) is det.
%
%  Handles manual training of a model by collecting spam and ham messages from user input.
%
%  Parameters:
%    - FilePath: Path where the temporary CSV training data will be stored.
%    - ModelName: The name to associate with the trained model.
%
%  Flow:
%    1. Creates a new CSV file with headers.
%    2. Prompts the user to input spam messages first.
%    3. Then prompts the user to input ham messages.
%    4. Asks the user whether to save the model.
%       - If yes, the models path is saved in the model registry (models.json).
%       - If no, the temporary training file is deleted.
%    5. Returns to the main menu after completion.
training_manual_submenu(CSVName, ModelName) :-
    atomic_list_concat(["data/train_data/", CSVName], FilePath),
    open(FilePath, write, Stream),
    format(Stream, "Label,Message\n", []),
    close(Stream),

    write('Enter spam messages first. Type "exit" to move to ham messages.\n'),
    flush_output,
    collect_messages(FilePath, "spam"),

    clear_screen,
    write('Now enter ham messages. Type "exit" to stop.\n'),
    flush_output,
    collect_messages(FilePath, "ham"),

    clear_screen,
    write('Do you want to save this model? (y/n): '),
    flush_output,
    read_line_to_string(user_input, Save),
    ( Save == "y" ->
        % Salva o caminho no JSON
        save_model_to_json(ModelName, FilePath),
        writeln('Model saved successfully.\n')
    ; 
        delete_file(FilePath),
        writeln('Model not saved. File deleted.\n')
    ),
    wait_for_any_key,
    menu.

%% collect_messages(+FilePath, +Label) is det.
%
%  Opens a file in append mode and starts collecting labeled messages from user input.
%
%  Parameters:
%    - FilePath: The path to the CSV file where the labeled messages will be saved.
%    - Label: The label to assign to each message (e.g., 'ham' or 'spam').
%
%  Behavior:
%    - Opens the specified file for appending.
%    - Calls 'collect_loop/2' to read and store messages with the given label.
%    - Ensures the stream is properly closed after the loop ends.
collect_messages(FilePath, Label) :-
    open(FilePath, append, Stream),
    collect_loop(Stream, Label),
    close(Stream).


%% collect_loop(+Stream, +Label) is det.
%
%  Reads user input messages in a loop and writes them to a given stream in CSV format.
%
%  Parameters:
%    - Stream: The output stream to write the data into (typically a CSV file).
%    - Label: The label for the messages (e.g., 'ham' or 'spam').
%
%  Behavior:
%    - Prompts the user to input a message with the given label.
%    - If the user types "exit", the loop stops.
%    - If the input is empty (""), it re-prompts without writing.
%    - Otherwise, it writes the message in the format: 'Label,"Message"' to the stream.
%    - Repeats the process recursively.
collect_loop(Stream, Label) :-
    format("~w> ", [Label]),
    flush_output,
    read_line_to_string(user_input, Msg),
    ( Msg == "exit" ->
        true
    ; Msg == "" ->
        collect_loop(Stream, Label)
    ; 
        format(Stream, "~w,\"~w\"\n", [Label, Msg]),
        collect_loop(Stream, Label)
    ).

%% classification_submenu(+Ham_Probs, +Spam_Probs) is det.
%
%  Displays the classification submenu and handles user interaction.
%
%  Parameters:
%    - Ham_Probs: List of word probabilities for 'ham' messages.
%    - Spam_Probs: List of word probabilities for 'spam' messages.
%
%  Behavior:
%    - Clears the screen.
%    - Shows submenu options to the user:
%        1. Classify a message.
%        2. Return to the main menu.
%    - Reads the user input as a string.
%    - Delegates the handling of the selected option to 'classification_submenu_two/3'.
classification_submenu(Ham_Probs, Spam_Probs):-
    clear_screen,
    write('\nClassification Submenu:\n'),
    write('1. Classify a message\n'),
    write('2. Return to main menu\n'),
    write('\nChoose an option: '),
    read_line_to_string(user_input, Option),
    classification_submenu_two(Option, Ham_Probs, Spam_Probs), !.

%% classification_submenu_two(+Option, +Ham_Probs, +Spam_Probs) is det.
%
%  Handles the user selection from the classification submenu.
%
%  Parameters:
%    - Option: User input (as a string) representing the menu choice.
%    - Ham_Probs: List of word probabilities for 'ham' messages.
%    - Spam_Probs: List of word probabilities for 'spam' messages.
%
%  Behavior:
%    - If Option is "1":
%        - Clears the screen.
%        - Starts the message classification loop via 'loop/2'.
%    - If Option is "2":
%        - Clears the screen and returns to the main menu with a message.
%    - For any other Option:
%        - Informs the user of the invalid input.
%        - Calls 'classification_submenu/2' again to re-prompt the user.
classification_submenu_two("1", Ham_Probs, Spam_Probs):-
    clear_screen,
    loop(Ham_Probs, Spam_Probs), !.
classification_submenu_two("2", _, _):-
    clear_screen,
    write('\nReturning to main menu...\n'), !.
classification_submenu_two(_, Ham_Probs, Spam_Probs):-
    write('Invalid option. Please try again.\n'),
    classification_submenu(Ham_Probs, Spam_Probs).

%% loop(+Ham_Probs, +Spam_Probs) is det.
%
%  Provides an interactive loop for classifying user-input messages.
%
%  Parameters:
%    - Ham_Probs: List of word probabilities for 'ham' messages.
%    - Spam_Probs: List of word probabilities for 'spam' messages.
%
%  Behavior:
%    - Prompts the user to input a message to classify.
%    - If the user enters "exit", the loop ends and control returns to the main menu.
%    - Otherwise, the message is classified using 'classify_message/4':
%        - Displays whether the message is classified as "ham" or "spam".
%    - The loop then repeats, allowing continuous classification.
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

%% reusing_previous_model_submenu is det.
%
%  Displays a submenu allowing the user to select and reuse an existing model.
%
%  Behavior:
%  - Loads the saved models from './data/models/models.json'.
%  - Displays a formatted list of available models.
%  - Prompts the user to enter the name of the model they wish to reuse.
%  - If the user enters "exit", returns to the main menu.
%  - Otherwise, attempts to find and use the selected model via 'previous_model_submenu_two/2'.
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

%% previous_model_submenu_two(+Model_Name:string, +Model_Map:list) is det.
%
%  Handles the selection of a previously saved model from a submenu.
%
%  Behavior:
%  - If the user inputs "exit", it clears the screen and returns to the main menu.
%  - Otherwise, it attempts to find and load the model by calling 'lookup_model_name/2'.
%
%  @param Model_Name The name of the model entered by the user.
%  @param Model_Map A list of pairs ModelName-CSVPath representing the available models.
previous_model_submenu_two('exit', _):-
    clear_screen,
    menu, !.
previous_model_submenu_two(Model_Name, Model_Map):-
    lookup_model_name(Model_Name, Model_Map).


%% lookup_model_name(+Key:string, +ModelMap:list) is det.
%
%  Searches for a model by its name (Key) within a list of model pairs (ModelMap).
%  Each element in ModelMap is a pair Key-Value, where Key is the model name and Value is the path to the models CSV file.
%
%  Behavior:
%  - If the model is found and the CSV file exists, it loads and trains the model using 'train_model_csv/3',
%    launches the classification submenu with the trained model, then clears the screen and returns to the main menu.
%  - If the model is found but the file does not exist, it notifies the user and redirects to the previous submenu.
%  - If the model is not found after checking all pairs, it shows an error and redirects to the submenu.
%
%  @param Key The name of the model to look for.
%  @param ModelMap A list of pairs ModelName-CSVPath representing stored models.
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
lookup_model_name(Key, [_ | Tail]):-
    lookup_model_name(Key, Tail).
lookup_model_name(Model_Name, []):-
    clear_screen,
    format('\n⚠️  Model ~w not found. Please try again.\n', [Model_Name]),
    reusing_previous_model_submenu.


%% remove_model_submenu is det.
%
%  Displays a submenu to remove a model from the models JSON file.
%  When a model is removed, its key and value are deleted from the JSON.
%
remove_model_submenu :-
    JsonPath = "./data/models/models.json",
    load_model_map(JsonPath, ModelMap),
    (   is_dict(ModelMap)
    ->  true
    ;   ModelMap = _{}  % Garante que ModelMap seja um dict.
    ),
    (   ModelMap = {} ->
            nl, write("\nNo models found to remove."), nl,
            wait_for_any_key
    ;   nl, write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"), nl,
        write("       Available Models       "), nl,
        write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"), nl,
        print_models(ModelMap),
        write("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"), nl,
        write("\nEnter the name of the model to remove (or 'exit' to cancel): "),
        flush_output(current_output),
        read_line_to_string(user_input, InputString),
        (   InputString = "exit" ->
                true
        ;   atom_string(ModelAtom, InputString),
            (   member(ModelAtom, [model1, model2]) ->
                    format("\n⚠️  Model '~w' cannot be removed as it is a default model of the system.\n", [ModelAtom]),
                    wait_for_any_key,
                    remove_model_submenu
            ;   (   get_dict(ModelAtom, ModelMap, _) ->
                        % Remove a chave usando o helper remove_key_from_dict/3.
                        remove_key_from_dict(ModelAtom, ModelMap, UpdatedModels),
                        write_json(JsonPath, UpdatedModels),
                        format("\nModel '~w' removed successfully!\n", [ModelAtom]),
                        wait_for_any_key
                ;   format("\nModel '~w' not found. Please try again.\n", [ModelAtom]),
                    remove_model_submenu
                )
            )
        )
    ).
