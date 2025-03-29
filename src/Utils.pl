:- module('Utils.pl', [clear_screen/0, save_to_csv/3, save_model_to_json/2, print_models/1, ensure_csv_extension/2]).

:- use_module(library(http/json)).

% Clears the terminal, works on Linux and VSCode terminal, not on Windows Command Prompt.
clear_screen :-
    write('\e[H\e[2J'),
    flush_output.

%% save_to_csv(+FileName:string, +Classification:string, +Message:string) is det.
%
%  Saves a message along with its classification to a CSV file.
%
%  @param FileName The file to save the data.
%  @param Classification The classification of the message (e.g., 'ham' or 'spam').
%  @param Message The message to be saved.
%
%  This predicate appends the classification and message to the CSV file and clears the terminal.
%
save_to_csv(FileName, Classification, Message) :-
    % Create the CSV line format.
    atomic_list_concat([Classification, Message], ',', CSVLine),
    atom_concat(CSVLine, '\n', FormattedLine),

    % Open the file, append the data, and close the file.
    open(FileName, append, Stream),
    write(Stream, FormattedLine),
    close(Stream),

    % Clear the terminal and print the success message.
    clear_screen,
    writeln("Data saved successfully!\n").

%% save_model_to_json(+ModelName:string, +FilePath:string) is det.
%
%  Updates and saves the models list to a JSON file.
%
%  @param ModelName The name of the model.
%  @param FilePath The path where the model is located.
%
%  This predicate updates the JSON file with the models information and shows a message indicating success.
%
save_model_to_json(ModelName, FilePath) :-
    JsonPath = './data/models/models.json',

    % Load the existing models from the JSON file or create a new dictionary if not exists.
    (   exists_file(JsonPath)
    ->  open(JsonPath, read, Stream),
        json_read_dict(Stream, ExistingModels),
        close(Stream)
    ;   ExistingModels = _{}
    ),

    % Insert the new model into the dictionary.
    NewModels = ExistingModels.put(ModelName, FilePath),

    % Write the updated dictionary back to the JSON file.
    open(JsonPath, write, OutStream),
    json_write_dict(OutStream, NewModels),
    close(OutStream),

    % Print success message.
    writeln('\n✅ Model saved successfully!').

%% load_model_map(+Path:string, -Models:dict) is det.
%
%  Loads a JSON file containing the models and returns them as a dictionary.
%
%  @param Path The path to the JSON file containing the models.
%  @param Models A dictionary mapping model names to file paths.
%
%  This predicate loads the JSON file and returns a dictionary with the models.
%  If the file does not exist, it returns an empty dictionary.
%
load_model_map(Path, Models) :-
    % Check if the file exists
    (   exists_file(Path)
    ->  % The file exists, open and read the JSON content
        open(Path, read, Stream),
        json_read_dict(Stream, Models),
        close(Stream)
    ;   % The file does not exist, return an empty dictionary
        Models = _{}
    ).

%% print_models(+ModelsDict:dict) is det.
%
%  Displays the available models stored in a dictionary.
%
%  @param ModelsDict The dictionary containing model names and their file paths.
%
%  This predicate iterates over the dictionary and prints each model name.
%
print_models(ModelsDict) :-
    % Extract keys from the dictionary which represent model names
    dict_keys(ModelsDict, ModelNames),
    
    % Print each model name
    maplist(writeln, ModelNames).

%% ensure_csv_extension(+FileName:string, -Result:string) is det.
%
%  Ensure that the file name has the suffix `.csv`.
%
%  @param FileName The original file name.
%  @param Result   The file name ensured to end with `.csv`.
%
ensure_csv_extension(FileName, Result) :-
    (   sub_atom(FileName, _, 4, 0, '.csv')
    ->  Result = FileName
    ;   atom_concat(FileName, '.csv', Result)
    ).

remove_header([row('Label', 'Message') | Tail], Tail):-!.
remove_header(Rows, Rows).
