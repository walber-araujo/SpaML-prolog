:- module('Utils.pl', [clear_screen/0, divide_dataset/3, divide_csv_training_test/4,save_to_csv/3, save_model_to_json/2, print_models/1, ensure_csv_extension/2, remove_header/2, load_model_map/2, read_csv/2, clean_input/2]).

:- use_module(library(http/json)).
:- use_module(library(csv)).

%% clear_screen is det.
%
%  Clears the terminal.
%
clear_screen :-
    write('\e[H\e[2J'),
    flush_output.

%% convert_to_records(+Rows:list, -Records:list) is det.
%
%  Converts a list of row terms into a list of record terms.
%
convert_to_records([], []).
convert_to_records([row(Label, Message) | Rest], [record(Label, Message) | Records]) :-
    convert_to_records(Rest, Records).

%% read_csv(+FilePath:string, -Records:list) is det.
%
%  Reads a CSV file and returns its contents as a list of records.
%
%  @param FilePath The path to the CSV file to be read.
%  @param Records  A list of records, where each record is a term record(Label, Message).
%
read_csv(FilePath, Records) :-
    (   exists_file(FilePath) 
    ->  catch(
            (csv_read_file(FilePath, Rows, [functor(row), arity(2)]),
             remove_header(Rows, RowRecords),
             convert_to_records(RowRecords, Records)),
            Error,
            (print_message(error, Error), fail)
        )
    ;   print_message(error, error('File does not exist.')), fail
    ).

% divide_dataset(+Records, -TrainSet, -TestSet)
% Divide um conjunto de dados em treino (70%) e teste (30%).
divide_dataset([], [], []) :- !.
divide_dataset(Records, TrainSet, TestSet) :-
    length(Records, Total),
    TrainSize is (Total * 7) // 10,
    split_at(TrainSize, Records, TrainSet, TestSet), !.

% split_at(+N, +List, -FirstPart, -SecondPart)
% Divide a lista em duas partes: os primeiros N elementos e o restante.
split_at(0, List, [], List).
split_at(N, [H|T], [H|First], Second) :-
    N > 0,
    N1 is N - 1,
    split_at(N1, T, First, Second).

% download_default/1 lê o arquivo CSV e retorna uma lista de registros.
download_default(Records) :-
    File = '../data/train_data/SMSSpamCollection.csv',
    read_csv(File, Records).

% divide_csv_training_test(+FilePath:string, +Records:list, -TrainingSet:list, -TestSet:list)
% Divide o dataset em conjuntos de treinamento e teste com base no arquivo CSV pré-definido ou usa um dataset padrão.
divide_csv_training_test("data/train_data/SMSSpamCollection.csv", Records, TrainingSet, TestSet) :-
    divide_dataset(Records, TrainingSet, TestSet), !.  % Dividir o dataset se for o arquivo esperado.
    
divide_csv_training_test(_, Records, Records, VectorCsvDefault) :-
    download_default(VectorCsvDefault).  % Se não for o arquivo esperado, usar o dataset padrão.

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

%Retira o ., das entradas
clean_input(_, _) :-
    string_chars(_, Chars),
    (   append(Core, ['.'], Chars)
    ->  true
    ;   Core = Chars
    ),
    string_chars(_, Core).