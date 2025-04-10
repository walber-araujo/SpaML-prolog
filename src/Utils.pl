%%Module      : Utils
%Description : Utility functions for file handling, data manipulation, and model management.
%Stability   : stable
:- module('Utils.pl', [clear_screen/0, divide_dataset/3, divide_csv_training_test/4,save_to_csv/3, save_model_to_json/2, print_models/1, ensure_csv_extension/2, remove_header/2, load_model_map/2, read_csv/2, clean_input/2, write_json/2, remove_key_from_dict/3]).

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
%  @param Rows     A list of terms in the format row(Label, Message).
%  @param Records  A list of terms in the format record(Label, Message).
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

%% divide_dataset(+Records:list, -TrainSet:list, -TestSet:list) is det.
%
%  Splits a list of records into training and test sets using a 70/30 ratio.
%
%  @param Records   The complete list of records.
%  @param TrainSet  The resulting list containing 70% of the records for training.
%  @param TestSet   The remaining 30% of the records for testing.
divide_dataset([], [], []) :- !.
divide_dataset(Records, TrainSet, TestSet) :-
    length(Records, Total),
    TrainSize is (Total * 7) // 10,
    split_at(TrainSize, Records, TrainSet, TestSet), !.

%% split_at(+N:integer, +List:list, -First:list, -Second:list) is det.
%
%  Splits a list into two parts at the specified index.
%
%  @param N       The index at which to split the list.
%  @param List    The input list to be split.
%  @param First   The sublist containing the first N elements.
%  @param Second  The sublist containing the remaining elements.
split_at(0, List, [], List).
split_at(N, [H|T], [H|First], Second) :-
    N > 0,
    N1 is N - 1,
    split_at(N1, T, First, Second).

%% download_default(-Records:list) is det.
%
%  Loads the default dataset from a predefined CSV file path.
%
%  @param Records  A list of records read from the default CSV file.
%
download_default(Records) :-
    File = './data/train_data/SMSSpamCollection.csv',
    read_csv(File, Records).

%% divide_csv_training_test(+FilePath:string, +Records:list, -TrainingSet:list, -TestSet:list) is det.
%
%  Splits the dataset into training and test sets using the given records.
%
%  @param FilePath    The path to the original CSV file (used for identifying the dataset context).
%  @param Records     The list of all records to be divided.
%  @param TrainingSet The resulting list of training records.
%  @param TestSet     The resulting list of test records.
%
divide_csv_training_test("data/train_data/SMSSpamCollection.csv", Records, TrainingSet, TestSet) :-
    divide_dataset(Records, TrainingSet, TestSet), !.

%% divide_csv_training_test(+InputPath:string, +Records:list, -TrainingRecords:list, +VectorCsvDefault:string) is det.
%
%  Divides the CSV records into training data. In this version, no actual division is performed; all records are used as training data,
%  and a default vector CSV is downloaded.
%
%  @param InputPath         Placeholder for input file path (not used in logic).
%  @param Records           The list of all records.
%  @param TrainingRecords   The output list of training records (same as input records).
%  @param VectorCsvDefault  The path or identifier used to download the default vector CSV.
%
divide_csv_training_test(_, Records, Records, VectorCsvDefault) :-
    download_default(VectorCsvDefault).

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
    atomic_list_concat([Classification, Message], ',', CSVLine),
    atom_concat(CSVLine, '\n', FormattedLine),

    open(FileName, append, Stream),
    write(Stream, FormattedLine),
    close(Stream),

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

    (   exists_file(JsonPath)
    ->  open(JsonPath, read, Stream),
        json_read_dict(Stream, ExistingModels),
        close(Stream)
    ;   ExistingModels = _{}
    ),

    NewModels = ExistingModels.put(ModelName, FilePath),

    open(JsonPath, write, OutStream),
    json_write_dict(OutStream, NewModels),
    close(OutStream),

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
%  Ensure that the file name has the suffix '.csv'.
%
%  @param FileName The original file name.
%  @param Result   The file name ensured to end with '.csv'.
%
ensure_csv_extension(FileName, Result) :-
    (   sub_atom(FileName, _, 4, 0, '.csv')
    ->  Result = FileName
    ;   atom_concat(FileName, '.csv', Result)
    ).

remove_header([row('Label', 'Message') | Tail], Tail):-!.
remove_header(Rows, Rows).

%% clean_input(+Input:string, -Cleaned:string) is det.
%
%  Cleans an input string by removing a trailing period if present.
%
%  @param Input    The original input string.
%  @param Cleaned  The cleaned version of the string without a trailing period.
%
clean_input(_, _) :-
    string_chars(_, Chars),
    (   append(Core, ['.'], Chars)
    ->  true
    ;   Core = Chars
    ),
    string_chars(_, Core).

%% write_json(+Path:string, +Data:dict) is det.
%
%  Writes the given Data (a dict) to the file at Path in JSON format.
%
%  @param Path  The file path where the JSON data will be written.
%  @param Data  The dictionary to be written as JSON.
%
write_json(Path, Data) :-
    open(Path, write, Stream),
    json_write_dict(Stream, Data),
    close(Stream).

%% remove_key_from_dict(+Key:any, +Dict:dict, -NewDict:dict) is det.
%
%  Removes a key-value pair from a dictionary based on the specified key.
%
%  @param Key      The key to be removed from the dictionary.
%  @param Dict     The original dictionary.
%  @param NewDict  The resulting dictionary without the specified key.
%
remove_key_from_dict(Key, Dict, NewDict) :-
    dict_pairs(Dict, Tag, Pairs),
    exclude(pair_with_key(Key), Pairs, NewPairs),
    dict_create(NewDict, Tag, NewPairs).

%% pair_with_key(+Key:any, +Pair:pair) is semidet.
%
%  Succeeds if the given pair is of the form Key-_, indicating it is paired with the specified Key.
%
%  @param Key   The key to match in the pair.
%  @param Pair  A term of the form Key-Value.
%
pair_with_key(Key, Key-_) :- !.
pair_with_key(_, _) :- false.