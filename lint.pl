:- initialization(main).
:- set_prolog_flag(warnings, on).

:- dynamic lint_error/0.

% Message hook to capture warning and error messages.
% When a warning or error message is generated, assert a flag (lint_error)
% to indicate that an issue was encountered during linting.
:- multifile prolog:message_hook/3.
prolog:message_hook(_, Level, _Lines) :-
    member(Level, [warning,error]),
    ( lint_error -> true ; assertz(lint_error) ),
    fail.
prolog:message_hook(_,_,_).

% main/0 - The entry point of the linter.
%
% It searches for all .pl files in the "src" directory, lints them, 
% and then halts with exit code 1 if any warnings or errors were captured,
% or with exit code 0 if everything passed.
main :-
    % Search for .pl files within the "src" directory (adjust the path as needed)
    expand_file_name('src/*.pl', Files),
    lint_files(Files),
    ( lint_error -> halt(1) ; halt(0) ).

% lint_files/1 - Recursively processes a list of files.
%
% For each file, it prints a message indicating that the file is being checked.
% It then uses catch/3 with load_files/2 to attempt to load the file.
% If an exception occurs, the error message is printed and the process halts with exit code 1.
lint_files([]).
lint_files([File|Rest]) :-
    format("Checking ~w~n", [File]),
    catch(load_files(File, [if(true)]), Error, (
        print_message(error, Error),
        halt(1)
    )),
    lint_files(Rest).
