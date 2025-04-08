:- initialization(main).
:- set_prolog_flag(warnings, on).

% Use a non-backtrackable global flag to indicate if a warning or error occurred.
:- nb_setval(lint_error_flag, false).

% Message hook to capture warning and error messages.
% When a warning or error is encountered, set the lint_error_flag to true.
:- multifile prolog:message_hook/3.
prolog:message_hook(Message, Level, _Lines) :-
    member(Level, [warning, error]),
    nb_setval(lint_error_flag, true),
    fail.
prolog:message_hook(_,_,_).

main :-
    % Expand all .pl files in the src directory.
    expand_file_name('src/*.pl', Files),
    lint_files(Files),
    nb_getval(lint_error_flag, Flag),
    ( Flag == true -> halt(1) ; halt(0) ).

lint_files([]).
lint_files([File|Rest]) :-
    format("Checking ~w~n", [File]),
    catch(load_files(File, [if(true)]), Error, (
        print_message(error, Error),
        halt(1)
    )),
    lint_files(Rest).
