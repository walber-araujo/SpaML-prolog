% lint.pl
:- initialization(main).
:- set_prolog_flag(warnings, on).

main :-
    % Busca por arquivos .pl dentro da pasta src (ajuste o caminho conforme necessário)
    expand_file_name('src/*.pl', Files),
    lint_files(Files),
    halt(0).

lint_files([]).
lint_files([File|Rest]) :-
    format("Verificando ~w~n", [File]),
    catch(load_files(File, [if(true)]), Error, (
        print_message(error, Error),
        halt(1)
    )),
    lint_files(Rest).
