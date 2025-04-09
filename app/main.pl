:- initialization(main, main).

:- use_module('../src/CLI.pl').
:- use_module('../src/Utils.pl').
:- load_files(["src/Intro.pl"],[encoding(utf8)]).

main:-
    show_intro,
    menu, halt.
