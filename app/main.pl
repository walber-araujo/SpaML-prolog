:- initialization(main, main).

:- use_module('../src/CLI.pl').
:- use_module('../src/Utils.pl').
:- use_module('../src/Intro.pl').

main:-
    show_intro,
    menu, halt.
