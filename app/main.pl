:- initialization(main, main).

:- use_module('src/CLI.pl').
:- use_module('src/Utils.pl').
:- consult('Utils.pl').

main:-
    menu, halt.
