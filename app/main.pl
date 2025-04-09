:- initialization(main, main).

:- use_module('../src/CLI.pl').
:- use_module('../src/Utils.pl').
:- load_files(["src/Intro.pl"],[encoding(utf8)]).

main:-
    show_intro,
    clear_screen,
    write("Welcome to spaML!\n"),
    write("This project performs spam detection using Machine Learning with the Naive Bayes algorithm.\n"),
    write("The code was developed in Prolog by Alex, João, Vinícius, and Walber.\n"),
    wait_for_any_key,
    menu, halt.
