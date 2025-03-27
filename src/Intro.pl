:- module(intro, [
    type_writer/1,
    animated_logo/0,
    wait_for_any_key/0,
    show_intro/0,
    show_out/0
]).

:- use_module(library(readutil)).

%% clear_screen is det.
%
%  Clears the terminal.
%
clear_screen :-
    write('\e[H\e[2J'),
    flush_output.

%% type_writer(+Text:string) is semidet.
%
%  Simulates typing effect, character by character,
%  with a slight delay between each character.
%
type_writer(Text) :-
    string_chars(Text, Chars),
    type_writer_chars(Chars).

type_writer_chars([]).
type_writer_chars([C|Cs]) :-
    put_char(C),
    flush_output,
    sleep(0.007),
    type_writer_chars(Cs).

%% animated_logo is det.
%
%  Displays the system logo with a typing effect line by line.
%
animated_logo :-
    Logo = [
        "  █████    ██████    █████    ███    ███  ██       \n",
        " ██        ██   ██  ██   ██   ████  ████  ██       \n",
        "   ███     ██████   ███████   ██ ████ ██  ██       \n",
        "      ██   ██       ██   ██   ██  ██  ██  ██       \n",
        "  ████     ██       ██   ██   ██      ██  ███████  \n"
    ],
    maplist(type_writer, Logo).

%% wait_for_any_key is det.
%
%  Waits for the user to press any key to continue.
%
wait_for_any_key :-
    write("\nPress any key to continue..."),
    flush_output,
    get_single_char(_),  % Changed from get_char to get_single_char.
    nl.

%% show_intro is det.
%
%  Displays the introduction animation with the logo and welcome messages.
%
show_intro :-
    clear_screen,
    animated_logo,
    writeln("=========================================="),
    type_writer("       Welcome to S P A M L Classifier    \n"),
    writeln("=========================================="),
    type_writer("\nThis program helps you classify messages as spam or ham.\n"),
    type_writer("You can train a model, classify messages, and check accuracy results.\n"),
    wait_for_any_key.

%% show_out is det.
%
%  Displays the final message with a typing effect when the user exits the program.
%
show_out :-
    clear_screen,
    writeln("=========================================="),
    type_writer("   Thank you for using S P A M L!   \n"),
    writeln("=========================================="),
    type_writer("\nGoodbye!\n").
