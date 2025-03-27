type_writer_main([]).
type_writer_main([Char|Rest]) :-
    write(Char),
    flush_output,
    wait_for_input([user_input], Inputs, 0.024),
    length(Inputs, L),
    L =:= 0,
    type_writer_main(Rest).

type_writer(String) :-
    string_chars(String, CharList),
    with_tty_raw(type_writer_main(CharList)) -> true ; true.

show_out:-
    write('\n==========================================\n'),
    type_writer('   Thank you for using S P A M L!   \n'),
    write('==========================================\n'),
    type_writer('\nGoodbye!\n').
