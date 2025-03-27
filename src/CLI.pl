menu:-
    ensure_loaded('src/Utils'),
    ensure_loaded('src/Intro'),
    clear_screen(),
    write('\n====================================================================================\n'),
    write('Menu Options:\n\n'),
    write('[1]. Reuse previous models.\n'),
    write('[2]. Add new model.\n'),
    write('[3]. Remove a model.\n'),
    write('[4]. Train model manually.\n'),
    write('[5]. Classify individual messages using the default model.\n'),
    write('[6]. Show results with accuracy rates.\n'),
    write('[7]. Exit.\n'),
    write("\nChoose an option (1-7): "),
    read_line_to_codes(user_input, X),
    string_to_atom(X,Option),
    process_option(Option).

process_option('1'):- write('Not implemented'), !.
process_option('2'):- write('Not implemented'), !.
process_option('3'):- write('Not implemented'), !.
process_option('4'):- write('Not implemented'), !.
process_option('5'):- write('Not implemented'), !.
process_option('6'):- write('Not implemented'), !.
process_option('7'):- show_out(), halt.

process_option(_):- write('\nInvalid option. Please try again.\n').
