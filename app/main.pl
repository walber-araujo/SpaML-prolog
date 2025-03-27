:- initialization(main, main).

main:-
    ensure_loaded('src/CLI'), menu, halt.
