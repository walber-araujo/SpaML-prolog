:- initialization(main, main).

main:-
    ensure_loaded('test/ModelTest.plt'),    
    load_test_files([]),
    run_tests,
    halt.
