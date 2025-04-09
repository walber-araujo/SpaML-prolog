:- initialization(main, main).

main:-
    ensure_loaded('test/ModelTest.plt'),
    ensure_loaded('test/ClassifierTest.plt'),
    ensure_loaded('test/PreprocessingTest.plt'),
    ensure_loaded('test/TrainingTest.plt'),
    load_test_files([]),
    run_tests,
    halt.
