:- begin_tests(training_test).
:- use_module('src/Training.pl').
:- use_module('src/Model.pl').
:- use_module('src/Classifier.pl').

% Sample records for testing
sample_records([
    record('ham', 'hello'),
    record('ham', 'hello world'), 
    record('spam', 'buy premium'),
    record('spam', 'hello buy')
]).

test(train_model):-
    sample_records(Records),
    train_model(Records, HamProbs, SpamProbs),
    
    % Test ham probabilities
    find_with_default("hello", HamProbs, HamHelloProb),
    find_with_default("world", HamProbs, HamWorldProb),
    D1 is (3/4)/(2/5),
    D2 is (2/4)/(1/5),
    assertion(HamHelloProb =:= D1),
    assertion(HamWorldProb =:= D2),
    
    % Test spam probabilities  
    find_with_default("hello", SpamProbs, SpamHelloProb),
    find_with_default("buy", SpamProbs, SpamBuyProb),
    find_with_default("premium", SpamProbs, SpamPremiumProb),
    D3 is (2/5)/(3/4),
    D4 is (3/5)/(1/4),
    D5 is (2/5)/(1/4),
    assertion(SpamHelloProb =:= D3),
    assertion(SpamBuyProb =:= D4),
    assertion(SpamPremiumProb =:= D5).

:- end_tests(training_test).
