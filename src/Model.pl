:- module('Model.pl', [count_words/2, calculate_word_probabilities/5]).

:- use_module(library(lists)).
:- use_module('Classifier.pl').
:- use_module('Utils.pl').
:- consult('Utils.pl').
:- use_module('Preprocessing.pl').

update_current(Word, [], [(Word, 1)]):- !.
update_current(Word, [(Word, X)|T], [(Word, X2)|T]):- X2 is X+1, !.
update_current(Word, [W|T], [W|T2]):- update_current(Word, T, T2).

count_phrase_words([], X, X).
count_phrase_words([W|T], WordsState, FinalState):-
    update_current(W, WordsState, NextState),
    count_phrase_words(T, NextState, FinalState).

count_phrase('ham', Tokens, HamWordsState, NextHamState, SpamWordsState, SpamWordsState):-
    count_phrase_words(Tokens, HamWordsState, NextHamState).

count_phrase('spam', Tokens, HamWordsState, HamWordsState, SpamWordsState, NextSpamState):-
    count_phrase_words(Tokens, SpamWordsState, NextSpamState).

count_class('ham', CurrentHamCount, CurrentSpamCount, ReturnHamCount, CurrentSpamCount):- ReturnHamCount is CurrentHamCount + 1.
count_class('spam', CurrentHamCount, CurrentSpamCount, CurrentHamCount, ReturnSpamCount):- ReturnSpamCount is CurrentSpamCount + 1.

count_words([], HamCount, SpamCount, CurrentHamState, CurrentSpamState, (CurrentHamState, CurrentSpamState, HamCount, SpamCount)).

count_words([Head|Rows], HamCount, SpamCount, CurrentHamState, CurrentSpamState, R):-
    record(Category, Message) = Head,
    count_class(Category, HamCount, SpamCount, ReturnHamCount, ReturnSpamCount),

    tokenize(Message, Tokens),
    count_phrase(Category, Tokens, CurrentHamState, NextState, CurrentSpamState, NextSpamState),
    count_words(Rows, ReturnHamCount, ReturnSpamCount, NextState, NextSpamState, R).

count_words(Rows, R):- count_words(Rows, 0, 0, [], [], R). %%tira?

calculate_word_probabilities([], _, _, _, []).
calculate_word_probabilities([(Word, Count) | Tail], Total_Count, Other_Words, Other_Words_Count, [New_Head | Tail2]):-
                    length([(Word, Count) | Tail], Size_Words_Count),
                    Word_Given_Class is ((Count + 1) / (Total_Count + Size_Words_Count)), % Laplace smoothing
                    find_with_default(Word, Other_Words, Other_Count_Word),
                    length(Other_Words, Size_Other_Words_Count),
                    Other_Class_Given_Word is ((Other_Count_Word + 1) / (Other_Words_Count + Size_Other_Words_Count)), % Laplace smoothing
                    New_Count is Word_Given_Class / Other_Class_Given_Word,
                    New_Head = row(Word, New_Count),
                    calculate_word_probabilities(Tail, Total_Count, Other_Words, Other_Words_Count, Tail2).