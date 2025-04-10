%%Module      : Model
%Description : Model data structure (probabilities, word count).
%Stability   : stable.
:- module('Model.pl', [count_words/2, calculate_word_probabilities/5]).

:- use_module(library(lists)).
:- use_module('Classifier.pl').
:- use_module('Utils.pl').
:- consult('Utils.pl').
:- use_module('Preprocessing.pl').

%% update_current(+Word:string, +CurrentState:list, -UpdatedState:list) is det.
%
%  Updates the word count for a given word in the current state list.
%  If the word exists, increments its count. Otherwise, adds it with count 1.
%
%  @param Word           The word to be updated or added.
%  @param CurrentState   Current list of (Word, Count) pairs.
%  @param UpdatedState   Resulting list after the update.
update_current(Word, [], [(Word, 1)]):- !.
update_current(Word, [(Word, X)|T], [(Word, X2)|T]):- X2 is X+1, !.
update_current(Word, [W|T], [W|T2]):- update_current(Word, T, T2).

%% count_phrase_words(+Tokens:list, +WordsState:list, -FinalState:list) is det.
%
%  Iteratively updates the word-count state by processing each token.
%  For each word in the token list, it increments its count in the state.
%
%  @param Tokens       List of tokenized words.
%  @param WordsState   Current list of (Word, Count) pairs.
%  @param FinalState   Updated list with incremented counts for each word.
count_phrase_words([], X, X).
count_phrase_words([W|T], WordsState, FinalState):-
    update_current(W, WordsState, NextState),
    count_phrase_words(T, NextState, FinalState).

%% count_phrase(+Category:string, +Tokens:list, +HamWordsState:list, -NextHamState:list,
%%              +SpamWordsState:list, -NextSpamState:list) is det.
%
%  Updates the word-count state for either ham or spam based on the message category.
%  It counts the frequency of each token (word) in the corresponding class state.
%
%  @param Category         Message label, either 'ham' or 'spam'.
%  @param Tokens           Tokenized list of words from the message.
%  @param HamWordsState    Current list of (Word, Count) pairs for ham.
%  @param NextHamState     Updated ham word-count state (unchanged if message is spam).
%  @param SpamWordsState   Current list of (Word, Count) pairs for spam.
%  @param NextSpamState    Updated spam word-count state (unchanged if message is ham).
count_phrase('ham', Tokens, HamWordsState, NextHamState, SpamWordsState, SpamWordsState):-
    count_phrase_words(Tokens, HamWordsState, NextHamState).
count_phrase('spam', Tokens, HamWordsState, HamWordsState, SpamWordsState, NextSpamState):-
    count_phrase_words(Tokens, SpamWordsState, NextSpamState).


%% count_class(+Category:string, +CurrentHamCount:int, +CurrentSpamCount:int,
%%              -ReturnHamCount:int, -ReturnSpamCount:int) is det.
%
%  Updates the message count for either ham or spam depending on the category.
%
%  @param Category          The message label, either 'ham' or 'spam'.
%  @param CurrentHamCount   Current number of ham messages.
%  @param CurrentSpamCount  Current number of spam messages.
%  @param ReturnHamCount    Updated ham message count.
%  @param ReturnSpamCount   Updated spam message count.
count_class('ham', CurrentHamCount, CurrentSpamCount, ReturnHamCount, CurrentSpamCount):- ReturnHamCount is CurrentHamCount + 1.
count_class('spam', CurrentHamCount, CurrentSpamCount, CurrentHamCount, ReturnSpamCount):- ReturnSpamCount is CurrentSpamCount + 1.

%% count_words(+Records:list, +HamCount:int, +SpamCount:int,
%%             +CurrentHamState:list, +CurrentSpamState:list,
%%             -Result:tuple) is det.
%
%  Traverses a list of labeled records and counts word occurrences separately for ham and spam messages.
%  It also maintains running totals of ham and spam message counts.
%
%  Each message is tokenized, and its tokens are counted based on the message category.
%
%  @param Records            List of record(Category, Message) terms.
%  @param HamCount           Accumulator for the count of ham messages.
%  @param SpamCount          Accumulator for the count of spam messages.
%  @param CurrentHamState    Current word-count state for ham messages.
%  @param CurrentSpamState   Current word-count state for spam messages.
%  @param Result             Tuple in the form (FinalHamWords, FinalSpamWords, FinalHamCount, FinalSpamCount).
count_words([], HamCount, SpamCount, CurrentHamState, CurrentSpamState, (CurrentHamState, CurrentSpamState, HamCount, SpamCount)).

count_words([Head|Rows], HamCount, SpamCount, CurrentHamState, CurrentSpamState, R):-
    record(Category, Message) = Head,
    count_class(Category, HamCount, SpamCount, ReturnHamCount, ReturnSpamCount),

    tokenize(Message, Tokens),
    count_phrase(Category, Tokens, CurrentHamState, NextState, CurrentSpamState, NextSpamState),
    count_words(Rows, ReturnHamCount, ReturnSpamCount, NextState, NextSpamState, R).

count_words(Rows, R):- count_words(Rows, 0, 0, [], [], R).

%% calculate_word_probabilities(+WordCounts:list, +TotalCount:integer,
%%                               +OtherWordCounts:list, +OtherTotalCount:integer,
%%                               -Probabilities:list) is det.
%
%  Calculates the smoothed word probabilities using Laplace smoothing and 
%  estimates their informativeness by comparing with the other classs distribution.
%
%  For each word, computes:
%    P(word|class) / P(word|other_class)
%  This ratio helps indicate how characteristic the word is for the class.
%
%  @param WordCounts         List of (Word, Count) pairs for the target class.
%  @param TotalCount         Total number of words in the target class.
%  @param OtherWordCounts    List of (Word, Count) pairs in the opposing class.
%  @param OtherTotalCount    Total word count in the opposing class.
%  @param Probabilities      Output list of row(Word, SmoothedRatio) entries.
calculate_word_probabilities([], _, _, _, _, []).
calculate_word_probabilities([(Word, Count) | Tail], Total_Count, Other_Words, Other_Words_Count, Size_Words_Count, [New_Head | Tail2]):-
    % Laplace smoothing
    (Total_Count + Size_Words_Count =\= 0 -> Word_Given_Class is ((Count + 1) / (Total_Count + Size_Words_Count)) ; Word_Given_Class = 0),
    find_with_default(Word, Other_Words, Other_Count_Word),
    length(Other_Words, Size_Other_Words_Count),
    % Laplace smoothing
    (Other_Words_Count + Size_Other_Words_Count =\= 0 -> Other_Class_Given_Word is ((Other_Count_Word + 1) / (Other_Words_Count + Size_Other_Words_Count)) ; Other_Class_Given_Word = 0),
    (Other_Class_Given_Word =\= 0 -> New_Count is Word_Given_Class / Other_Class_Given_Word ; New_Count = 0),
    New_Head = row(Word, New_Count),
    calculate_word_probabilities(Tail, Total_Count, Other_Words, Other_Words_Count, Size_Words_Count, Tail2).

calculate_word_probabilities(WordsCount, Total_Count, Other_Words, Other_Words_Count, R):-
    length(WordsCount, Size_Words_Count),
    calculate_word_probabilities(WordsCount, Total_Count, Other_Words, Other_Words_Count, Size_Words_Count, R).