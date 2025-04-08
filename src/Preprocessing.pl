%%Module      : Preprocessing
%Description : Preprocessing of text (cleaning and tokenization).
%Stability   : stable.
:- module('Preprocessing.pl', [tokenize/2]).

%% stop_words_en(-StopWords:list) is det.
%
%  Provides a predefined list of English stop words commonly filtered out in text processing.
%
%  @param StopWords  The list of stop words.
stop_words_en([
    "i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", 
    "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", 
    "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", 
    "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", 
    "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", 
    "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", 
    "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", 
    "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", 
    "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", 
    "should", "now", "d", "ll", "m", "o", "re", "ve", "y", "ain", "aren", "couldn", "didn", "doesn", "hadn", 
    "hasn", "haven", "isn", "ma", "mightn", "mustn", "needn", "shan", "shouldn", "wasn", "weren", "won", "wouldn"
]).

%% tokenize(+Text:string, -Tokens:list) is det.
%
%  Tokenizes input text into a list of filtered words by:
%  - Converting to lowercase
%  - Replacing non-alphanumeric characters with spaces
%  - Splitting into words
%  - Removing empty strings, stop words, and words containing digits
%
%  @param Text    The input text to be tokenized.
%  @param Tokens  The resulting list of clean tokens.
tokenize(Text, Tokens) :-
    stop_words_en(StopWords),
    string_lower(Text, LowerText),  
    replace_non_alpha(LowerText, CleanText),  
    split_string(CleanText, " ", " ", Words), 
    exclude(is_empty, Words, NonEmptyWords), 
    exclude(is_stop_word(StopWords), NonEmptyWords, NoStopWords),
    exclude(contains_digit, NoStopWords, FilteredTokens),
    !, %
    Tokens = FilteredTokens.

%% is_stop_word(+StopWords:list, +Word:string) is semidet.
%
%  Succeeds if the given word is a member of the stop word list.
%
%  @param StopWords  A list of stop words.
%  @param Word       The word to check against the stop word list.
is_stop_word(StopWords, Word) :-
    member(Word, StopWords).

%% is_empty(+String:string) is semidet.
%
%  Succeeds if the given string is empty.
%
%  @param String  The input string to be checked.
is_empty("").

%% replace_non_alpha(+String:string, -Result:string) is det.
%
%  Processes a string by replacing all non-alphabetic and non-digit characters with spaces.
%
%  @param String  The original input string.
%  @param Result  The resulting string after replacement.
replace_non_alpha(String, Result) :-
    string_chars(String, Chars),       
    replace_non_alpha_chars(Chars, CleanChars),
    string_chars(Result, CleanChars).   

%% replace_non_alpha_chars(+Chars:list, -Cleaned:list) is det.
%
%  Replaces all non-alphabetic and non-digit characters in a character list with spaces.
%
%  @param Chars    The input list of characters.
%  @param Cleaned  The resulting list with non-alphanumeric characters replaced by spaces.
replace_non_alpha_chars([], []).
replace_non_alpha_chars([H|T], [H|R]) :-
    (   char_type(H, alpha)        % Mantém letras
    ;   char_type(H, digit)        % Mantém números
    ),
    replace_non_alpha_chars(T, R).
replace_non_alpha_chars([_|T], [' '|R]) :-  % Substitui não-letras nem números por espaços
    replace_non_alpha_chars(T, R).

%% contains_digit(+Word:string) is semidet.
%
%  Succeeds if the given word contains at least one digit character.
%
%  @param Word  The input word to be checked for digits.
contains_digit(Word) :-
    string_chars(Word, Chars),
    member(Char, Chars),
    char_type(Char, digit).