:- module('Preprocessing.pl', [stop_words_en/1, is_stop_word/2, is_empty/1, replace_non_alpha/2]).

% Lista de palavras de parada em inglês
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

% Função de tokenização
tokenize(Text, Tokens) :-
    stop_words_en(StopWords),
    string_lower(Text, LowerText),  % Converte o texto para minúsculas
    replace_non_alpha(LowerText, CleanText),  % Substitui caracteres não alfabéticos por espaços
    split_string(CleanText, " ", " ", Words),  % Divide a string em palavras, removendo espaços extras
    exclude(is_empty, Words, NonEmptyWords),  % Remove strings vazias
    exclude(is_stop_word(StopWords), NonEmptyWords, FilteredTokens),
    !, % **Corte para impedir multiplas soluções**
    Tokens = FilteredTokens. % Assegura que apenas um resultado é retornado

% Verifica se a palavra é uma palavra de parada
is_stop_word(StopWords, Word) :-
    member(Word, StopWords).

% Verifica se a string está vazia
is_empty("").

% Substitui caracteres não alfabéticos (exceto números) por espaços
replace_non_alpha(String, Result) :-
    string_chars(String, Chars),        % Converte a string para uma lista de caracteres
    replace_non_alpha_chars(Chars, CleanChars),
    string_chars(Result, CleanChars).   % Converte a lista de volta para string

replace_non_alpha_chars([], []).
replace_non_alpha_chars([H|T], [H|R]) :-
    (   char_type(H, alpha)        % Mantém letras
    ;   char_type(H, digit)        % Mantém números
    ),
    replace_non_alpha_chars(T, R).
replace_non_alpha_chars([_|T], [' '|R]) :-  % Substitui não-letras nem números por espaços
    replace_non_alpha_chars(T, R).

% Teste de tokenização
%test_tokenize :-
%    Text = "This is a simple test.",
%    tokenize(Text, Tokens),
%    format('Tokens retornados: ~w~n', [Tokens]).  % Exibe os tokens gerados