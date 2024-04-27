lexer(Input, Tokens) :-
    string_chars(Input, Chars),
    tokenize(Chars, [], TokensRev),
    reverse(TokensRev, Tokens),
    print_tokens(Tokens).

% Tokenize the character list into tokens
tokenize([], Tokens, Tokens).
tokenize([H|T], TokenAcc, Tokens) :-
    (   char_type(H, space)
    ->  tokenize(T, TokenAcc, Tokens)  % Skip spaces
    ;   handle_token([H|T], NewT, Token)
    ->  tokenize(NewT, [Token|TokenAcc], Tokens)  % Add new Token to the end of list correctly
    ;   tokenize(T, TokenAcc, Tokens)  % Skip character if no token can be formed
    ).

% Handle different types of tokens
handle_token(Input, RestT, Token) :-
    multi_char_operator_or_keyword(Input, RestT, Token), !;
    single_char_operator(Input, RestT, Token), !;
    consume_number(Input, RestT, Token), !;
    (   Input = ['"'|_]  % Start of string literal
    ->  consume_string(Input, RestT, Token)
    ;   consume_identifier_or_keyword(Input, RestT, Token)
    ).

% Multi-character operators and keywords
multi_char_operator_or_keyword([':', '='|T], T, ':=').
multi_char_operator_or_keyword(['n', 'o', 't'|T], RestT, 'not') :- peek_non_alpha(T, RestT).

% Single character operators and punctuation
single_char_operator([H|T], T, Token) :-
    member(H, ['+', '-', '*', '/', '=', ':', '?', ';', ',', '.']),
    atom_chars(Token, [H]).
single_char_operator(['('|T], T, '\'(\'').
single_char_operator([')'|T], T, '\')\'').

% Check for non-alphanumeric characters after a token
peek_non_alpha([H|T], [H|T]) :-
    (char_type(H, space); char_type(H, punct)).

% Consume a numeric token
consume_number([H|T], RestT, Number) :-
    char_type(H, digit),
    consume_digits([H|T], Digits, RestT),
    atom_chars(AtomDigits, Digits),
    atom_number(AtomDigits, Number).

% Consume string literals including quotes
consume_string(['"'|T], RestT, Token) :-
    consume_string_chars(T, Chars, RestT),
    atom_concat('\"', Chars, TempToken),
    atom_concat(TempToken, '\"', Quoted),
    atom_concat('\'', Quoted, TempToken2),
    atom_concat(TempToken2, '\'', Token).

% Helper to consume characters until the closing quote
consume_string_chars(['"'|T], '', T).  % End of string
consume_string_chars([H|T], Chars, RestT) :-
    consume_string_chars(T, TailChars, RestT),
    atom_concat(H, TailChars, Chars).

% Consume identifiers or keywords
consume_identifier_or_keyword([H|T], RestT, Identifier) :-
    valid_identifier_start(H),
    consume_identifier_chars([H|T], IdChars, RestT),
    atom_chars(Identifier, IdChars).

% Helper predicates for digits and identifiers
consume_digits([H|T], [H|RestDigits], RestT) :-
    char_type(H, digit),
    consume_digits(T, RestDigits, RestT).
consume_digits(T, [], T).

consume_identifier_chars([H|T], [H|RestIdChars], RestT) :-
    valid_identifier_char(H),
    consume_identifier_chars(T, RestIdChars, RestT).
consume_identifier_chars(T, [], T).

valid_identifier_start(H) :-
    \+ char_type(H, space),
    \+ member(H, ['+', '-', '*', '/', '=', ':', '?', ';', ',', '.', '"']).

valid_identifier_char(H) :-
    char_type(H, alnum); H == '_'.

% Print the tokens in a readable format
print_tokens(Tokens) :-
    write('Tokens: '), write(Tokens), nl.


