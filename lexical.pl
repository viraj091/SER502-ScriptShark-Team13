% Entry point for the lexer
lexer(Input, Tokens) :-
    string_chars(Input, Chars),
    tokenize(Chars, [], Tokens).

% Tokenize the character list into tokens
tokenize([], TokenAcc, Tokens) :-
    reverse(TokenAcc, Tokens).  % Reverse the accumulated tokens for correct order
tokenize([H|T], TokenAcc, Tokens) :-
    (   char_type(H, space)    % Skip whitespace
    ->  tokenize(T, TokenAcc, Tokens)
    ;   handle_token([H|T], NewT, Token, TokenAcc),
        tokenize(NewT, [Token|TokenAcc], Tokens)
    ).

% Dispatch to specific token handlers
handle_token(Chars, RestChars, Token, TokenAcc) :-
    (   Chars = ['"'|_]  % String token
    ->  consume_string(Chars, RestChars, Token)
    ;   Chars = [H|_], char_type(H, digit)  % Numeric token
    ->  consume_number(Chars, RestChars, Token)
    ;   Chars = [H|_], member(H, ['+', '-', '*', '/', '=', '(', ')', ':', '?', ';', ','])  % Operators and punctuation
    ->  Token = H, RestChars = T
    ;   consume_identifier(Chars, RestChars, Token)
    ).

% Consume characters until the closing quote for strings
consume_string(['"'|T], RestChars, String) :-
    consume_until_quote(T, Chars, RestChars),
    atom_chars(String, ['"'|Chars]).

% Helper to consume until a closing quote, handling escaped quotes
consume_until_quote([], [], []).
consume_until_quote(['"'|T], ['"'|[]], T).
consume_until_quote([H|T], [H|RestChars], RestT) :-
    consume_until_quote(T, RestChars, RestT).

% Consume a sequence of digits for a number
consume_number([H|T], RestChars, Number) :-
    char_type(H, digit),
    consume_digits(T, Digits, RestChars),
    atom_number(Number, [H|Digits]).

% Helper to consume digits
consume_digits([], [], []).
consume_digits([H|T], [], [H|T]) :- \+ char_type(H, digit).
consume_digits([H|T], [H|RestDigits], RestT) :-
    char_type(H, digit),
    consume_digits(T, RestDigits, RestT).

% Consume an identifier
consume_identifier([H|T], RestChars, Identifier) :-
    valid_identifier_start(H),
    consume_id_chars(T, IdChars, RestChars),
    atom_chars(Identifier, [H|IdChars]).

% Helper to consume identifier characters
consume_id_chars([], [], []).
consume_id_chars([H|T], [], [H|T]) :-
    invalid_identifier_char(H).
consume_id_chars([H|T], [H|RestIdChars], RestT) :-
    valid_identifier_char(H),
    consume_id_chars(T, RestIdChars, RestT).

% Define valid start characters for an identifier
valid_identifier_start(H) :-
    \+ char_type(H, space),
    \+ member(H, ['+', '-', '*', '/', '=', '(', ')', ':', '?', ';', ',']).

% Define valid characters for an identifier
valid_identifier_char(H) :-
    \+ char_type(H, space),
    \+ member(H, ['+', '-', '*', '/', '=', '(', ')', ':', '?', ';', ',']).

% Define invalid characters for an identifier
invalid_identifier_char(H) :-
    char_type(H, space) ;
    member(H, ['+', '-', '*', '/', '=', '(', ')', ':', '?', ';', ',', '"']).
