% Tokenize the input into a list of tokens
lexer(Input, Tokens) :-
    string_chars(Input, Chars),
    tokenize(Chars, Tokens).

% Main tokenizing logic
tokenize([], []).
tokenize([H|T], Tokens) :-
    % Skip whitespace
    char_type(H, space), 
    tokenize(T, Tokens).
tokenize(['"'|T], [String|RestTokens]) :-
    % Handle strings
    consume_string(T, Chars, RestT),
    atom_chars(String, ['"'|Chars]),
    tokenize(RestT, RestTokens).
tokenize([H|T], [Number|RestTokens]) :-
    % Handle numbers
    char_type(H, digit),
    consume_number(T, Digits, RestT),
    atom_number(Number, [H|Digits]),
    tokenize(RestT, RestTokens).
tokenize([H|T], [H|RestTokens]) :-
    % Handle single character tokens (operators and punctuation)
    member(H, ['+', '-', '*', '/', '=', '(', ')', ':', '?', ';', ',']),
    tokenize(T, RestTokens).
tokenize([H|T], [Token|RestTokens]) :-
    % Handle identifiers and keywords
    \+ char_type(H, space),
    \+ member(H, ['+', '-', '*', '/', '=', '(', ')', ':', '?', ';', ',']),
    consume_identifier(T, IdChars, RestT),
    atom_chars(Token, [H|IdChars]),
    tokenize(RestT, RestTokens).

% Helper to consume a string until the closing quote
consume_string([], [], []).
consume_string(['"'|T], ['"'|[]], T).
consume_string([H|T], [H|RestChars], RestT) :-
    consume_string(T, RestChars, RestT).

% Helper to consume a number until a non-digit
consume_number([], [], []).
consume_number([H|T], [], [H|T]) :-
    \+ char_type(H, digit).
consume_number([H|T], [H|RestDigits], RestT) :-
    char_type(H, digit),
    consume_number(T, RestDigits, RestT).

% Helper to consume an identifier until a special character or whitespace
consume_identifier([], [], []).
consume_identifier([H|T], [], [H|T]) :-
    member(H, ['+', '-', '*', '/', '=', '(', ')', ':', '?', ';', ',', ' ']) ; char_type(H, space).
consume_identifier([H|T], [H|RestIdChars], RestT) :-
    \+ member(H, ['+', '-', '*', '/', '=', '(', ')', ':', '?', ';', ',', ' ']),
    \+ char_type(H, space),
    consume_identifier(T, RestIdChars, RestT).
