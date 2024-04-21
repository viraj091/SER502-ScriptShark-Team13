token(begin) --> ['b', 'e', 'g', 'i', 'n'].
token(end) --> ['e', 'n', 'd'].
token(if) --> ['i', 'f'].
token(then) --> ['t', 'h', 'e', 'n'].
token(else) --> ['e', 'l', 's', 'e'].
token(endif) --> ['e', 'n', 'd', 'i', 'f'].
token(assign) --> [':', '='].
token(semicolon) --> [';'].
token(plus) --> ['+'].
token(minus) --> ['-'].
token(multiply) --> ['*'].
token(divide) --> ['/'].
token(lparen) --> ['('].
token(rparen) --> [')'].
token(comma) --> [','].
token(question) --> ['?'].
token(colon) --> [':'].
token(print) --> ['p', 'r', 'i', 'n', 't'].
token(true) --> ['t', 'r', 'u', 'e'].
token(false) --> ['f', 'a', 'l', 's', 'e'].
token(in) --> ['i', 'n'].
token(range) --> ['r', 'a', 'n', 'g', 'e'].

token(string(S)) --> ['"'], string_chars(C), ['"'], { atom_chars(S, C) }.
string_chars([H|T]) --> string_char(H), string_chars(T).
string_chars([]) --> [].
string_char(C) --> [C], { C \= '"' }.

token(number(N)) --> digits(D), { number_chars(N, D) }.
digits([D|T]) --> digit(D), digits(T).
digits([D]) --> digit(D).
digit(D) --> [D], { char_type(D, digit) }.

token(identifier(Id)) --> identifier_chars(Chars), { atom_chars(Id, Chars) }.
identifier_chars([C|Cs]) --> identifier_char(C), identifier_chars(Cs).
identifier_chars([C]) --> identifier_char(C).
identifier_char(C) --> [C], { char_type(C, alpha) ; char_type(C, lower) }.

whitespace --> [C], { char_type(C, white) }, whitespace.
whitespace --> [].

% Comments (skip anything after % to the end of the line)
comment --> ['%'], comment_chars, comment.
comment --> [].
comment_chars --> [C], { C \= '\n' }, comment_chars.
comment_chars --> ['\n'].

% The main lexer function
lexer(Tokens) --> whitespace, ( comment ; token(Token) ), !, { Tokens = [Token | TokenList] }, lexer(TokenList).
lexer([]) --> [].

% string reader test
read_string(String, Tokens) :-
    string_chars(String, Chars),
    phrase(lexer(Tokens), Chars).
