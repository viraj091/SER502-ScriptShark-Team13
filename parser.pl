% Lexical Analyzer
lexer(Input, TokenList) :-
    split_string(Input, " \n\t", " \n\t", Tokens),
    remove_empty_strings(Tokens, TokenList).

remove_empty_strings([], []).
remove_empty_strings([Token|Tokens], FilteredTokens) :-
    Token \= "",
    !,
    FilteredTokens = [Token|Rest],
    remove_empty_strings(Tokens, Rest).
remove_empty_strings([_|Tokens], FilteredTokens) :-
    remove_empty_strings(Tokens, FilteredTokens).

% Parser
parse(Input) :-
    catch(
        (
            lexer(Input, Tokens),
            writeln('Tokens:'),
            writeln(Tokens),  % Print the tokens
            program(Tokens, []),
            writeln('Parsing successful.')
        ),
        Error,
        (
            writeln('Error during parsing:'),
            writeln(Error)
        )
    ).

% Program
program --> statement, program.
program --> [].

% Statement
statement --> assignment.
statement --> conditional.
statement --> loop.
statement --> print.

% Assignment
assignment --> identifier, [':='], expression.

% Conditional
conditional --> ['if'], expression, ['then'], statement, ['else'], statement.
conditional --> expression, ['?'], expression, [':'], expression.

% Loop
loop --> for_loop.
loop --> while_loop.

% For Loop
for_loop --> ['for'], identifier, [':='], expression, [';'],
            ['not'], identifier, ['='], expression, [';'],
            identifier, [':='], identifier, ['+'], expression, ['do'],
            identifier, [':='], identifier, ['*'], expression,
            ['endfor'].

% While Loop
while_loop --> ['while'], ['('], expression, [')'], statement.

% Print
print --> ['print'], ['('], identifier, [')'], ['end'], ['.'].

% Expression
expression --> term.
expression --> term, operator, expression.

% Term
term --> identifier.
term --> value.

% Value
value --> boolean(true).
value --> boolean(false).
value --> number.
value --> string.

% Boolean
boolean(true) --> ['true'].
boolean(false) --> ['false'].

% Number
number --> [Num], { atom_number(Num, NumAsNumber), integer(NumAsNumber) }.

% String
string --> ['"'], string_chars, ['"'].

% String Characters
string_chars --> [].
string_chars --> [Char], { \+ Char = '"', \+ Char = '\n', \+ Char = '\t', \+ Char = ' ', char_type(Char, ascii) }, string_chars.

% Identifier
identifier --> [Id], { atom(Id) }.

% Operator
operator --> ['+'].
operator --> ['-'].
operator --> ['*'].
operator --> ['/'].
operator --> ['and'].
operator --> ['or'].
operator --> ['not'].

% Command-line Invocation
:- initialization(main).
main :-
    writeln('Please enter a string:'),
    read_line_to_string(user_input, Input),
    catch(
        (
            parse(Input)
        ),
        Error,
        (
            writeln('Error during execution:'),
            writeln(Error)
        )
    ).
