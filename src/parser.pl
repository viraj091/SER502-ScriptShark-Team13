% Parser
parse(Input) :- 
    lexer(Input, Tokens),
    program(Tokens, []).

% Program
program --> statement, program.
program --> statement.

% Statement
statement --> assignment.
statement --> conditional.
statement --> loop.
statement --> print.

% Assignment
assignment --> identifier, ['='], expression.

% Conditional
conditional --> ['if'], expression, ['then'], statement, ['else'], statement.
conditional --> expression, ['?'], expression, [':'], expression.

% Loop
loop --> ['for'], identifier, ['in'], ['range'], ['('], expression, [',' ], expression, [')'], statement.
loop --> ['while'], ['('], expression, [')'], statement.

% Print
print --> ['print'], identifier.

% Expression
expression --> term.
expression --> term, operator, expression.

% Term
term --> identifier.
term --> value.

% Value
value(boolean) --> boolean.
value(number) --> number.
value(string) --> string.

% Boolean
boolean(true) --> ['true'].
boolean(false) --> ['false'].

% Number
number --> integer.

% String
string --> ['"'], chars, ['"'].

% Identifier
identifier --> letter, idchars.
letter --> [Char], { code_type(Char, alpha) }.
idchars --> [].
idchars --> [Char], { code_type(Char, alnum) }, idchars.

% Operator
operator --> ['+'].
operator --> ['-'].
operator --> ['*'].
operator --> ['/'].
operator --> ['and'].
operator --> ['or'].
operator --> ['not'].

% Chars
chars --> [].
chars --> char, chars.

% Char
char --> letter.
char --> digit.
char --> specialchar.

% Digit
digit --> [Char], { code_type(Char, digit) }.

% Special Characters
specialchar --> [Char], { member(Char, [33,35,36,37,38,40,41,42,43,44,45,46,47,58,59,60,61,62,63,64,91,92,93,94,95,96,123,124,125,126]) }.

% Command-line Invocation
:- initialization(main).
main :-
    current_prolog_flag(argv, Argv),
    nth0(0, Argv, InputFile),
    read_file_to_string(InputFile, Input, []),
    parse(Input),
    halt.
