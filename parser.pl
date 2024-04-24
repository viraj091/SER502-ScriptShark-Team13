:- use_module(library(pcre)).

:- table boolean_expr/3.
:- table arithmetic_expr/3.
:- table term/3.

program(prog(X)) --> block(X), [.].
block(block(X)) --> [begin], command(X), [end].

command(assgn_num(X,Y)) --> identifier(X), [:=], arithmetic_expr(Y).
command(assgn_bool(X,Y)) --> identifier(X), [:=], boolean_expr(Y).
command(assgn_str(X,Y)) --> identifier(X), [:=], string_expr(Y).

command(if(X,Y,Z)) --> [if], boolean_expr(X), [then], command(Y), [else], command(Z), [endif].
command(tern(X,Y,Z)) --> boolean_expr(X), [?], command(Y), [:], command(Z), [endtern].

command(while(X,Y)) --> [while], boolean_expr(X), [do], command(Y), [endwhile].
command(for(W,X,Y,Z)) --> [for], command(W), [;], boolean_expr(X), [;], command(Y), [do], command(Z), [endfor].
command(for_range(W,X,Y,Z)) --> [for], identifier(W), [in], [range], ['('], arithmetic_expr(X), [','], arithmetic_expr(Y), [')'], [do], command(Z), [endfor].

command(print(X)) --> [print], ['('], identifier(X), [')'].
command(print(X)) --> [print], ['('], number(X), [')'].
command(print(X)) --> [print], ['('], string(X), [')'].

% recursive commands

command(assgn_num(X,Y,Z)) --> identifier(X), [:=], arithmetic_expr(Y), [;], command(Z).
command(assgn_bool(X,Y,Z)) --> identifier(X), [:=], boolean_expr(Y), [;], command(Z).
command(assgn_str(X,Y,Z)) --> identifier(X), [:=], string_expr(Y), [;], command(Z).

command(if(W,X,Y,Z)) --> [if], boolean_expr(W), [then], command(X), [else], command(Y), [endif], [;], command(Z).
command(tern(W,X,Y,Z)) --> boolean_expr(W), [?], command(X), [:], command(Y), [endtern], [;], command(Z).

command(while(X,Y,Z)) --> [while], boolean_expr(X), [do], command(Y), [endwhile], [;], command(Z).
command(for(V,W,X,Y,Z)) --> [for], command(V), [;], boolean_expr(W), [;], command(X), [do], command(Y), [endfor], [;], command(Z).
command(for_range(V,W,X,Y,Z)) --> [for], identifier(V), [in], [range], arithmetic_expr(W), [','], arithmetic_expr(X), [do], command(Y), [endfor], [;], command(Z).

command(print(X,Y)) --> [print], ['('], identifier(X), [')'], [;], command(Y).
command(print(X,Y)) --> [print], ['('], number(X), [')'], [;], command(Y).
command(print(X,Y)) --> [print], ['('], string(X), [')'], [;], command(Y).

% boolean expressions

boolean_expr(bool_expr(true)) --> [true].
boolean_expr(bool_expr(false)) --> [false].
boolean_expr(bool_expr(X)) --> identifier(X).

boolean_expr(and_expr(X,Y)) --> boolean_expr(X), [and], boolean_expr(Y).
boolean_expr(or_expr(X,Y)) --> boolean_expr(X), [or], boolean_expr(Y).
boolean_expr(not(X)) --> [not], boolean_expr(X).

boolean_expr(eq(X,Y)) --> arithmetic_expr(X), [=], arithmetic_expr(Y).
boolean_expr(eq(X,Y)) --> boolean_expr(X), [=], boolean_expr(Y).
boolean_expr(eq(X,Y)) --> string_expr(X), [=], string_expr(Y).

% arithmetic expressions

arithmetic_expr(term(X)) --> term(X).
arithmetic_expr(add_expr(X,Y)) --> arithmetic_expr(X), [+], term(Y).
arithmetic_expr(sub_expr(X,Y)) --> arithmetic_expr(X), [-], term(Y).

term(factor_term(X)) --> factor(X).
term(div_term(X,Y)) --> term(X), [/], factor(Y).
term(mult_term(X,Y)) --> term(X), [*], factor(Y).

factor(factor(X)) --> identifier(X); number(X); ['('], arithmetic_expr(X), [')'].

% identifiers, strings, numbers

identifier(id(X)) --> [X], {atom_string(X, X_str), re_match("[a-z]+"/i, X_str)}.

string_expr(str_expr(X)) --> string(X); identifier(X).
string(str(X)) --> ['"'], [X], ['"'].

number(num(X)) --> [X], {number(X)}.