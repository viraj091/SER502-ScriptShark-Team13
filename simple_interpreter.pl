:- table boolean_expr/3.
:- table arithmetic_expr/3.
:- table term/3.

lookup(X, Env, Ret) :- member((_, X, Ret), Env).
remove(X, Env, NewEnv) :- (lookup(X, Env, _), delete(Env, (_, X, _), NewEnv)); (NewEnv = Env).
update(Type, X, Val, Env, NewEnv) :- remove(X, Env, NewEnv1), append(NewEnv1, [(Type, X, Val)], NewEnv).

% program, block

program_eval(P, Z) :- 
    program_eval_h(P, [], NewEnv, Z).

program_eval_h(prog(X), Env, NewEnv, Ret) :- 
    program_eval_h(X, Env, NewEnv, _),
    lookup(z, NewEnv, Ret).

program_eval_h(block(X), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, NewEnv, _).

% assignment commands

program_eval_h(assgn_num(X,Y), Env, NewEnv, Ret) :- 
    program_eval_h(X, Env, _, Id),
    program_eval_h(Y, Env, _, Val),

    (member((_,Val,ValN), Env); (number(_,[Val],[]), ValN = Val)),
    % ValN = Val,

    update(num, Id, ValN, Env, NewEnv),
    Ret = Id.

program_eval_h(assgn_bool(X,Y), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Id),
    program_eval_h(Y, Env, _, Val),

    % (member((_,Val,ValN), Env); (boolean_expr(_,[Val],[]), ValN = Val)),
    ValN = Val,

    update(bool, Id, Val, Env, NewEnv),
    Ret = Id.

program_eval_h(assgn_str(X,Y), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Id),
    program_eval_h(Y, Env, _, Val),

    (member((_,Val,ValN), Env); (string(Val), ValN = Val)),

    update(bool, Id, ValN, Env, NewEnv),
    Ret = Id.

program_eval_h(assgn_num(X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(assgn_num(X,Y), Env, NewEnv1, _),
    program_eval_h(Z, NewEnv1, NewEnv, _),
    Ret = Id.

program_eval_h(assgn_bool(X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(assgn_bool(X,Y), Env, NewEnv1, _),
    program_eval_h(Z, NewEnv1, NewEnv, _),
    Ret = Id.

program_eval_h(assgn_str(X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(assgn_str(X,Y), Env, NewEnv1, _),
    program_eval_h(Z, NewEnv1, NewEnv, _),
    Ret = Id.

% control commands

program_eval_h(if(X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Bool),

    ((Bool == true, program_eval_h(Y, Env, NewEnv, _));
    (Bool == false, program_eval_h(Z, Env, NewEnv, _))).

program_eval_h(tern(X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(if(X,Y,Z), Env, NewEnv, Ret).

program_eval_h(while(X,Y), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Bool),

    ((Bool == true, program_eval_h(Y, Env, NewEnv1, _),
    program_eval_h(while(X,Y), NewEnv1, NewEnv, _));

    (Bool == false, NewEnv = Env)).

program_eval_h(while_and(X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Bool),

    ((Bool == true, program_eval_h(Y, Env, NewEnv1, _), program_eval_h(Z, NewEnv1, NewEnv2, _),
    program_eval_h(while_and(X,Y,Z), NewEnv2, NewEnv, _));

    (Bool == false, NewEnv = Env)).

program_eval_h(for(W,X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(W, Env, NewEnv1, Id),
    program_eval_h(while_and(X,Y,Z), NewEnv1, NewEnv2, _),
    remove(Id, NewEnv2, NewEnv).

program_eval_h(for_range(W,X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(W, Env, _, Id),
    program_eval_h(X, Env, _, Start),
    program_eval_h(Y, Env, _, End),

    update(num, Id, Start, Env, NewEnv1),

    (((Start =\= End), program_eval_h(Z, NewEnv1, NewEnv2, _), NewStart is Start + 1,
    program_eval_h(for_range(W,num(NewStart),Y,Z), NewEnv2, NewEnv, _));
    ((Start =:= End), remove(Id, NewEnv1, NewEnv))).

program_eval_h(print(X), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Val),
    (member((_,Val,ValN), Env); (number(_,[Val],[]), ValN = Val); (string(Val), ValN = Val)),
    write(ValN), nl,
    NewEnv = Env.

% recursive commands
    
program_eval_h(if(W,X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(if(W,X,Y), Env, NewEnv1, _),
    program_eval_h(Z, NewEnv1, NewEnv, _).

program_eval_h(tern(W,X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(if(W,X,Y), Env, NewEnv1, _),
    program_eval_h(Z, NewEnv1, NewEnv, _).

program_eval_h(while(X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(while(X,Y), Env, NewEnv1, _),
    program_eval_h(Z, NewEnv1, NewEnv, _).

program_eval_h(for(V,W,X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(V, Env, NewEnv1, _),
    program_eval_h(while_and(W,X,Y), NewEnv1, NewEnv2, _),
    remove(Id, NewEnv2, NewEnv3),

    program_eval_h(Z, NewEnv3, NewEnv, _).

program_eval_h(for_range(V,W,X,Y,Z), Env, NewEnv, Ret) :-
    program_eval_h(for_range(V,W,X,Y), Env, NewEnv1, _),
    program_eval_h(Z, NewEnv1, NewEnv, _).

program_eval_h(print(X,Y), Env, NewEnv, Ret) :-
    program_eval_h(print(X), Env, _, Val),
    program_eval_h(Y, Env, NewEnv, _).

% boolean expressions

program_eval_h(bool_expr(true), Env, NewEnv, Ret) :-
    Ret = true,
    NewEnv = Env.

program_eval_h(bool_expr(false), Env, NewEnv, Ret) :-
    Ret = false,
    NewEnv = Env.

program_eval_h(and_expr(X,Y), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Bool1),
    program_eval_h(Y, Env, _, Bool2),
    ((Bool1 == true, Bool2 == true, Ret = true);
    (Ret = false)),
    NewEnv = Env.

program_eval_h(or_expr(X,Y), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Bool1),
    program_eval_h(Y, Env, _, Bool2),
    (((Bool1 == true; Bool2 == true), Ret = true);
    (Ret = false)),
    NewEnv = Env.

program_eval_h(not(X), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Bool),
    (((Bool == true), Ret = false);
    ((Bool == false), Ret = true)),
    NewEnv = Env.

program_eval_h(eq(X,Y), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Val1),
    ((member((_,Val1,Val1N), Env)); (number(_,[Val1],[]), Val1N = Val1); (string(Val1), Val1N = Val1)),

    program_eval_h(Y, NewEnv1, NewEnv2, Val2),
    (member((_,Val2,Val2N), Env); (number(_,[Val2],[]), Val2N = Val2); (string(Val1), Val1N = Val1)),

    ((Val1N == Val2N, Ret = true);
    (Val1N =\= Val2N, Ret = false)),
    NewEnv = Env.

% arithmetic expressions

program_eval_h(term(X), Env, NewEnv, Val) :-
    program_eval_h(X, Env, NewEnv, Val).

program_eval_h(add_expr(X,Y), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Term),
    program_eval_h(Y, Env, _, Expr),

    ((identifier(_,[Term],[]), member((_,Term,Val1), Env));
    (number(_,[Term],[]), Val1 = Term)),

    ((identifier(_,[Expr],[]), member((_,Expr,Val2), Env));
    (number(_,[Expr],[]), Val2 = Expr)),

    Ret is Val1 + Val2,
    NewEnv = Env.
    
program_eval_h(sub_expr(X,Y), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Term),
    program_eval_h(Y, Env, _, Expr),

    ((identifier(_,[Term],[]), member((_,Term,Val1), Env));
    (number(_,[Term],[]), Val1 = Term)),

    ((identifier(_,[Expr],[]), member((_,Expr,Val2), Env));
    (number(_,[Expr],[]), Val2 = Expr)),

    Ret is Val1 - Val2,
    NewEnv = Env.

program_eval_h(factor_term(X), Env, NewEnv, Ret) :- 
    program_eval_h(X, Env, NewEnv, Ret).

program_eval_h(mult_term(X,Y), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Factor),
    program_eval_h(Y, Env, _, Expr),

    ((identifier(_,[Factor],[]), member((_,Factor,Val1), Env));
    (number(_,[Factor],[]), Val1 = Factor)),

    ((identifier(_,[Expr],[]), member((_,Expr,Val2), Env));
    (number(_,[Expr],[]), Val2 = Expr)),

    Ret is Val1 * Val2,
    NewEnv = Env.

program_eval_h(div_term(X,Y), Env, NewEnv, Ret) :-
    program_eval_h(X, Env, _, Factor),
    program_eval_h(Y, Env, _, Expr),

    ((identifier(_,[Factor],[]), member((_,Factor,Val1), Env));
    (number(_,[Factor],[]), Val1 = Factor)),

    ((identifier(_,[Expr],[]), member((_,Expr,Val2), Env));
    (number(_,[Expr],[]), Val2 = Expr)),

    Ret is Val1 / Val2,
    NewEnv = Env.

% factors, identifiers, numbers

program_eval_h(factor(X), Env, NewEnv, Ret) :- 
    program_eval_h(X, Env, NewEnv, Ret).

program_eval_h(id(X), Env, NewEnv, Ret) :- 
    Ret = X,
    NewEnv = Env.

program_eval_h(num(X), Env, NewEnv, Ret) :-
    Ret = X,
    NewEnv = Env.

program_eval_h(str(X), Env, NewEnv, Ret) :-
    atom_string(X, Ret),
    NewEnv = Env.
