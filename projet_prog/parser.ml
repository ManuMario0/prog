open Lexer

exception Error of string;;

(* Syntax tree structure *)
type synt =
	| ADD of synt*synt
	| SUB of synt*synt
	| MUL of synt*synt
	| DIV of synt*synt
	| MOD of synt*synt
	| ADD_f of synt*synt
	| SUB_f of synt*synt
	| MUL_f of synt*synt
	| DIV_f of synt*synt
	| Float of float
	| Int of int
	| Float_fun of synt
	| Int_fun of synt
    | FACT of synt
    | POW of synt*synt
    | Var of string
;;

(* this type is used to keep track of the type we are currently dealing with *)
type exp =
	| NOPE
	| INT of synt
	| FLOAT of synt
;;

(* This funtion is used to optimize the inversion computation *)
(* We are distributing the minus sign to see if this produce a better result (in term of number of operations) and choose the better one *)
let rec invert syn d =
    match syn with
    | Int(x) -> Int(-x), 0
    | Float(x) -> Float(-.x), 0
    | ADD(x, y) ->
        let (h, n) = invert x d in
        let (t, m) = invert y d in
        if n<m then (SUB(h, y), n)
        else (SUB(t, x), m)
    | SUB(x, y) -> (
        match x with
        | Int(0) -> (y, -1)
        | _ ->
            let (h, n) = invert x d in
            let (t, m) = invert y d in
            if n+m<=0 then (SUB(h, t), n+m)
            else (SUB(y, x), 0)
        )
    | MUL(x, y) ->
        let (h, n) = invert x d in
        let (t, m) = invert y d in
        if n<m then (MUL(h, y), n)
        else
        (MUL(x, t), m)
    | DIV(x, y) ->
        let (h, n) = invert x d in
        let (t, m) = invert y d in
        if n<m then (DIV(h, y), n)
        else
        (DIV(x, t), m)
    | MOD(x, y) ->
        (MOD(x, y), 1)
    | ADD_f(x, y) ->
        let (h, n) = invert x d in
        let (t, m) = invert y d in
        if (n<0 && m<0) then (ADD_f(h, t), n+m)
        else if n<m then (SUB_f(h, y), n)
        else (SUB_f(t, x), m)
    | SUB_f(x, y) ->
            let (h, n) = invert x d in
            let (t, m) = invert y d in
            if n+m<=0 then (SUB_f(h, t), n+m)
            else (SUB_f(y, x), 0)
    | MUL_f(x, y) ->
        let (h, n) = invert x d in
        let (t, m) = invert y d in
        if n<m then (MUL_f(h, y), n)
        else
        (MUL_f(x, t), m)
    | DIV_f(x, y) ->
        let (h, n) = invert x d in
        let (t, m) = invert y d in
        if n<m then (DIV_f(h, y), n)
        else
        (DIV_f(x, t), m)
    | Int_fun(x) ->
        let (h, n) = invert x d in
        (Int_fun(h), n)
    | Float_fun(x) ->
        let (h, n) = invert x d in
        (Float_fun(h), n)
    | Var(x) -> (
        match (Hashtbl.find_opt d x) with
        | None -> raise (Error "should never happend so I can put anything I want lol")
        | Some n -> if n=0 then SUB((Int 0), Var(x)), 1
                    else SUB_f((Float 0.), Var(x)), 1
        )
    | FACT(x) ->
        SUB((Int 0), FACT(x)), 1
    | POW(x, y) ->
        SUB((Int 0), POW(x, y)), 1
;;

(*
this function takes as parameters :
    - the list of lexem l
    - the precedent expression prec
    - the amount of brace
    - a priority value prior
    - a brace signal m (used to force the use of brace, this is not at all a good way to do it, but making it more efficient means writing most of the code back again, which was not worth it)
    - a dictionnary d which keeps track of variables
    - the line number for debugging
and return :
    - a flag (for brace managment)
    - the remaining syntaxic tree
    - the expression
*)
(*
- The priority value is used to detect which operation within range (i.e. within the same 'brace') have the higher priority. The values of priorities are :
    - 0 : no priotities
    - 1 : add priority (i.e. every thing which has the same priority as add)
    - 3 : mul priority (can't remember why I skiped 2 ...)
    - 4 : pow priority
    - 5 : factorial, invertion and type casting priority (we do that to ensure we evaluate them before everything else)
    Using this, we can have two thing :
    - the priority between different operations are kept
    - we can always brace to the left (e.g. a-b-c=(a-b)-c) because we can stop any looking furthur when we hit an operation we the same level of priority
- finally, the flag is used to come back when we hit a closing brace, so we don't extend too far
*)
let rec anal_synt_aux l prec brace prior m d line =
	match l with
	| [] -> (0, [], prec)
	| LBRACE::t ->
			let f, tt, exp = anal_synt_aux t NOPE (brace+1) 0 0 d line in (* go through the brace *)
            if f = 1 then anal_synt_aux tt exp brace prior m d line (* this case is used after we came back from the closing brace *)
            else raise (Error ("Line "^(string_of_int line)^": No matching closing brace"))
	| RBRACE::t -> if (brace = 0) then raise (Error ("Line "^(string_of_int line)^": Too much closing braces"))
            else
            (1, t, prec) (* we set the flag on 1 : we come back to the openning brace *)
	| IADD::t -> (
        if m=1 then raise (Error ("Line "^(string_of_int line)^": You are not allowed to stack multiple sign change"))
        else
		if (prec = NOPE) then let flag, rest, exp = (anal_synt_aux t NOPE brace 5 1 d line) in
            (* Here we check if are not only inverting.
                If this is the case, we won't have anything to pair the next value with,
                which correspond to an empty stack in terms of non-brace terms.
                You might also see that this is the higher priority we have,
                this is because we want it to be evaluated first *)
            match exp with (* fun thing to notice here, we can have a float as a parameter, so be carefull *)
            | INT(x) ->
				if (flag = 1) then (1, rest, INT(x))
                (* if the flag is set on 1,
                    then we have hit the closing brace when going through
                    the next parameter so we have to return directly *)
                else
                anal_synt_aux rest (INT(x)) brace prior 0 d line
			| FLOAT(x) ->
				if (flag = 1) then (1, rest, FLOAT(x))
                else
                anal_synt_aux rest (FLOAT(x)) brace prior m d line
            | _ -> raise (Error ("Line "^(string_of_int line)^": An unexpected error occures"))
		else
		if (prior > 0) then (0, IADD::t, prec)
        (* we evaluate the operation only if we didn't hit a higher priority operation before *)
		else
		match prec with
		| INT(x) -> ( let flag, rest, exp = (anal_synt_aux t NOPE brace 1 m d line) in
			match exp with
			| INT(y) ->
				if (flag = 1) then (1, rest, (INT(ADD(x, y))))
                else
				anal_synt_aux rest (INT(ADD(x, y))) brace prior m d line
			| _ -> raise (Error ("Line "^(string_of_int line)^": + : incompatible types : expected integer as second operand"))
			)
		| _ -> raise (Error ("Line "^(string_of_int line)^": + : incompatible types : expected integer as first operand"))
		)
	| IMUL::t -> (
        if (prior > 2) then (0, IMUL::t, prec)
		else
        match prec with
        | INT(x) -> ( let flag, rest, exp = (anal_synt_aux t NOPE brace 3 m d line) in
            match exp with
            | INT(y) ->
				if (flag = 1) then (1, rest, (INT(MUL(x, y))))
                else
                anal_synt_aux rest (INT(MUL(x, y))) brace prior m d line
            | _ -> raise (Error ("Line "^(string_of_int line)^": * : incompatible types : expected integer as second operand"))
            )
        | _ -> raise (Error ("Line "^(string_of_int line)^": * : incompatible types : expected integer as first operand"))
        )
	| IDIV::t -> ( if (prior > 2) then (0, IDIV::t, prec)
		else
        match prec with
        | INT(x) -> ( let flag, rest, exp = (anal_synt_aux t NOPE brace 3 m d line) in
            match exp with
            | INT(y) ->
				if (flag = 1) then (1, rest, (INT(DIV(x, y))))
                else
                anal_synt_aux rest (INT(DIV(x, y))) brace prior m d line
            | _ -> raise (Error ("Line "^(string_of_int line)^": / : incompatible types : expected integer as second operand"))
            )
        | _ -> raise (Error ("Line "^(string_of_int line)^": / : incompatible types : expected integer as first operand"))
        )
	| ISUB::t -> (
        if m=1 then raise (Error ("Line "^(string_of_int line)^": You are not allowed to stack multiple sign change"))
        else
		if (prec = NOPE) then let flag, rest, exp = (anal_synt_aux t NOPE brace 5 1 d line) in (* here we check if are not only inverting *)
			match exp with
			| INT(x) -> (match x with
                | Int(y) ->
                    if (flag = 1) then (1, rest, (INT(Int(-y))))
                    else
                    anal_synt_aux rest (INT(Int(-y))) brace prior m d line
                | a -> let (h, n) = invert a d in
                    if (flag = 1) then (1, rest, INT(h))
                    else
                    anal_synt_aux rest (INT(h)) brace prior m d line
            )
			| FLOAT(x) -> (match x with
                | Float(y) ->
                    if (flag = 1) then (1, rest, (FLOAT(Float(-.y))))
                    else
                    anal_synt_aux rest (FLOAT(Float(-.y))) brace prior m d line
                | a -> let (h, n) = invert a d in
                    if (flag = 1) then (1, rest, FLOAT(h))
                    else
                    anal_synt_aux rest (FLOAT(h)) brace prior m d line
            )
			| _ -> raise (Error ("Line "^(string_of_int line)^": - : too mush open brace after this symbol"))
		else (* else this is a normal substraction *)
		if (prior > 0) then (0, ISUB::t, prec)
		else
        match prec with
        | INT(x) -> ( let flag, rest, exp = (anal_synt_aux t NOPE brace 1 m d line) in (* if we get an int from the analyses, this is what we want *)
            match exp with
            | INT(y) ->
				if (flag = 1) then (1, rest, (INT(SUB(x, y))))
                else
                anal_synt_aux rest (INT(SUB(x, y))) brace prior m d line
            | _ -> raise (Error ("Line "^(string_of_int line)^": - : incompatible types : expected integer as second operand"))
            )
        | _ -> raise (Error ("Line "^(string_of_int line)^": - : incompatible types : expected integer as first operand"))
        )
	| IMOD::t -> ( if (prior > 2) then (0, IMOD::t, prec)
		else
        match prec with
        | INT(x) -> ( let flag, rest, exp = (anal_synt_aux t NOPE brace 3 m d line) in
            match exp with
            | INT(y) ->
				if (flag = 1) then (1, rest, (INT(MOD(x, y))))
                else
                anal_synt_aux rest (INT(MOD(x, y))) brace prior m d line
            | _ -> raise (Error ("Line "^(string_of_int line)^": % : incompatible types : expected integer as second operand"))
            )
        | _ -> raise (Error ("Line "^(string_of_int line)^": % : incompatible types : expected integer as first operand"))
        )
    | FACT_FUN::t -> (
        if m<>0 then raise (Error ("Line "^(string_of_int line)^": ! : there are missing braces"))
        else
        if (prior > 5) then (0, FACT_FUN::t, prec)
        else
        match prec with
        | INT(x) -> anal_synt_aux t (INT(FACT(x))) brace prior m d line
        | _ -> raise (Error ("Line "^(string_of_int line)^": ! : incompatible type : expected integer"))
    )
    | POW_FUN::t -> (
        if (prior > 3) then (0, FACT_FUN::t, prec)
        else
        match prec with
        | INT(x) -> ( let flag, rest, exp = (anal_synt_aux t NOPE brace 4 m d line) in
            match exp with
            | INT(y) ->
                if (flag = 1) then (1, rest, (INT(POW(x, y))))
                else
                anal_synt_aux rest (INT(POW(x, y))) brace prior m d line
            | _ -> raise (Error ("Line "^(string_of_int line)^": ** : incompatible types : expected integer as second operand"))
            )
        | _ -> raise (Error ("Line "^(string_of_int line)^": ** : incompatible types : expected integer as first operand"))
    )
	| FADD::t -> ( if (prior > 0) then (0, FADD::t, prec) (* it works mostly like the add function, except we don't have to check for things like +.(0.5)*)
        else
        match prec with
        | FLOAT(x) -> ( let flag, rest, exp = (anal_synt_aux t NOPE brace 1 m d line) in
            match exp with
            | FLOAT(y) ->
				if (flag = 1) then (1, rest, (FLOAT(ADD_f(x, y))))
                else
                anal_synt_aux rest (FLOAT(ADD_f(x, y))) brace prior m d line
            | _ -> raise (Error ("Line "^(string_of_int line)^": +. : incompatible types : expected float as second operand"))
            )
        | _ -> raise (Error ("Line "^(string_of_int line)^": +. : incompatible types : expected float as first operand"))
        )
	| FSUB::t -> ( if (prior > 0) then (0, FSUB::t, prec) (* it works mostly like the add function, except we don't have to check for things like +.(0.5)*)
        else
        match prec with
        | FLOAT(x) -> ( let flag, rest, exp = (anal_synt_aux t NOPE brace 1 m d line) in
            match exp with
            | FLOAT(y) ->
				if (flag = 1) then (1, rest, (FLOAT(SUB_f(x, y))))
                else
                anal_synt_aux rest (FLOAT(SUB_f(x, y))) brace prior m d line
            | _ -> raise (Error ("Line "^(string_of_int line)^": -. : ncompatible types : expected float as second operand"))
            )
        | _ -> raise (Error ("Line "^(string_of_int line)^": -. : incompatible types : expected float as first operand"))
        )
	| FMUL::t -> ( if (prior > 2) then (0, FMUL::t, prec)
		else
        match prec with
        | FLOAT(x) -> ( let flag, rest, exp = (anal_synt_aux t NOPE brace 3 m d line) in
            match exp with
            | FLOAT(y) ->
				if (flag = 1) then (1, rest, (FLOAT(MUL_f(x, y))))
                else
                anal_synt_aux rest (FLOAT(MUL_f(x, y))) brace prior m d line
            | _ -> raise (Error ("Line "^(string_of_int line)^": *. : incompatible types : expected float as second operand"))
            )
        | _ -> raise (Error ("Line "^(string_of_int line)^": *. : incompatible types : expected float as first operand"))
        )
	| FDIV::t -> ( if (prior > 2) then (0, FDIV::t, prec)
		else
        match prec with
        | FLOAT(x) -> ( let flag, rest, exp = (anal_synt_aux t NOPE brace 3 m d line) in
            match exp with
            | FLOAT(y) ->
				if (flag = 1) then (1, rest, (FLOAT(DIV_f(x, y))))
            	else
                anal_synt_aux rest (FLOAT(DIV_f(x, y))) brace prior m d line
            | _ -> raise (Error ("Line "^(string_of_int line)^": /. : incompatible types : expected float as second operand"))
            )
        | _ -> raise (Error ("Line "^(string_of_int line)^": /. : incompatible types : expected float as first operand"))
        )
	| I(x)::t -> if (prec <> NOPE) then raise (Error ("Line "^(string_of_int line)^": Too much paramters"))
        else
        if m=0 then anal_synt_aux t (INT(Int(x))) brace prior m d line
        else if m=1 then (0, t, (INT(Int(x))))
        else raise (Error ("Line "^(string_of_int line)^": You are not allowed to call int() or float() without braces"))
	| F(x)::t -> if (prec <> NOPE) then raise (Error ("Line "^(string_of_int line)^": Too much parameters"))
        else
        if m=0 then anal_synt_aux t (FLOAT(Float(x))) brace prior m d line
        else if m=1 then (0, t, (FLOAT(Float(x))))
        else raise (Error ("Line "^(string_of_int line)^": You are not allowed to call int() or float() without braces"))
	| F_FUN::t -> (
        if m <> 0 then raise (Error ("Line "^(string_of_int line)^": float() : you are not allowed to change the sign of this function"))
        else
        if (prior > 5) then (0, F_FUN::t, prec) (* we will hilghly prioritize this function *)
		else
		let flag, rest, exp = (anal_synt_aux t NOPE brace 5 2 d line) in
		match exp with
		| INT(x) ->
			if (flag = 1) then (1, rest, (FLOAT(Float_fun(x))))
            else
			anal_synt_aux rest (FLOAT(Float_fun(x))) brace prior m d line
		| _ -> raise (Error ("Line "^(string_of_int line)^": float() : incompatible types : you should pass a integer as parameter"))
		)
	| I_FUN::t -> (
        if m<>0 then raise (Error ("Line "^(string_of_int line)^": int() : you are not allowed to change the sign of this function"))
        else
        if (prior > 5) then (0, I_FUN::t, prec) (* we will hilghly prioritize this function *)
        else
        let (flag, rest, exp) = (anal_synt_aux t NOPE brace 5 2 d line) in
        match exp with
        | FLOAT(x) ->
			if (flag = 1) then (1, rest, (INT(Int_fun(x))))
			else
            anal_synt_aux rest (INT(Int_fun(x))) brace prior m d line
        | _ -> raise (Error ("Line "^(string_of_int line)^": int() : incompatible types : you should pass a float as parameter"))
        )
	| EOF::t -> (0, [], prec)
    | ASSIGN::t -> raise (Error ("Line "^(string_of_int line)^": Unexpected occurence of the symbol ="))
    | VAR(x)::t -> if not(prec = NOPE) then raise (Error "Too much parameters")
        else if m<>0 then raise (Error ("Line "^(string_of_int line)^": You are not allowed to use this parameter without any brace"))
        else
        match (Hashtbl.find_opt d x) with
        | None -> raise (Error ("Line "^(string_of_int line)^": Uninitialized data used"))
        | Some n -> if n=0 then anal_synt_aux t (INT(Var(x))) brace prior m d line
            else anal_synt_aux t (FLOAT(Var(x))) brace prior m d line
;;

(*
This functions go trough all the lines of lexem
It takes :
    - a list of list of lexems
It returns :
    - a syntaxic tree
    - a flag which correpond to the type returned :
        - 0 : integer
        - 1 : float
*)
let rec anal_synt l d line =
    match l with
	| h::t -> (
        match h with
            | VAR(x)::ASSIGN::t2 -> ( let _, _, res = anal_synt_aux t2 NOPE 0 0 0 d line in
                match res with
                    | INT(y) -> Hashtbl.replace d x 0;
                        (y, 0, Some x)::(anal_synt t d (line+1))
                    | FLOAT(y) -> Hashtbl.replace d x 1;
                        (y, 1, Some x)::(anal_synt t d (line+1))
                    | _ -> raise (Error ("Line "^(string_of_int line)^": Missing parameters to ="))
                )
            | _ -> ( let _, _, res = anal_synt_aux h NOPE 0 0 0 d line in
                match res with
                    | INT(x) -> (x, 0, None)::(anal_synt t d (line+1))
                    | FLOAT(x) -> (x, 1, None)::(anal_synt t d (line+1))
                    | _ -> raise (Error ("Line "^(string_of_int line)^": Empty lines are not allowed")) (* this case happend when there is an empty line *)
                )
    )
    | [] -> []
;;
