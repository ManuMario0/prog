(* parser *)
type lexem =
    | LBRACE
    | RBRACE
    | IADD
    | ISUB
    | IMUL
    | IDIV
    | IMOD
    | FADD
    | FSUB
    | FMUL
    | FDIV
    | F of float
    | I of int
    | F_FUN (* this is the converter function from int to float *)
    | I_FUN
    | FACT_FUN
    | POW_FUN
    | ASSIGN
    | VAR of string
    | EOF
;;

exception Error of string;;

(* this function is meant to count the number of dot in the next sequence, it is used to detect float AND syntax error *)
let rec parse_number l =
	match l with
	| '.'::t -> let (i, v, t2) = (parse_number t) in
			((i+1), ((Char.escaped '.')^v), t2)
	| a::t when (a>= '0' && a<='9') -> let (i, v, t2) = (parse_number t) in
			(i, ((Char.escaped a)^v), t2)
	| _ -> (0, "", l)
;;

let rec get_var l =
    match l with
    | ' '::t -> "", t
    | a::t when (a >= 'A' && a <= 'z') -> let n, t2 = get_var t in
        ((Char.escaped a)^n), t2
    | _ -> "", l
;;

(* this is the main parser *)
let rec parse_aux buf =
	match buf with
	| '('::t -> LBRACE::(parse_aux t)
	| ')'::t -> RBRACE::(parse_aux t)
	| ' '::t -> parse_aux t
	| '\t'::t -> parse_aux t
	| '#'::t -> []
    | '*'::'*'::t -> POW_FUN::(parse_aux t)
	| '+'::'.'::t -> FADD::(parse_aux t)
	| '-'::'.'::t -> FSUB::(parse_aux t)
	| '*'::'.'::t -> FMUL::(parse_aux t)
	| '/'::'.'::t -> FDIV::(parse_aux t)
	| '+'::t -> IADD::(parse_aux t)
	| '-'::t -> ISUB::(parse_aux t)
	| '*'::t -> IMUL::(parse_aux t)
	| '/'::t -> IDIV::(parse_aux t)
	| '%'::t -> IMOD::(parse_aux t)
    | '!'::t -> FACT_FUN::(parse_aux t)
	| a::t when (a >= '0' && a <= '9') -> (match (parse_number (a::t)) with
					| (0, v, t2) -> (I(int_of_string v))::(parse_aux t2)
					| (1, v, t2) -> (F(Float.of_string v))::(parse_aux t2)
					| _ -> raise (Error "Too much . in your input !!!!!!!!!!!")
					)
	| '.'::t -> (match (parse_number ('.'::t)) with
                    | (0, v, t2) -> (I(int_of_string v))::(parse_aux t2)
                    | (1, v, t2) -> (F(Float.of_string v))::(parse_aux t2)
                    | _ -> raise (Error "Too much . in your input !!!!!!!!!!!")
				)
	| 'f'::'l'::'o'::'a'::'t'::t -> F_FUN::(parse_aux t)
	| 'i'::'n'::'t'::t -> I_FUN::(parse_aux t)
	| '='::t -> ASSIGN::(parse_aux t)
	| a::t when (a >= 'A' && a <= 'z') -> let name, next = get_var (a::t) in
        (VAR(name))::(parse_aux next)
	| [] -> []
    | _ -> raise (Error "An unexpected error occures while parsing the file")
;;

(* this function compute the lexem list of every line *)
let rec parse_line fp =
	try
		let l = input_line fp in
		let buf = Array.make (String.length l) '0' in
		for i=0 to ((String.length l)-1) do
			buf.(i) <- l.[i]
		done;
        let res = parse_aux (Array.to_list buf) in
        if res=[] then (parse_line fp)
        else res::(parse_line fp)
	with
		| End_of_file -> []
;;

(* I/O *)
let parse f =
    let fp = open_in f in
	let res=parse_line fp in
    close_in fp ;
	res
;;
