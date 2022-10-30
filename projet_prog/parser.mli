open Hashtbl

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

val anal_synt : Lexer.lexem list list -> (string, int) t -> int -> (synt * int * string option) list
