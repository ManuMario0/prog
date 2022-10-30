(* type of lexem *)
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

(* Return the lexem list of the given file *)
val parse : string -> lexem list list
