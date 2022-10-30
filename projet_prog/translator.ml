open Parser
open X86_64

exception Error of string
		
(* the idea is to parse our tree while producing our asm file, the most difficult thing is to order the way we compute tasks *)
(* For the order, we need to go trough the child before getting producing our result, and put thos result on the stack *)
(* this is not optimize but it seams to work fine *)
(* HAHAHAHHHAHAHAHHAHAHHA, you thought it would be easy but no !!!!!! Now you've just realized that you needed variables *)

(*
Input :
    - a syntaxic tree
    - an integer i which is used to name integer variables
    - an integer f which is used to name float variables
    - a dictionnary to remember variable types (maybe I could design it in a way so that it could be infered but I don't think this would be much simpler and efficient)
Output :
    - the text section of the assembly code which compute the given syntaxic tree
    - the data section of the assembly code which compute the given syntaxic tree
    - the updated value i
    - the updated value f
*)
let rec translate_aux s i f d =
	match s with
	| ADD(x, y) ->
		let t1, d1, i1, f1 = translate_aux x i f d in
		let t2, d2, i2, f2 = translate_aux y i1 f1 d in
		(t2 ++ pushq !%rax ++ t1 ++ popq rdi ++ addq !%rdi !%rax), (d1++d2), i2, f2
	| SUB(x, y) ->
		let t1, d1, i1, f1 = translate_aux x i f d in
        let t2, d2, i2, f2 = translate_aux y i1 f1 d in
        (t2 ++ pushq !%rax ++ t1 ++ popq rdi ++ subq !%rdi !%rax), (d1++d2), i2, f2
	| MUL(x, y) ->
		let t1, d1, i1, f1 = translate_aux x i f d in
        let t2, d2, i2, f2 = translate_aux y i1 f1 d in
        (t2 ++ pushq !%rax ++ t1 ++ popq rdi ++ imulq !%rdi !%rax), (d1++d2), i2, f2
	| DIV(x, y) ->
		let t1, d1, i1, f1 = translate_aux x i f d in
        let t2, d2, i2, f2 = translate_aux y i1 f1 d in
        (t2 ++ pushq !%rax ++ t1 ++ popq rdi ++ cqo ++ idivq !%rdi), (d1++d2), i2, f2
	| MOD(x, y) ->
		let t1, d1, i1, f1 = translate_aux x i f d in
        let t2, d2, i2, f2 = translate_aux y i1 f1 d in
        (t2 ++ pushq !%rax ++ t1 ++ popq rdi ++ cqo ++ idivq !%rdi ++ movq !%rdx !%rax), (d1++d2), i2, f2
	| ADD_f(x, y) ->
		let t1, d1, i1, f1 = translate_aux x i f d in
        let t2, d2, i2, f2 = translate_aux y i1 f1 d in
        (t2++pushsd !%xmm0++t1++popsd !%xmm1++addsd !%xmm1 !%xmm0), (d1++d2), i2, f2
	| SUB_f(x, y) ->
		let t1, d1, i1, f1 = translate_aux x i f d in
        let t2, d2, i2, f2 = translate_aux y i1 f1 d in
        (t2++pushsd !%xmm0++t1++popsd !%xmm1++subsd !%xmm1 !%xmm0), (d1++d2), i2, f2
        (*
        Here, we directly play with the rsp register to load and store data from the stack. Since each
        peace of data is stored on 8 byte, we juste have to move the data, and update the value of
        rsp accordingly
        *)
	| MUL_f(x, y) ->
		let t1, d1, i1, f1 = translate_aux x i f d in
        let t2, d2, i2, f2 = translate_aux y i1 f1 d in
        (t2++pushsd !%xmm0++t1++popsd !%xmm1++mulsd !%xmm1 !%xmm0), (d1++d2), i2, f2
	| DIV_f(x, y) ->
		let t1, d1, i1, f1 = translate_aux x i f d in
        let t2, d2, i2, f2 = translate_aux y i1 f1 d in
        (t2++pushsd !%xmm0++t1++popsd !%xmm1++divsd !%xmm1 !%xmm0), (d1++d2), i2, f2
	| Float(x) ->
		(movsd (lab (".F"^(string_of_int f)^"(%rip)")) !%xmm0), (inline ("\t.F"^(Int.to_string f)^": .double "^(Float.to_string x)^"\n")), i, (f+1)
	| Int(x) ->
		(inline ("\tmovq .I"^(Int.to_string i)^"(%rip), %rax\n")), (inline ("\t.I"^(Int.to_string i)^": .quad "^(Int.to_string x)^"\n")), (i+1), f
	| Float_fun(x) ->
		let t, d, i2, f2 = translate_aux x i f d in
		(t++(inline "\tcvtsi2sdq %rax, %xmm0\n")), d, i2, f2
	| Int_fun(x) ->
		let t, d, i2, f2 = translate_aux x i f d in
        (t++(inline "\tcvttsd2siq %xmm0, %rax\n")), d, i2, f2
    | Var(x) -> ( match Hashtbl.find_opt d x with
        | None -> raise (Error "Unknonw variable !")
        | Some y -> if y=0 then (inline ("\tmovq .I"^x^"(%rip), %rax\n")), nop, i, f
                else (movsd (lab (".F"^x^"(%rip)")) !%xmm0), nop, i, f
    )
    | FACT(x) ->
        let t, d, i2, f2 = translate_aux x i f d in
        (t++movq !%rax !%rdx++call "fact"), d, i2, f2
    | POW(x, y) ->
        let t1, d1, i1, f1 = translate_aux x i f d in
        let t2, d2, i2, f2 = translate_aux y i1 f1 d in
        (t2++pushq !%rax++t1++popq rdx++movq (imm 1) !%rdi++call "pow"), (d1++d2), i2, f2
;;

(* we translate every line we get in parameter *)
let rec translate l i j d =
	match l with
	| h::t -> (
		let s, typeof, var = h in
        match var with
        | None -> let (text, data, i1, j1) = translate_aux s i j d in
            let (tend, dend) = translate t i1 j1 d in
            if typeof=0 then
                (text++(inline "\tmovq %rax, %rsi\n\tcallq print_i\n")++tend), (data++dend)
            else
                (text++(inline "\tcallq print_f\n")++tend), (data++dend)
        | Some x ->
                match Hashtbl.find_opt d x with
                | None -> Hashtbl.add d x typeof;
                    let (text, data, i1, j1) = translate_aux s i j d in
                    let (tend, dend) = translate t i1 j1 d in
                    if typeof=0 then
                        (text
                        ++(inline ("\tmovq %rax, .I"^x^"(%rip)\n"))
                        ++tend), (data++dend++(inline (".I"^x^": .quad 0\n"))++(inline (".F"^x^": .double 0.\n")))
                    else
                        (text
                        ++movsd !%xmm0 (lab (".F"^x^"(%rip)\n"))
                        ++tend), (data++dend++(inline (".I"^x^": .quad 0\n"))++(inline (".F"^x^": .double 0.\n")))
                | Some y ->
                    let (text, data, i1, j1) = translate_aux s i j d in
                    Hashtbl.replace d x typeof;
                    let (tend, dend) = translate t i1 j1 d in
                    if typeof=0 then
                        (text
                        ++(inline ("\tmovq %rax, .I"^x^"(%rip)\n"))
                        ++tend), (data++dend)
                    else
                        (text
                        ++movsd !%xmm0 (lab (".F"^x^"(%rip)\n"))
                        ++tend), (data++dend)
    )
	| [] -> nop, nop
;;
