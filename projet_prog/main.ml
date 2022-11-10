open Translator;;
open Parser;;
open Lexer;;
open X86_64

exception Error of string;;

let check_format f =
    let n = String.length f in
    if not(n > 3 && f.[n-1] = 'p' && f.[n-2] = 'x' && f.[n-3] = 'e') then raise (Error "You are not using the good format")
    else ()
;;

let main f =
    check_format f;
    let d = Hashtbl.create 50 in
	let l = anal_synt (parse f) d 1 in
    Hashtbl.clear d;
	let text, data = translate l 0 0 d in
	let prog = {text=((inline ("\t.globl main"
                                ^ "\nmain:"
                                ^ "\n\tpushq %rbp\n"))
                        ++text
                        ++(inline ("\tpopq %rbp\n"
                                ^ "\tret\n\n"
                                ^ "print_i:\n"
                                ^ "\tpushq %rbp\n"
                                ^ "\tmovq $0, %rax\n"
                                ^ "\tleaq msg_i(%rip), %rdi\n"
                                ^ "\tcallq printf\n"
                                ^ "\tpopq %rbp\n"
                                ^ "\tret\n\n"
                                ^ "print_f:\n"
                                ^ "\tpushq %rbp\n"
                                ^ "\tmovq $1, %rax\n"
                                ^ "\tleaq msg_f(%rip), %rdi\n"
                                ^ "\tcallq printf\n"
                                ^ "\tpopq %rbp\n"
                                ^ "\tret\n\n"
                                ^ "fact:\n"
                                ^ "\tsubq $1, %rdx\n"
                                ^ "\tcmpq $1, %rdx\n"
                                ^ "\tjle fact_end\n"
                                ^ "\timulq %rdx, %rax\n"
                                ^ "\tjmp fact\n\n"
                                ^ "fact_end:\n"
                                ^ "\tcmpq $1, %rax\n"
                                ^ "\tjg exit\n"
                                ^ "\tmovq $1, %rax\n"
                                ^ "exit:\n"
                                ^ "\tret\n\n"
                                ^ "pow:\n"
                                ^ "\tcmpq $0, %rdx\n"
                                ^ "\tjle pow_end\n"
                                ^ "\timulq %rax, %rdi\n"
                                ^ "\tdec %rdx\n"
                                ^ "\tjmp pow\n\n"
                                ^ "pow_end:\n"
                                ^ "\tmovq %rdi, %rax\n"
                                ^ "\tret\n")));
                data=(data
                        ++(inline "\tmsg_i: .string \"Output : %d\\n\"\n\tmsg_f: .string \"Output : %lf\\n\"\n"))}
    in
    let n = String.length f in
	print_in_file ((String.sub f 0 (n-3))^"s") prog
;;

main Sys.argv.(1);;
