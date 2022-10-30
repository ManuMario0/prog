open X86_64
open Parser
open Hashtbl

val translate: (synt*int*string option) list -> int -> int -> (string, int) t -> ([`text] asm * [`data] asm)
