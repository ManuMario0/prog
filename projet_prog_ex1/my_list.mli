type 'a my_list = 
| Nil
| Cons of 'a * 'a my_list

val string_of_list: ('a -> string) -> 'a my_list -> string

val hd: 'a my_list -> 'a option

val tl: 'a my_list -> 'a my_list option

val length: 'a my_list -> int

val map: ('a -> 'b) -> 'a my_list -> 'b my_list