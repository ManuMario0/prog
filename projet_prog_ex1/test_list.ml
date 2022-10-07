open My_list;;

let string_of_list str_fun l = 
  let rec string_content = function
    | []  -> ""
    | [x]  -> (str_fun x)
    | x::l -> (str_fun x) ^ ", " ^ (string_content l) 
  in "[" ^ (string_content l) ^ "]" in

let string_of_nat_list = string_of_list string_of_int in
let string_of_string_list = string_of_list (fun x -> x) in

let empty = [] in
let one = ["a"] in 
let lst = [1; 3; 6; 10; 15; 21; 28; 36; 45; 55] in

let test_hd () = 
  Printf.printf "Tête de %s : %s.\n" (string_of_string_list one) (List.hd one);
  Printf.printf "Tête de %s : %d.\n\n" (string_of_nat_list lst) (List.hd lst)

in let test_tl () = 
  Printf.printf "Queue de %s : %s.\n" (string_of_string_list one) (string_of_string_list (List.tl one));
  Printf.printf "Queue de %s : %s.\n\n" (string_of_nat_list lst) (string_of_nat_list (List.tl lst))

in let test_length () = 
  Printf.printf "Taille de %s : %d.\n" (string_of_string_list one) (List.length one);
  Printf.printf "Taille de %s : %d.\n" (string_of_nat_list lst) (List.length lst);
  Printf.printf "Taille de %s : %d.\n\n" (string_of_string_list empty) (List.length empty)

in let test_map ()= 
  Printf.printf "Map de (x -> xx) sur %s : %s.\n" (string_of_string_list one) (string_of_string_list (List.map (fun s -> s ^ s) one));
  Printf.printf "Map de (x -> 2x) sur %s : %s.\n" (string_of_nat_list lst) (string_of_nat_list (List.map (fun n -> 2 * n) lst));
  Printf.printf "Map de (x -> 2x) sur %s : %s.\n\n" (string_of_nat_list empty) (string_of_nat_list (List.map (fun n -> 2 * n) empty));

in test_hd(); test_tl(); test_length(); test_map()
