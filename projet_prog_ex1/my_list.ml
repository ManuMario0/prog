type 'a my_list =
        | Nil
        | Cons of 'a * 'a my_list
;;

let rec string_of_list f l =
        match l with
        | Nil -> ""
        | Cons(a, b) -> (f a)^(string_of_list f b)
;;

let hd l =
        match l with
        | Nil -> None
        | Cons (a, _) -> Some a
;;

let tl l =
        match l with
        | Nil -> None
        | Cons (_, b) -> Some b
;;

let rec length l =
        match l with
        | Nil -> 0
        | Cons (_, b) -> 1 + (length b)
;;

let rec map f l =
        match l with
        | Nil -> Nil
        | Cons (a, b) -> Cons(f a, map f b)
;;
