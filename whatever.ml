let rec foldl : 'b -> ('b -> 'a -> 'b) -> 'a list -> 'b =
  fun b f -> function
          | [] -> b
          | hd :: tl -> foldl (f b hd) f tl
;;

let rec foldr : 'b -> ('a -> 'b -> 'b) -> 'a list -> 'b =
  fun b f -> function
          | [] -> b
          | hd :: tl -> foldr b f tl |> f hd
;;

(* tails recursion *)
let rec foldr_tail : 'b -> ('a -> 'b -> 'b) -> 'a list -> 'b =
  fun b f ls ->
  let rec g : ('b -> 'b) -> 'a list -> 'b =
    fun k ls ->
    match ls with
    | [] -> k b
    | hd :: tl -> g (fun b -> k (f hd b)) tl
  in
  g (fun b -> b) ls
;;

let rec concat_tail : 'a list list -> 'a list = foldr_tail [] (@)
;;
