module Deriv

type Zipper<'a> = Zipper of 'a list * 'a * 'a list

let create l = 
    match l with 
    | [] -> failwith "Musn't be empty"
    | h :: t -> Zipper ([], h , t)

let right =
    function
    | Zipper (l, _, []) as z -> z
    | Zipper (l, z, h::rt) -> Zipper (z::l, h, rt)

let left = 
    function 
    | Zipper ([], _, r) as z -> z
    | Zipper (h::lt, z, r) -> Zipper (lt, h, z::r)

let update x (Zipper (l, _, r)) = Zipper (l, x, r)

let zipper = create [1;2;3;4;5]

let r =
    zipper
    |> right
    |> right
    |> update -3
    |> left
    |> left
    |> right |> right
 
type List<'a> = Empty | List of 'a * List<'a>