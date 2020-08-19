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

type Nat = Zero | Succ of Nat

type System.Int32 with
    member x.Return a = a
    member x.Quote a = a

let quote = 23
let workflow = quote {
    return 1
}

let rec (<<|) funcs args = 
    match funcs, args with 
    | f :: fs, a :: as' -> f a :: (fs <<| as')
    | _, _ -> []

let rec repeat f = [ yield f; yield! repeat f ]

let succ k funcs args = funcs <<| args |> k
let zero = id
let one fs a = succ zero fs a
let two fs a = succ one fs a
let three fs a = succ two fs a
let four fs a = succ three fs a
let five fs a = succ four fs a

let zipWith n f = n (repeat f)

let v = zipWith four (fun a b c d -> a + b + c + d) [1;3;5] [] [1;3;5] [2;4;5]