module TheZipper

//
// From the Functional Pearl
// "The Zipper" by Gerald Heut
//

type 'a tree =
    | Item of 'a
    | Section of 'a tree list

type 'a path =
    | Top
    | Node of 'a tree list * 'a path * 'a tree list

type 'a location = Loc of 'a tree * 'a path

// from weaving a web on what a zipper for a specific data structure should be
type Ctx<'a> =
    | Top'
    | Section' of tree<'a> list * Ctx<'a> * tree<'a> list

// a * b + c * d
let t =
    Section [ Section [ Item "a"; Item "*"; Item "b" ]
              Item "+"
              Section [ Item "c"; Item "*"; Item "d" ] ]

//
// A focus on the second *
//

let focus =
    Loc
        (Item "*",
         Node
             ([ Item "c" ],
              Node
                  ([ Item "+"
                     Section [ Item "a"; Item "*"; Item "b" ] ],
                   Top,
                   []),
              [ Item "d" ]))

let go_left (Loc (t, p)) =
    match p with
    | Top -> failwith "Left of top"
    | Node (l :: left, up, right) -> Loc(l, Node(left, up, t :: right))
    | Node ([], up, right) -> failwith "Left of first"

let go_right (Loc (t, p)) =
    match p with
    | Top -> failwith "Right of top"
    | Node (left, up, r :: right) -> Loc (r, Node (t :: left, up, right))
    | _ -> failwith "Right of last"

let go_up (Loc (t, p)) =
    let rev = List.rev
    match p with
    | Top -> failwith "Up of top"
    | Node (left, up, right) -> Loc (Section ((rev left) @ t :: right), up)

let go_down (Loc (t, p)) =
    match t with
    | Item _ -> failwith "down of item"
    | Section (t1 :: trees) -> Loc(t1, Node([], p, trees))
    | Section [] -> failwith "Down of empty"

let rec nth loc =
    function
    | 1 -> go_down loc
    | n when n > 0 -> nth (go_right loc) (n - 1)
    | n -> failwith "nth expects a +ve integer."

let change (Loc (_, p)) t = Loc(t, p)

let insert_right (Loc (t, p)) r =
    match p with
    | Top -> failwith "Right of top"
    | Node (left, up, right) -> Loc(t, Node(left, up, r :: right))

let insert_left (Loc (t, p)) l =
    match p with
    | Top -> failwith "Insert of top"
    | Node (left, up, right) -> Loc (t, Node (l :: left, up, right))

let insert_down (Loc (t, p)) t1 =
    match t with
    | Item _ -> failwith "Down of item"
    | Section sons -> Loc(t1, Node([], p, sons))

let delete (Loc (_, p)) =
    match p with
    | Top -> failwith "Delete of top"
    | Node (left, up, r :: right) -> Loc (r, Node (left, up, right))
    | Node (l :: left, up, []) -> Loc (l, Node (left, up, []))

module Memo =
    type 'a memo_tree =
        | Item of 'a
        | Siblings of 'a memo_tree list * 'a memo_tree * 'a memo_tree list

    type 'a memo_path =
        | Top
        | Node of 'a memo_tree list * 'a memo_path * 'a memo_tree list

    type 'a memo_location = Loc of 'a memo_tree * 'a memo_path

    let go_up_memo (Loc (t, p)) =
        match p with 
        | Top -> failwith "Top of top"
        | Node (left, path, right) -> Loc (Siblings (left, t, right), path)
    
    let go_down_memo (Loc (t1, p)) =
        match t1 with 
        | Item _ -> failwith "down of Item"
        | Siblings (left, t2, right) -> Loc (t2, Node (left, p, right))
    
    let go_right_memo (Loc (t, p)) = 
        match p with 
        | Top -> failwith "right of Top"
        | Node (left, up, r :: right) -> Loc (r, Node (left @ [t], up, right))
        | Node (l, up, []) -> failwith "right of the rightmost element"

    let go_left_memo (Loc (t, p)) = 
        match p with
        | Top -> failwith "left of top"
        | Node (left, up, right) -> 
            let front, last = 
                left |> List.take (List.length left - 2), 
                left |> List.last 
            Loc (last, Node (front, up, t :: right))

module BinaryTree = 
    type binary_tree = 
        | Nil 
        | Cons of binary_tree * binary_tree
    
    type binary_path = 
        | Top 
        | Left of binary_path * binary_tree
        | Right of binary_tree * binary_path

    type binary_location = Loc of binary_tree * binary_path

    let change (Loc (_, p)) t = Loc (t, p)

    let go_left (Loc (right, p)) = 
        match p with 
        | Top -> failwith "left of Top"
        | Left _ -> failwith "you're already Left"
        | Right (left, path) -> Loc (left, Left (path, right))
    
    let go_right (Loc (left, p)) = 
        match p with 
        | Top -> failwith "left of Top"
        | Left (up, right) -> Loc (right, Right (left, up))
        | Right _ -> failwith "you're already Right"

    let go_up (Loc (item, path)) = 
        match path with 
        | Top -> failwith "Up of top"
        | Left (up, right) -> Loc (Cons (item, right), up)
        | Right (left, up) -> Loc (Cons (left, item), up)
    
    let go_first (Loc (item, path)) = 
        match item with 
        | Nil -> failwith "first of Nil"
        | Cons (first, second) -> Loc (first, Left (path, second))
    
    let go_second (Loc (item, path)) = 
        match item with 
        | Nil -> failwith "second of Nil"
        | Cons (first, second) -> Loc (second, Right (first, path))
