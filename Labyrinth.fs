module Labyrinth

// 
// https://en.wikibooks.org/wiki/Haskell/Zippers
// 

type Node<'a> = 
    | DeadEnd of 'a 
    | Passage of 'a * Node<'a> 
    | Fork of 'a * Node<'a> * Node<'a> 
    | Gold 

let get = 
    function 
    | DeadEnd a | Passage (a, _) 
    | Fork (a, _, _) -> a

let put a = 
    function
    | DeadEnd _ -> DeadEnd a 
    | Passage (_, node) -> Passage (a, node)
    | Fork (_, n1, n2) -> Fork (a, n1, n2)

// let gold = failwith ""

let lab = 
    Fork ((2, 4), 
        Fork ((0, 2),
            DeadEnd (1, 2), 
            DeadEnd (2, 0)), 
        Passage ((4, 2), 
            Fork ((1, 2), 
                Passage ((2, 3), Gold), 
                DeadEnd (2, 1))))


// let turnRight = 
//     function 
//     | Fork (_, l, r) -> Some r 
//     | _ -> None 


type Branch = 
    | KeepStraightOn 
    | TurnLeft 
    | TurnRight 

type Thread = Branch list 

let turnRight (t: Thread) : Thread = t @ [TurnRight]

let rec retreive (t: Thread) node = 
    match t, node with 
    | [], _ -> get node
    | _, DeadEnd a -> assert false; a // this shouldn't happen 
    | TurnLeft :: bs, Fork (_, l, _) -> retreive bs l 
    | TurnRight :: bs, Fork (_, _, r) -> retreive bs r
    | KeepStraightOn :: bs, Passage (_, d) -> retreive bs d
    | (TurnLeft | TurnRight) :: bs, Passage (_, d) -> 
        assert false; // this shouldn't happen 
        retreive bs d 
    | KeepStraightOn :: bs, Fork (_, l, _) -> 
        assert false; // This shouldn't happen
        retreive bs l

let rec update f (t: Thread) node = 
    match t, node with 
    | [], _ -> put (f (get node)) node
    | TurnLeft :: bs, Fork (a, l, r) -> Fork (a, update f bs l, r) 
    | TurnRight :: bs, Fork (a, l, r) -> Fork (a, l, update f bs r)
    | KeepStraightOn :: bs, Passage (a, s) -> Passage (a, update f bs s)
    | _, DeadEnd a -> failwith "update: _, DeadEnd a" 
    | (TurnLeft | TurnRight) :: bs, Passage (_, d) -> 
        failwith "update: (TurnLeft | TurnRight) :: bs, Passage (_, d)"
    | KeepStraightOn :: bs, Fork (_, l, _) -> 
        failwith "update: KeepStraightOn :: bs, Fork (_, l, _)"
    
module Ariadne = 

    type Branch<'a> = 
        | KeepStraightOn of 'a 
        | TurnLeft of 'a * Node<'a> 
        | TurnRight of 'a * Node<'a> 
    
    type Thread<'a> = Branch<'a> list 

    type Zipper<'a> = Thread<'a> * Node<'a> 

    let turnRight (zipper: Zipper<'a>) : Zipper<'a> option =
        match zipper with 
        | t, Fork (x, l, r) -> Some (TurnRight (x, l) :: t, r)
        | _ -> None
    
    let turnLeft (zipper: Zipper<'a>) : Zipper<'a> option =
        match zipper with 
        | t, Fork (x, l, r) -> Some (TurnLeft (x, r) :: t, l)
        | _ -> None
    
    let keepStraightOn (zipper: Zipper<'a>) : Zipper<'a> option =
        match zipper with 
        | t, Passage (x, n) -> Some (KeepStraightOn x :: t, n)
        | _ -> None
    
    let back (zipper: Zipper<'a>) : Zipper<'a> option = 
        match zipper with 
        | [], _ -> None 
        | KeepStraightOn x :: t, n -> Some (t, Passage (x, n))
        | TurnLeft (x, r) :: t, l -> Some (t, Fork (x, l, r)) 
        | TurnRight (x, l) :: t, r -> Some (t, Fork (x, l ,r))
    
    let get (x: Zipper<'a>) =
        match x with 
        | _, node -> get node

    let put (x: Zipper<'a>) a : Zipper<'a> = 
        match x with 
        | thread, _ -> thread, a

    let update f (x: Zipper<'a>) : Zipper<'a> =
        match x with 
        | thread, Fork (x, l, r) -> thread, Fork (f x, l, r)
        | thread, Passage (x, d) -> thread, Passage (f x, d)
        | thread, DeadEnd x -> thread, DeadEnd (f x)