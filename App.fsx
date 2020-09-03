

//
// Just some random code 
//

// 
// Implementation inspired by: 
// https://twitter.com/TartanLlama/status/1300721014532444160
// 
let fn f vs =
    let folder v map =
        let k = f v 
        match Map.tryFind k map with
        | Some l -> Map.add k (v :: l) map
        | None -> Map.add k [v] map

    List.foldBack folder vs Map.empty



// notRev x ++ notRev y == notRev (y ++ x)

let rec zipWith f xs ys =
    match xs, ys with 
    | x :: xs, y :: ys -> f x y :: zipWith f xs ys
    | [], [] -> [] // same length, but done
    | _ -> [] // not the same length 
 
let rec accumulate f vector = 
    match vector with 
    | [] -> []
    | row :: rest -> 
        List.reduce f (List.head row :: List.map List.head rest)
        :: accumulate f (List.tail row :: List.map List.tail rest)

type Lzy<'a> = Lzy of (unit -> 'a)

let frc (Lzy f) = f ()

let ($) = (<|)

let a = Lzy $ fun () -> 10

let p x = fun y -> x 

let (<*>) f a = 
    fun x -> f x (a x)

let a' l = 
    zipWith (<=) l l.[1..] 
    |> List.forall id

let a'' l =
    l 
    |> List.fold (fun (v, b) a -> (a, v <= a && b)) (0, true)
    |> snd

let rec qsort l =
    match l with 
    | x :: xs -> 
        let left = [ for i in xs do if i <= x then yield i ]
        let right = [ for i in xs do if i > x then yield i ]
        (qsort left) @ [x] @ (qsort right)
    | [] -> []

#load "Labyrinth.fs"
open Labyrinth

#load "Util.fs"
open Util

let lab = 
    Fork ((2, 4), 
        Fork ((0, 2),
            DeadEnd (1, 2), 
            DeadEnd (2, 0)), 
        Passage ((4, 2), 
            Fork ((1, 2), 
                Passage ((2, 3), Gold (2, 2)), 
                DeadEnd (2, 1))))



type Item = 
    | Dead = 1
    | Fork = 2
    | Passage = 3
    | Gold = 4

type WhoShouldGetTheGold = Me | NotMe 

type LeftOrRight = Lefty | Righty 


//
// seed with just a random prime
//
let random = System.Random 193

let o = System.Object ()
let start () = random.Next (2, 4)
let game () = random.Next (1, 4)
let gold () = random.Next (2, 4)


let toItem (x:int) = enum<Item> x

let left_OR_right () = 
    match gold () |> toItem with 
    | Item.Passage -> Lefty
    | Item.Fork -> Righty
    | _ -> Lefty // should never be the case !!

let rec gen depth ingame x limit = 

    let step = 1
    if depth > limit then 
        match x with 
        | NotMe -> DeadEnd 0
        | Me -> Gold 100000
    else
        let top =
            if ingame then game () 
            else start () 
            |> toItem
    
        match x with 
        | Me -> 
            let gold = gold () |> toItem

            match top, gold with 
            | Item.Fork, _ -> 
                match left_OR_right () with 
                | Righty -> Fork (0, gen (depth+1) true NotMe limit, gen (depth+2) true Me limit)
                | Lefty -> Fork (0, gen (depth+1) true Me limit, gen (depth+2) true NotMe limit)
            // | Item.Fork, _ -> Fork (0, gen (depth+1) true NotMe, gen (depth+2) true NotMe)
            // | Item.Passage, Item.Passage -> Passage (0, gen (depth+2) true Me)
            | Item.Passage, _ -> Passage (0, gen (depth+2) true Me limit)
            | Item.Dead, _ -> 
                match x with 
                | Me -> Gold 1000000 
                | NotMe -> DeadEnd 0

        | NotMe -> 
            match top with 
            | Item.Fork -> 
                match left_OR_right () with 
                | Righty -> Fork (0, gen (depth+1) true NotMe limit, gen (depth+2) true NotMe limit)
                | Lefty -> Fork (0, gen (depth+1) true NotMe limit, gen (depth+2) true NotMe limit)
            
            | Item.Passage-> Passage (0, gen (depth+2) true NotMe limit)
            | Item.Dead -> DeadEnd 0

let rec height = 
    function 
    | DeadEnd _ -> 1
    | Fork (_, l, r) -> 1 + height l + height r
    | Passage (_, n) -> 1 + height n
    | Gold _ -> 0

let rec hasGold = 
    function 
    | Gold _ -> true 
    | Fork (_, l, r) -> hasGold l || hasGold r
    | Passage (_, n) -> hasGold n
    | DeadEnd _ -> false 

let rec countGold = 
    function 
    | Gold _ -> 1 
    | Fork (_, l, r) -> countGold l + countGold r
    | Passage (_, n) -> countGold n
    | DeadEnd _ -> 0
    
let rec goldFarEnough limit c node = 
    if hasGold >> not $ node then false 
    else 
        match node with 
        | Passage (_, n) -> goldFarEnough (limit-2) c n 
        | Fork (_, l, r) -> goldFarEnough (limit-2) c l || goldFarEnough (limit-2) c r
        | DeadEnd _ -> c >= limit 
        | Gold _ -> c >= limit

type colour = Black | White 

open Labyrinth.Ariadne 

let visit x = 
    let f branch = 
        match branch with 
        | _ -> Black

    mapTopZipper id f x

let visited = 
    function
    | Gold Black 
    | DeadEnd Black 
    | Fork (Black, _, _)
    | Passage (Black, _) -> true 
    | _ -> false 

let get = Option.get 
let bget a = back a |> get 

let rec goBackUntilYouFindUnvisitedThenEnter zipper =
    match List.tryHead (fst zipper) with 
    | Some (TurnLeft _) -> 
        printfn "At left of a fork"
        printfn "Trying right..."
        let right = zipper |> bget |> turnRight |> get 
        match visited $ snd right with 
        | true -> 
            printfn "Right branch visited, going back!"
            goBackUntilYouFindUnvisitedThenEnter (zipper |> bget |> bget)
        | false ->
            printfn "Right branch not visited, going there!" 
            right
    | Some (TurnRight _) -> 
        printfn "At Right of a fork"
        printfn "Trying left..."
        let left = zipper |> bget |> turnLeft |> get 
        match visited $ snd left with 
        | true -> 
            printfn "Left branch visited, going back!"
            goBackUntilYouFindUnvisitedThenEnter (zipper |> bget |> bget)
        | false ->  
            printfn "Left branch not visited, going there!" 
            left
    | Some (KeepStraightOn Black) -> 
        printfn "At a passage, going back!"
        goBackUntilYouFindUnvisitedThenEnter (zipper |> bget)
    // | Some (KeepStraightOn White) -> zipper 
    | None -> zipper

let rec findGold zipper = 
    match zipper with 
    | _, Gold _ -> 
        fst zipper
        |> List.map (function
                    | TurnLeft _ -> "Left"
                    | TurnRight _ -> "Right"
                    | KeepStraightOn _ -> "Straight On")
        |> List.rev  
    | _, Passage (_, r) -> 
        let zipper = (visit zipper) |> keepStraightOn |> get

        printfn "-> Going Straight up"
        findGold zipper
    | _, DeadEnd _ ->
        //
        // [...;Left;DeadEnd],  [...;Right;DeadEnd], [x;Straight; DeadEnd]
        // x -> (x...;Straight) | (x..;Left) | (x..;Right)
        printfn "-> Deadend" 
        let newZipper =  goBackUntilYouFindUnvisitedThenEnter (visit zipper)
        findGold newZipper
    | _, Fork (_,l, r) -> 
        let vl, vr = visited l, visited r
        match vl, vr with 
        | false, _ ->
            printfn "A fork, going left, although right may be un visited"
            let zipper = (visit zipper) |> turnLeft |> get
            findGold zipper
        | _, false -> 
            printfn "A fork, going right, left is visited"
            let zipper = (visit zipper) |> turnRight |> get
            findGold zipper
        | true, true -> 
            // BUG here
            printfn "About to enter a fork, but both left and right have been visited"
            printfn "So no need? Going back anyways"
            
            findGold ((visit zipper) |> bget)

let z () = 
    gen 0 false Me 5
    |> map (fun _ -> White)
    |> create

let z' () =
    let l = z ();
    printfn "%A" l
    System.Console.ReadLine () |> ignore 
    findGold l


module Finder = 

    let visit (_, node)= 
        match node with 
        | Gold _ -> Gold Black
        | DeadEnd _ -> DeadEnd Black
        | Fork (_, l, r) -> Fork (Black, l, r)
        | Passage (_, node) -> Passage (Black, node)

    let goLeft x =  turnLeft x >>= (visit >> Some) 
    let goRight x = turnRight x >>= (visit >> Some)
    let goUp x = keepStraightOn x >>= (visit >> Some)
    let goBack x = back x >>= (visit >> Some)

    let find zipper = 
        match zipper with 
        | _, Gold _ -> 
            fst zipper
            |> List.rev



//  ([], Passage (White,Passage (White,Fork (White,DeadEnd White,Gold White)))) 
// ["Straight On"; "Straight On"; "Right"]
// 

let undecidable = 
    ([], 
        Fork (White, 
            Fork (White, 
                DeadEnd White,
                Passage (White, 
                    DeadEnd White)), 
            Gold White))
//

let ``mod`` x y = x % y
let filter xs =
    let f a s  = a :: (List.filter (fun x -> x mod a <> 0) s)
    List.foldBack f xs []

 