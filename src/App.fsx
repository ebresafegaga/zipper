

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



type Nat = Zero | Succ of Nat

type System.Int32 with
    member x.Return a = a
    member x.Quote a = a

let quote = 23
let workflow = quote {
    return 1
}


// 
// From the paper : "Do we need Dependent types?"
//

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

// let zipWith n f = n (repeat f)

// let v = zipWith four (fun a b c d -> a + b + c + d) [1;3;5] [] [1;3;5] [2;4;5]


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

let rec qsort' l =
    let folder x xs = 
        let left = [ for i in xs do if i <= x then yield i ]
        let right = [ for i in xs do if i > x then yield i ]
        qsort' left @ [x] @ qsort' right
    List.foldBack folder l []

#load "Labyrinth.fs"
open Labyrinth

#load "Util.fs"
open Util

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

    // 
    // TODO: make the illegal stated unrepresentable
    //
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
            
            | Item.Passage -> Passage (0, gen (depth+2) true NotMe limit)
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

        let me = (zipper |> top)

        match me with 
        | Fork (_, l, r) ->
            let vl, vr = visited l, visited r
            match vl, vr with 
            | false, _ ->
                printfn "This node is also a fork and the left is un visited, going there!" 
                zipper |> turnLeft |> get// goLeft
            | _, false ->
                printfn "This node is also a fork and the right is un visited, going there!" 
                zipper |> turnRight |> get // goRight
            | true, true ->  // now we can go back
                let right = zipper |> bget |> turnRight |> get 
                printfn "Trying right..."
                match visited $ snd right with 
                | true -> 
                    printfn "Right branch visited, going back!"
                    goBackUntilYouFindUnvisitedThenEnter (zipper |> bget |> bget)
                | false ->
                    printfn "Right branch not visited, going there!" 
                    right
        | _ -> 
            let right = zipper |> bget |> turnRight |> get 
            printfn "Trying right..."
            match visited $ snd right with
            | true -> 
                printfn "Right branch visited, going back!"
                goBackUntilYouFindUnvisitedThenEnter (zipper |> bget |> bget)
            | false ->
                printfn "Right branch not visited, going there!" 
                right
    | Some (TurnRight _) -> 
        printfn "At Right of a fork"
        let me = (zipper |> top)

        match me with 
        | Fork (_, l, r) ->
            let vl, vr = visited l, visited r
            match vl, vr with 
            | false, _ -> 
                printfn "This node is also a fork and the left is un visited, going there!" 
                zipper |> turnLeft |> get // goLeft
            | _, false -> 
                printfn "This node is also a fork and the right is un visited, going there!" 
                zipper |> turnRight |> get // goRight
            | true, true ->  // now we can go back
                printfn "Trying left..."
                let left = zipper |> bget |> turnLeft |> get 
                match visited $ snd left with 
                | true -> 
                    printfn "Left branch visited, going back!"
                    goBackUntilYouFindUnvisitedThenEnter (zipper |> bget |> bget)
                | false ->  
                    printfn "Left branch not visited, going there!" 
                    left      
        | _ -> 
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
    | Some (KeepStraightOn White) -> assert false; zipper // This should never be the case 
    | None -> zipper

let rec findGold zipper =
  lazy
    match zipper with
    | _, Gold _ ->
        fst zipper
        |> List.rev
        |> List.map (function
                    | TurnLeft _ -> "Left"
                    | TurnRight _ -> "Right"
                    | KeepStraightOn _ -> "Straight On")
    | _, Passage (_, r) -> 
        let zipper = (visit zipper) |> keepStraightOn |> get
        printfn "-> Going Straight up"
        force $ findGold zipper
    | _, DeadEnd _ ->
        //
        // [...;Left;DeadEnd],  [...;Right;DeadEnd], [x;Straight; DeadEnd]
        // x -> (x...;Straight) | (x..;Left) | (x..;Right)
        printfn "-> Deadend" 
        let newZipper =  goBackUntilYouFindUnvisitedThenEnter (visit zipper)
        force $ findGold newZipper
    | _, Fork (a,l, r) -> 
        let vl, vr = visited l, visited r
        match vl, vr with 
        | false, _ ->
            printfn "A fork, going left, although right may be un visited"
            let zipper = (visit zipper) |> turnLeft |> get
            force $ findGold zipper
        | _, false -> 
            printfn "A fork, going right, left is visited"
            let zipper = (visit zipper) |> turnRight |> get
            force $ findGold zipper
        | true, true -> 
            // BUG here?
            printfn "At a fork: %A, but both left and right have been visited" (Fork(a, l, r))
            printfn "So no need? Going back anyways"
            
            force $ findGold ((visit zipper) |> bget)

let z () = 
    gen 0 false Me 1000
    |> map (fun _ -> White)
    |> create

let z' () =
    let l = z ()
    printfn "%A" l
    System.Console.ReadLine () |> ignore 
    findGold l

type Direction = Left | Right | Up | Down


let threadToDirection = 
    function 
    | TurnLeft _ -> Left
    | TurnRight _ -> Right
    | KeepStraightOn _ -> Up

let hint correct zipper = 
    let wrong = List.map threadToDirection $ fst zipper
    let cl, wl = List.length correct, List.length wrong

    if wl <= cl && isSubsetFromStart wrong correct then 
        correct.[List.length wrong]
    else Down

let undecidable =
    ([],
     Fork (White,
        DeadEnd White,
        Fork (White,
           Fork (White,
                Fork (White,DeadEnd White,DeadEnd White),
                Fork (White,Gold White,DeadEnd White)),
           Passage (White,DeadEnd White))))

let ``mod`` x y = x % y
let filter xs =
    let f a s  = a :: (List.filter (fun x -> x mod a <> 0) s)
    List.foldBack f xs []

let (!@) (Lazy a) = a

let ccc a = !@a + 10 


let foldl f init l = 
    List.foldBack (fun a s -> fun x -> f x a |> s) l id init

//let fff x = foldl (fun s a -> a :: s) 

let seqA list = 
    List.foldBack (fun a s x -> a x :: s x) list (fun _ -> [])

// https://github.com/fsharp/fsharp/issues/867
open System.Collections.Generic

type A = A of Lazy<A>

let f a = 
    let vs = HashSet HashIdentity.Reference
    let rec loop (a: Lazy<A>) = 
        if vs.Add a then 
            match a.Value with 
            | A a -> loop a
    loop a

let rec va = lazy (A va)

// https://github.com/c-cube/gen/blob/0719c2451a12d586d6b06c378d9021e21d31c82f/bench/bench_persistent.ml#L26-L27

module MList = 
    type 'a t = 'a node option ref
      and 'a node = {
        content : 'a;
        mutable prev : 'a node;
        mutable next : 'a node;
      }
    
    type Rec<'a> = 
        { a : unit -> Rec<'a>
          b : 'a } 
    
    and R<'a> = R of Rec<'a>

    let rec v = { a = (fun () -> v); b = 10}
    

  // let rec v = R { a = 10;  b = v}

    let create () = ref None

    let is_empty d =
        match !d with
        | None -> true
        | Some _ -> false

    let push_back d x =
        match !d with
        | None ->
          let rec elt = {
            content = x; prev = elt; next = elt; } in
          d := Some elt
        | Some first ->
          let elt = { content = x; next=first; prev=first.prev; } in
          first.prev.next <- elt;
          first.prev <- elt

// https://stackoverflow.com/questions/24264187/f-error-fs0695-this-recursive-binding-uses-an-invalid-mixture-of-recursive-for
// type Parent = 
//     { ParentName : string 
//       Children : Child list }
// and Child = 
//     { ChildName : string 
//       Parent : Parent }
 
// let createChild parent childName = 
//     { ChildName = childName
//       Parent = parent }

// let createParent parentName childNames = 
//     let rec parent = 
//         { ParentName = parentName
//           Children = children }
//     and children = 
//         childNames
//         |> List.map (createChild parent)
//     parent
 
type Parent = internal { children : Children option }
and internal Children = { first : Child; rest : Children option }
and Child = internal { parent : Parent }

let rec internal listToChildren = function
    | [] -> None
    | c :: cs -> Some { first = c; rest = listToChildren cs }

let rec internal childrenToList = function
    | None -> []
    | Some { first = c; rest = cs } -> c :: childrenToList cs

// 
// Free monad : https://dev.to/shimmer/monads-for-free-in-f-30dl
// 
type Nest<'t> = 
    | Free of Option<Nest<'t>>
    | Pure of 't

let rec bind f = function
    | Free opt -> 
        opt 
        |> Option.map (bind f) 
        |> Free
    | Pure value -> f value

type NestBuilder () = 
    member x.Bind (nest, f) = bind f nest
    member x.Return value = Pure value
    member x.ReturnFrom nest = nest 

let nest = NestBuilder ()

let some value = 
    value 
    |> Pure 
    |> Some
    |> Free

let ex = 
    nest {
        let! a = some "A"
        let! b = some $ a + "B"
        return! some $ b + "C"
    }
 

// https://dev.to/shimmer/why-algebraic-effects-matter-in-f-3m7g

let hypotenuse a b =
    printfn "Side a: %g" a
    printfn "Side b: %g" b
    let c = sqrt $ a*a + b*b
    printfn "Side c: %g" c
    c


type Effect<'result> =
    | Log of string * (unit -> Effect<'result>)
    | Result of 'result


let hypotenuse' a b =
    Log ((sprintf "Side a: %g" a), fun () ->
        Log ((sprintf "Side b: %g" b), fun () ->
            let c = sqrt $ a*a + b*b
            Log ((sprintf "Side c: %g" c), fun () ->
                Result c)))

// Just converting a let binding to a lambda
let hypotenuse'' a b =
    Log ((sprintf "Side a: %g" a), fun () ->
        Log ((sprintf "Side b: %g" b), fun () ->
                (fun c -> 
                    Log ((sprintf "Side c: %g" c), 
                        fun () -> Result c))(sqrt $ a*a + b*b)))

let abbb = (fun () -> Result 10.)()
let x c = 
    Log ((sprintf "Side c: %g" c), fun () -> Result c)

let handle effect = 
    let rec loop k = function   
        | Log (str, cont) -> 
            loop (fun result -> k (str :: result)) (cont ())
        | Result result ->
            result, k []
    loop id effect

let c, log = 
    hypotenuse' 10. 10.
    |> handle

let rec bind2 f = function 
    | Log (str, cont) -> 
        Log (str, fun () -> 
                    cont () |> bind2 f)
    | Result result ->  f result

type EffectBuilder () = 
    member x.Return value = Result value 
    member x.Bind (effect, f) = bind2 f effect

let effect = EffectBuilder ()

let log' str = Log (str, fun () -> Result ())
let logf fmt = Printf.ksprintf log' fmt

let hypo a b =
    effect {
        do! logf "Side a: %g" a
        do! logf "Side b: %g" b
        let c = sqrt $ a*a + b*b
        do! logf "Side c: %g" c
        return c
    }
