module Game 

open Labyrinth
open Labyrinth.Ariadne // the order of the opens matter, Ariadne does some name shadowing of Labyrinth

// 
// The implementation of the Labyrinth game.
//

let welcome = "Welcome to the Labyrinth game. Press any key to continue"


type Player = Ariadne | Theseus | Other of string 

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


// 
// colour of a node: Black means visited, White means not visited 
// 
type Colour = Black | White


let visitThread = 
    let f branch = 
        match branch with 
        | KeepStraightOn _ -> KeepStraightOn Black
        | TurnLeft (_, node) -> TurnLeft (Black, node) 
        | TurnRight (_, node) -> TurnRight (Black, node)

    mapZipper f id


let visited = function
    | Gold Black 
    | DeadEnd Black 
    | Fork (Black, _, _)
    | Passage (Black, _) -> true 
    | _ -> false // TODO: Make this unabriged to handle cases all explicitly

let cross = 
    """
        Left       Right
          \        /
           \      /
            \    /
             \  /
              \/ 
            A FORK           
    """

let straight = 
    """
        0
        |
        |
        |  
        0
    A PASSAGE
    """

let drawgold = 
    """
                 /\
           .--._/  \_.--.
            `)        (`
         _.-'          '-._
        '-.              .-'
           `)          ('
           /.-"-.  .-"-.\
           `     \/ 
      CONGRATS YOU FOUND THE GOLD!
    """

let gold2 = 
    """
           \  :  /
        `. __/ \__ .'
        _ _\     /_ _
           /_   _\
         .'  \ /  `.
           /  :  \    
              ' 
    """

let deadend = 
    """
        _________________
       ||               ||
       ||    DEAD END   ||
       ||_______________||
    """

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

let begn () = random.Next (2, 4)
let in_game () = random.Next (1, 4)
let gold () = random.Next (2, 4)


let toItem (x:int) = enum<Item> x

let left_OR_right () = 
    match gold () |> toItem with 
    | Item.Passage -> Lefty
    | Item.Fork -> Righty
    | _ -> Lefty // should never be the case !!

//
// Generate a random tree of arbitrary depth.
//
let rec gen depth ingame x limit = 

    let step = 1
    if depth > limit then 
        match x with 
        | NotMe -> DeadEnd 0
        | Me -> Gold 100000
    else
        let top =
            if ingame then in_game () 
            else begn () 
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

let genMaze () = 
    let a () = gen 0 false Me 10
    a ()
    |> map (fun _ -> White)
    |> create

let visit x = 
    let f branch = 
        match branch with 
        | _ -> Black

    mapTopZipper id f x

let get = Option.get 
let bget a = back a |> get 

let rec goBackUntilYouFindUnvisitedThenEnter zipper =
    match List.tryHead (fst zipper) with 
    | Some (TurnLeft _) ->
        let me = (zipper |> top)

        match me with 
        | Fork (_, l, r) ->
            let vl, vr = visited l, visited r
            match vl, vr with 
            | false, _ ->
                zipper |> turnLeft |> get// goLeft
            | _, false ->
                zipper |> turnRight |> get // goRight
            | true, true ->  // now we can go back
                let right = zipper |> bget |> turnRight |> get 
                match visited $ snd right with 
                | true -> 
                    goBackUntilYouFindUnvisitedThenEnter (zipper |> bget |> bget)
                | false ->
                    right
        | _ -> 
            let right = zipper |> bget |> turnRight |> get 
            match visited $ snd right with
            | true -> 
                goBackUntilYouFindUnvisitedThenEnter (zipper |> bget |> bget)
            | false ->
                right
    | Some (TurnRight _) -> 
        let me = (zipper |> top)

        match me with 
        | Fork (_, l, r) ->
            let vl, vr = visited l, visited r
            match vl, vr with 
            | false, _ -> 
                zipper |> turnLeft |> get // goLeft
            | _, false -> 
                zipper |> turnRight |> get // goRight
            | true, true ->  // now we can go back
                let left = zipper |> bget |> turnLeft |> get 
                match visited $ snd left with 
                | true -> 
                    goBackUntilYouFindUnvisitedThenEnter (zipper |> bget |> bget)
                | false ->  
                    left      
        | _ -> 
            let left = zipper |> bget |> turnLeft |> get 
            match visited $ snd left with 
            | true -> 
                goBackUntilYouFindUnvisitedThenEnter (zipper |> bget |> bget)
            | false ->
                left
    | Some (KeepStraightOn Black) ->
        goBackUntilYouFindUnvisitedThenEnter (zipper |> bget)
    | Some (KeepStraightOn White) -> assert false; zipper // This should never be the case 
    | None -> zipper

let rec findGold zipper =
 lazy
    match zipper with
    | _, Gold _ ->
            fst zipper
            |> List.rev
            |> List.map threadToDirection
    | _, Passage (_, r) -> 
        let zipper = (visit zipper) |> keepStraightOn |> get
        force $ findGold zipper
    | _, DeadEnd _ ->
        let newZipper =  goBackUntilYouFindUnvisitedThenEnter (visit zipper)
        force $ findGold newZipper
    | _, Fork (_,l, r) -> 
        let vl, vr = visited l, visited r
        match vl, vr with 
        | false, _ ->
            let zipper = (visit zipper) |> turnLeft |> get
            force $ findGold zipper
        | _, false -> 
            let zipper = (visit zipper) |> turnRight |> get
            force $ findGold zipper
        | true, true -> 
            force $ findGold ((visit zipper) |> bget)

let beep () = System.Console.Beep ()

let clearConsole () = System.Console.Clear ()

let red () = 
    System.Console.ForegroundColor <- System.ConsoleColor.White

let restore () = 
    System.Console.ResetColor ()

let readKey () = 
    let k = System.Console.ReadKey false
    k.Key

// TODO: rename this function 
let directions (zipper: Zipper<'a>) = 
    match zipper with 
    | [], DeadEnd _ -> [] // Ideally, the game shouldn't start with a dead end
    | _, DeadEnd _ -> [Down]
    | [], Passage _ -> [Up] 
    | _, Passage _ -> [Up; Down] 
    | [], Fork _ -> [Left; Right]
    | _, Fork _ -> [Left; Right; Down]
    | _, Gold _ -> assert false; [] // Ideally, we should have won the game before we get here

let won (_, node) = 
    match node with 
    | Gold _ -> true 
    | DeadEnd _  | Passage _  | Fork _ -> false

let goThere direction zipper =
    match direction with 
    | Up -> zipper |> keepStraightOn
    | Down -> zipper |> back 
    | Left -> zipper |> turnLeft
    | Right -> zipper |> turnRight

let printStateInfo x = ()

let printInfo ((thread, node): Zipper<_>) =

    let t = 
        thread
        |> List.rev 
        |> List.map (function 
                     | TurnLeft _ -> "Left"
                     | TurnRight _ -> "Right"
                     | KeepStraightOn _ -> "Straight")

    printfn "Your position from the start: %A" t

    printStateInfo thread

    if visited node then 
        red ()

    match node with 
    | DeadEnd _ -> 
        printfn "%s" deadend
        printfn "You can only go back!"
        restore ()
    | Passage _ -> 
        printfn "%s" straight
        printfn "You can go back or forward!"
        restore ()
    | Fork _ -> 
        printfn "%s" cross
        printfn "You can go left, right or back!"
        restore ()
    | Gold _ -> printfn "%s" drawgold; restore ()

let printWonInfo x = printInfo x

let rec showHint correct zipper =
    clearConsole ()
    printfn "           HINT     " 
    printfn "%s" gold2
    printfn "I think you should go %A. Press enter to continue" $ hint correct zipper
    match readKey () with 
    | System.ConsoleKey.Enter -> clearConsole()
    | _ -> beep (); showHint correct zipper

let rec checkHint (correct: Lazy<_>) zipper =
    match readKey() with 
    | System.ConsoleKey.H -> 
        showHint (correct.Force ()) zipper
        printInfo zipper
        getInputKey (correct, zipper) (directions zipper)
    | _ ->  getInputKey (correct, zipper)  (directions zipper)

//
// this must return one of then Directions given to it 
// 
and getInputKey (c, z) list =
    let k = readKey ()

    let ret key = 
        if List.contains key list then key 
        else 
            beep ()
            getInputKey (c, z) list // NOTE: Tail call 


    match k with 
    | System.ConsoleKey.DownArrow | System.ConsoleKey.X -> ret Down
    | System.ConsoleKey.UpArrow | System.ConsoleKey.W -> ret Up
    | System.ConsoleKey.LeftArrow | System.ConsoleKey.A-> ret Left
    | System.ConsoleKey.RightArrow | System.ConsoleKey.D -> ret Right 
    | System.ConsoleKey.H -> checkHint c z
    | _ -> beep (); getInputKey (c, z) list

let rec start () =
    clearConsole ()
    printfn "%s" welcome
    printfn "Press Enter to start"
    match readKey () with 
    | System.ConsoleKey.Enter -> 
        game (genMaze ())
    | System.ConsoleKey.Q -> ()
    | _ -> beep (); restart () // NOTE: Tail call

and restart () = 
    // clearConsole ()
    printfn "%s" welcome
    printfn "Press enter to play or Q to quit"
    match readKey () with 
    | System.ConsoleKey.Enter -> 
        clearConsole ()
        game (genMaze ())
    | System.ConsoleKey.Q -> ()
    | _ -> clearConsole (); beep (); restart () // NOTE: Tail call


and game (zipper: Zipper<_>) = 
    printInfo zipper

    let correct = findGold zipper

    let key = checkHint correct zipper

    let result = goThere key zipper

    let check () = 
        match result with
        | None ->
            // Ideally, we shouldn't get here.
            // But if we get here, just know there's a bug in the "directions" function.
            failwithf "can't go %A on %A" key zipper 
        | Some x -> visitThread x

    let z = check ()
    if won z then 
        clearConsole ()
        printWonInfo z
        restart ()
    else 
        clearConsole ()
        game z