module Game 

open Labyrinth
open Labyrinth.Ariadne // the order of the opens matter, Ariadne does some name shadowing of Labyrinth

// 
// The implementation of the Labyrinth game.
//

let welcome = "Welcome to the Labyrinth game. Press any key to continue"


type Player = Ariadne | Theseus | Other of string 

type Direction = Left | Right | Up | Down 

// 
// colour of a node: Black means visited, White means not visited 
// 
type colour = Black | White 


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

let o = System.Object ()
let begn () = random.Next (2, 4)
let in_game () = random.Next (1, 4)
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
            
            | Item.Passage-> Passage (0, gen (depth+2) true NotMe limit)
            | Item.Dead -> DeadEnd 0

let genMaze () = 
    let a () = gen 0 false Me 10
    a ()
    |> map (fun _ -> White)
    |> create 
    

let beep () = System.Console.Beep ()

let clearConsole () = System.Console.Clear ()

let red () = 
    System.Console.ForegroundColor <- System.ConsoleColor.White

let restore () = 
    System.Console.ResetColor ()

let readKey () = 
    let k = System.Console.ReadKey false
    k.Key

//
// this must return one of then Directions given to it 
// 
let rec getInputKey list =
    let k = readKey ()

    let ret key = 
        if List.contains key list then key 
        else 
            beep ();
            getInputKey list // NOTE: Tail call 


    match k with 
    | System.ConsoleKey.DownArrow | System.ConsoleKey.X -> ret Down
    | System.ConsoleKey.UpArrow | System.ConsoleKey.W -> ret Up
    | System.ConsoleKey.LeftArrow | System.ConsoleKey.A-> ret Left
    | System.ConsoleKey.RightArrow | System.ConsoleKey.D -> ret Right 
    | _ -> beep (); getInputKey list // NOTE: Tail call
    

// TODO: rename this function 
let directions (zipper: Zipper<'a>) = 
    match zipper with 
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

    let key = getInputKey $ directions zipper

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