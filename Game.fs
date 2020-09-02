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

let gold = 
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

//
// TODO: Generates a random maze when called
//  
let genMaze () = 
    lab 
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
let rec getInputKey (list: Direction list) : Direction =
    let k = readKey ()

    let ret key = 
        if List.contains key list then key 
        else 
            beep ();
            getInputKey list // NOTE: Tail call 

    //
    // TODO: add other keys 
    // 
    match k with 
    | System.ConsoleKey.DownArrow -> ret Down
    | System.ConsoleKey.UpArrow -> ret Up
    | System.ConsoleKey.LeftArrow -> ret Left
    | System.ConsoleKey.RightArrow -> ret Right 
    | _ -> beep (); getInputKey list // NOTE: Tail call
    

let directions (thread, node) = 
    match thread, node with 
    | _, DeadEnd _ -> [Down]
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


let printInfo ((thread, node): Zipper<_>) =
    printfn "Previous Locations: %A" thread

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
    | Gold _ -> printfn "%s" gold; restore ()

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
            // Ideally, we shouldn't get here
            failwithf "can't go %A on %A" key zipper 
        | Some x -> x |> visitThread

    let z = check ()
    if won z then 
        clearConsole ()
        printWonInfo z
        restart ()
    else 
        clearConsole ()
        game z