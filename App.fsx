

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