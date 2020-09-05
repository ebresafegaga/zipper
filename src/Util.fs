[<AutoOpen>]
module Util


module List = 
    let rec zipWith f xs ys =
        match xs, ys with 
        | x :: xs, y :: ys -> f x y :: zipWith f xs ys
        | [], [] -> [] // same length, but done
        | _ -> [] // not the same length     

    let equal l1 l2 =
        List.foldBack (&&) (zipWith (=) l1 l2) true

type OptionBuiler () =
    member x.Return a = Some a
    member x.ReturnFrom a = a
    member x.Bind (option, f) = option |> Option.bind f

let option = OptionBuiler ()

let (>>=) m f = m |> Option.bind f

let ($) = (<|)

let force (Lazy z) = z 

let rec isSubsetFromStart smaller larger = 
    match smaller, larger with 
    | [], _ -> true 
    | _, [] -> failwith "Larger cannot be less than smaller"
    | [s], l :: ls -> s = l 
    | s :: ss, l :: ls -> 
        s = l && isSubsetFromStart ss ls
    