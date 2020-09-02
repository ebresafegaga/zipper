[<AutoOpen>]
module Util

type OptionBuiler () =
    member x.Return a = Some a
    member x.ReturnFrom a = a
    member x.Bind (option, f) = option |> Option.bind f

let option = OptionBuiler ()

let (>>=) m f = m |> Option.bind f

let ($) = (<|)