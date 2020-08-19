module ListZipper

type List<'a> =
    | Empty
    | Cons of 'a * List<'a>

type ListZipper<'a> = list<'a> * list<'a>

type Ctx<'a> =
    | Top
    | Data of 'a list * Ctx<'a> * 'a list

type D<'a> = D of 'a * Ctx<'a>

let f () = List.rev >> List.tail >> List.rev

let left (D (v, ctx)) =
    match ctx with
    | Top -> D (v, ctx)
    | Data (a, c, b) -> D (List.last a, Data (f () a, c, b))

let goForward ls bs =
    match ls, bs with
    | y :: ys, _ -> ys, y :: bs
    | [], _ -> [], bs

let goBack ls bs =
    match ls, bs with
    | _, y :: ys -> y :: ls, bs
    | _, [] -> ls, []
