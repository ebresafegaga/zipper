module ListZipper

type List<'a> = Empty | Cons of 'a * List<'a>

type ListZipper<'a> = list<'a> * list<'a>

let goForward ls bs = 
    match ls, bs with 
    | y :: ys, _ -> ys, y :: bs
    | [], _ -> [], bs

let goBack ls bs = 
    match ls, bs with 
    | _, y :: ys -> y :: ls, bs
    | _, [] -> ls, []