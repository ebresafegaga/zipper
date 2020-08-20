module Weaving

//
// From the Functional Pearl
// "Weaving a Web" by Ralf Hinze and Johan Jeuring
//

type Term =
    | Var of string
    | Abs of string * Term
    | App of Term * Term
    | If of Term * Term * Term

//                         -> * <-
// n, if (= n) 0 then 1 else (+ n) (fac (pred n))
let rhs =
    Abs
        ("n",
         If
             (App(App(Var "=", Var "n"), Var "0"),
              Var "1",
              App(App(Var "+", Var "n"), (App(Var "fac", App(Var "pred", Var "n"))))))

type Ctx =
    | Top
    | Abs' of string * Ctx
    | App' of Ctx * Term
    | App'' of Term * Ctx
    | If' of Ctx * Term * Term
    | If'' of Term * Ctx * Term
    | If''' of Term * Term * Ctx

type at = { it: Term; ctx: Ctx }
type Loc = At of at

let down =
    function
    | At { it = Var s; ctx = c } -> At { it = Var s; ctx = c }
    | At { it = Abs (s, t); ctx = c } -> At { it = t; ctx = Abs'(s, c) }
    | At { it = App (f, a); ctx = c } -> At { it = f; ctx = App'(c, a) }
    | At { it = If (p, a, b); ctx = c } -> At { it = p; ctx = If'(c, a, b) }

let up =
    function
    | At { ctx = Top } as top -> top
    | At { it = t; ctx = Abs' (s, c) } -> At { it = Abs(s, t); ctx = c }
    | At { it = t; ctx = App' (c, e) } -> At { it = App(t, e); ctx = c }
    | At { it = t; ctx = App'' (e, c) } -> At { it = App(e, t); ctx = c }
    | At { it = t; ctx = If' (c, e1, e2) } -> At { it = If(t, e1, e2); ctx = c }
    | At { it = t; ctx = If'' (e1, c, e2) } -> At { it = If(e1, t, e2); ctx = c }
    | At { it = t; ctx = If''' (e1, e2, c) } -> At { it = If(e1, e2, t); ctx = c }

let left =
    function
    | At { ctx = Top } as x -> x
    | At { ctx = Abs' _ } as x -> x
    | At { ctx = App' _ } as x -> x
    | At { it = t; ctx = App'' (e, c) } -> At { it = e; ctx = App'(c, t) }
    | At { ctx = If' _ } as x -> x
    | At { it = t; ctx = If'' (p, c, e) } -> At { it = p; ctx = If'(c, t, e) }
    | At { it = t; ctx = If''' (p, e, c) } -> At { it = e; ctx = If''(p, c, t) }

let right =
    function
    | At { ctx = Top } as x -> x
    | At { ctx = Abs' _ } as x -> x
    | At { it = f; ctx = App' (c, e) } -> At { it = e; ctx = App''(f, c) }
    | At { ctx = App'' _ } as x -> x
    | At { it = t; ctx = If' (c, a, b) } -> At { it = a; ctx = If''(t, c, b) }
    | At { it = t; ctx = If'' (p, c, b) } -> At { it = b; ctx = If'''(p, t, c) }
    | At { ctx = If''' _ } as x -> x

#nowarn "40" // Recursive values and weird F# runtime initialization "sanity" check
module Web =

    open System // for this guy: Lazy<'a>

    type Loc<'a> = At of at<'a>

    and at<'a> =
        { it: 'a
          down: Loc<'a> Lazy
          up: Loc<'a> Lazy
          left: Loc<'a> Lazy
          right: Loc<'a> Lazy }

    
    // These functions are too general ...
    let loc0 wv l0 = l0
    let loc1 wv l0 t1 = 
        let rec l1 =
                lazy At { it = t1
                          down = wv l1 t1
                          up = l0
                          left = l1
                          right = l1 }
        l1
    let loc2 wv l0 t1 t2 = 
        let rec l1 = 
                lazy At { it = t1
                          down = wv l1 t1
                          up = l0
                          left = l1
                          right = l2 }
            and l2 = 
                lazy At { it = t2
                          down = wv l2 t2
                          up = l0
                          left = l1
                          right = l2 }
        l1
    let loc3 wv l0 t1 t2 t3 = 
        let rec l1 = 
                lazy At { it = t1
                          down = wv l1 t1
                          up = l0
                          left = l1
                          right = l2 }
            and l2 = 
                lazy At { it = t2
                          down = wv l2 t2
                          up = l0
                          left = l1
                          right = l2 }
            and l3 = 
                lazy At { it = t3 
                          down = wv l3 t3 
                          up = l0 
                          left = l2 
                          right = l3 }
        l1

    let rec weave l0 =
        function
        | Var s -> l0
        | Abs (s, t1) -> loc1 weave l0 t1
            // let rec l1 =
            //     lazy At { it = t1
            //               down = weave l1 t1
            //               up = l0
            //               left = l1
            //               right = l1 }
            // l1
        | App (t1, t2) -> 
            let rec l1 = 
                lazy At { it = t1
                          down = weave l1 t1
                          up = l0
                          left = l1
                          right = l2 }
            and l2 = 
                lazy At { it = t2
                          down = weave l2 t2
                          up = l0
                          left = l1
                          right = l2 }
            l1
        | If (t1, t2, t3) -> 
            let rec l1 = 
                lazy At { it = t1
                          down = weave l1 t1
                          up = l0
                          left = l1
                          right = l2 }
            and l2 = 
                lazy At { it = t2
                          down = weave l2 t2
                          up = l0
                          left = l1
                          right = l2 }
            and l3 = 
                lazy At { it = t3 
                          down = weave l3 t3 
                          up = l0 
                          left = l2 
                          right = l3 }
            l1

    let top t =
        let rec r = 
            lazy At { it = t
                      down = weave r t 
                      up = r 
                      left = r
                      right = r }
        r

module ReadWriteWeb = 
    

    let () = ()