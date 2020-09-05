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
    | At { it = t; ctx = App'' (e, c) } -> At { it = e; ctx = App' (c, t) }
    | At { ctx = If' _ } as x -> x
    | At { it = t; ctx = If'' (p, c, e) } -> At { it = p; ctx = If' (c, t, e) }
    | At { it = t; ctx = If''' (p, e, c) } -> At { it = e; ctx = If'' (p, c, t) }

let right =
    function
    | At { ctx = Top } as x -> x
    | At { ctx = Abs' _ } as x -> x
    | At { it = f; ctx = App' (c, e) } -> At { it = e; ctx = App'' (f, c) }
    | At { ctx = App'' _ } as x -> x
    | At { it = t; ctx = If' (c, a, b) } -> At { it = a; ctx = If'' (t, c, b) }
    | At { it = t; ctx = If'' (p, c, b) } -> At { it = b; ctx = If''' (p, t, c) }
    | At { ctx = If''' _ } as x -> x

// #nowarn "40" // Recursive values and weird F# runtime initialization "sanity" check
module Web =

    type Loc<'a> = At of at<'a>

    and at<'a> =
        { it: 'a
          down: Loc<'a> Lazy
          up: Loc<'a> Lazy
          left: Loc<'a> Lazy
          right: Loc<'a> Lazy }
    
    type Loc2<'a> = At2 of at2<'a>

    and at2<'a> =
        { it2: 'a
          down2: Loc2<'a> Lazy
          up2: Loc2<'a> 
          left2: Loc2<'a> Lazy
          right2: Loc2<'a> Lazy }

    let unAt (At a) = a


    // These functions are too general ...
    let loc0 wv l0 = l0

    let loc1 wv l0 t1 =
        let rec l1 =
            lazy
                (At
                    { it = t1
                      down = wv l1 t1
                      up = l0
                      left = l1
                      right = l1 })

        l1

    let loc2 wv l0 t1 t2 =
        let rec l1 =
            lazy
                (At
                    { it = t1
                      down = wv l1 t1
                      up = l0
                      left = l1
                      right = l2 })

        and l2 =
            lazy
                (At
                    { it = t2
                      down = wv l2 t2
                      up = l0
                      left = l1
                      right = l2 })

        l1

    let loc3 wv l0 t1 t2 t3 =
        let rec l1 =
            lazy
                (At
                    { it = t1
                      down = wv l1 t1
                      up = l0
                      left = l1
                      right = l2 })

        and l2 =
            lazy
                (At
                    { it = t2
                      down = wv l2 t2
                      up = l0
                      left = l1
                      right = l2 })

        and l3 =
            lazy
                (At
                    { it = t3
                      down = wv l3 t3
                      up = l0
                      left = l2
                      right = l3 })

        l1

    let rec weave l0 =
        function
        | Var _ -> l0
        | Abs (_, t1) ->
            let rec l1 =
                At
                    { it = t1
                      down = weave (lazy l1) t1
                      up = l0
                      left = lazy l1
                      right = lazy l1 }

            lazy l1
        | App (t1, t2) ->
            let rec l1 =
                At
                    { it = t1
                      down = weave (lazy l1) t1
                      up = l0
                      left = lazy l1
                      right = lazy l2 }

            and l2 =
                At
                    { it = t2
                      down = weave (lazy l2) t2
                      up = l0
                      left = lazy l1
                      right = lazy l2 }

            lazy l1
        | If (t1, t2, t3) ->
            let rec l1 =
                At
                    { it = t1
                      down = weave (lazy l1) t1
                      up = l0
                      left = lazy l1
                      right = lazy l2 }

            and l2 =
                At
                    { it = t2
                      down = weave (lazy l2) t2
                      up = l0
                      left = lazy l1
                      right = lazy l3 }

            and l3 =
                At
                    { it = t3
                      down = weave (lazy l3) t3
                      up = l0
                      left = lazy l2
                      right = lazy l3 }

            lazy l1
    
    let rec weave2 l0 =
        function
        | Var _ -> l0
        | Abs (_, t1) ->
            let rec l1 =
                At2
                    { it2 = t1
                      down2 = lazy weave2 l1 t1
                      up2 = l0
                      left2 = lazy l1
                      right2 = lazy l1 }
            l1
        | App (t1, t2) ->
            let rec l1 =
                At2
                    { it2 = t1
                      down2 = lazy weave2 l1 t1
                      up2 = l0
                      left2 = lazy l1
                      right2 = lazy l2 }

            and l2 =
                At2
                    { it2 = t2
                      down2 = lazy weave2 l2 t2
                      up2 = l0
                      left2 = lazy l1
                      right2 = lazy l2 }

            l1
        | If (t1, t2, t3) ->
            let rec l1 =
                At2
                    { it2 = t1
                      down2 = lazy weave2 l1 t1
                      up2 = l0
                      left2 = lazy l1
                      right2 = lazy l2 }

            and l2 =
                At2
                    { it2 = t2
                      down2 = lazy weave2 l2 t2
                      up2 = l0
                      left2 = lazy l1
                      right2 = lazy l3 }

            and l3 =
                At2
                    { it2 = t3
                      down2 = lazy weave2 l3 t3
                      up2 = l0
                      left2 = lazy l2
                      right2 = lazy l3 }

            l1

    let top t =
        let rec r =
            At
                { it = t
                  down = weave (lazy r) t
                  up = lazy r
                  left = lazy r
                  right = lazy r }
        lazy r
    
    // let top2 t =
    //     let rec r =
    //         At2
    //             { it2 = t
    //               down2 = lazy weave2 r t
    //               up2 = r
    //               left2 = lazy r
    //               right2 = lazy r }
    //     r

    let down (Lazy (At t)) = t.down
    let right (Lazy (At t)) = t.right
    let left (Lazy (At t)) = t.left

module ReadWriteWeb =

    type Loc<'a> = At of at<'a>

    and at<'a> =
        { it: 'a
          fdown: 'a -> Loc<'a>
          fup: 'a -> Loc<'a>
          fleft: 'a -> Loc<'a>
          fright: 'a -> Loc<'a> }


    let down (At l) = l.fdown l.it
    let up (At l) = l.fup l.it
    let left (At l) = l.fleft l.it
    let right (At l) = l.fright l.it

    let loc0 wv fl0' = fl0'

    let loc1 wv fl0' =
        let rec fl1 t1 =
            At
                { it = t1
                  fdown = wv (upd fl1)
                  fup = (upd fl0')
                  fleft = (upd fl1)
                  fright = (upd fl1) }

        and upd fl t1' = fl t1'
        fl1

    let loc2 wv fl0' =
        let rec fl1 t1 t2 =
            At
                { it = t1
                  fdown = wv (upd fl1 t2)
                  fup = (upd fl0' t2)
                  fleft = (upd fl1 t2)
                  fright = (upd fl2 t2) }

        and upd fl t2 t1' = fl t1' t2

        and fl2 t1 t2 =
            At
                { it = t2
                  fdown = wv (upd' fl2 t1)
                  fup = (upd' fl0' t1)
                  fleft = (upd' fl1 t1)
                  fright = (upd' fl2 t1) }

        and upd' fl t1 t2 = fl t1 t2

        fl1
    
    let loc3 wv fl0' =
        let rec fl1 t1 t2 t3 =
            At
                { it = t1
                  fdown = wv (upd fl1 t2 t3)
                  fup = (upd fl0' t2 t3)
                  fleft = (upd fl1 t2 t3)
                  fright = (upd fl2 t2 t3) }

        and upd fl t2 t3 t1' = fl t1' t2 t3

        and fl2 t1 t2 t3 =
            At
                { it = t2
                  fdown = wv (upd' fl2 t1 t3)
                  fup = (upd' fl0' t1 t3)
                  fleft = (upd' fl1 t1 t3)
                  fright = (upd' fl3 t1 t3) }

        and upd' fl t1 t3 t2 = fl t1 t2 t3

        and fl3 t1 t2 t3 =
            At
                { it = t3
                  fdown = wv (upd'' fl3 t1 t2)
                  fup = (upd'' fl0' t1 t2)
                  fleft = (upd'' fl2 t1 t2)
                  fright = (upd'' fl3 t1 t2) }

        and upd'' fl t1 t2 t3 = fl t1 t2 t3

        fl1

    let rec weave fl0 =
        function
        | Var _ as v -> loc0 weave (fl0 v)
        | Abs (s, t1) -> loc1 weave (fun t1' -> fl0 (Abs (s, t1'))) t1
        | App (t1, t2) -> loc2 weave (fun t1' t2' -> fl0 (App (t1', t2'))) t1 t2
        | If (t1, t2, t3) -> loc3 weave (fun t1' t2' t3' -> fl0 (If (t1', t2', t3'))) t1 t2 t3

    let top =
        let rec fr t =
            At
                { it = t
                  fdown = weave fr
                  fup = fr
                  fleft = fr
                  fright = fr }

        fr

    module Interface = 
        type Weaver<'a> = W of (('a -> Loc<'a>) -> Loc<'a>)
        let unW (W f) = f
        
        let call wv fl0 t = unW (wv t) fl0
        
        let con0 wv k = W (fun fl0 -> loc0 (call wv) (fl0 k))
        let con1 wv k t1 = 
            W (fun fl0 -> loc1 (call wv) (k >> fl0) t1)
        let con2 wv k t1 t2 =
            W (fun fl0 -> loc2 (call wv) (fun t1 t2 -> fl0 (k t1 t2)) t1 t2)
        let con3 wv k t1 t2 t3 =
            W (fun fl0 -> loc3 (call wv) (fun t1 t2 t3 -> fl0 (k t1 t2 t3)) t1 t2 t3)

        let explore wv =
            let rec fr t = 
                At { it = t 
                     fdown = call wv fr 
                     fup = fr 
                     fleft = fr
                     fright = fr }
            fr        

        // let rec apply f xs = 
        //     match xs with 
        //     | [x] -> f x
        //     | x :: xs -> apply (f x) xs
        //     | [] -> failwith "Empty list"
        
        // the direction we're moving 
        type Direction = L | R
        let loc wv fl0' = 
            let rec f ts p =
                let length = List.length ts
                let check p = // Should we move with p or p+1 or p-1?
                    function
                    | L -> if p-1 < 0 then p else p-1
                    | R -> if p+1 >= length then p else p+1
                let chg ls pos value = 
                    ls |> List.mapi (fun i x -> if i = pos then value else x)
                let upd fl ts pos a = 
                    let nl = chg ts pos a
                    fl nl pos

                At
                    { it = ts.[p]
                      fdown =  wv (upd f ts p)
                      fup = fun l -> let nl = chg ts p l in fl0' nl
                      fleft = wv (upd f ts (check p L))
                      fright = wv (upd f ts (check p R)) }

            f

        // Excercise 1
        // ('a -> Weaver<'a>) -> ('a list -> 'a) -> ('a list -> Weaver<'a>)
        let con wv kf vs =
            W (fun fl0 -> loc (call wv) (kf >> fl0) vs 0)