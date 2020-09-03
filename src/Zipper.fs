module Zipper

type Tree<'a> =
    | Empty
    | Node of 'a * Tree<'a> * Tree<'a>

let freeTree =
    Node
        ('P',
         Node
             ('O',
              Node('L', Node('N', Empty, Empty), Node('T', Empty, Empty)),
              Node('Y', Node('S', Empty, Empty), Node('A', Empty, Empty))),
         Node
             ('L',
              Node('W', Node('C', Empty, Empty), Node('R', Empty, Empty)),
              Node('A', Node('A', Empty, Empty), Node('C', Empty, Empty))))

let changeWToP (Node (p, a, (Node (l, (Node (w, c, r)), b)))) =
    (Node (p, a, (Node (l, (Node('P', c, r)), b))))

type Direction =
    | L
    | R

type Directions = Direction list

let rec changeToP (ds: Directions) tree =
    match ds, tree with
    | (L :: rest), Node (x, l, r) -> Node(x, (changeToP rest l), r)
    | (R :: rest), Node (x, l, r) -> Node(x, l, (changeToP rest r))
    | [], Node (x, l, r) -> Node('P', l, r)
    | _, Empty -> failwith "Not yet implemented"

let rec elemAt (ds: Directions) tree =
    match ds, tree with
    | (L :: rest), Node (_, l, _) -> elemAt rest l
    | (R :: rest), Node (_, _, r) -> elemAt rest r
    | [], Node (x, _, _) -> x
    | _, Empty -> failwith "Not yet implemented."


let newTree = changeToP [ R; L ] freeTree
let elem = elemAt [ R; L ] newTree

type Crumb<'a> =
    | LeftCrumb of 'a * Tree<'a>
    | RightCrumb of 'a * Tree<'a>

type BreadCrumbs<'a> = Crumb<'a> list

type Zipper<'a> = Tree<'a> * BreadCrumbs<'a>

(* From weaving a web *)

type Ctx<'a> =
    | Top
    | Node' of 'a * Ctx<'a> * Tree<'a>
    | Node'' of 'a * Tree<'a> * Ctx<'a>

let goLeft (tree, bs) =
    match tree with
    | Node (x, l, r) -> Some (l, LeftCrumb (x, r) :: bs)
    | Empty -> None

let goRight (tree, bs) =
    match tree with
    | Node (x, l, r) -> Some (r, (RightCrumb (x, l)) :: bs)
    | Empty -> None

let goUp (tree, bs) =
    match tree, bs with
    | l, LeftCrumb (x, r) :: rest -> Some(Node(x, l, r), rest)
    | r, RightCrumb (x, l) :: rest -> Some(Node(x, l, r), rest)
    | n, [] -> None

let modify f tree =
    match tree with
    | Node (x, l, r) -> Node (f x, l, r)
    | Empty -> Empty

let attact t (_, bs) = (t, bs)

let rec topMost zipper =
    match zipper with
    | t, [] -> Some t
    | z -> goUp z >>= topMost

let t =
    Some (freeTree, [])
    >>= goLeft
    >>= goRight
    >>= goRight
    >>= goUp
    >>= topMost
