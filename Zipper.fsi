module Zipper
type Tree<'a> =
  | Empty
  | Node of 'a * Tree<'a> * Tree<'a>
val freeTree : Tree<char>
val changeWToP : Tree<char> -> Tree<char>
type Direction =
  | L
  | R
type Directions = Direction list
val changeToP : ds:Direction list -> tree:Tree<char> -> Tree<char>
val elemAt : ds:Direction list -> tree:Tree<'a> -> 'a
type Crumb<'a> =
  | LeftCrumb of 'a * Tree<'a>
  | RightCrumb of 'a * Tree<'a>
type BreadCrumbs<'a> = Crumb<'a> list
type Zipper<'a> = Tree<'a> * BreadCrumbs<'a>
val goLeft : tree:Tree<'a> * bs:Crumb<'a> list -> Tree<'a> * Crumb<'a> list
val goRight : tree:Tree<'a> * bs:Crumb<'a> list -> Tree<'a> * Crumb<'a> list
val goUp : tree:Tree<'a> * bs:Crumb<'a> list -> Tree<'a> * Crumb<'a> list
val modify : f:('a -> 'a) -> tree:Tree<'a> -> Tree<'a>
val attact : t:'a -> 'b * bs:'c -> 'a * 'c
val topMost : Tree<'a> * Crumb<'a> list -> Tree<'a>

