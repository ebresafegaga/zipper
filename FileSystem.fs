module FileSystem


type Name = string 
type Data = string 
type FSItem = File of Name * Data | Folder of Name * FSItem list

let myDisk = 
    Folder ("root", 
        [ File ("fff.wap", "some data")
        ; File ("coolest_fsprogram.fs", "[<EntryPoint>] let main _= 0")
        ; Folder ("pics",
                    [ File ("dancing.jpg", "someone dancing!")
                    ]
                )
        ; File ("random.hs", "main = undefined")
        ; Folder ("source_code", 
                    [ File ("cool.hs", "main = print $ fix error")
                    ]
                )
        ]
    )

type FSCrumb = FSCrumb of Name * FSItem list * FSItem list
type FSZipper = FSItem * FSCrumb list

let fsUp (zipper : FSZipper) : FSZipper = 
    match zipper with 
    | item, FSCrumb (name, before, after) :: rest -> 
        Folder (name, before @ [item] @ after), rest
    | _, [] -> zipper // bad?

let rec break' f l = 
    let (++) (a, b) (c, d) = (a::c), (b@d)
    match l with 
    | x :: xs when f x -> [], l 
    | x :: xs -> (x,[]) ++ break' f xs
    | [] -> ([], [])

// let result = [1;2;3;4;] |> break' ((<) 4)

let nameIs name = 
    function 
    | File (fname, _) -> fname = name
    | Folder (fname, _) -> fname = name

let fsTo name (zipper: FSZipper) : FSZipper = 
    match zipper with
    | Folder (fname, items), bs ->    
        let start, items' = break' (nameIs name) items
        let former = FSCrumb (fname, start, List.tail items')
        List.head items', former :: bs
    | File _, _ -> failwith "Cannot focus on a file"

let fsRename name =
    function
    | File (_, data), bs -> File (name, data), bs
    | Folder (_, items), bs -> Folder (name, items), bs

let fsNewFile file = 
    function 
    | Folder (name, items), bs -> Folder (name, file :: items), bs
    | File _, _ -> failwith "Cannot add a file to a file, duh!"

let newFocus = 
    (myDisk, [])
    |> fsTo "pics"
    |> fsUp