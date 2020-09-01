module Reader


type CustId = CustId of string 
type ProductId = ProductId of string 
type ProductInfo = { ProductName : string }


type ApiClient () = 
    static let mutable data = Map.empty<string, obj>

    member private x.TryCast<'a> key (value:obj) = 
        match value with 
        | :? 'a as a -> Ok a 
        | _ -> 
            let typeName = typeof<'a>.Name 
            Error [sprintf "Cant cast value at %s to %s" key typeName]
    
    member x.Get<'a> (id:obj) = 
        let key = sprintf "%A" id
        printfn "[API] Get %s" key 
        match Map.tryFind key data with 
        | Some o -> x.TryCast<'a> key o
        | None -> Error [sprintf "Key %s not found" key]
    
    member this.Set (id:obj) (value:obj) =
        let key = sprintf "%A" id
        printfn "[API] Set %s" key
        if key = "bad" then
            // for testing failure paths
            Error [sprintf "Bad Key %s " key]
        else
            data <- Map.add key value data
            Ok ()
    
    member x.Open () = 
        printfn "[API Opening]"

    member x.Close () = 
        printfn "[API] Closing"
    

    interface System.IDisposable with 
        member x.Dispose () = 
            printfn "[API] Disposing"

let test () = 
    use api = new ApiClient()
    api.Get "K1" |> printfn "[K1] %A"
    api.Set "K2" "hello" |> ignore
    api.Get<string> "K2" |> printfn "[K2] %A"
    api.Set "K3" "hello" |> ignore
    api.Get<int> "K3" |> printfn "[K3] %A" 

let getPurchaseInfo (CustId _ as id) =
    use api = new ApiClient ()
    api.Open ()

    let productIdsResults = api.Get<ProductId list> id

    let productInfosResult = 
        match productIdsResults with 
        | Ok productIds -> 
            let productInfos = ResizeArray ()
            let mutable anyFailures = false
            for productId in productIds do
                let productInfoResult = api.Get<ProductInfo> productId
                match productInfoResult with
                | Ok productInfo ->
                    productInfos.Add productInfo
                | Error err -> failwith "" // Error err
            Ok productInfos
        | Error e -> Error e 

    api.Close ()

    productInfosResult

let executeApiAction apiAction =
    use api = new ApiClient ()

    api.Open ()

    let result = apiAction api 

    api.Close ()

    result

let getPurchaseIds (custId:CustId) (api:ApiClient) =
    api.Get<ProductId list> custId

let getProductInfo (productId:ProductId) (api:ApiClient) =
    api.Get<ProductInfo> productId

type ApiAction<'a> = ApiAction of (ApiClient -> 'a) 

module ApiAction = 
    let run api (ApiAction action) = 
        let result = action api
        result 
    
    let map f action =
        let newAction api =
            let x = run api action 
            f x 
        ApiAction newAction

    let retn x = 
        let action api = x 
        ApiAction action  
    
    let apply fAction xAction = 
        let newAction api = 
            let f = run api fAction
            let x = run api xAction
            f x
        ApiAction newAction
   
    let bind f action = 
        let newAction api = 
            let a = run api action
            let b = f a
            run api b
        ApiAction newAction

    let execute action = 
        use api = new ApiClient () 
        api.Open ()
        let result = run api action
        api.Close ()
        result 

module Result = 
    let apply fR xR = 
        match fR, xR with 
        | Ok f, Ok x -> Ok (f x)
        | Error e, _ -> Error e
        | _, Error e -> Error e

module ApiActionResult =
    let map f = 
        ApiAction.map (Result.map f)
    let retn x =
        ApiAction.retn (Ok x)

    let apply fActionResult xActionResult =
        let newAction api =
            let fResult = ApiAction.run api fActionResult
            let xResult = ApiAction.run api xActionResult
            Result.apply fResult xResult
        ApiAction newAction
    
    let bind f xActionResult =
        let newAction api =
            let xResult = ApiAction.run api xActionResult
        // create a new action based on what xResult is
            let yAction =
                match xResult with
                | Ok x ->
                    // Success? Run the function
                    f x
                | Error err ->
                    // Failure? wrap the error in an ApiAction
                    (Error err) |> ApiAction.retn
            ApiAction.run api yAction
        ApiAction newAction

let traverse f list = 
    let (<*>) = ApiActionResult.apply
    let retn = ApiActionResult.retn

    let cons head tail = head :: tail

    let initState = retn []
    let folder head tail = retn cons <*> f head <*> tail

    List.foldBack folder list initState

// let getPurchaseInfo =
//     let getProductInfoLifted =
//         getProductInfo
//         |> traverse
//         |> ApiActionResult.bind
//     getPurchaseIds >> getProductInfoLifted
        
let foldl f init l = 
    List.foldBack (fun a s -> fun x -> f x a |> s) l id init

//let fff x = foldl (fun s a -> a :: s) 

let seqA list = 
    List.foldBack (fun a s x -> a x :: s x) list (fun _ -> [])