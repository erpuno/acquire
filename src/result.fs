namespace N2O

[<AutoOpen>]
module Result =
    let apply fr xr =
        match fr, xr with
        | Ok f, Ok x -> Ok (f x)
        | Error e, _ -> Error e
        | _, Error e -> Error e        

    let lift (f:'a -> 'b) (a:'a) (err:'b -> string option): Result<'b,string> = 
        let (|Err|_|) b = err b            
        let (<!>) = Result.bind
        let f' a = try match f a with
                        | Err s -> Error s
                        | v -> Ok v
                    with 
                        | e -> Error e.Message
        f' <!> Ok a

    let lift2 (f: 'a -> 'b -> 'c) (a:'a) (b:'b) (err: 'c -> string option): Result<'c, string> =
        let (<*>) = apply
        let (|Err|_|) b = err b 
        try 
            match Ok f <*> Ok a <*> Ok b with
            | Ok (Err s) -> Error s
            | Ok v       -> Ok v
            | Error e    -> Error e
        with
            | e -> Error e.Message
