namespace N2O
open System
open Scan
open Err

[<AutoOpen>]
module Twain =
    module Result =
        let lift (f:'a -> 'b) (a:'a) (err:'b -> string option): Result<'b,string> = 
            let (|Err|_|) b = err b            
            let (<!>) = Result.bind
            let f' a = try match f a with
                            | Err s -> Error s
                            | v -> Ok v
                        with 
                            | e -> Error e.Message
            f' <!> Ok a

    type ITwain with
        member self.init():             Result<int,string> =
            Result.lift self.Init () (function |_ -> None) |> Result.map (fun _ -> 0)            

        member self.manager(_:int):     Result<int,string> = 
            Result.lift self.OpenDSM () (function | TwCC s -> Some s) |> Result.map (fun _ -> 0)

        member self.ds(_:int):          Result<string list,string> = 
            Result.lift self.GetDataSources () (function | [] -> Some "На цій ситемі відсутні TWAIN драйвери.";| _ -> None)

        member self.id(_:string list):  Result<string,string> = 
            Result.lift self.DefaultIdentity () (function | "" -> Some "Пристрій за замовчуванням відсутній.";| _ -> None)

        member self.scanner(s: string): Result<string,string> =
            Result.lift self.OpenScanner s (function | "" -> Some "Неможливо відкрити сканер.";|_-> None)

        member self.nativeTransfer(_:_): Result<int,string> =
            Result.lift self.NativeTransfer () (function | true -> Some "Помилка налаштування (native transfer).";|_-> None) |> Result.map (fun _ -> 0)

        member self.autoFeed(_): Result<int,string> =
            Result.lift self.AutoFeed () (function | true -> Some "Помилка налаштування (auto feed).";|_ -> None) |> Result.map (fun _ -> 0)
            
        member self.disableProgressUi(prev): Result<int, string> =
            Result.lift self.ProgressDriverUi false (function | true -> Some "Помилка налаштування (progress ui)";|_-> None) |> Result.map(fun _ -> 0)

        member self.enableDs(_): Result<int,string> =
            // lift for twcc codes, but we don't know codes yet
            Result.lift self.EnableDS () (function | 0 -> None;|_-> Some "Неможливо почати сканування (enable DS without ui).")

        member self.start(scanloop): Result<int,string> =
            Result.lift self.Start 4 (function | 0 -> None;|_->Some "Помилка налашування фінального стану так колбеку сканування.") 
                |> Result.map (fun x -> self.ScanCallback <- new Callback(scanloop); x)

        member self.currentCap(cap:string)(current:string -> string): Result<string,string> =
            try Ok (current(cap))
            with e ->
              let code = try e.Message |> int |> Some with _ -> None
              match code with 
              | Some(TwCC s) -> Error s
              | _            -> Error e.Message

        member self.get(cap:Cap): Result<string,string> =
            let cp,co,ty,vs = cap
            let testcap = sprintf "%s,%s,%s,%s" (TwCap.MkString(cp)) (TwON.MkString(co)) (TwType.MkString(ty)) (vs.ToUpperInvariant())
            printfn "test CAP: %s" testcap
            self.currentCap(testcap)(self.ControlCapGetCurrent)

        member self.set(cap:Cap): Result<string,string> =
            let cp,co,ty,vs = cap
            
            let testcap = sprintf "%s,%s,%s,%s" (TwCap.MkString(cp)) (TwON.MkString(co)) (TwType.MkString(ty)) (vs.ToUpperInvariant())

            printfn "test CAP: %s" testcap

            let (|Test|_|) (s:string) =
              if String.Compare(s, testcap, StringComparison.InvariantCultureIgnoreCase) = 0 then Some() else None
            
            let (getcurrent,set__) =
              match cp with
              | TwCap.PixelType |  TwCap.XrefMech -> (self.ImageCapGetCurrent, self.ControlCapSet)
              | _               -> (self.ControlCapGetCurrent, self.ImageCapSet)

            let set_(cap: string): Result<string,string> =
              try
                match cap with 
                | Test _ -> Ok "already set"
                | _      -> Ok (set__(cap))
              with e -> 
                let code = try e.Message |> int |> Some with _ -> None
                match code with 
                | Some(TwCC s) -> Error s
                | _            -> Error e.Message

            self.currentCap(testcap)(getcurrent)
              |> Result.bind set_


