namespace N2O
open Scan
open Err

[<AutoOpen>]
module Twain =
    // Result.traverseR
    module Result =
        let lift (f: unit -> int): Result<int,string> = 
            try
                match f () with
                | 0 -> Ok 0
                | TwCC s -> Error s
            with
                | e -> Error e.Message
        
        let lift_ (f: _ -> 'b) (error: 'b -> string option) : Result<'b,string> =
            let (|Err|_|) b = error b
            try 
                match f() with
                | Err s -> Error s
                | e -> Ok e
            with
                | e -> Error e.Message
        
        let liftS (f: string -> string) a: Result<string,string> =
            try
              match f(a) with
              | "" -> Error "Неможливо відкрити сканер."
              | r1 -> Ok r1
            with
              | e -> Error e.Message

    type ITwain with
        member self.init():             Result<int,string> = try self.Init(); Ok 0 with e -> Error e.Message

        member self.manager(_:int):     Result<int,string> = Result.lift self.OpenDSM        

        member self.ds(_:int):          Result<string list,string> = 
            Result.lift_ self.GetDataSources (function | [] -> Some "На цій ситемі відсутні TWAIN драйвери.";| _ -> None)

        member self.id(_:string list):  Result<string,string> = 
            Result.lift_ self.DefaultIdentity (function | "" -> Some "Пристрій за замовчуванням відсутній.";| _ -> None)

        member self.scanner(s: string): Result<string,string> = 
            Result.liftS self.OpenScanner s

        member self.nativeTransfer(_:_): Result<int,string> =
            Result.lift_ self.NativeTransfer (function | true -> Some "Помилка налаштування (native transfer).";|_-> None) |> Result.map (fun _ -> 0)

        member self.autoFeed(_): Result<int,string> =
            Result.lift_ self.AutoFeed (function | true -> Some "Помилка налаштування (auto feed).";|_ -> None) |> Result.map (fun _ -> 0)
            
        member self.disableProgressUi(_): Result<int, string> =
            try
              match self.ProgressDriverUi(false) with
              | false -> Ok 0
              | true -> Error "Помилка налаштування (progress ui)."
            with
              | e -> Error e.Message

        member self.enableDs(_): Result<int,string> =
            // lift for twcc codes, but we don't know codes yet
            Result.lift_ self.EnableDS (function | 0 -> None;|_-> Some "Неможливо почати сканування (enable DS without ui).")

        member self.start(scanloop): Result<int,string> =
            try
                match self.Start(4) with
                | 0 ->
                    self.ScanCallback <- new Callback(scanloop)
                    Ok 0
                | _ -> Error "Помилка налашування фінального стану так колбеку сканування."
            with
            | e -> Error e.Message

