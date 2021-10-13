namespace N2O
open System
open Scan
open Err
open Result

[<AutoOpen>]
module Twain =
    let scap (cap:Cap) = 
        let cp,co,ty,vs = cap
        sprintf "%s,%s,%s,%s" (TwCap.MkString(cp)) (TwON.MkString(co)) (TwType.MkString(ty)) (vs.ToUpperInvariant())

    type ITwain with
        member self.init():             Result<int,string> =
            Result.lift self.Init () (function |_ -> None) |> Result.map (fun _ -> 0)            

        member self.openDSM(_:int):     Result<int,string> = 
            Result.lift self.OpenDSM () (function |0 -> None| TwCC s -> Some $"Неможливо відкрити джерело даних {s}") |> Result.map (fun _ -> 0)
        
        member self.closeDSM(l: string list): Result<string list,string> = 
            Result.lift self.CloseDSM () (function |0 -> None| TwCC s -> Some $"Неможливо закрити {s}" ) |> Result.map (fun _ -> l)

        member self.ds(_:int):          Result<string list,string> = 
            Result.lift self.GetDataSources () (function | [] -> Some "На цій ситемі відсутні TWAIN драйвери.";| _ -> None)

        member self.id(_:string list):  Result<string,string> = 
            Result.lift self.DefaultIdentity () (function | "" -> Some "Пристрій за замовчуванням відсутній.";| _ -> None)

        member self.scanner(s: string): Result<string,string> =
            Result.lift self.OpenScanner s (function | "" -> Some "Неможливо відкрити сканер.";|_-> None)

        member self.nativeTransfer(_): Result<string,string> =
            let cap = (TwCap.XrefMech,TwON.Value,TwType.uint16,"TWSX_NATIVE")            
            Result.lift2 self.Cap TwMsg.Set (scap cap) errCode //Помилка налаштування (native transfer)

        member self.autoFeed(_): Result<int,string> =
            Result.lift self.AutoFeed () (function | true -> Some "Помилка налаштування (auto feed).";|_ -> None) |> Result.map (fun _ -> 0)
            
        member self.disableProgressUi(prev): Result<int, string> =
            Result.lift self.ProgressDriverUi false (function | true -> Some "Помилка налаштування (progress ui)";|_-> None) |> Result.map(fun _ -> 0)

        member self.enableDs(_): Result<int,string> =
            Result.lift self.EnableDS () (function | 0 -> None;|_-> Some "Неможливо почати сканування (enable DS without ui).")

        member self.start(scanloop): Result<int,string> =
            Result.lift self.Start 4 (function | 0 -> None;|_->Some "Помилка налашування фінального стану так колбеку сканування.") 
                |> Result.map (fun x -> self.ScanCallback <- new Callback(scanloop); x)

        member self.get(cap:Cap): Result<string,string> =
            Result.lift2 self.Cap TwMsg.Get (scap cap) errCode
        
        member self.getCurrent(cap:Cap): Result<string,string> = 
            Result.lift2 self.Cap TwMsg.GetCurrent (scap cap) errCode

        member self.getDefault(cap:Cap): Result<string,string> = 
            Result.lift2 self.Cap TwMsg.GetDefault (scap cap) errCode

        member self.set(cap:Cap): Result<string,string> =
            let (|Eq|_|) (s:string) =
              if String.Compare(s, (scap cap), StringComparison.InvariantCultureIgnoreCase) = 0 then Some() else None

            let set_ cap = Result.lift2 self.Cap TwMsg.Set cap errCode

            Result.lift2 self.Cap TwMsg.GetCurrent (scap cap) errCode
                |> Result.bind (function | Eq _ -> Error "already set";| cap -> Ok cap)
                |> Result.bind set_
