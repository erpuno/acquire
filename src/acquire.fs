namespace N2O
open System
open System.IO
open System.Threading
open System.Text
open Scan

module Acquire =
    let control(tw) = MailboxProcessor.Start(fun(inbox:MailboxProcessor<Cmd>)  ->
        let mutable tw: ITwain = tw
        let mutable reader: BinaryReader option = None

        let init (): Result<int,string> = try tw.Init(); Ok 0 with e -> Error e.Message

        let manager(_:int): Result<int,string> =
            try
                match tw.OpenDSM() with
                | 0 -> Ok 0
                | TwCC s -> Error s
            with
                | e -> Error e.Message

        let ds(_:int): Result<string list,string> =
            try 
              match tw.GetDataSources() with
                | [] -> Error "На цій ситемі відсутні TWAIN драйвери."
                | xs -> Ok xs
            with 
              | e -> Error e.Message

        let id(_: string list): Result<string,string> =
            try
              match tw.DefaultIdentity() with
              | "" -> Error "Пристрій за замовчуванням відсутній."
              | id -> Ok id
            with
              | e -> Error e.Message

        let scanner(s: string): Result<string,string> =
            try
              match tw.OpenScanner(s) with
              | "" -> Error "Неможливо відкрити сканер."
              | r1 -> Ok r1
            with
              | e -> Error e.Message

        let nativeTransfer(_:_): Result<int,string> =
            try
              match tw.NativeTransfer() with
              | false -> Ok 0
              | true -> Error "Помилка налаштування (native transfer)."
            with
              | e -> Error e.Message

        let autoFeed(_): Result<int,string> = 
            try
              match tw.AutoFeed() with
              | false -> Ok 0
              | true -> Error "Помилка налаштування (auto feed)."
            with
              | e -> Error e.Message

        let disableProgressUi(_): Result<int, string> =
            try
              match tw.ProgressDriverUi(false) with
              | false -> Ok 0
              | true -> Error "Помилка налаштування (progress ui)."
            with
              | e -> Error e.Message

        let enableDs(_): Result<int,string> =
            try
              match tw.EnableDS() with
              | 0 -> Ok 0
              | _ -> Error "Неможливо почати сканування (enable DS without ui)."
            with
              | e -> Error e.Message

        let rec loop () = async {
            let! (msg, ch) = inbox.Receive()

            let rec scanloop b =
                Thread.Sleep 300
                match tw.State with
                | s when s <= 3 -> scanloop b
                | 4 -> //after scan we've set on start
                    let name = tw.FileInfo()
                    let mutable size = 0
                    try
                      let stream = File.Open(name, FileMode.Open, FileAccess.Read)
                      reader <- Some(new BinaryReader(stream, Encoding.UTF8, true))
                      size <- (int)stream.Length
                    with
                    | _ -> () |> ignore // :? FileNotFoundException | :? UnauthorizedAccessException | :? IOException

                    match reader with
                    | None -> 
                        ch.Reply (Out(Encoding.UTF8.GetBytes("can't read scanned file")))
                    | Some r ->
                        let mutable buf: byte array = Array.zeroCreate size
                        let size = r.Read(buf,0,buf.Length)
                        ch.Reply (Out(buf))

                    reader |> Option.map(fun r -> 
                                  r.BaseStream.Close()
                                  r.Dispose() ) |> ignore
                    reader <- None
                    tw.Rollback(2) // todo: add posibility to scan from state 4
                    tw.CloseDSM()  // close source manager for compatibility with ui forms
                | r ->
                  tw.NativeCallback(false)
                  scanloop b

            let start(_): Result<int,string> =
                try
                    match tw.Start(4) with
                    | 0 ->
                        tw.ScanCallback <- new Callback(scanloop)
                        Ok 0
                    | _ -> Error "Помилка налашування фінального стану так колбеку сканування."
                with
                | e -> Error e.Message

            match msg with
            | Scan(device,profile,caps) ->
                let res =
                    init()
                    |> Result.bind manager
                    |> Result.bind ds  // use profile here
                    |> Result.bind id  // use device from command, not the default one
                    |> Result.bind scanner
                    |> Result.bind nativeTransfer
                    |> Result.bind autoFeed
                    |> Result.bind disableProgressUi
                    |> Result.bind enableDs
                    |> Result.bind start
                    |> Result.mapError (fun e -> tw.Exit <- true; tw.CloseDSM() |> ignore; e)

                match res with
                | Ok sts  -> sts |> ignore
                | Error e -> ch.Reply (Out(Encoding.UTF8.GetBytes(e)))

            | Get(device,profile,caps) ->
              let res = Error "not implemeneed"

              match res with
              | Ok sts -> ch.Reply (Out(Encoding.UTF8.GetBytes($"{sts}")))
              | Error e -> ch.Reply (Out(Encoding.UTF8.GetBytes(e)))
            
            | Set (device,profile,caps) ->
              let testcap = "CAP_AUTOFEED,TWON_ONEVALUE,0,0"

              let (|Test|_|) (s:string) =
                if String.Compare(s, testcap, StringComparison.InvariantCultureIgnoreCase) = 0 then Some() else None

              let current(_): Result<string,string> =
                try Ok (tw.ControlCapGetCurrent(testcap)) // read from caps
                with e ->
                  let code = try e.Message |> int |> Some with _ -> None
                  match code with 
                  | Some(TwCC s) -> Error s
                  | _            -> Error e.Message

              let set(cap: string): Result<string,string> =
                try
                  match cap with 
                  | Test _ -> Ok "already set"
                  | _     -> Ok (tw.ControlCapSet(cap))
                with e -> 
                  let code = try e.Message |> int |> Some with _ -> None
                  match code with 
                  | Some(TwCC s) -> Error s
                  | _            -> Error e.Message

              let res = 
                init()
                  |> Result.bind manager
                  |> Result.bind ds  // use profile here
                  |> Result.bind id  // use device from command, not the default one
                  |> Result.bind scanner
                  |> Result.bind current
                  |> Result.bind set

              match res with
              | Ok sts -> ch.Reply(Out(Encoding.UTF8.GetBytes($"{sts}")))
              | Error e -> ch.Reply (Out(Encoding.UTF8.GetBytes(e)))

            | r ->
              printfn "doesn't match %A" r
              ch.Reply (Out(Encoding.UTF8.GetBytes("not implemented")))

            return! loop ()
        }
        loop ()
    )

    let proto(twain: ITwain): Msg -> Msg =
        let ctl = control(twain)
        fun msg ->
            match msg with
            | Text (Proto(p)) ->
                let out = ctl.PostAndAsyncReply(fun r -> Cmd(p,r))
                out |> Async.RunSynchronously |> fun (Out b) -> b |> Bin
            | _ -> Nope
