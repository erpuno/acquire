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

        let manager(): Result<int,string> =
            try
                match tw.OpenManager() with
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
              | "" -> Error "Пристрій за замовченням відсутній."
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

        let setcups(_): Result<int, string> = 
            try
              match tw.SetCaps() with
              | false -> Ok 0
              | true -> Error "Помилка налаштування (capabilities)."
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
                    // rolback to state 2 (reset ui and allow to scan again) 
                    // todo: add posibility to scan from state 4
                    reader <- None
                    tw.Rollback(2)
                    0
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
                    manager()
                    |> Result.bind ds  // use profile here
                    |> Result.bind id  // use device from command, not the default one
                    |> Result.bind scanner
                    |> Result.bind nativeTransfer
                    |> Result.bind autoFeed
                    |> Result.bind disableProgressUi
                    |> Result.bind setcups // form program from caps
                    |> Result.bind enableDs
                    |> Result.bind start
                    |> Result.mapError (fun e -> tw.Exit <- true;e)

                match res with
                | Ok sts  -> sts |> ignore
                | Error e -> ch.Reply (Out(Encoding.UTF8.GetBytes(e)))

            | _ -> ch.Reply (Out(Encoding.UTF8.GetBytes("not implemented")))

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
