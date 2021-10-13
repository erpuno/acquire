namespace N2O
open System
open System.Globalization
open System.IO
open System.Threading
open System.Text
open Scan
open Result
open Twain

module Acquire =
    let rec traverseR f list = 
        let (<*>) fr xr = 
          match fr,xr with
          | Ok f, Ok x       -> Ok (f x)
          | Error e, Ok x    -> Error e
          | Ok f, Error e    -> Error e
          | Error f, Error x -> Error (f+x)

        let ok = Result.Ok
        let cons hd tl = hd :: tl

        match list with 
        | []     -> ok []
        | hd::tl -> ok cons <*> (f hd) <*> (traverseR f tl)

    let control(tw) = MailboxProcessor.Start(fun(inbox:MailboxProcessor<Cmd>)  ->
        let mutable tw: ITwain = tw
        let mutable reader: BinaryReader option = None

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

            match msg with
            | Scan(device,profile,caps) ->
                let res =
                    tw.init()
                    |> Result.bind tw.openDSM
                    |> Result.bind tw.ds  // use profile here
                    |> Result.bind tw.id  // use device from command, not the default one
                    |> Result.bind tw.scanner
                    |> Result.bind tw.nativeTransfer
                    |> Result.bind tw.autoFeed
                    |> Result.bind tw.disableProgressUi
                    |> Result.bind tw.enableDs
                    |> Result.bind (fun e -> tw.start(scanloop))
                    |> Result.mapError (fun e -> tw.Exit <- true; tw.CloseDSM() |> ignore; e)

                match res with
                | Ok sts  -> sts |> ignore
                | Error e -> ch.Reply (Out(Encoding.UTF8.GetBytes(e)))

            | Get(device,profile,caps) ->
              let res =
                tw.init()
                  |> Result.bind tw.openDSM
                  |> Result.bind tw.ds
                  |> Result.bind tw.id
                  |> Result.bind tw.scanner
                  |> Result.bind (fun (_:string) -> traverseR tw.getCurrent (Seq.toList caps))
                  |> Result.bind tw.closeDSM
                  |> Result.mapError (fun e -> tw.Exit<-true; tw.CloseDSM() |> ignore; e)

              match res with
              | Ok sts -> ch.Reply (Out(Encoding.UTF8.GetBytes($"{sts}")))
              | Error e -> ch.Reply (Out(Encoding.UTF8.GetBytes(e)))
            
            | Set (device,profile,caps) ->
              let res = 
                tw.init()
                  |> Result.bind tw.openDSM
                  |> Result.bind tw.ds  // use profile here
                  |> Result.bind tw.id  // use device from command, not the default one
                  |> Result.bind tw.scanner
                  |> Result.bind (fun _ -> traverseR tw.set (Seq.toList caps))
                  |> Result.bind tw.closeDSM
                  |> Result.mapError (fun e -> tw.Exit<-true; tw.CloseDSM() |> ignore; e)

              // form profile message
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
