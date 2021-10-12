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

        let currentCap(cap:string)(current:string -> string): Result<string,string> =
            try Ok (current(cap))
            with e ->
              let code = try e.Message |> int |> Some with _ -> None
              match code with 
              | Some(TwCC s) -> Error s
              | _            -> Error e.Message

        let set(cap:Cap): Result<string,string> =
            let cp,co,ty,vs = cap
            
            let testcap = sprintf "%s,%s,%s,%s" (TwCap.MkString(cp)) (TwON.MkString(co)) (TwType.MkString(ty)) (vs.ToUpperInvariant())

            printfn "test CAP: %s" testcap

            let (|Test|_|) (s:string) =
              if String.Compare(s, testcap, StringComparison.InvariantCultureIgnoreCase) = 0 then Some() else None
            
            let (getcurrent,set__) =
              match cp with
              | TwCap.PixelType |  TwCap.XrefMech -> (tw.ImageCapGetCurrent, tw.ControlCapSet)
              | _               -> (tw.ControlCapGetCurrent, tw.ImageCapSet)

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

            currentCap(testcap)(getcurrent)
              |> Result.bind set_
        
        let get(cap:Cap): Result<string,string> =
            let cp,co,ty,vs = cap
            let testcap = sprintf "%s,%s,%s,%s" (TwCap.MkString(cp)) (TwON.MkString(co)) (TwType.MkString(ty)) (vs.ToUpperInvariant())
            printfn "test CAP: %s" testcap
            currentCap(testcap)(tw.ControlCapGetCurrent)

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
                    |> Result.bind tw.manager
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
                  |> Result.bind tw.manager
                  |> Result.bind tw.ds
                  |> Result.bind tw.id
                  |> Result.bind tw.scanner
                  |> Result.bind (fun (_:string) -> traverseR get (Seq.toList caps))

              match res with
              | Ok sts -> ch.Reply (Out(Encoding.UTF8.GetBytes($"{sts}")))
              | Error e -> ch.Reply (Out(Encoding.UTF8.GetBytes(e)))
            
            | Set (device,profile,caps) ->
              let res = 
                tw.init()
                  |> Result.bind tw.manager
                  |> Result.bind tw.ds  // use profile here
                  |> Result.bind tw.id  // use device from command, not the default one
                  |> Result.bind tw.scanner
                  |> Result.bind (fun (_:string) -> traverseR set (Seq.toList caps))

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
