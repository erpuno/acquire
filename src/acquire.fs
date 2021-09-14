namespace N2O
open System
open System.IO
open System.Text
open Scan

module Acquire =
    let control(tw) = MailboxProcessor.Start(fun(inbox:MailboxProcessor<Cmd>)  ->
        let mutable tw: ITwain = tw
        let mutable reader: BinaryReader option = None

        let rec loop () = async {
            let! (msg, ch) = inbox.Receive()

            match msg with
            | Scan(device,profile,caps) ->
                let stub(): Result<int,string> = Error "scan process not defined yet"

                // scan program here
                let res = 
                    stub() 
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
