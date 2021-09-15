namespace N2O
open System
open Scan

// pure man's non 10K lines csv -> proto parser
[<AutoOpen>]
module Parser =
    let (|Proto|_|) (t:string) =
        let pts = t.Split([|','|])
        if pts.Length < 4 then None
        else 
            try
              let device = Device pts.[1]
              let profile = Setup pts.[2]
              let caps = pts.[3].Split([|'+'|]) |> Seq.map Cap

              let (|Eq|_|) arg (s:string) =
                if String.Compare(s, arg, StringComparison.InvariantCultureIgnoreCase) = 0 then Some() else None

              match pts.[0] with
                | Eq "scan"    _ -> Some (Scan(device, profile, caps))
                | Eq "get"     _ -> Some (Get(device, profile, caps))
                | Eq "set"     _ -> Some (Set(device, profile, caps))
                | Eq "profile" _ -> Some (Profile(device, profile, caps))
                | _ -> None
            with
              | _ -> None
