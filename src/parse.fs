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
              let (|Eq|_|) arg (s:string) =
                if String.Compare(s, arg, StringComparison.InvariantCultureIgnoreCase) = 0 then Some() else None

              let device = Device pts.[1]
              let profile = Setup pts.[2]
              let caps = pts.[3].Split([|'+'|]) |> Seq.map (fun s ->
                match s with
                | Eq "autofeed"   _ -> (TwCap.AutoFeed,TwON.Value,TwType.bool,"true")
                | Eq "autoscan"   _ -> (TwCap.AutoScan,TwON.Value,TwType.bool,"true")
                | Eq "duplex"     _ -> (TwCap.Duplex,TwON.Value,TwType.bool,"true")
                | Eq "indicators" _ -> (TwCap.Indicators,TwON.Value,TwType.bool,"true")
                | Eq "native"     _ -> (TwCap.XrefMech,TwON.Value,TwType.uint16,"TWSX_NATIVE") // use types
                | Eq "color-bw"   _ -> (TwCap.PixelType,TwON.Value,TwType.uint16,"TWPT_BW")    // enum support
                |                 _ -> (TwCap.Profile, TwON.Value,TwType.str32,"")
              )

              match pts.[0] with
                | Eq "scan"    _ -> Some (Scan(device, profile, caps))
                | Eq "get"     _ -> Some (Get(device, profile, caps))
                | Eq "set"     _ -> Some (Set(device, profile, caps))
                | Eq "profile" _ -> Some (Profile(device, profile, caps))
                | _ -> None
            with
              | _ -> None
