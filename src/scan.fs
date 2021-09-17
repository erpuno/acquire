namespace N2O
open System

module Scan =
    type TwCap =
      | Profile  = 0
      | AutoFeed = 0x1007

    type TwON =
      | Array = 3
      | Enum  = 4
      | Value = 5
      | Range = 6

    type TwType = 
      | int8   = 0x0000
      | int16  = 0x0001
      | int32  = 0x0002
      | uint8  = 0x0003
      | uint16 = 0x0004
      | uint32 = 0x0005
      | bool   = 0x0006
      | fix32  = 0x0007
      | frame  = 0x0008
      | str32  = 0x0009
      | str64  = 0x000a
      | str128 = 0x000b
      | str255 = 0x000c
      | handle = 0x000f

    type Device = Device of string
    type Setup  = Setup of string
    type Cap    = TwCap * TwON * TwType * string

    type Proto =
      | Scan    of Device * Setup * Cap seq
      | Get     of Device * Setup * Cap seq
      | Set     of Device * Setup * Cap seq
      | Profile of Device * Setup * Cap seq

    type Out = Out of byte array
    type Cmd = Proto * AsyncReplyChannel<Out>

    type Callback = delegate of bool -> int

    [<Interface>]
    type ITwain =
      inherit IDisposable
      abstract Init: unit -> unit
      abstract OpenDSM: unit -> int
      abstract CloseDSM: unit -> int
      abstract EnableDS: unit -> int
      abstract GetDataSources: unit -> string list
      abstract DefaultIdentity: unit -> string
      abstract OpenScanner: string -> string
      abstract NativeTransfer: unit -> bool
      abstract NativeCallback: bool -> unit
      abstract AutoFeed: unit -> bool
      abstract AutoScan: unit -> bool
      abstract EnableDuplex: unit -> bool
      abstract ProgressDriverUi: bool -> bool
      abstract FileInfo: unit -> string
      abstract Start: int -> int
      abstract Rollback: int -> unit
      abstract ControlCapGetCurrent: string -> string
      abstract ControlCapGet: string -> string
      abstract ControlCapSet: string -> string
      abstract ControlCapReset: unit -> string
      abstract member Exit: bool with  get,set
      abstract member ScanCallback: Callback with get,set
      abstract member State: int with get

    type TwCC = (*condition code*)
      | Baddest = 0x1000c
      | UnsupportedCap = 0x1000d

    module TwCap = 
      let MkString = function
        | TwCap.AutoFeed -> "CAP_AUTOFEED"
        | _ -> ""

    module TwType = 
      let MkString = function
        | TwType.int8   -> "TWTY_INT8"
        | TwType.int16  -> "TWTY_INT16"
        | TwType.int32  -> "TWTY_INT32"
        | TwType.uint8  -> "TWTY_UINT8"
        | TwType.uint16 -> "TWTY_UINT16"
        | TwType.uint32 -> "TWTY_UINT32"
        | TwType.bool   -> "TWTY_BOOL"
        | TwType.fix32  -> "TWTY_FIX32"
        | TwType.frame  -> "TWTY_FRAME"
        | TwType.str32  -> "TWTY_STR32"
        | TwType.str64  -> "TWTY_STR64"
        | TwType.str128 -> "TWTY_STR128"
        | TwType.str255 -> "TWTY_STR255"
        | TwType.handle -> "TWTY_HANDLE"
        | _ -> ""

    module TwON = 
      let MkString = function
        | TwON.Array -> "TWON_ARRAY"
        | TwON.Enum  -> "TWON_ENUMERATION"
        | TwON.Value -> "TWON_ONEVALUE"
        | TwON.Range -> "TWON_RANGE"
        | _ -> ""
