namespace N2O
open System

module Scan =
    type Device = Device of string
    type Setup  = Setup of string
    type Cap    = Cap of string

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
