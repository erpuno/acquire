namespace N2O

[<AutoOpen>]
module TwTypes =
    type TwMsg =
      | Get         = 0x1us
      | GetCurrent  = 0x2us
      | GetDefault  = 0x3us
      | Set         = 0x6us
      | Reset       = 0x7us
      | ResetAll    = 0x0A01us

    type TwCap =
      | Profile     = 0
      | CustomData  = 0x1015  // check if source suppports new opeation triplets DG_CONTROL/MSG_CUSTOMDATA/MSG_ TW_ONEVALUE TW_BOOL
                              // valid is state 4
                              // DG_CONTROL/DAT_CUSTOMDSDATA/MSG_GET 
                              // query datasource for its current settings (DPI, paper size, color format)
                              // TW_CUSTOMDATA structure
                              // actual format of this structure not defined by twain
                              // 
      | AutoFeed    = 0x1007
      | AutoScan    = 0x1010
      | Duplex      = 0x1013
      | Indicators  = 0x100b
      | XrefMech    = 0x0103
      | PixelType   = 0x0101  // can be set on onevalue or enumeration. 
                              // any xrefmech other that native and can't support every value, must support negotiation on this cap
                              // ICAP!
      | CameraEnabled = 0x1036  // implicitely set to true by setting pixeltype.
      | CameraOrder = 0x1037  // select the order or output for SDMI
                              // 

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

    type TwData =
      | ImgNativeXfer = 0x0104

    type TwCC = (*condition code*)
      | Error = 0x10000
      | OperationError = 0x10005
      | BadCap = 0x10006
      | BadProtocol = 0x10009
      | BadValue = 0x1000a
      | BadDest = 0x1000c
      | UnsupportedCap = 0x1000d
      | BadOperationCap = 0x1000e
      | SeqError = 0x1000b
      | SeqErrorCap = 0x1000f

    module TwCap = 
      let MkString = function
        | TwCap.AutoFeed    -> "CAP_AUTOFEED"
        | TwCap.AutoScan    -> "CAP_AUTOSCAN"
        | TwCap.Duplex      -> "CAP_DUPLEXENABLED"
        | TwCap.Indicators  -> "CAP_INDICATORS"
        | TwCap.XrefMech    -> "ICAP_XFERMECH"
        | TwCap.PixelType   -> "ICAP_PIXELTYPE"
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

    module TwData =
      let MkString = function
        | TwData.ImgNativeXfer -> "TWSX_NATIVE"
        | _ -> ""

    module TwON = 
      let MkString = function
        | TwON.Array -> "TWON_ARRAY"
        | TwON.Enum  -> "TWON_ENUMERATION"
        | TwON.Value -> "TWON_ONEVALUE"
        | TwON.Range -> "TWON_RANGE"
        | _ -> ""
