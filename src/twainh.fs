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
