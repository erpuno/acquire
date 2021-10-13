namespace N2O
open Scan

[<AutoOpen>]
module Err =
    let (|TwCC|) (code:int) =
      match enum<TwCC>(code) with
      | TwCC.UnsupportedCap -> "Задані можливості не підтримуються джерелом даних."
      | TwCC.BadDest        -> "Невідомий пристрій в DSM менеджері джерел даних."
      | TwCC.BadValue       -> "Values are outside the supported values for capability."
      | TwCC.Error          -> "some generic error on the border of custom spce"
      | TwCC.SeqError       -> "Illegal operation for current Source Manager or Source state."
      | TwCC.SeqErrorCap    -> "Capability cannot be modified due to a setting for a related capability."
      | c                   -> $"Поомилка {c}."
