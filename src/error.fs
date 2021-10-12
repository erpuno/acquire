namespace N2O
open Scan

[<AutoOpen>]
module Err =
    let (|TwCC|) (code:int) =
      match enum<TwCC>(code) with
      | TwCC.UnsupportedCap -> "Задані можливості не підтримуються джерелом даних."
      | TwCC.BadDest        -> "Невідомий пристрій в DSM менеджері джерел даних."
      | TwCC.Error          -> "some generic error on the border of custom spce"
      | TwCC.SeqError       -> "Illegal operation for current Source Manager or Source state."
      | c                   -> $"Поомилка {c}."
