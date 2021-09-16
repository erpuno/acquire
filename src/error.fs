namespace N2O
open Scan

[<AutoOpen>]
module Err =
    let (|TwCC|) (code:int) =
      match enum<TwCC>(code) with
      | TwCC.UnsupportedCap -> "Задані можливості не підтримуються джерелом даних."
      | TwCC.Baddest        -> "Невідомий пристрій в DSM менеджері джерел даних."
      | c                   -> $"Неможливо відкрити джерело даних. {c}"
