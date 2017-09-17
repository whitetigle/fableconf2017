module Perlin

open Fable.Core

[<Emit("noise.seed($0)")>]
let seed (s:float) : unit = jsNative

[<Emit("noise.perlin2($0,$1)")>]
let perlin2 (x:float,y:float) : float = jsNative
