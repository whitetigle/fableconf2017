module State

open System
open Fable.Core
open Fable.Import.Browser
open Fable.Import.JS

type CanvasInfo =
    { Context: CanvasRenderingContext2D
      Width: float
      Height: float
    }

let [<Literal>] LifeDec = 0.1

type Particle = {
  X:float
  Y:float
  A:float
  V:float
  Life:float
}


[<RequireQualifiedAccess>]
module Keys =
    let [<Literal>] Space = 32.
    let [<Literal>] Left = 37.
    let [<Literal>] Up = 38.
    let [<Literal>] Right = 39.

type ControlKeys =
    { mutable Up: bool
      mutable Left: bool
      mutable Right: bool }

type Model =
    { Keys: ControlKeys
      Initialized: bool
      X : float
      Particles : Particle List
    }

type Msg =
    | KeyUp of code: float
    | KeyDown of code: float
    | NewFrame

let subscribeToKeyEvents dispatch =
    window.addEventListener_keydown(fun ev ->
        KeyDown ev.keyCode |> dispatch; null)
    window.addEventListener_keyup(fun ev ->
        KeyUp ev.keyCode |> dispatch; null)

let subscribeToFrames dispatch =
    let run =
      let rec run (dt:float) =
        window.requestAnimationFrame(FrameRequestCallback run) |> ignore
        NewFrame |> dispatch
      run
    run 0.0

let initModel (canvasinfo:CanvasInfo) =
    let particles =
      [
        for i in 0..8000 do
          let p = {
            X=Math.random() * canvasinfo.Width
            Y=Math.random() * canvasinfo.Height
            A=0.
            V=0.
            Life=Math.random() * 100.
          }
          yield
            p
      ]
    let model =
        { Keys = { Up=false; Left=false; Right=false }
          Initialized = true
          X= 0.
          Particles=particles
        }
    model, [subscribeToKeyEvents; subscribeToFrames]

let update (msg: Msg) (model: Model) =
    let model =
        match msg with
        | NewFrame ->
        (*
    p = particles[_j];
    v = noise.perlin2(p.x * period, p.y * period);
    ctx.fillStyle = "hsla(" + (Math.floor(v * 360)) + ", 95%, 20%, 0.05)";
    ctx.fillRect(p.x, p.y, 1.5, 1.5);
    p.h++;
    a = v * 2 * Math.PI + p.a;
    p.x += Math.cos(a);
    p.y += Math.sin(a)
        *)
          let particles =
            model.Particles
              |> List.map( fun p ->
                let v = Perlin.perlin2( p.X * 0.00125, p.Y * 0.00125 )
                let a = v * 2. * Math.PI + p.A
                {p with X = p.X + Math.cos(a); Y = p.Y + Math.sin(a); V = v; Life = p.Life - LifeDec }
              )
              |> List.filter( fun p -> p.Life > 0.)

          {model with Particles=particles}
        | KeyDown code ->
            match code with
            | Keys.Up -> model.Keys.Up <- true
            | Keys.Left -> model.Keys.Left <- true
            | Keys.Right -> model.Keys.Right <- true
            | _ -> ()
            model
        | KeyUp code ->
            match code with
            | Keys.Up -> model.Keys.Up <- false
            | Keys.Left -> model.Keys.Left <- false
            | Keys.Right -> model.Keys.Right <- false
            | _ -> ()
            model
    model, []
