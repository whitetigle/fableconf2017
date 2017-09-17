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

let [<Literal>] Radius = 200.


type Particle = {
  startX:float
  startY:float
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
        for i in 0..2000 do
          let x = Math.random() * canvasinfo.Width
          let y = Math.random() * canvasinfo.Height
          let p = {
            X=x
            Y=y
            startX=x
            startY=y
            A=0.
            V=0.
            Life=Math.random() * 1000.
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

          // update our particles
          let particles =
            model.Particles
              |> List.map( fun p ->
                let v = Perlin.perlin2( p.X * 0.00125, p.Y * 0.00125 )
                let a = p.A + 0.01
                let y = p.startY + Math.sin(a*2.) * Radius
                let x = p.startX + Math.sin(a) * Radius * 2.5
                {p with X = x; Y = y; V = v; A=a; Life = p.Life - LifeDec }
 //              let a = v * 2. * Math.PI + p.A
 //              {p with X = p.X + Math.cos(a); Y = p.Y + Math.sin(a); V = v; Life = p.Life - LifeDec }
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
