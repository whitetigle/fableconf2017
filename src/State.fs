module State

open System
open Fable.Core
open Fable.Import.Browser
open Fable.Import.JS
open System.Collections.Generic

type CanvasInfo =
    { Context: CanvasRenderingContext2D
      Width: float
      Height: float
    }

let [<Literal>] Radius = 200.
let [<Literal>] ParticleSpeed = 2.

type Composition =
  | Top
  | Bottom

type Particle = {
  startX:float
  startY:float
  mutable X:float
  mutable Y:float
  mutable A:float
  mutable V:float
  mutable Life:float
  mutable Speed:float
  mutable Composition: Composition
  mutable Size:float
  mutable LifeDec:float
  mutable PerlinCoeff:float
  mutable Color:string
  mutable Alpha:float
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

type Saturation = int
type PaintingKind =
  | Flows of Saturation
  | FableCurtain of string
  | ShowTitle of string

type Screen =
  | Start
  | Transition
  | DisplayText of string
  | LaunchPainting of PaintingKind
  | DoNothing
  | GoNextFrame
  | ClearScreen
  | NextScreen

type ScreenContent = {
  Text: string
}

type Model =
    { Keys: ControlKeys
      Initialized: bool
      X : float
      Particles : Particle []
      Screen : Screen
      Screens : Screen list
      CurrentIndex : int
      ScreenContent: ScreenContent
      CanvasInfo : CanvasInfo
      BackgroundAnimation: PaintingKind option
    }

type Msg =
  | KeyDown of code: float
  | NewFrame
  | Resize

let subscribeToKeyEvents dispatch =
    window.addEventListener_keydown(fun ev ->
        KeyDown ev.keyCode |> dispatch; null)

let subscribeToResize dispatch =
    window.addEventListener_resize(fun ev ->
        Resize |> dispatch; null)

let subscribeToFrames dispatch =
    let run =
      let rec run (dt:float) =
        window.requestAnimationFrame(FrameRequestCallback run) |> ignore
        NewFrame |> dispatch
      run
    run 0.0

let initModel (canvasinfo:CanvasInfo) =
    let model =
        { Keys = { Up=false; Left=false; Right=false }
          Initialized = true
          X= 0.
          Particles=[||]
          Screen = Start
          Screens =
            [
              ClearScreen
              DisplayText "Hello!"
              Transition

              ClearScreen
              DisplayText "Disclaimer: no magic inside"
              Transition

              ClearScreen
              DisplayText "Sharing my personal experience"
              Transition

              ClearScreen
              DisplayText "More time for my kids"
              Transition

              ClearScreen
              DisplayText "Game balancing applied to my life"
              Transition

              ClearScreen
              DisplayText "My Goals"
              Transition

              ClearScreen
              DisplayText "Current context"
              Transition

              ClearScreen
              DisplayText "What I need"
              Transition

              ClearScreen
              DisplayText "Since 2014: changes for the best "
              Transition

              ClearScreen
              DisplayText "Fable !!"
              Transition

              ClearScreen
              DisplayText "10 months later"
              Transition

              ClearScreen
              DisplayText "That's all folks!"
              Transition

              ClearScreen
              DisplayText "Thanks!"
              Transition

              ClearScreen
              DisplayText "Please, enjoy your meal!"
              Transition

            ]
          CurrentIndex = 0
          ScreenContent = { Text="Hello!"}
          CanvasInfo=canvasinfo
          BackgroundAnimation = None
        }
    model, [subscribeToKeyEvents; subscribeToFrames; subscribeToResize]

let update (msg: Msg) (model: Model) =
    let model =
        match msg with
        | Resize -> model

        | NewFrame ->

          let model =
            match model.BackgroundAnimation with
            | Some kind ->
              match kind with
              | Flows saturation->

                let l = model.Particles |> Seq.length
                for i in 0..(l-1) do
                  let p = model.Particles.[i]
                  let coeff = p.PerlinCoeff
                  let v = Perlin.perlin2( p.X * coeff, p.Y * coeff )
                  let a = v * 2. * Math.PI + p.A
                  let color = (sprintf "hsla(%f, %i%%, 80%%, 0.06)" (Math.floor(v * 360.)) saturation )
                  p.X <- p.X + Math.cos(a) * p.Speed
                  p.Y <-  p.Y + Math.sin(a) * p.Speed
                  p.V <- v
                  p.Life <-  p.Life - p.LifeDec
                  p.Color <- color

                let particles = model.Particles |> Seq.filter(fun p -> p.Life > 0.)
                if particles |> Seq.length <=0 then
                  {model with Particles = [||]; BackgroundAnimation=None }
                else
                  model

                (*
                //drawing eights!!
                let a = p.A + 0.01
                let y = p.startY + Math.sin(a*2.) * Radius
                let x = p.startX + Math.sin(a) * Radius * 2.5
                {p with X = x; Y = y; V = v; A=a; Life = p.Life - LifeDec }
                *)

                (*
                let length = model.Particles
                let particles =
                  model.Particles
                    |> Seq.map( fun p ->
                      let coeff = p.PerlinCoeff
                      let v = Perlin.perlin2( p.X * coeff, p.Y * coeff )

                      // waves
                      // source: https://josephg.com/perlin/3/p.js
                      let a = v * 2. * Math.PI + p.A
                      let color = (sprintf "hsla(%f, %i%%, 80%%, 0.06)" (Math.floor(v * 360.)) saturation )
                      {p with X = p.X + Math.cos(a) * p.Speed; Y = p.Y + Math.sin(a) * p.Speed; V = v; Life = p.Life - p.LifeDec; Color=color }
                    )
                    |> Seq.filter( fun p -> p.Life > 0.)
                    |> Seq.toArray
                {model with Particles = particles }
                *)

              | FableCurtain title ->

                let l = model.Particles |> Seq.length
                for i in 0..(l-1) do
                  let p = model.Particles.[i]
                  let a = p.A + p.Speed
                  p.X <- p.startX + Math.cos(a) * 3.0
                  p.Y <- p.Y + p.Speed
                  p.Life <- p.Life - p.LifeDec
                  p.A = a
                  |> ignore

                (*
                let particles =
                  model.Particles
                    |> List.map( fun p ->
                      let a = p.A + p.Speed
                      {p with X = p.startX + Math.cos(a) * 3.0;A=a;Y = p.Y + p.Speed;Life = p.Life - p.LifeDec }
//                      {p with X = p.X + p.Speed; A=a; Y = p.startY; V = v; Life = p.Life - LifeDec;}
                    )
                    |> List.filter( fun p -> p.Life > 0.)
                *)
                let particles = model.Particles |> Seq.filter(fun p -> p.Life > 0.)
                if particles |> Seq.length <=0 then
                  {model with Screen=NextScreen; Particles = [||]; BackgroundAnimation=None }
                else model
//                  {model with Particles = particles }

              | ShowTitle title ->

                let l = model.Particles |> Seq.length
                for i in 0..(l-1) do
                  let p = model.Particles.[i]
                  p.Life <- p.Life - p.LifeDec
                  p.Alpha = if p.Alpha <1.0 then p.Alpha + 0.1 else 1.0
                  |> ignore

                model

            | None -> model

          printfn "%A" model.Screen

          match model.Screen with
          | NextScreen ->
              let screen = model.Screens.[model.CurrentIndex]
              {model with Screen=screen; CurrentIndex = model.CurrentIndex + 1}

          | ClearScreen ->
            let sat = int (Math.random() * 100.)
            {model with Screen = LaunchPainting (Flows sat); Particles = [||]}

          | Start -> model

          | Transition ->
            let particles =
              let radius = 10
              let max = (int model.CanvasInfo.Width / radius)
              [|
                  let p =
                    {
                      X=0.
                      Y= -200.
                      startX=0.
                      startY= -200.
                      A=0.//Math.random() * 1000.
                      V=0.
                      Life=50.//Math.random() * 1000.
                      Speed=ParticleSpeed * 3.0
                      Composition=Top
                      Size=300.
                      LifeDec=0.1
                      PerlinCoeff=1.
                      Color = "rgba(255, 255, 255, 0.05)"
                      Alpha = 1.0
                    }
                  yield
                    p

              |]

            {model with Particles=particles; Screen = DoNothing; BackgroundAnimation=Some (FableCurtain "Fable 2017") }

          | DoNothing -> model
          | GoNextFrame -> { model with ScreenContent={Text=""}; Screen = DoNothing}
          | LaunchPainting kind ->

            let particles =
              match kind with
              | Flows saturation ->
                  let coeff = (100. + 100. * Math.random()) * 0.00001
                  //let coeff = 0.00125
                  printfn "%f" coeff
                  [|
                    for i in 0..10000 do
                      let x = Math.random() * model.CanvasInfo.Width
                      let y = Math.random() * model.CanvasInfo.Height
                      let v = Perlin.perlin2( x * coeff, y * coeff )
                      let p = {
                        X=x
                        Y=y
                        startX=x
                        startY=y
                        A=0.//Math.random() * 1000.
                        V=0.
                        Life=10.//Math.random() * 1000.
                        Speed=ParticleSpeed
                        Composition=Bottom
                        Size=1.5
                        LifeDec=0.01
                        PerlinCoeff=coeff// 0.00125
                        Color=(sprintf "hsla(%f, %i%%, 80%%, 0.06)" (Math.floor(v * 360.)) saturation )
                        Alpha=1.0
                      }
                      yield
                        p
                  |]
              | _ -> [||]


            let sat = int (Math.random() * 100.)
            let ps = (model.Particles |> Seq.toList) @ (particles |> Seq.toList)
            let psa = ps |> Seq.toArray

            {model with Particles=psa; Screen = NextScreen; BackgroundAnimation=Some (Flows sat) }
          | DisplayText text ->
            (*
            let particles =
              let radius = 10
              let max = (int model.CanvasInfo.Width / radius)
              [|
                  let p =
                    {
                      X=0.
                      Y= -200.
                      startX=0.
                      startY= -200.
                      A=0.//Math.random() * 1000.
                      V=0.
                      Life=20.//Math.random() * 1000.
                      Speed=ParticleSpeed * 3.0
                      Composition=Top
                      Size=300.
                      LifeDec=0.5
                      PerlinCoeff=1.
                      Color = "rgba(255, 255, 255, 0.05)"
                      Alpha = 0.01
                    }
                  yield
                    p

              |]

            // TODO: hopefully we can do concat operation way better!!
            let ps = (model.Particles |> Seq.toList) @ (particles |> Seq.toList)
            let psa = ps |> Seq.toArray
            {model with Particles= psa; Screen = DoNothing; BackgroundAnimation=Some (ShowTitle text) }
            *)

            { model with ScreenContent = {model.ScreenContent with Text=text}; Screen = GoNextFrame }

        | KeyDown code ->
            match code with
            | Keys.Up -> model
            | Keys.Left -> model
            | Keys.Right ->
              model.Keys.Right <- true
              {model with Screen=NextScreen }
            | _ -> model
    model, []
