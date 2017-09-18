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

let [<Literal>] Radius = 200.
let [<Literal>] ParticleSpeed = 2.

type Composition =
  | Top
  | Bottom

type Particle = {
  startX:float
  startY:float
  X:float
  Y:float
  A:float
  V:float
  Life:float
  Speed:float
  Composition: Composition
  Size:float
  LifeDec:float
  PerlinCoeff:float
  Color:string
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
  | BlackWave

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
      Particles : Particle List
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
          Particles=[]
          Screen = ClearScreen
          Screens =
            [
              DisplayText "Disclaimer: no magic inside"
              Transition
              ClearScreen
              DisplayText "Beware: sharing highly personal experience"
              Transition
              ClearScreen
              DisplayText "More time for my kids"
              Transition
              ClearScreen
              DisplayText "Game balancing applied to my life"
              Transition
              DisplayText "My Goals"
              ClearScreen
              Transition
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
                let particles =
                  model.Particles
                    |> List.map( fun p ->
                      let coeff = p.PerlinCoeff
                      let v = Perlin.perlin2( p.X * coeff, p.Y * coeff )

                      (*
                      //drawing eights!!
                      let a = p.A + 0.01
                      let y = p.startY + Math.sin(a*2.) * Radius
                      let x = p.startX + Math.sin(a) * Radius * 2.5
                      {p with X = x; Y = y; V = v; A=a; Life = p.Life - LifeDec }
                      *)

                      // waves
                      // source: https://josephg.com/perlin/3/p.js
                      let a = v * 2. * Math.PI + p.A
                      let color = (sprintf "hsla(%f, %i%%, 80%%, 0.06)" (Math.floor(v * 360.)) saturation )
                      {p with X = p.X + Math.cos(a) * p.Speed; Y = p.Y + Math.sin(a) * p.Speed; V = v; Life = p.Life - p.LifeDec; Color=color }
                    )
                    |> List.filter( fun p -> p.Life > 0.)
                {model with Particles = particles }

              | BlackWave ->

                let particles =
                  model.Particles
                    |> List.map( fun p ->
                      let a = p.A + p.Speed
                      {p with X = p.startX + Math.cos(a) * 3.0;A=a;Y = p.Y + p.Speed;Life = p.Life - p.LifeDec }
//                      {p with X = p.X + p.Speed; A=a; Y = p.startY; V = v; Life = p.Life - LifeDec;}
                    )
                    |> List.filter( fun p -> p.Life > 0.)

                if particles.IsEmpty then
                  {model with Screen=NextScreen; Particles = []; BackgroundAnimation=None }
                else
                  {model with Particles = particles }

            | None -> model

          printfn "%A" model.Screen

          match model.Screen with
          | NextScreen ->
              let screen = model.Screens.[model.CurrentIndex]
              {model with Screen=screen; CurrentIndex = model.CurrentIndex + 1}

          | ClearScreen ->
            let sat = int (Math.random() * 100.)
            {model with Screen = LaunchPainting (Flows sat);}

          | Start -> model

          | Transition ->
            let particles =
              let radius = 10
              let max = (int model.CanvasInfo.Width / radius)
              [
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
                    LifeDec=0.1
                    PerlinCoeff=1.
                    Color = "rgba(255, 255, 255, 0.05)"
                  }
                yield
                  p
              ]

            {model with Particles=particles; Screen = DoNothing; BackgroundAnimation=Some BlackWave }

          | DoNothing -> model
          | GoNextFrame -> { model with ScreenContent={Text=""}; Screen = DoNothing}
          | LaunchPainting kind ->

            let particles =
              match kind with
              | Flows saturation ->
                  let coeff = (100. + 100. * Math.random()) * 0.00001
                  //let coeff = 0.00125
                  printfn "%f" coeff
                  [
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
                        Life=50.//Math.random() * 1000.
                        Speed=ParticleSpeed
                        Composition=Bottom
                        Size=1.5
                        LifeDec=0.01
                        PerlinCoeff=coeff// 0.00125
                        Color=(sprintf "hsla(%f, %i%%, 80%%, 0.06)" (Math.floor(v * 360.)) saturation )
                      }
                      yield
                        p
                  ]
              | _ -> []


            let sat = int (Math.random() * 100.)
            {model with Particles=model.Particles @ particles; Screen = GoNextFrame; BackgroundAnimation=Some (Flows sat) }
          | DisplayText text ->
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
