module State

open System
open Fable.Core
open Fable.Import
open Fable.Import.Browser
open System.Collections.Generic

type CanvasInfo =
    {
      TextContext: CanvasRenderingContext2D
      DrawingContext: CanvasRenderingContext2D
      Width: float
      Height: float
      ScaleFactor: float
    }

let [<Literal>] Radius = 200.
let [<Literal>] ParticleSpeed = 2.

// Here mutability is used so that the rendering remains smooths
// that's often the case with Javascript
type Particle = {
  mutable X:float
  mutable Y:float
  mutable A:float
  mutable V:float
  mutable Life:float
  mutable Speed:float
  mutable Size:float
  mutable LifeDec:float
  mutable PerlinCoeff:float
  mutable Color:string
  mutable Alpha:float
  Text: string
}

let EmptyParticle =
  {
    X=0.
    Y= 0.
    A=0.
    V=0.
    Life=0.
    Speed=0.
    Size=0.
    LifeDec=0.
    PerlinCoeff=0.
    Color = ""
    Alpha = 0.
    Text = ""
  }

type Saturation = int
type PaintingKind =
  | Flows of Saturation
  | ShowTitle
  | TextLabel

type Text = string
type Probability = float

type Layer = {
  Context : CanvasRenderingContext2D
  Animation : PaintingKind option
  Particles : Particle []
}

type ScreenLayer =
  | TopScreen
  | BottomScreen

type Screen =
  | Start
  | StartBackground
  | DisplayText of string
  | AddLabel of string
  | LaunchPainting of PaintingKind
  | DoNothing
  | GoNextFrame
  | ClearScreen of ScreenLayer
  | NextScreen

type ScreenContent = {
  Text: string
}

type Model =
    {
      Initialized: bool
      X : float
      BottomParticles : Particle []
      TopParticles : Particle []
      Screen : Screen
      Screens : Screen list
      CurrentIndex : int
      ScreenContent: ScreenContent
      CanvasInfo : CanvasInfo
      BackgroundAnimation: PaintingKind option
      TopAnimation: PaintingKind option
    }


// ---------------------------* OUR EVENTS  *---------------------------


// These are our Elmish subscriptions
module ElmishSubscriptions =

  // We'll subscribe to the following Messages
  type Msg =
    | KeyDown of code: float
    | NewFrame
    | Resize
    | OnClick

  // Actually we won't be using keys, but let's add it for the fun
  // TODO: add behaviour when pressing a key
  let subscribeToKeyEvents dispatch =
      window.addEventListener_keydown(fun ev ->
          KeyDown ev.keyCode |> dispatch; null)

  let subscribeToMouseClickEvents dispatch =
      window.addEventListener_click(fun ev ->
          OnClick |> dispatch; null)

  // Actually we won't be handling resize events
  // TODO: add behaviour on resize event
  let subscribeToResize dispatch =
      window.addEventListener_resize(fun ev ->
          Resize |> dispatch; null)

  // we ask for canvas updates every frame
  // so that we can have our particles actually move
  let subscribeToFrames dispatch =
      let run =
        let rec run (dt:float) =
          window.requestAnimationFrame(FrameRequestCallback run) |> ignore
          NewFrame |> dispatch
        run
      run 0.0

// ---------------------------* INIT *---------------------------

let init (canvasinfo:CanvasInfo) =

    let model =
        {
          Initialized = true
          X= 0.
          BottomParticles=[||]
          TopParticles=[||]
          Screen = Start
          Screens =
            [
              DisplayText "Click to start..."
              ClearScreen BottomScreen
              DisplayText "Fun With Canvas & Elmish"
              ClearScreen TopScreen
              AddLabel "Highly Immutable"
              AddLabel "Enthusiastic"
              AddLabel "Elmish Powered"
              AddLabel "Fun Fable Presentation"
              ClearScreen TopScreen

              //ClearScreen TopScreen // to clear text canvas
              //ClearScreen BottomScreen // to clear drawing canvas
              //Displaytext "mytext" // to display the text using a simple linear fade effect
              //AddLabel "mytext" // to display the text using some black labels

            ]
          CurrentIndex = 0
          ScreenContent = { Text="Hello!"}
          CanvasInfo=canvasinfo
          BackgroundAnimation = None
          TopAnimation = None
        }
    model, [ElmishSubscriptions.subscribeToFrames; ElmishSubscriptions.subscribeToResize; ElmishSubscriptions.subscribeToMouseClickEvents]

// ---------------------------* UPDATE *---------------------------

open ElmishSubscriptions
let update (msg: Msg) (model: Model) =

  let proceedToNextScreen = {model with Screen=NextScreen }

  let model =
      match msg with
      | Resize -> model

      | NewFrame ->

        let model =
          match model.BackgroundAnimation with
          | Some kind ->
            match kind with
            | Flows saturation->

              let l = model.BottomParticles |> Seq.length
              for i in 0..(l-1) do
                let p = model.BottomParticles.[i]
                let coeff = p.PerlinCoeff
                let v = Perlin.perlin2( p.X * coeff, p.Y * coeff )
                let a = v * 2. * JS.Math.PI + p.A
                let color = (sprintf "hsla(%f, %i%%, 80%%, 0.06)" (JS.Math.floor(v * 360.)) saturation )
                p.X <- p.X + JS.Math.cos(a) * p.Speed
                p.Y <-  p.Y + JS.Math.sin(a) * p.Speed
                p.V <- v
                p.Life <-  p.Life - p.LifeDec
                p.Color <- color

              let particles = model.BottomParticles |> Seq.filter(fun p -> p.Life > 0.)
              if particles |> Seq.length <=0 then
                {model with BottomParticles = [||]; BackgroundAnimation=None }
              else
                model

          | None -> model

        let model =
          match model.TopAnimation with
          | Some kind ->
            match kind with

            // fade in title animation
            // update our alpha values to create the fadein effect
            | ShowTitle  ->

              let l = model.TopParticles |> Seq.length
              for i in 0..(l-1) do
                let p = model.TopParticles.[i]
                let alpha = if p.Alpha <=1.0 then p.Alpha + p.LifeDec else 1.0
                p.Alpha <- alpha
                if p.Alpha >= 1.0 then p.Life <- -1.0

              let particles = model.TopParticles |> Seq.filter(fun p -> p.Life >= 0.)
              if particles |> Seq.length <=0 then
                {model with TopParticles = [||]; TopAnimation=None }
              else model

            |  _ -> model
          | None -> model

        match model.Screen with
        | NextScreen ->
            if model.CurrentIndex < model.Screens.Length then
              let screen = model.Screens.[model.CurrentIndex]
              {model with Screen=screen; CurrentIndex = model.CurrentIndex + 1}
            else model

        | ClearScreen which->
          match which with
          | TopScreen ->
            {model with Screen = DoNothing; TopParticles = [||]}
          | BottomScreen ->
            {model with Screen = StartBackground; BottomParticles = [||]}

        | Start -> model

        | StartBackground ->
          let sat = int (JS.Math.random() * 100.)
          {model with Screen = LaunchPainting (Flows sat); BackgroundAnimation=None }

        | DoNothing -> model
        | GoNextFrame -> { model with ScreenContent={Text=""}; Screen = DoNothing}
        | LaunchPainting kind ->

          let particles =
            match kind with
            | Flows saturation ->
                let test = JS.Math.random() * 5. + 1.
                let test = 100. * test
                let coeff = (100. + 100. * JS.Math.random()) * 0.00001
                //let coeff = 0.00125
                printfn "%f" coeff
                [|
                  for i in 0..10000 do
                    let x = JS.Math.random() * model.CanvasInfo.Width
                    let y = JS.Math.random() * model.CanvasInfo.Height
                    let v = Perlin.perlin2( x * coeff, y * coeff )
                    let p = {
                      X=x
                      Y=y
                      A=0.//JS.Math.random() * 1000.
                      V=0.
                      Life=10.//JS.Math.random() * 1000.
                      Speed=ParticleSpeed
                      Size=1.5
                      LifeDec=0.01
                      PerlinCoeff=coeff// 0.00125
                      Color=(sprintf "hsla(%f, %i%%, 80%%, 0.06)" (JS.Math.floor(v * 360.)) saturation )
                      Alpha=1.0
                      Text = ""
                    }
                    yield
                      p
                |]
            | _ -> [||]


          let sat = int (JS.Math.random() * 100.)
          let psa = [model.BottomParticles;particles] |> Array.concat

          {model with BottomParticles=psa; Screen = NextScreen; BackgroundAnimation=Some (Flows sat) }

        // Display
        | DisplayText text ->

          let particles =
            [|
                { EmptyParticle with LifeDec=0.1;Alpha=0.1; Size=150.; Text=text }
            |]

          // TODO: hopefully we can do concat operation way better!!
          let psa = [model.TopParticles;particles] |> Array.concat
          {model with TopParticles= psa; Screen = DoNothing; TopAnimation=Some ShowTitle }

        | AddLabel text ->

          let xmargin = model.CanvasInfo.Width * 0.2
          let ymargin = model.CanvasInfo.Height * 0.2
          let x = model.CanvasInfo.Width * 0.1 + ( model.CanvasInfo.Width * 0.4) * JS.Math.random()
          let y = ( model.CanvasInfo.Height - ymargin * 2.) * JS.Math.random()
          let y = if y < 50. then 500. * JS.Math.random() else y
          let particles =
            [|
                { EmptyParticle with LifeDec=10.; Size=90.; Text=text; X=x;Y=y }
            |]

          let psa = [model.TopParticles;particles] |> Array.concat
          {model with TopParticles= psa; Screen = DoNothing; TopAnimation=Some TextLabel }

      | OnClick -> proceedToNextScreen

      | ElmishSubscriptions.KeyDown code -> proceedToNextScreen

  model, []
