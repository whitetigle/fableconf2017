module State

open System
open Fable.Core
open Fable.Import.Browser
open Fable.Import.JS
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
  Text: string
}

let EmptyParticle =
  {
    X=0.
    Y= 0.
    startX=0.
    startY=0.
    A=0.
    V=0.
    Life=0.
    Speed=0.
    Composition=Top
    Size=0.
    LifeDec=0.
    PerlinCoeff=0.
    Color = ""
    Alpha = 0.
    Text = ""
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
  | FableCurtain
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
  | PopText of (Text*Probability) []

type ScreenContent = {
  Text: string
}

type Model =
    { Keys: ControlKeys
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

type Msg =
  | KeyDown of code: float
  | NewFrame
  | Resize
  | OnClick

let subscribeToKeyEvents dispatch =
    window.addEventListener_keydown(fun ev ->
        KeyDown ev.keyCode |> dispatch; null)

let subscribeToMouseClickEvents dispatch =
    window.addEventListener_click(fun ev ->
        OnClick |> dispatch; null)

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
    let chapters =
      [
        ["Hello!"]
      ]
    let model =
        { Keys = { Up=false; Left=false; Right=false }
          Initialized = true
          X= 0.
          BottomParticles=[||]
          TopParticles=[||]
          Screen = Start
          Screens =
            [
              ClearScreen BottomScreen
              DisplayText "Hello!"
              ClearScreen TopScreen
              DisplayText "François"
              ClearScreen TopScreen
              AddLabel "Full"
              AddLabel "Stack"
              AddLabel "Developer"

              ClearScreen BottomScreen
              ClearScreen TopScreen
              DisplayText "Time with my kids!"
              ClearScreen TopScreen
              AddLabel "A few years ago..."
              AddLabel "kids: want to play daddy?"
              AddLabel "Me: no. Too tired."
              AddLabel "kids: later maybe?"
              AddLabel "Me: I don't know. "
              AddLabel "Me: ask your Mum!"
              AddLabel "kids: ok..."
              ClearScreen TopScreen
              AddLabel "..."
              AddLabel "..."
              AddLabel "..."
              AddLabel "My mind was like..."
              AddLabel "work"
              AddLabel "work"
              AddLabel "work"
              ClearScreen TopScreen
              DisplayText "I had to change this!"
              ClearScreen TopScreen
              DisplayText "So here's my dev story..."

              ClearScreen BottomScreen
              ClearScreen TopScreen
              DisplayText "I want so many things!"

              ClearScreen TopScreen
              AddLabel "A good job"
              AddLabel "Work with great people"
              AddLabel "On interesting projects"
              AddLabel "Keep learning"
              AddLabel "Teach as well"
              AddLabel "Earn enough money"
              AddLabel "But also..."
              ClearScreen TopScreen
              AddLabel "Enjoy time with my family"
              AddLabel "Hang out with Friends"
              ClearScreen TopScreen
              AddLabel "JUST"
              AddLabel "BE"
              AddLabel "ABLE"
              AddLabel "TO"
              AddLabel "ENJOY"
              AddLabel "MY LIFE"
              ClearScreen TopScreen
              DisplayText "Yes. Very personal goals."
              ClearScreen TopScreen
              DisplayText "How could I reach them?"
              ClearScreen TopScreen
              DisplayText "How Fable did help me?"
              ClearScreen TopScreen
              DisplayText "It's a short story"
              ClearScreen TopScreen
              DisplayText "Once upon a time..."

              ClearScreen BottomScreen
              ClearScreen TopScreen
              DisplayText "2000-2007"
              ClearScreen TopScreen
              DisplayText "Before I got kids"
              ClearScreen TopScreen
              AddLabel "Worked a lot"
              AddLabel "Worked a lot"
              AddLabel "Developed Video Games!"
              AddLabel "Learnt a lot"
              AddLabel "Met great people"
              AddLabel "created great projects"
              AddLabel "and eventually..."

              ClearScreen BottomScreen
              ClearScreen TopScreen
              DisplayText "Got Kids!"
              ClearScreen TopScreen
              DisplayText "2007-2010"
              ClearScreen TopScreen
              AddLabel "Worked a lot"
              AddLabel "made some crazy money"
              AddLabel "Worked a lot"
              AddLabel "Made more Video Games!"
              AddLabel "Worked a lot"
              ClearScreen TopScreen
              AddLabel "Kids?"
              AddLabel "Sorry honey..."
              AddLabel "Too much things to do..."
              AddLabel "..."
              AddLabel "it sucks"
              AddLabel "failure"

              ClearScreen TopScreen
              ClearScreen BottomScreen
              DisplayText "Failure?"
              ClearScreen TopScreen
              DisplayText "2010"
              ClearScreen TopScreen
              DisplayText "Enter one more kid"
              ClearScreen TopScreen
              DisplayText "Bye Bye Great Job"
              ClearScreen TopScreen
              DisplayText "One year off"
              ClearScreen TopScreen
              AddLabel "Family"
              AddLabel "Kids"
              AddLabel "Friends"
              AddLabel "Enjoying life"
              AddLabel "BUT"
              ClearScreen TopScreen
              DisplayText "NO MONEY"

              ClearScreen TopScreen
              ClearScreen BottomScreen
              DisplayText "2011-2014"
              ClearScreen TopScreen
              DisplayText "Entrepreneur!"
              ClearScreen TopScreen
              AddLabel "Let's do it!"
              AddLabel "Make video games again!"
              AddLabel "Indie video Games!"
              AddLabel "Yeah!"
              AddLabel "but take time"
              AddLabel "with my family"
              AddLabel "..."
              ClearScreen TopScreen
              DisplayText "Working hard"
              ClearScreen TopScreen
              DisplayText "Did excellent projects!"
              ClearScreen TopScreen
              DisplayText "Met with Awesome people"
              ClearScreen TopScreen
              DisplayText "but..."
              ClearScreen TopScreen
              DisplayText "Ran out of money!"
              ClearScreen TopScreen
              AddLabel "Failure"

              ClearScreen TopScreen
              ClearScreen BottomScreen
              DisplayText "2014-now"
              ClearScreen TopScreen
              DisplayText "Work & Family"
              ClearScreen TopScreen
              AddLabel "Freelancing / consulting"
              AddLabel "Teaching"
              AddLabel "Work with great people"
              AddLabel "..."
              AddLabel "earn enough money"
              ClearScreen TopScreen
              AddLabel "Work hard"
              AddLabel "Yet have time"
              AddLabel "for family!"
              AddLabel "for friends!"
              AddLabel "for enjoying great wine"
              AddLabel "in front of the beach"
              AddLabel "Beautiful sunshines"
              AddLabel "Stars in my eyes..."
              ClearScreen TopScreen
              AddLabel "HOW IS THAT POSSIBLE?"

              ClearScreen TopScreen
              ClearScreen BottomScreen // TODO: add random
              DisplayText "Changes for the best!"
              ClearScreen TopScreen
              AddLabel "XP: 15+ years"
              AddLabel "Still fond of learning"
              AddLabel "Still fond of solving problems"
              AddLabel "Still fond of being a developer"
              ClearScreen TopScreen
              DisplayText "THINK & WORK BETTER"
              ClearScreen TopScreen
              AddLabel "Avoid vapor-projects"
              AddLabel "Avoid projects with shallow requirements"
              AddLabel "9 months max for any project"
              AddLabel "Enough XP for any project"
              ClearScreen TopScreen
              AddLabel "web apps"
              AddLabel "desktop apps"
              AddLabel "mobile apps"
              AddLabel "IOT"
              AddLabel "Cloud"
              ClearScreen TopScreen
              DisplayText "DEVELOP"
              ClearScreen TopScreen
              AddLabel "Way Faster!"
              AddLabel "Way Safer!"
              AddLabel "Stable!"
              AddLabel "Release bug free apps!"
              AddLabel "...almost..."
              ClearScreen TopScreen
              DisplayText "Projects 70% JS based"
              ClearScreen TopScreen
              DisplayText "BYE BYE OOP!"
              ClearScreen TopScreen
              DisplayText "FUNCTIONAL!"
              ClearScreen TopScreen
              AddLabel "Haxe"
              AddLabel "Scala"
              AddLabel "And then..."
              ClearScreen TopScreen
              DisplayText "Love is in the air!"

              ClearScreen TopScreen
              ClearScreen BottomScreen // TODO: add random
              DisplayText "F# + Fable"
              ClearScreen TopScreen
              AddLabel "why?"
              AddLabel "Great JS interop!"
              AddLabel "Great gateway to .NET"
              AddLabel "More things to add?"
              ClearScreen TopScreen
              DisplayText "DEFINITELY!"
              ClearScreen TopScreen
              DisplayText "The Great Fable Stack"
              ClearScreen TopScreen
              AddLabel "F#"
              AddLabel "Dotnet"
              AddLabel "nodeJS"
              AddLabel "Paket"
              AddLabel "npm/yarn"
              AddLabel "babel"
              AddLabel "webpack"
              AddLabel "IONIDE!"

              ClearScreen TopScreen
              ClearScreen BottomScreen // TODO: add random
              DisplayText "F#?"
              ClearScreen TopScreen
              AddLabel "easy to learn functional with"
              AddLabel "indentation based"
              AddLabel "Easy to read"
              AddLabel "Easy to understand"
              AddLabel "Now"
              AddLabel "And... LATER!"
              AddLabel "Easy to refactor"
              AddLabel "a lot of info"
              AddLabel "in just few lines of code"
              AddLabel "Excellent documentation"
              AddLabel "Last but not least..."
              ClearScreen TopScreen
              DisplayText "NO SEMICOLONS!"
              ClearScreen TopScreen
              AddLabel "crossplatform"
              AddLabel ".NET"
              AddLabel "JS"

              ClearScreen TopScreen
              ClearScreen BottomScreen // TODO: add random
              DisplayText "Fablulous tools!"
              ClearScreen TopScreen
              AddLabel "FAST TOOLS SETUP"
              AddLabel "Fable templates!"
              AddLabel "Fable-Suave scaffolding!"
              ClearScreen TopScreen
              AddLabel "IONIDE + VSCODE"
              AddLabel "easy to refactor"
              AddLabel "explore project"
              AddLabel "syntax highlighting"
              AddLabel "error highlighting"
              AddLabel "find usages"
              AddLabel "..."
              AddLabel "and so much more!"

              ClearScreen TopScreen
              ClearScreen BottomScreen // TODO: add random
              DisplayText "Documentation"
              ClearScreen TopScreen
              AddLabel "A little bit young but..."
              AddLabel "Fable web site"
              AddLabel "samples"
              AddLabel "Awesome-Fable web site"
              AddLabel "GitHub"
              AddLabel "very clean source code"
              AddLabel "very clean unit tests!"
              ClearScreen BottomScreen // TODO: add random
              ClearScreen TopScreen
              DisplayText "Work in progress"

              ClearScreen TopScreen
              ClearScreen BottomScreen // TODO: add random
              DisplayText "Community!"
              ClearScreen TopScreen
              AddLabel "Community first!"
              AddLabel "Open source!"
              AddLabel "Gitter"
              AddLabel "Slack"
              AddLabel "Github"
              AddLabel "Twitter"
              AddLabel "Great People"
              AddLabel "Funny People"
              AddLabel "Funny People with long hair!"

              ClearScreen TopScreen
              ClearScreen BottomScreen // TODO: add random
              DisplayText "10 months of Fable later..."
              ClearScreen TopScreen
              AddLabel "3 pixi.js based projects"
              AddLabel "1 local multiplayer game"
              AddLabel "1 SPA: JA network youth contest"
              AddLabel "1 SPA: Haïti environmental issues"
              AddLabel "1 Fable+Suave 'client-server' app"
              AddLabel "so many side projects..."

              ClearScreen TopScreen
              ClearScreen BottomScreen // TODO: add random
              DisplayText "In the end"
              ClearScreen TopScreen
              AddLabel "Develop faster?"
              AddLabel "YES!"
              AddLabel "Develop safer?"
              AddLabel "YES!"
              AddLabel "Less bugs?"
              AddLabel "YES!"
              ClearScreen TopScreen
              AddLabel "Fable is young"
              AddLabel "Promising technology"
              AddLabel "Real Challenger"
              AddLabel "in the JS transpiler competition"
              AddLabel "Backed up by awesome people"
              ClearScreen TopScreen
              AddLabel "Me: ok kids, let's play together"
              AddLabel "kids: finished your work, already?"
              AddLabel "Me: yes."
              ClearScreen TopScreen
              DisplayText "It's ready"
              ClearScreen TopScreen
              DisplayText "It's clean"
              ClearScreen TopScreen
              DisplayText "It just works..."
              ClearScreen TopScreen
              DisplayText "FABLE"

              ClearScreen TopScreen
              ClearScreen BottomScreen // TODO: add random
              DisplayText "Thanks!"
              ClearScreen TopScreen
              DisplayText "Now let's have some great food!"
              ClearScreen BottomScreen // TODO: add random

              (*
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
              *)

            ]
          CurrentIndex = 0
          ScreenContent = { Text="Hello!"}
          CanvasInfo=canvasinfo
          BackgroundAnimation = None
          TopAnimation = None
        }
    model, [subscribeToFrames; subscribeToResize; subscribeToMouseClickEvents]
//    model, [subscribeToKeyEvents; subscribeToFrames; subscribeToResize; subscribeToMouseClickEvents]

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
                let a = v * 2. * Math.PI + p.A
                let color = (sprintf "hsla(%f, %i%%, 80%%, 0.06)" (Math.floor(v * 360.)) saturation )
                p.X <- p.X + Math.cos(a) * p.Speed
                p.Y <-  p.Y + Math.sin(a) * p.Speed
                p.V <- v
                p.Life <-  p.Life - p.LifeDec
                p.Color <- color

              let particles = model.BottomParticles |> Seq.filter(fun p -> p.Life > 0.)
              if particles |> Seq.length <=0 then
                {model with BottomParticles = [||]; BackgroundAnimation=None }
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

            | FableCurtain ->

              let l = model.BottomParticles |> Seq.length
              for i in 0..(l-1) do
                let p = model.BottomParticles.[i]
                let a = p.A + p.Speed
                p.X <- p.startX + Math.cos(a) * 3.0
                p.Y <- p.Y + p.Speed
                p.Life <- p.Life - p.LifeDec
                p.A <- a

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
              let particles = model.BottomParticles |> Seq.filter(fun p -> p.Life > 0.)
              if particles |> Seq.length <=0 then
                {model with Screen=NextScreen; BottomParticles = [||]; BackgroundAnimation=None }
              else model
//                  {model with Particles = particles }

          | None -> model

        let model =
          match model.TopAnimation with
          | Some kind ->
            match kind with
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

            | TextLabel  ->

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

        printfn "%A" model.Screen

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
          let sat = int (Math.random() * 100.)
          {model with Screen = LaunchPainting (Flows sat); BackgroundAnimation=None }

        | DoNothing -> model
        | GoNextFrame -> { model with ScreenContent={Text=""}; Screen = DoNothing}
        | LaunchPainting kind ->

          let particles =
            match kind with
            | Flows saturation ->
                let test = Math.random() * 5. + 1.
                let test = 100. * test
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
                      Text = ""
                    }
                    yield
                      p
                |]
            | _ -> [||]


          let sat = int (Math.random() * 100.)
          let ps = (model.BottomParticles |> Seq.toList) @ (particles |> Seq.toList)
          let psa = ps |> Seq.toArray

          {model with BottomParticles=psa; Screen = NextScreen; BackgroundAnimation=Some (Flows sat) }
        | DisplayText text ->

          let particles =
            [|
                { EmptyParticle with LifeDec=0.1;Alpha=0.1; Size=150.; Text=text }
            |]

          // TODO: hopefully we can do concat operation way better!!
          let ps = (model.TopParticles |> Seq.toList) @ (particles |> Seq.toList)
          let psa = ps |> Seq.toArray
          {model with TopParticles= psa; Screen = DoNothing; TopAnimation=Some ShowTitle }

        | AddLabel text ->

          let xmargin = model.CanvasInfo.Width * 0.2
          let ymargin = model.CanvasInfo.Height * 0.2
          let x = ( model.CanvasInfo.Width * 0.4) * Math.random()
          let y = ( model.CanvasInfo.Height - ymargin * 2.) * Math.random()
          let y = if y < 50. then 500. * Math.random() else y
          let particles =
            [|
                { EmptyParticle with LifeDec=10.; Size=90.; Text=text; X=x;Y=y }
            |]

          // TODO: hopefully we can do concat operation way better!!
          let ps = (model.TopParticles |> Seq.toList) @ (particles |> Seq.toList)
          let psa = ps |> Seq.toArray
          {model with TopParticles= psa; Screen = DoNothing; TopAnimation=Some TextLabel }

        | PopText data ->

          let particles =
            [|
              for i in 0..100 do
                let rnd = System.Random()
                let next = data.[rnd.Next(data.Length)]
                let text = fst next
                let x = model.CanvasInfo.Width * Math.random()
                let y = model.CanvasInfo.Height * Math.random()
                let p = { EmptyParticle with Life=Math.random() * 100.; LifeDec=0.1;Alpha=0.0; Size=50.; Text=text; X=x;Y=y }
                yield p
            |]

          // TODO: hopefully we can do concat operation way better!!
          let ps = (model.TopParticles |> Seq.toList) @ (particles |> Seq.toList)
          let psa = ps |> Seq.toArray
          {model with TopParticles= psa; Screen = DoNothing; TopAnimation=Some TextLabel }


//          { model with ScreenContent = {model.ScreenContent with Text=text}; Screen = GoNextFrame }

      | OnClick -> proceedToNextScreen

      | KeyDown code -> proceedToNextScreen

  model, []
