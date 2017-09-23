module View

open System
open State
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.Import.JS

let [<Literal>] Ratio = 1.0 // full screen
let [<Literal>] BaseWidth = 1920.

let initCanvas() =

    // initialise our Perlin noise
    Perlin.seed(Math.random())

    let width = window.innerWidth  * window.devicePixelRatio * Ratio
    let height = window.innerHeight * window.devicePixelRatio * Ratio

    let drawingCanvas = document.getElementById("drawingcanvas") :?> HTMLCanvasElement
    drawingCanvas.width <- width
    drawingCanvas.height <- height

    let textCanvas = document.getElementById("textcanvas") :?> HTMLCanvasElement
    textCanvas.width <- width
    textCanvas.height <- height

    {
      TextContext = textCanvas.getContext_2d()
      DrawingContext = drawingCanvas.getContext_2d()
      Width = width
      Height = height
      ScaleFactor = width / BaseWidth
    }

let render (model: Model) (dispatch: Msg->unit) =

  let fontToRatio size =
    sprintf "%ipx Quicksand" (int (size * model.CanvasInfo.ScaleFactor))

  if model.Initialized then

      match model.Screen with

      | ClearScreen which ->

        match which with
        | TopScreen ->
          let ctx = model.CanvasInfo.TextContext//
          ctx.clearRect(0.,0.,model.CanvasInfo.Width, model.CanvasInfo.Height)

        | BottomScreen ->
          let ctx = model.CanvasInfo.DrawingContext//
          ctx.clearRect(0.,0.,model.CanvasInfo.Width, model.CanvasInfo.Height)

      | _ ->


        model.BottomParticles
          |> Seq.iter( fun p ->

            // dynamic color using perlin noise
            if model.BackgroundAnimation.IsSome then
              let kind = model.BackgroundAnimation.Value
              let ctx = model.CanvasInfo.DrawingContext

              match p.Composition with
              | Top ->
                // play anim on top of everything
                ctx.globalCompositeOperation <- "source-over"
              | Bottom ->
                // play anim at the back
                ctx.globalCompositeOperation <- "destination-over"

              match kind with
              | Flows sat ->

                ctx.fillStyle <- !^ p.Color
                ctx.fillRect(p.X, p.Y, p.Size, p.Size)

              | _ -> printfn ""
          )

        if model.TopAnimation.IsSome then
          let ctx = model.CanvasInfo.TextContext
          model.TopParticles
            |> Seq.iter( fun p ->

                let kind = model.TopAnimation.Value
                match kind with
                | ShowTitle ->

                  // draw text at the center of the screen
                  ctx.font <- fontToRatio p.Size
                  let color = ( sprintf "rgba(255,255,255,%f)" p.Alpha)
                  printfn "%s" color
                  ctx.fillStyle <- !^ color
                  let textWidth = ctx.measureText(p.Text).width
                  ctx.textBaseline <- "middle"
                  let x = model.CanvasInfo.Width * 0.5 - textWidth * 0.5
                  let y = model.CanvasInfo.Height * 0.5
                  let height = 500.
                  let mid = height * 0.5
                  ctx.clearRect(x,y - mid,textWidth, height)
                  ctx.fillText( p.Text, x, y)

                | TextLabel ->

                  // draw text at the center of the screen
                  ctx.font <- fontToRatio p.Size
                  let textWidth = ctx.measureText(p.Text).width
                  ctx.textBaseline <- "middle"
                  let height = p.Size * 1.3
                  ctx.fillStyle <- !^ "black"
                  let y = p.Y - height * 0.5
                  let y = if y < 0. then Math.random() * 150. else y
                  ctx.fillRect(p.X, y-height * 0.5, textWidth, height);
                  ctx.fillStyle <- !^ "white"
                  ctx.fillText( p.Text, p.X, y)

                | _ -> printfn ""
          )



