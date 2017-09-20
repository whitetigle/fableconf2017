module View

open System
open State
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.Import.JS

let [<Literal>] Ratio = 1.0 // full screen

let initCanvas() =

    let drawingCanvas = document.getElementsByTagName_canvas().[0]
    drawingCanvas.width <- window.innerWidth  * window.devicePixelRatio * Ratio
    drawingCanvas.height <- window.innerHeight * window.devicePixelRatio * Ratio
    let ctx = drawingCanvas.getContext_2d()

    let textCanvas = document.getElementsByTagName_canvas().[1]
    textCanvas.width <- drawingCanvas.width
    textCanvas.height <- drawingCanvas.height
    let tctx = textCanvas.getContext_2d()

    Perlin.seed(Math.random())

    {
      TextContext = tctx
      DrawingContext = ctx
      Width = drawingCanvas.width
      Height = drawingCanvas.height
    }

let render (model: Model) (dispatch: Msg->unit) =
    if model.Initialized then

        match model.Screen with
        | DisplayText text ->

          let ctx = model.CanvasInfo.TextContext

          // draw text on top
//          ctx.globalCompositeOperation <- "source-over"

          // draw text at the center of the screen
          let fontSize = model.CanvasInfo.Width / 15.
          ctx.font <- sprintf "%ipx Quicksand" (int fontSize)
          ctx.fillStyle <- !^ "rgba(255,255,255,0.7)"
          let textWidth = ctx.measureText(text).width
          ctx.textBaseline <- "middle"
          ctx.fillText( text, model.CanvasInfo.Width * 0.5 - textWidth * 0.5, model.CanvasInfo.Height * 0.5)

        | ClearScreen ->

          let ctx = model.CanvasInfo.TextContext
          ctx.clearRect(0.,0.,model.CanvasInfo.Width, model.CanvasInfo.Height)

        | _ ->

          let ctx = model.CanvasInfo.DrawingContext

          model.Particles
            |> Seq.iter( fun p ->
              //ctx.fillStyle <- !^ "red"
              match p.Composition with
              | Top ->
                // play anim on top of everything
                ctx.globalCompositeOperation <- "source-over"
              | Bottom ->
                // play anim at the back
                ctx.globalCompositeOperation <- "destination-over"

              // dynamic color using perlin noise
              if model.BackgroundAnimation.IsSome then
                let kind = model.BackgroundAnimation.Value
                match kind with
                | Flows sat ->
                  ctx.fillStyle <- !^ p.Color
                  ctx.fillRect(p.X, p.Y, p.Size, p.Size)

                | FableCurtain text ->
                  ctx.clearRect(0.,0.,model.CanvasInfo.Width, p.Y - p.Size*0.5)
//                  ctx.fillStyle <- !^ "red"
                  ctx.fillStyle <- !^ p.Color
                  ctx.font <- sprintf "%ipx Quicksand" (int p.Size)
                  let textWidth = ctx.measureText(text).width
                  ctx.fillText( text, p.X + model.CanvasInfo.Width * 0.5 - textWidth * 0.5, p.Y)

                | _ -> printfn ""
                (*
                | ShowTitle text ->

                  // draw text at the center of the screen
                  ctx.font <- "90px Quicksand"
                  ctx.fillStyle <- !^ ( sprintf "rgba(255,255,255,%f)" p.Alpha)
                  let textWidth = ctx.measureText(text).width
                  ctx.textBaseline <- "middle"
                  ctx.fillText( text, model.CanvasInfo.Width * 0.5 - textWidth * 0.5, model.CanvasInfo.Height * 0.5)
                *)


              //ctx.save()
            )



