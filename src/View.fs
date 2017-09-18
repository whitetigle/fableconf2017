module View

open System
open State
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.Import.JS

let [<Literal>] Radius = 1.5

let initCanvas() =
    let canvas = document.getElementsByTagName_canvas().[0]
    canvas.width <- window.innerWidth  * window.devicePixelRatio * 0.9
    canvas.height <- window.innerHeight * window.devicePixelRatio * 0.9
    let ctx = canvas.getContext_2d()

    Perlin.seed(Math.random())

    { Context = ctx
      Width = canvas.width
      Height = canvas.height
    }

let render (model: Model) (dispatch: Msg->unit) =
    if model.Initialized then
        let ctx = model.CanvasInfo.Context

        match model.Screen with
        | DisplayText text ->

          // draw text on top
          ctx.globalCompositeOperation <- "source-over"

          // draw text at the center of the screen
          ctx.font <- "90px Quicksand"
          ctx.fillStyle <- !^ "rgba(255,255,255,0.8)"
          let textWidth = ctx.measureText(text).width
          ctx.textBaseline <- "middle"
          ctx.fillText( text, model.CanvasInfo.Width * 0.5 - textWidth * 0.5, model.CanvasInfo.Height * 0.5)

        | ClearScreen ->
          ctx.clearRect(0.,0.,model.CanvasInfo.Width, model.CanvasInfo.Height)
        | _ ->

          model.Particles
            |> List.iter( fun p ->
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

                | BlackWave ->
                  ctx.clearRect(0.,0.,model.CanvasInfo.Width, p.Y - p.Size*0.5)
//                  ctx.fillStyle <- !^ "red"
                  ctx.fillStyle <- !^ p.Color
                  ctx.font <- sprintf "%ipx Quicksand" (int p.Size)
                  let text = "Fable"
                  let textWidth = ctx.measureText(text).width
                  ctx.fillText( text, p.X + model.CanvasInfo.Width * 0.5 - textWidth * 0.5, p.Y)


              //ctx.save()
            )



