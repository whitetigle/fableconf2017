module View

open System
open State
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.Import.JS


let initCanvas() =
    let canvas = document.getElementsByTagName_canvas().[0]
    canvas.width <- window.innerWidth  * window.devicePixelRatio * 0.9
    canvas.height <- window.innerHeight * window.devicePixelRatio * 0.9
    let ctx = canvas.getContext_2d()

    ctx.fillStyle <- !^ "black"
    ctx.fillRect(0., 0., canvas.width, canvas.height)

    Perlin.seed(Math.random())

    { Context = ctx
      Width = canvas.width
      Height = canvas.height
    }

let render(info: CanvasInfo) (model: Model) (dispatch: Msg->unit) =
    if model.Initialized then
        let ctx = info.Context

        model.Particles
          |> List.iter( fun p ->
            ctx.fillStyle <- !^ (sprintf "hsla(%f, 95%%, 80%%, 0.05)" (Math.floor(p.V * 360.)) )
            ctx.fillRect(p.X, p.Y, 1.5, 1.5)
          )

