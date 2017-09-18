module App

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.JS
open State
open View
open Elmish
open Elmish.Worker

let init() =
    let info = View.initCanvas()

    Program.mkProgram (fun () -> State.initModel info) State.update View.render
//    |> Program.withAnimationWorker
    |> Program.run

init()
