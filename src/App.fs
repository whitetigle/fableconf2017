module App

open State
open View
open Elmish

let init() =
    // we need to init our model with the canvas info
    // so that we get access to the available size for instance
    let info = View.initCanvas()

    // Start Elmish!
    Program.mkProgram (fun () -> State.init info) State.update View.render
    |> Program.run

init()
