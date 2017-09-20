module App

open State
open View
open Elmish

let init() =
    let info = View.initCanvas()

    Program.mkProgram (fun () -> State.initModel info) State.update View.render
    |> Program.run

init()
