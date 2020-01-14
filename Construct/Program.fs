// Learn more about F# at http://fsharp.org

open System
open Elmish
open Elmish.WPF
open Construct.WpfViews
open Construct.Framework



[<EntryPoint; STAThread>]
let main _ =
    Program.mkProgramWpf (fun () -> App.init) App.update App.bindings
    |> Program.withConsoleTrace
    |> Program.runWindowWithConfig
      { ElmConfig.Default with LogConsole = true; Measure = true }
      (MainWindow())

