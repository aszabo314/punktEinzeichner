open punktEinzeichner

open System
open FSharp.Data.Adaptive

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.Service
open Aardvark.UI
open Aardium
open Suave
open Suave.WebPart

type EmbeddedResources = EmbeddedResources


[<EntryPoint>]
let main args =
    Aardvark.Init()
    Aardium.init()

    let app = new OpenGlApplication()

    WebPart.startServerLocalhost 4325 [
        MutableApp.toWebPart' app.Runtime false (App.start App.app)
        Reflection.assemblyWebPart typeof<EmbeddedResources>.Assembly
    ] |> ignore
    
    Aardium.run {
        title "Punkt Einzeichner"
        width 1024
        height 768
        url "http://localhost:4325/"
    }

    0