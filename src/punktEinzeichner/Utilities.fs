namespace Aardvark.UI

open System
open System.Runtime.InteropServices

open Aardvark.Application
open Aardvark.Rendering

open Aardvark.Base
open FSharp.Data.Adaptive
open FShade

module Shader =
    let samplesOffsets : V2d[] =
        Array.map (fun v -> v / 4.0) [|
            V2d(1,1); V2d(-1,-3); V2d(-3,2); V2d(4,1);
            V2d(-5,-2); V2d(2,5); V2d(5,3); V2d(3,-5);
            V2d(-2,6); V2d(0,-7); V2d(-4,-6); V2d(-6,4);
            V2d(-8,0); V2d(7,-4); V2d(6,7); V2d(-7,8)
        |]

    [<ReflectedDefinition>][<Inline>] //needs to be inline in order to support ddx/ddy
    let texelOutline(tc : V2d) : float =

        let pixelOffset = V2d.Max((ddx tc).Abs(), (ddy tc).Abs())

        // define transparency of outlines
        let f = (max pixelOffset.X pixelOffset.Y) * 1.5 + 0.5
        if f >= 1.0 then // if texel element difference x 1.5 + 0.5 is greater than 1 -> do not show outlines
            0.0
        else

            let mutable cnt = 0
            Preprocessor.unroll()
            for i in 0..15 do
                let s = tc + pixelOffset * samplesOffsets.[i]
                if (V2i(tc) <> V2i(s)) then
                    cnt <- cnt + 1

            (1.0 - f) * (float cnt / 16.0)
    type UniformScope with
        member x.TextureSize : V2d = x?TextureSize

    let sammy =
        sampler2d {
            texture uniform?DiffuseColorTexture
            filter Filter.MinLinearMagPointMipLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }

    let myshader (v : Effects.Vertex) = 
        fragment {
            let s = uniform.TextureSize
            let alpha = texelOutline(s * v.tc)
            let c = sammy.Sample(v.tc)
            return V4d(lerp c.XYZ V3d.OOO alpha, 1.0)
        }

[<AutoOpen>]
module TimeUtilities =

    let private anytime = System.DateTime.Now
    let private sw = System.Diagnostics.Stopwatch.StartNew()

    let now() = anytime + sw.Elapsed

type MouseEvent =
    {
        pixel       : V2d
        viewport    : V2d
        button      : MouseButtons
        alt         : bool
        ctrl        : bool
        shift       : bool
    }
  
type KeyEvent =
    {
        key         : Keys
        repeat      : bool
        alt         : bool
        ctrl        : bool
        shift       : bool
    }

[<AutoOpen>]
module MissingUIThings =
    open Aardvark.UI
    open Aardvark.Base
    open Aardvark.Application

    type MouseButtons with
        static member Button4 = unbox<MouseButtons> 8
        static member Button5 = unbox<MouseButtons> 16


    module MouseButtons =
        let (|Button4|_|) (m : MouseButtons) =
            match int m with
            | 8 -> Some ()
            | _ -> None
        
        let (|Button5|_|) (m : MouseButtons) =
            match int m with
            | 16 -> Some ()
            | _ -> None

    type private CaptureOperation =
        | Set
        | Release
        | None

    let private button (str : string) =
        match int (float str) with
            | 1 -> MouseButtons.Left
            | 2 -> MouseButtons.Middle
            | 3 -> MouseButtons.Right
            | 4 -> MouseButtons.Button4
            | 5 -> MouseButtons.Button5
            | _ -> MouseButtons.None

    let private onPointerAbs (kind : string) (capture : CaptureOperation) (needButton : bool) (callback : MouseEvent -> #seq<'msg>) =
        kind, AttributeValue.Event {
            clientSide = fun send src -> 
                String.concat ";" [
                    "var rect = getBoundingClientRect(event.target)"
                    "var x = (event.clientX - rect.left)"
                    "var y = (event.clientY - rect.top)"

                    match capture with
                    | None -> ()
                    | Release -> "this.releasePointerCapture(event.pointerId)"
                    | Set -> "this.setPointerCapture(event.pointerId)"
                    send src ["event.which"; "{ X: x.toFixed(10), Y: y.toFixed(10) }"; "{ X: rect.width.toFixed(10), Y: rect.height.toFixed(10) }"; "event.altKey || false"; "event.shiftKey || false"; "event.ctrlKey || false"]
                        
                ]
            serverSide = fun client src args -> 
                match args with
                    | which :: pos :: size :: alt :: shift :: ctrl :: _ ->
                        let pos : V2d = Pickler.json.UnPickleOfString pos
                        let size : V2d = Pickler.json.UnPickleOfString size
                        let button = if needButton then button which else MouseButtons.Left
                        let alt : bool = Pickler.json.UnPickleOfString alt
                        let shift : bool = Pickler.json.UnPickleOfString shift
                        let ctrl : bool = Pickler.json.UnPickleOfString ctrl
                        callback {
                            button = button
                            pixel = pos
                            viewport = size
                            alt = alt
                            shift = shift
                            ctrl = ctrl
                        } :> seq<_>
                    | _ ->
                        Seq.empty      
        }

    let private onSimple (kind : string) (needButton : bool) (callback : MouseEvent -> #seq<'msg>) =
        kind, AttributeValue.Event {
            clientSide = fun send src -> 
                String.concat ";" [
                    "var rect = getBoundingClientRect(event.target)"
                    "var x = (event.clientX - rect.left)"
                    "var y = (event.clientY - rect.top)"
                    "event.stopPropagation();"
                    send src ["event.which || 1"; "{ X: x.toFixed(10), Y: y.toFixed(10) }"; "{ X: rect.width.toFixed(10), Y: rect.height.toFixed(10) }"; "event.altKey || false"; "event.shiftKey || false"; "event.ctrlKey || false"]
                        
                ]
            serverSide = fun client src args -> 
                match args with
                    | which :: pos :: size :: alt :: shift :: ctrl :: _ ->
                        let pos : V2d = Pickler.json.UnPickleOfString pos
                        let size : V2d = Pickler.json.UnPickleOfString size
                        let button = if needButton then button which else MouseButtons.Left
                        let alt : bool = Pickler.json.UnPickleOfString alt
                        let shift : bool = Pickler.json.UnPickleOfString shift
                        let ctrl : bool = Pickler.json.UnPickleOfString ctrl
                        callback {
                            button = button
                            pixel = pos
                            viewport = size
                            alt = alt
                            shift = shift
                            ctrl = ctrl
                        } :> seq<_>
                    | _ ->
                        Seq.empty      
        }

    let private onKeys (kind : string) (callback : KeyEvent -> #seq<'msg>) =
        kind, AttributeValue.Event {
            clientSide = fun send src -> 
                String.concat ";" [
                    send src ["event.keyCode || 0"; "event.repeat || false"; "event.altKey || false"; "event.shiftKey || false"; "event.ctrlKey || false"]
                        
                ]
            serverSide = fun client src args -> 
                match args with
                    | key :: repeat :: alt :: shift :: ctrl :: _ ->
                        let keyCode : int = Pickler.json.UnPickleOfString key
                        let alt : bool = Pickler.json.UnPickleOfString alt
                        let repeat : bool = Pickler.json.UnPickleOfString repeat
                        let shift : bool = Pickler.json.UnPickleOfString shift
                        let ctrl : bool = Pickler.json.UnPickleOfString ctrl
                        
                        let key = KeyConverter.keyFromVirtualKey keyCode

                        callback {
                            key = key
                            repeat = repeat
                            alt = alt
                            shift = shift
                            ctrl = ctrl
                        } :> seq<_>
                    | _ ->
                        Seq.empty      
        }

    let onPointerDownAbs (f : MouseEvent -> #seq<'msg>) =
        onPointerAbs "onpointerdown" Set true f

    let onPointerUpAbs (f : MouseEvent ->  #seq<'msg>) =
        onPointerAbs "onpointerup" Release true f

    let onPointerMoveAbs (f : MouseEvent ->  #seq<'msg>) =
        onPointerAbs "onpointermove" None false f
        
    let onMouseMoveAbs (f : MouseEvent ->  #seq<'msg>) =
        onPointerAbs "onmousemove" None false f
        
    let onWheelAbs (f : float -> MouseEvent ->  #seq<'msg>) =

        "onwheel", AttributeValue.Event {
            clientSide = fun send src -> 
                String.concat ";" [
                    "var rect = getBoundingClientRect(event.target)"
                    "var x = (event.clientX - rect.left)"
                    "var y = (event.clientY - rect.top)"
                    send src ["event.deltaY.toFixed()"; "{ X: x.toFixed(10), Y: y.toFixed(10) }"; "{ X: rect.width.toFixed(10), Y: rect.height.toFixed(10) }"; "event.altKey || false"; "event.shiftKey || false"; "event.ctrlKey || false"]
                        
                ]
            serverSide = fun client src args -> 
                match args with
                    | delta :: pos :: size :: alt :: shift :: ctrl :: _->
                        let delta : float = Pickler.json.UnPickleOfString delta
                        let pos : V2d = Pickler.json.UnPickleOfString pos
                        let size : V2d = Pickler.json.UnPickleOfString size
                        let alt : bool = Pickler.json.UnPickleOfString alt
                        let shift : bool = Pickler.json.UnPickleOfString shift
                        let ctrl : bool = Pickler.json.UnPickleOfString ctrl
                        f delta {
                            button = MouseButtons.None
                            pixel = pos
                            viewport = size
                            alt = alt
                            shift = shift
                            ctrl = ctrl
                        } :> seq<_>
                    | _ ->
                        Seq.empty      
        }
        
    let onKeyDownE (f : KeyEvent -> #seq<'msg>) =   
        onKeys "onkeydown" f
        
    let onKeyUpE (f : KeyEvent -> #seq<'msg>) =   
        onKeys "onkeyup" f

    let onClickE (f : MouseEvent ->  #seq<'msg>) =
        onSimple "onclick" false f
        
    let onDoubleClickE (f : MouseEvent ->  #seq<'msg>) =
        onSimple "ondblclick" false f
        
    let onRendered (f : V2d -> #seq<'msg>) =
        "onRendered", AttributeValue.Event {
            clientSide = fun send src -> 
                String.concat ";" [
                    "var rect = this.getBoundingClientRect()"
                    send src ["{ X: rect.width.toFixed(10), Y: rect.height.toFixed(10) }"]
                ]
            
            serverSide = fun client src args -> 
                match args with
                    | size :: _ ->
                        let size : V2d = Pickler.json.UnPickleOfString size
                        f size :> seq<_>
                    | _ ->
                        Seq.empty    
        }

    let onKeyDownPre (f : KeyEvent -> #seq<'msg>) =
        let code =
            String.concat "" [
                "const node = document.getElementById('__ID__');"
                "node.setAttribute('tabindex', '0');"
                "node.style.outline = 'none';"
                "node.addEventListener('keydown', function (e) {" 
                "   aardvark.processEvent('__ID__', 'onpreviewkeydown', event.keyCode || 0, event.repeat || false, event.altKey || false, event.shiftKey || false, event.ctrlKey || false);"
                "   if(e.key == 'ArrowUp' || e.key == 'ArrowDown') {"
                "       e.stopImmediatePropagation();"
                "       e.preventDefault();" 
                "   }"
                "}, true);"
            ]
        
        let additional =
            AttributeMap.ofList [
                onEvent' "onpreviewkeydown" [] (fun args ->
                    match args with
                    | key :: repeat :: alt :: shift :: ctrl :: _ ->
                        let keyCode : int = Pickler.json.UnPickleOfString key
                        let alt : bool = Pickler.json.UnPickleOfString alt
                        let repeat : bool = Pickler.json.UnPickleOfString repeat
                        let shift : bool = Pickler.json.UnPickleOfString shift
                        let ctrl : bool = Pickler.json.UnPickleOfString ctrl
                        
                        let key = KeyConverter.keyFromVirtualKey keyCode

                        f {
                            key = key
                            repeat = repeat
                            alt = alt
                            shift = shift
                            ctrl = ctrl
                        } :> seq<_>
                    | _ ->
                        Seq.empty    )
            ]

        fun (node : DomNode<'msg>) ->
            onBoot code (
                node.WithAttributes(additional)
            )
            

    let withShortcut (shortcut : string)  =
        let s = 
            if RuntimeInformation.IsOSPlatform OSPlatform.OSX then shortcut.ToLower().Replace(" ", "").Replace("Ctrl", "Command")
            else shortcut.ToLower().Replace(" ", "")
        let boot = sprintf "Mousetrap.bind('%s', () => { document.getElementById('__ID__').click(); return false; });" s
        let shutdown = sprintf "Mousetrap.unbind('%s');" s
        onBoot boot >> onShutdown shutdown 

    let withShortcut' (shortcut : aval<string>) (n : DomNode<'msg>) =
        Incremental.a AttributeMap.empty <|
            alist {
                let! shortcut = shortcut
                let s = 
                    if RuntimeInformation.IsOSPlatform OSPlatform.OSX then shortcut.ToLower().Replace(" ", "").Replace("Ctrl", "Command")
                    else shortcut.ToLower().Replace(" ", "")
                let boot = sprintf "Mousetrap.bind('%s', () => { document.getElementById('__ID__').click(); return false; });" s
                let shutdown = sprintf "Mousetrap.unbind('%s');" s
                yield n |> onBoot boot |> onShutdown shutdown
            }



namespace Aardvark.Base

module Helpers =
    let rx = System.Text.RegularExpressions.Regex "\\\"|\\\\"

    let escape (str : string) =
        rx.Replace(str, fun m ->
            if m.Value = "\"" then "\\\""
            else "\\\\"
        )


    let cmdToUpper (s : string) = 
        let d (sep : string) (s : string) = s.Split(sep) |> Array.map (fun s -> if s.Length > 0 then (string (System.Char.ToUpperInvariant(s.[0]))) + (s.Substring(1,s.Length-1)) else "") |> String.concat sep
        s |> d " " |> d "+"