﻿namespace punktEinzeichner

open System
open System.IO
open FSharp.Data.Adaptive

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Application
open Aardvark.Rendering.Text
open System.Text.Json

open punktEinzeichner.Model

type Message = 
    | AddControlPoint of V2d*V2i
    | UpdateDraggedControlPointPosition
    | UpdateControlPointName of int*string
    | RemoveControlPoint of int
    | SetCurrentlyHoveredPoint of Option<int>
    | SetMouseDragStart of Option<V2d*V2i>
    | SetMouseDragCurrent of Option<V2d*V2i>
    | SetMousePosition of Option<V2d*V2i>
    | SetPhotoFilename of Option<string>
    | MouseDown of bool //isLmb?
    | MouseUp of bool
    | IncreaseZoomFactor
    | DecreaseZoomFactor
    | SetCtrlDown of bool
    | WriteToJson
    | ParseFromJson
    | Nop


module App =
    let mutable hackysizeBlurg : V2i = V2i(1024,1024)
    let rec update (model : Model) (msg : Message) =
        match msg with
        | SetPhotoFilename s ->
            let us = 
                match s with 
                | None -> Nop
                | Some _ -> ParseFromJson
            update {model with PhotoFilename=s} us
        | AddControlPoint(ndc,px) -> 
            match model.PhotoFilename with
            | None -> Log.error "open a photo first!"; model
            | Some photoname -> 
                let cp =
                    {
                        InternalIndex = model.CurrentIndex
                        Name = sprintf "%d" model.CurrentIndex
                        ParentPhotoName = Path.GetFileName photoname
                        PixelPos = px
                        NdcPos = ndc
                    }
                let newIndex = model.CurrentIndex+1
                let newPoints = 
                    model.ControlPoints |> HashMap.add model.CurrentIndex cp
                {model with ControlPoints=newPoints; CurrentIndex=newIndex}
        | RemoveControlPoint i ->
            let newPoints =
                model.ControlPoints |> HashMap.remove i
            {model with ControlPoints=newPoints}
        | SetCurrentlyHoveredPoint h ->
            {model with CurrentlyHoveredPoint=h}
        | SetMouseDragStart v -> 
            {model with MouseDragStart=v}
        | SetMouseDragCurrent v -> 
            {model with MouseDragCurrent=v}
        | SetMousePosition pos ->
            match pos with 
            | None -> 
                {model with 
                    CurrentlyHoveredPoint=Model.initial.CurrentlyHoveredPoint
                    MouseDragStart=Model.initial.MouseDragStart
                    MouseDragCurrent=Model.initial.MouseDragCurrent
                    CurrentMousePosition=Model.initial.CurrentMousePosition
                    MouseDragPreview=Model.initial.MouseDragPreview
                }
            | Some (ndc,px) ->
                match model.MouseDragStart with 
                | None -> //not dragging, try find hovered point
                    let currentlyHovered =
                        let cands = 
                            model.ControlPoints 
                            |> HashMap.toValueList 
                            |> List.map (fun pt -> Vec.distance ndc pt.NdcPos, pt)
                            |> List.filter (fun (ndcDist,_) -> ndcDist < 0.1)
                        match cands with 
                        | [] -> None
                        | [(_,cp)] -> Some cp.InternalIndex
                        | _ -> cands |> List.sortBy fst |> List.head |> snd |> (fun cp -> cp.InternalIndex) |> Some
                    update {model with CurrentMousePosition=(ndc,px)} (SetCurrentlyHoveredPoint currentlyHovered)
                | Some _ ->
                    let newModel = {model with CurrentMousePosition=(ndc,px)}
                    match newModel.CurrentlyHoveredPoint with 
                    | None -> newModel
                    | Some _ -> 
                        let newModel2 = {newModel with MouseDragCurrent=Some(ndc,px)}
                        update newModel2 UpdateDraggedControlPointPosition
        | UpdateDraggedControlPointPosition -> 
            match model.CurrentlyHoveredPoint, model.MouseDragStart, model.MouseDragCurrent with 
            | Some hovered, Some (dragStartNdc,dragStartPx), Some (dragCurrentNdc,dragCurrentPx) -> 
                match model.ControlPoints |> HashMap.tryFind hovered with
                | Some cp -> 
                    let dragPreview = 
                        match model.MouseDragPreview with 
                        | None -> cp
                        | Some oldDragged -> 
                            let ndcDir = dragCurrentNdc - dragStartNdc 
                            let newNdcUnclamped = cp.NdcPos + ndcDir
                            let newNdc = V2d(clamp 0.0 1.0 newNdcUnclamped.X, clamp 0.0 1.0 newNdcUnclamped.Y)
                            let pxDir = dragCurrentPx - dragStartPx
                            let newPxUnclamped = cp.PixelPos + pxDir
                            let newPx = V2i(clamp 0 hackysizeBlurg.X newPxUnclamped.X, clamp 0 hackysizeBlurg.Y newPxUnclamped.Y)
                            {oldDragged with NdcPos=newNdc; PixelPos=newPx}
                    {model with MouseDragPreview=Some dragPreview}
                | None -> model
            | _ -> model
        | MouseUp left -> 
            if left then 
                let dist = 
                    (Some model.CurrentMousePosition,model.MouseDownPosition) ||> Option.map2 (fun (_,ci) (_,si) -> float <| Vec.distance ci si) |> Option.defaultValue System.Double.PositiveInfinity
                if dist < 1.0 then 
                    let newModel = update model (AddControlPoint model.CurrentMousePosition)
                    {newModel with 
                        MouseDragStart = Model.initial.MouseDragStart
                        CurrentlyHoveredPoint = Model.initial.CurrentlyHoveredPoint
                        MouseDragPreview = Model.initial.MouseDragPreview
                    }
                else 
                    match model.MouseDragStart, model.CurrentlyHoveredPoint, model.MouseDragPreview with 
                    | Some _, Some hovered, Some preview ->
                        {model with 
                            ControlPoints = model.ControlPoints |> HashMap.add hovered preview
                            MouseDragStart = Model.initial.MouseDragStart
                            CurrentlyHoveredPoint = Model.initial.CurrentlyHoveredPoint
                            MouseDragPreview = Model.initial.MouseDragPreview
                        }
                    | _ -> model 
            else model
        | MouseDown left -> 
            if left then 
                let newModel =
                    match model.CurrentlyHoveredPoint with 
                    | Some _ -> 
                        {model with 
                            MouseDragStart=Some model.CurrentMousePosition
                        }
                    | None -> model
                {newModel with MouseDownPosition=Some newModel.CurrentMousePosition}
            else
                match model.CurrentlyHoveredPoint with 
                | Some hovered -> 
                    update model (RemoveControlPoint hovered)
                | None -> model
        | UpdateControlPointName (idx,newName) -> 
            {model with ControlPoints=model.ControlPoints |> HashMap.alter idx (Option.map (fun cp -> {cp with Name=newName}))}
        | WriteToJson ->
            let cps = model.ControlPoints |> HashMap.toList
            match cps with 
            | [] -> model
            | _ -> 
                match model.PhotoFilename with 
                | None -> model
                | Some photoPath ->
                    let photoFile = Path.GetFileNameWithoutExtension photoPath
                    let photoDir = Path.GetDirectoryName(photoPath)
                    let jsonFile = photoFile + "_points.json"
                    Log.warn "write %d points to %s" cps.Length jsonFile
                    let jsonPath = Path.combine [photoDir;jsonFile]
                    let data =
                        cps |> List.map (fun (_,cp) -> 
                            {|
                                name = cp.Name
                                ndcX = cp.NdcPos.X
                                ndcY = cp.NdcPos.Y
                                pxX = cp.PixelPos.X
                                pxY = cp.PixelPos.Y
                            |}
                        )
                    let json = JsonSerializer.Serialize data
                    File.writeAllText jsonPath json
                    model
        | ParseFromJson -> 
            match model.PhotoFilename with 
            | None -> model
            | Some photoPath ->
                let photoFile = Path.GetFileNameWithoutExtension photoPath
                let photoDir = Path.GetDirectoryName(photoPath)
                let jsonFile = photoFile + "_points.json"
                let jsonPath = Path.combine [photoDir;jsonFile]
                if File.Exists(jsonPath) then 
                    try 
                        Log.warn "try parse from %s" jsonFile
                        let json = File.readAllText jsonPath
                        let data : System.Collections.Generic.List<{|name:string;ndcX:float;ndcY:float;pxX:int;pxY:int|}> = JsonSerializer.Deserialize json
                        let mutable li = 0
                        let hm =
                            data |> CSharpList.toList |> List.mapi (fun i e -> 
                                li<-i
                                i,{
                                    InternalIndex=i
                                    NdcPos=V2d(e.ndcX,e.ndcY)
                                    PixelPos=V2i(e.pxX,e.pxY)
                                    ParentPhotoName=photoFile
                                    Name=e.name
                                }
                            )|> HashMap.ofList
                        {model with ControlPoints=hm;CurrentIndex=li+1}
                    with e -> 
                        Log.error "[deserialize] %A" e
                        {model with ControlPoints=HashMap.empty;CurrentIndex=0}
                else {model with ControlPoints=HashMap.empty;CurrentIndex=0}
        | Nop -> 
            model
        | IncreaseZoomFactor -> 
            if not model.ctrlDown then model 
            else 
                let newFactor = (model.zoomFactor+0.01)
                {model with zoomFactor=clamp 0.01 10.0 newFactor}
        | DecreaseZoomFactor -> 
            if not model.ctrlDown then model 
            else 
                let newFactor = (model.zoomFactor-0.01)
                {model with zoomFactor=clamp 0.01 10.0 newFactor}
        | SetCtrlDown b -> 
            {model with ctrlDown=b}
              


    let view (m : AdaptiveModel) =
        let requires = 
            [
                { name = "style.css"; url = "style.css"; kind = Stylesheet }
                { name = "drop.js"; url = "drop.js"; kind = Script }
            ]
        let onDrop handler =
            "drop", AttributeValue.Event {
                clientSide = fun _ _ -> ""
                serverSide = fun c n args ->
                    match args with
                    | h :: _ ->
                        let paths = 
                            try Pickler.unpickleOfJson h
                            with _ -> []
                        match paths with
                        | [] -> Seq.empty
                        | _ -> Seq.singleton (handler paths)
                    | _ -> Seq.empty
            }
        let registerDrop =
            """
var dropped = 
    function dropped(e,dropping) {
        if (dropping) {
            var arr = [];
            for(let f of e.dataTransfer.files) {
                arr.push(f.path);
            }
            if(arr.length>0){ 
                window.aardvark.processEvent('__ID__',"drop",arr);
            }
        }
    };
registerDrop(dropped);
"""

        let pimg = m.PhotoFilename |> AVal.map (Option.map (fun fn -> PixImage.Create(fn)))

        let itexture =
            pimg |> AVal.bind (fun img -> 
                match img with 
                | None -> DefaultTextures.checkerboard
                | Some img -> AVal.constant (PixTexture2d(PixImageMipMap [|img|], TextureParams.mipmapped) :> ITexture)
            )

        let imgSizes =
            pimg |> AVal.map (fun img -> 
                let s = img |> Option.map (fun pi -> pi.Size) |> Option.defaultValue (V2i(1024,1024))
                hackysizeBlurg <- s
                s
            )
        let zoomedImgSizes =
            (m.zoomFactor,imgSizes) ||> AVal.map2 (fun z s -> V2i(int (float s.X * z),int (float s.Y * z)))
        let verts =
            let hms = m.ControlPoints |> AMap.toAVal 
            (hms,m.MouseDragPreview)||> AVal.map2 (fun hm dragPre ->
                let pos = 
                    hm |> HashMap.toArray |> Array.map snd |> Array.map (fun cp -> 
                        let x = cp.NdcPos.X*2.0-1.0
                        let y = -cp.NdcPos.Y*2.0+1.0
                        V4f(float32 x, float32 y, 1.0f, 1.0f)
                    )
                match dragPre with 
                | None -> pos 
                | Some cp -> 
                    let x = cp.NdcPos.X*2.0-1.0
                    let y = -cp.NdcPos.Y*2.0+1.0
                    Array.append pos [|V4f(float32 x, float32 y, 1.0f, 1.0f)|]
                    
            )
        let cols =
            let hms = m.ControlPoints |> AMap.toAVal
            (m.CurrentlyHoveredPoint,hms,m.MouseDragPreview) |||> AVal.map3 (fun hovered hm dragPre ->
                let cols =
                    hm |> HashMap.toArray |> Array.map (fun (idx,_) -> 
                        match hovered with 
                        | Some h when h=idx -> C4f.Red
                        | _ -> C4f.Black
                    )
                match dragPre with 
                | None -> cols 
                | Some cp -> 
                    Array.append cols [|C4f.Yellow|]
            )

        let picViewSg = 
            let picSg = 
                Sg.fullScreenQuad
                |> Sg.shader {
                    do! DefaultSurfaces.diffuseTexture
                }
                |> Sg.diffuseTexture itexture
            let pass1 = (RenderPass.after "asds" RenderPassOrder.Arbitrary RenderPass.main)
            let pass2 = (RenderPass.after "asdgags" RenderPassOrder.Arbitrary pass1)
            let textSg =
                let font = Font.create "Arial" FontStyle.Regular
                let col = C4b.White
                let cfg = TextConfig.create font col TextAlignment.Left true RenderStyle.Billboard
                let trafosTexts = 
                    m.ControlPoints |> AMap.toAVal |> AVal.map (fun hm -> 
                        hm |> Seq.map (fun (idx,cp) -> 
                            let x = cp.NdcPos.X*2.0-1.0
                            let y = -cp.NdcPos.Y*2.0+1.0
                            let shift = V2d(0.01,0.01)
                            let trafo = 
                                AVal.constant (
                                    Trafo3d.Scale 0.05 *
                                    Trafo3d.Translation (V3d(x+shift.X, y+shift.Y, 1.0))
                                )
                            let text = AVal.constant cp.Name
                            trafo,text
                        )
                    ) |> ASet.ofAVal
                Sg.textsWithConfig cfg trafosTexts
                |> Sg.noEvents
                |> Sg.onOff m.ctrlDown
                |> Sg.depthTest (AVal.constant DepthTest.None)
                |> Sg.blendMode (AVal.constant BlendMode.Blend)
                |> Sg.pass pass2
            let vertsSgGreen =
                Sg.draw IndexedGeometryMode.PointList
                |> Sg.vertexAttribute DefaultSemantic.Positions verts
                |> Sg.shader {
                    do! DefaultSurfaces.constantColor C4f.GreenYellow
                    do! DefaultSurfaces.pointSprite
                    do! DefaultSurfaces.pointSpriteFragment
                }
                |> Sg.uniform "PointSize" (AVal.constant 5.0)
                |> Sg.depthTest (AVal.constant DepthTest.None)
                |> Sg.blendMode (AVal.constant BlendMode.Blend)
                |> Sg.pass pass2
            let vertsSgBlack =
                Sg.draw IndexedGeometryMode.PointList
                |> Sg.vertexAttribute DefaultSemantic.Positions verts
                |> Sg.vertexAttribute DefaultSemantic.Colors cols
                |> Sg.shader {
                    do! DefaultSurfaces.vertexColor
                    do! DefaultSurfaces.pointSprite
                    do! DefaultSurfaces.pointSpriteFragment
                }
                |> Sg.uniform "PointSize" (AVal.constant 8.0)
                |> Sg.depthTest (AVal.constant DepthTest.None)
                |> Sg.blendMode (AVal.constant BlendMode.Blend)
                |> Sg.pass pass1
            Sg.ofList [picSg; vertsSgBlack; vertsSgGreen;textSg]
            |> Sg.viewTrafo' Trafo3d.Identity
            |> Sg.projTrafo' Trafo3d.Identity

        let picViewRendercontrol =            
            let dummyCam = 
                AVal.constant <| {cameraView = CameraView.lookAt V3d.IOO V3d.OOO V3d.OOI; frustum = Frustum.ortho Box3d.Unit}
            let sizesAttrib =
                AList.ofAVal (zoomedImgSizes |> AVal.map (fun s -> 
                    [style (sprintf "width:%dpx;height:%dpx" s.X s.Y)]
                )) |> AttributeMap.ofAList
            let remainingAttribs =
                AttributeMap.ofList [
                    clazz "picviewrendercontrol"
                ]
            let attribs = AttributeMap.union sizesAttrib remainingAttribs
            DomNode.RenderControl(
                attribs,
                dummyCam,
                picViewSg,
                RenderControlConfig.noScaling,
                None
            )

        let cpTable = 
            let tableAtts =
                AttributeMap.ofList [
                    clazz "cpTable"
                    onMouseLeave (fun _ -> SetCurrentlyHoveredPoint None)
                ]
            Incremental.table tableAtts (m.ControlPoints |> AMap.toASet |> ASet.sortBy fst |> AList.map (fun (idx,cp) -> 
                let rowAtts =
                    (AttributeMap.ofAList (AList.ofAVal (
                        m.CurrentlyHoveredPoint |> AVal.map (fun hp ->
                            let cc = 
                                match hp with 
                                | None -> [clazz ""] 
                                | Some hIdx -> 
                                    if hIdx=idx then [clazz "highlighted"] 
                                    else [clazz ""]
                            let evts =
                                [
                                    onMouseEnter (fun _ -> SetCurrentlyHoveredPoint (Some idx))
                                ]
                            List.concat [cc;evts]
                    ))))
                Incremental.tr rowAtts (AList.ofList [
                    td [clazz "cpTd"] [button [onClick (fun _ -> RemoveControlPoint idx)] [text "❌"]]
                    td [clazz "cpTd"] [text (sprintf "ndc=(%.4f,%.4f) px=(%d,%d)" cp.NdcPos.X cp.NdcPos.Y cp.PixelPos.X cp.PixelPos.Y)]
                    td [clazz "cpTd"] [div [style "display:inline-block"] [
                        SimplePrimitives.textbox (TextConfig.empty) AttributeMap.empty (m.ControlPoints |> AMap.find idx |> AVal.map (fun ic -> ic.Name)) (fun ns -> UpdateControlPointName (idx,ns))
                        text (sprintf "name=%s" cp.Name)
                    ]]
                ])
            ))

        let picview =
            let atts =
                AttributeMap.ofAList (AList.ofAVal ((imgSizes,zoomedImgSizes) ||> AVal.map2 (fun imgSize zoomedSize -> 
                    [
                        clazz "picview" 
                        style (sprintf "width:%dpx;height:%dpx" zoomedSize.X zoomedSize.Y)
                        onMouseLeave (fun _ -> SetMousePosition None)
                        onMouseMoveRel (fun pn -> 
                            let pi = V2i(int (float (pn.X*(float imgSize.X-0.5))),int (float (pn.Y*(float imgSize.Y-0.5))))
                            SetMousePosition (Some (pn,pi))
                        )
                        onMouseDown (fun b _ -> match b with MouseButtons.Left -> MouseDown true | MouseButtons.Right -> MouseDown false | _ -> Nop)
                        onMouseUp (fun b _ -> match b with MouseButtons.Left -> MouseUp true | MouseButtons.Right -> MouseUp false | _ -> Nop)
                    ]
                )))
            let children = 
                AList.ofList [
                    picViewRendercontrol
                ]
            Incremental.div atts children

        let bodyAtts =
            [
                onDrop (function [] -> Nop | pics -> pics |> List.head |> Some |> SetPhotoFilename)
                onWheel (fun d -> if d.Y >= 0 then IncreaseZoomFactor else DecreaseZoomFactor)
                onKeyDown (function Keys.LeftCtrl -> SetCtrlDown true | _ -> Nop)
                onKeyUp (function Keys.LeftCtrl -> SetCtrlDown false | _ -> Nop)
                style "overflow-y:visible;overflow-x:visible;"
            ]

        let statusText =
            div [style "display:inline-block;"; clazz "descriptiontext"] [
                div [] [text "Drop image here. LMB to add control point."]
                Incremental.div (AttributeMap.ofAList (AList.ofAVal (m.ctrlDown |> AVal.map (fun b -> if b then [clazz "highlightedtext"] else [])))) (AList.ofList [
                    text "Ctrl+Scroll to zoom. "
                ])
                div [] [Incremental.text (m.PhotoFilename |> AVal.map (Option.map Path.GetFileNameWithoutExtension >> Option.defaultValue "No photo loaded."))]
            ]

        require requires (
            onBoot registerDrop (
                body bodyAtts [
                    picview
                    cpTable
                    button [(onClick (fun _ -> WriteToJson))] [text "Export to .json"]
                    statusText
                ]
            )
        )

    let app =
        {
            initial = Model.initial
            update = update
            view = view
            threads = fun m -> ThreadPool.empty
            unpersist = Unpersist.instance
        }
