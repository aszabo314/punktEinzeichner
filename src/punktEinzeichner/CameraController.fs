namespace punktEinzeichner

open System
open Adaptify
open Aardvark.Base
open Aardvark.Application
open Aardvark.Rendering
open FSharp.Data.Adaptive
open Aardvark.UI

module CameraController =

    let withCamera (x : CameraModel) =
        let size = V2d x.viewportSize * x.current.scale
        let b = Box3d.FromCenterAndSize(V3d.Zero, V3d(size, 2.0))
        let cam = 
            {
                cameraView = CameraView.lookAt (V3d(x.current.center, 0.0)) (V3d(x.current.center, -1.0)) V3d.OIO
                frustum = Frustum.ortho b
            }
        { x with camera = cam }

    let startAnimation (x : CameraModel) =
        if x.current <> x.target then
            withCamera x
        else
            x
            
    let private t0 = DateTime.Now
    let private sw = System.Diagnostics.Stopwatch.StartNew()

    let now() = t0 + sw.Elapsed

    let initial =
        withCamera
            {
                sceneBounds         = Box2d.FromCenterAndSize(V2d.Zero, V2d(1024, 768))
                minPixelsVisible    = 128
                viewportSize        = V2i.II
                current             = { scale = 1.0; center = V2d.Zero }
                target              = { scale = 1.0; center = V2d.Zero }

                zoomSensitivity     = 0.3
                speed               = 1000.0

                panning             = false
                lastPos             = V2d.Zero
                lastTime            = now()
                camera              = Unchecked.defaultof<_>
            }

    type Message =
        | StartPan of position : V2d
        | StopPan

        | SetSceneBounds of bounds : Box2d
        | SetZoomSensitivity of float
        | SetSpeed of float
        | BestFit of bounds : Box2d

        | Move of position : V2d
        | Wheel of delta : float * position : V2d
        | Interpolate of DateTime
        | Resize of size : V2i

    let clampPosition (model : CameraModel) =

        let pixelsVisible = float model.minPixelsVisible

        let target = model.target
        let sceneBounds = model.sceneBounds
        let targetBounds = target.bounds(model.viewportSize)

        if targetBounds.Contains sceneBounds then
            model
        else
            let mutable shift = V2d.Zero
            let mutable targetBounds = targetBounds

            let p = 
                let s = sceneBounds.Size
                let p = pixelsVisible * model.target.scale
                V2d(min s.X p, min s.Y p)

            let l = sceneBounds.Max.X - targetBounds.Min.X
            let r = targetBounds.Max.X - sceneBounds.Min.X
            let b = sceneBounds.Max.Y - targetBounds.Min.Y
            let t = targetBounds.Max.Y - sceneBounds.Min.Y

            if l < p.X then
                let d = -V2d(p.X - l, 0.0)
                targetBounds <- targetBounds + d
                shift <- shift + d
                
            if r < p.X then
                let d = -V2d(r - p.X, 0.0)
                targetBounds <- targetBounds + d
                shift <- shift + d
                
            if b < p.Y then
                let d = -V2d(0.0, p.Y - b)
                targetBounds <- targetBounds + d
                shift <- shift + d
                
            if t < p.Y then
                let d = -V2d(0.0, t - p.Y)
                targetBounds <- targetBounds + d
                shift <- shift + d

            let target = { target with center = target.center + shift }

            { model with target = target }

    let rec update (model : CameraModel) (message : Message) =
        match message with
        | SetSceneBounds b ->
            (startAnimation << clampPosition)
                { model with
                    sceneBounds = b
                }
        | SetZoomSensitivity s ->
            { model with zoomSensitivity = max 0.0 s }
            
        | SetSpeed s ->
            { model with speed = max 0.0 s }

        | BestFit bounds ->
            let model = clampPosition { model with target = CenterAndScale.FromBounds(bounds, model.viewportSize) }
            withCamera { model with current = model.target }

        | StartPan p ->
            startAnimation
                { model with 
                    panning = true
                    lastPos = p
                    lastTime = now() 
                }

        | StopPan ->
            { model with 
                panning = false
                lastTime = now() 
            }

        | Move(p) when model.panning ->
            let delta = V2d.PN * (p - model.lastPos) * model.current.scale
            let target = { model.target with center = model.target.center - delta }

            (startAnimation << clampPosition)
                { model with 
                    target = target
                    lastPos = p
                    lastTime = now() 
                } 

        | Wheel(delta, p) ->
            let dst = { model.target with scale = model.target.scale * (1.0 + model.zoomSensitivity) ** (delta / 120.0) }
            let local = model.current.fromPixel(p, model.viewportSize)
            let after = dst.toPixel(local, model.viewportSize)
            let delta = V2d.PN * (after - p) * dst.scale
            let dst = { dst with center = dst.center + delta }

            
            (startAnimation << clampPosition)
                { model with 
                    target = dst
                    lastTime = now() 
                } 

        | Resize size when size <> model.viewportSize ->
            if model.viewportSize.AnyGreater 1 then
                let bounds = model.target.bounds(model.viewportSize).Intersection(model.sceneBounds)
                let state = CenterAndScale.FromBounds(bounds, size)
                withCamera
                    { model with
                        target = state
                        current = state
                        viewportSize = size
                        lastTime = now() 
                    }
            else
                withCamera
                    { model with
                        viewportSize = size
                        lastTime = now() 
                    }

        | Interpolate now ->
            if model.needsAnimation then
                let dt = (now - model.lastTime).TotalSeconds
                let cc = CenterAndScale.Lerp(model.current, model.target, dt * model.speed)

                withCamera
                    { model with
                        current = cc
                        lastTime = now
                    }
            elif model.current <> model.target then
                withCamera
                    { model with
                        current = model.target
                    }
            else
                model

        | _ ->
            model

    let attributes (shouldPan : MouseEvent -> bool) (shouldZoom : MouseEvent -> bool) (m : aval<AdaptiveCameraModel>) =
        let panning = m |> AVal.bind (fun m -> m.panning)
        let needsAnimation = m |> AVal.bind (fun m -> m.needsAnimation)
        AttributeMap.ofListCond [
            always                      <| onPointerDownAbs (fun e -> if shouldPan e then [StartPan(e.pixel)] else [])
            always                      <| onPointerUpAbs (fun e -> [StopPan])
            always                      <| onWheelAbs (fun delta e -> if shouldZoom e then [Wheel(delta, e.pixel)] else [])
            onlyWhen panning            <| onPointerMoveAbs (fun e -> [Move(e.pixel)])
            onlyWhen needsAnimation     <| onRendered (fun s -> [Interpolate (now())])
        ]
