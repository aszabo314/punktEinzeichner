namespace punktEinzeichner

open System
open Adaptify
open Aardvark.Base
open Aardvark.Rendering

[<ModelType>]
type CenterAndScale = 
    { center : V2d; scale : float }

    static member FromBounds(bounds : Box2d, viewportSize : V2i) =
        //let size = V2d viewportSize * x.scale
        //Box2d.FromCenterAndSize(x.center, size)
        let scale = bounds.Size / V2d viewportSize
        { center = bounds.Center; scale = scale.[scale.MajorDim] }


        

    static member Lerp(a : CenterAndScale, b : CenterAndScale, t : float) =
       if t <= 0.0 then a
       elif t >= 1.0 then b
       else
            {
                center = lerp a.center b.center t
                scale = lerp a.scale b.scale t
            }

    member x.bounds(viewportSize : V2i) =
        let size = V2d viewportSize * x.scale
        Box2d.FromCenterAndSize(x.center, size)
        
    member x.fromPixel (p : V2d, viewportSize : V2i) =
        let halfVP = V2d viewportSize / 2.0
        let v = (V2d.PN * (p - halfVP)) * x.scale

        v + x.center

    member x.toPixel (p : V2d, viewportSize : V2i) =
        let v = p - x.center

        let halfVP = V2d viewportSize / 2.0
        halfVP + V2d.PN * (v / x.scale)

[<ModelType>]
type CameraModel =
    {   
        sceneBounds         : Box2d
        minPixelsVisible    : int
        
        zoomSensitivity     : float
        speed               : float

        viewportSize        : V2i
        panning             : bool

        current             : CenterAndScale
        [<NonAdaptive>]
        target              : CenterAndScale
        [<NonAdaptive>]
        lastPos             : V2d
        [<NonAdaptive>]
        lastTime            : DateTime

        camera              : Camera
    }


    member x.needsAnimation =
        not (
            Fun.ApproximateEquals(x.current.center, x.target.center, 1E-2) && 
            Fun.ApproximateEquals(x.current.scale, x.target.scale, 1E-2)
        )

    member x.bounds = x.current.bounds x.viewportSize
    member x.fromPixel (p : V2d) = x.current.fromPixel(p, x.viewportSize)
    member x.toPixel (p : V2d) = x.current.toPixel(p, x.viewportSize)