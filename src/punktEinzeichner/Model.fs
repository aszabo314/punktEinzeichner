namespace punktEinzeichner.Model

open System
open FSharp.Data.Adaptive

open Aardvark.Base
open Aardvark.UI.Primitives
open Adaptify

type ControlPoint = 
    {
        InternalIndex : int
        Name : string
        ParentPhotoName : string
        PixelPos : V2i
        NdcPos : V2d
    }

[<ModelType>]
type Model =
    {
        CurrentIndex : int
        ControlPoints : HashMap<int,ControlPoint>
        CurrentlyHoveredPoint : Option<int>
        
        MouseDownPosition : Option<V2d*V2i>
        MouseDragStart : Option<V2d*V2i>
        MouseDragCurrent : Option<V2d*V2i>
        MouseDragPreview : Option<ControlPoint>

        zoomFactor : float
        CurrentMousePosition : V2d*V2i
        ctrlDown : bool

        PhotoFilename : Option<string>
    }

module Model =
    let initial =
        {
            MouseDownPosition = None
            CurrentIndex = 0
            ControlPoints = HashMap.empty
            CurrentlyHoveredPoint = None
        
            MouseDragStart = None
            MouseDragCurrent = None
            MouseDragPreview = None

            CurrentMousePosition = V2d.NN,V2i.NN

            PhotoFilename = None
            zoomFactor = 0.1
            ctrlDown = false
        }