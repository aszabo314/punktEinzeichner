namespace punktEinzeichner.Model

open System
open FSharp.Data.Adaptive

open Aardvark.Base
open Aardvark.UI.Primitives
open Adaptify
open punktEinzeichner

type ControlPoint = 
    {
        InternalIndex : int
        Name : string
        ParentPhotoName : string
        PixelPos : V2d
        NdcPos : V2d
    }

[<ModelType>]
type Model =
    {
        CurrentIndex : int
        ControlPoints : HashMap<int,ControlPoint>
        CurrentlyHoveredPoint : Option<int>
        
        MouseDownPosition : Option<V2d*V2d>
        MouseDragStart : Option<V2d*V2d>
        MouseDragCurrent : Option<V2d*V2d>
        MouseDragPreview : Option<ControlPoint>

        CurrentMousePosition : V2d*V2d
        ctrlDown : bool

        PhotoFilename : Option<string>
        Photo : Option<PixImage>

        CameraModel : CameraModel
        

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

            CurrentMousePosition = V2d.NN,V2d.NN

            PhotoFilename = None
            Photo = None
            ctrlDown = false

            CameraModel = CameraController.initial
        }