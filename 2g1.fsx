#r "nuget:DIKU.Canvas, 1.0.1"
open Canvas
let h = 800
let w = 800
let colour = fromRgb (100,100,100)
type vec = float * float
let rotationRadians = System.Math.PI/30.0
let mutable x = 0.0

let v = vec (150 , 0)
let pos = vec (400.0 , 400.0)

type state =
    | Move
    | Start

let Moving = function
    | Start -> Move
    | Move -> Move

let toInt (vector:vec) : (int * int) = 
    let i1 = int(fst vector+0.5)
    let i2 = int(snd vector+0.5)
    let intVector = (i1, i2)
    intVector

let add (v1:vec) (pos:vec) : (vec) =
    let vAX = fst v1 + fst pos
    let vAY = snd v1 + snd pos
    let vA = vec (vAX,vAY)
    vA

let rot (v1:vec) (rc:float) : (vec) =
    let v2A = fst v1 * cos rc - snd v1 * sin rc 
    let v2B = fst v1 * sin rc +  snd v1 * cos rc 
    let v2 = vec (v2A,v2B)
    v2

let setVector (C:canvas) (c:color) (vector:vec) (pos:vec) =
    let pV = add (vector) (pos)

    let posInt = toInt (pos)
    let posIntX = fst posInt
    let posIntY = snd posInt

    let pVInt = toInt (pV)
    let pVIntX = fst pVInt
    let pVIntY = snd pVInt
    setLine C c  (posIntX, posIntY) (pVIntX, pVIntY)

let draw (w:int) (h:int) (s:state) =
  let C = create w h
  let mutable vR = v
  let mutable i = 1.0
  while i < 37.1 do
    vR <- rot (v) ((10.0*i*(System.Math.PI))/180.0+x)
    setVector (C) (colour) (vR) (pos)
    
    i <- i + 1.0
  C


let react (s:state) (k:Canvas.key) : state option =
    match getKey k with
        | LeftArrow ->
            x <- x-rotationRadians
            printfn "Pressed left-arrow!"
            Moving s |> Some
        | RightArrow ->
            x <- x+rotationRadians
            printfn "Pressed right-arrow!"
            Moving s |> Some
        | _ -> None




do runApp "ColorBoxes" w h draw react Start





//do setVector (C) (colour) (v) (pos)
