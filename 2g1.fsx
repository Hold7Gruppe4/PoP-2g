#r "nuget:DIKU.Canvas, 1.0.1"
open Canvas

//Here we define the type vec, which is used in the program.
type vec = float * float

///<summary>
/// Here are the different parameters or settings that are used to define the color of the lines, the dimensions of the canvas and the angular offset.
/// /// </summary> 
/// <param name = "h" > The height of the canvas </param>
/// <param name = "w" > The width of the canvas </param>
/// <param name = "colour" > The color of the lines </param>
/// <param name = "rotationRadians" > The angular offset in the rotations </param>
/// <param name = "vectorLenght" > The lenght of the lines drawn </param>
/// <param name = "v" > The base vector </param>
/// <param name = "pos" > The starting position of our lines. This is always, in the center of the canvas, due to the settings. </param>
/// <param name = "s" > The float state parameter which will update based on which keys are pressed. </param>

let h = 800
let w = 800
let colour = fromRgb (100,100,100)
let rotationRadians = 0.01
let vectorLenght = 150
let v = vec (vectorLenght , 0)
let pos = vec ((float w)/2.0 , (float h)/2.0)
let mutable s = 0.0



//Here we define our states - the lines will either move, or the program has just started
type state =
    | Move
    | Start

//Here we define what will happen if the state is in either start or move. 
let Moving = function
    | Start -> Move
    | Move -> Move


///<summary>
/// Given  the parameter vector this function will convert that vector into a tuple with two integers.
/// </summary> 
/// <param name = "vector" > A tuple in the type vec. </param>
/// <returns> The new tuple is then returned as the intVector </param>



let toInt (vector:vec) : (int * int) = 
    let i1 = int(fst vector+0.5)
    let i2 = int(snd vector+0.5)
    let intVector = (i1, i2)
    intVector


///<summary>
/// Given parameters v1 and v1, this function will add them together and output a new vec.
/// when v1 = (1.1 , 1.2) and v2 = (1.3 , 1.5), the output of add will be v3 = (1.4 , 1.7)
/// </summary> 
/// <param name = "v1" > A tuple in the type vec. </param>
/// <param name = "v2" > A 2nd tuple in the type vec. </param>
/// <returns> The added vector, named vA, which is the two parameters's added together. </param>

let add (v1:vec) (pos:vec) : (vec) =
    let vAX = fst v1 + fst pos
    let vAY = snd v1 + snd pos
    let vA = vec (vAX,vAY)
    vA

///<summary>
/// Given parameters v1 and rc, this function will rotate the v1 based on the radians, given by rc.
/// when v1 = (1.2 , 1.5) and rc = pi, the output of rotated vec will be v2 = (-1.2 , -1.5), since this is a 180 degrees rotation.
/// </summary> 
/// <param name = "v1" > A tuple in the type vec. </param>
/// <param name = "rc" > A constant, with the datatype float, used to multiply v1. This is given in radians </param>
/// <returns> The rotated vector, named v2, which is result of the rotation. Here is the following formula used (rc*v1=(xcos(rc)-ysin(rc),xsin(rc)+ycos(rc))) </param>

let rot (v1:vec) (rc:float) : (vec) =
    let v2A = fst v1 * cos rc - snd v1 * sin rc 
    let v2B = fst v1 * sin rc +  snd v1 * cos rc 
    let v2 = vec (v2A,v2B)
    v2

///<summary>
/// /// Given the parameters canvas, color, vector and position, this function will draw a line, representing the vector, from the pos to the pos plus the vector coordinates.
/// It will first add the vector and position together in pV (position+vector). Afterwards both the pos and the pV will be converted to ints using the toInt function.
/// This makes it possible for the function to then set a line from the pos to the pV
/// If the color is black, the vector is (100 , 0) and the position is (0 , 0) there will be drawn a line from 0,0 to 100,0 on the given canvas
/// </summary> 
/// <param name = "C" > A choosen canvas. </param>
/// <param name = "c" > The color for the line </param>
/// <param name = "vector" > A tuple in the type vec.  </param>
/// <param name = "pos" > A tuple in the type vec. </param>

/// <returns> The output is a line draw on the canvas. </param>

let setVector (C:canvas) (c:color) (vector:vec) (pos:vec) =
    let pV = add (vector) (pos)

    let posInt = toInt (pos)
    let posIntX = fst posInt
    let posIntY = snd posInt

    let pVInt = toInt (pV)
    let pVIntX = fst pVInt
    let pVIntY = snd pVInt
    setLine C c  (posIntX, posIntY) (pVIntX, pVIntY)

///<summary>
/// The draw functions will first create the canvas for the setVector. Then we create a variable vR (vectorRotated), and create a while loop.
/// This while loop runs 36 times, and each time it will update the vR to be rotated based on the rotating varible s, and which iteration it is in. 
/// It will draw the line using the setVector function with the current vector and set position.
/// That means that the first iteration creates a line, and rotates it 10 degrees, and the 2nd one will rotate it 20 degrees
/// </summary> 
/// <param name = "w" > the width for the canvas </param>
/// <param name = "h" > the height for the canvas </param>
/// <returns> The output is a 36 lines drawn on the canvas. </param>

let draw (w:int) (h:int) (S:state) =
  let C = create w h
  let mutable vR = v
  let mutable i = 1.0
  while i < 37.1 do
    vR <- rot (v) ((10.0*i*(System.Math.PI))/180.0+s)
    setVector (C) (colour) (vR) (pos)
    
    i <- i + 1.0
  C

///<summary>
/// The react function will use the getKey to check if either a left or a right arrow key has been pressed.
/// If either of those things happen it will update s, to either be 0.01 bigger or smaller causing all the lines
/// on the next iteration of the canvas to be offset that much.
/// It will also update the state. Due to the configurations of the state, after the first press the state will always be moving
/// </summary> 
/// <param name = "S" > The current state (Move/start) </param>
/// <param name = "k" > key pressed by the user </param>
/// <returns> The output is a 36 lines drawn on the canvas. </param>


let react (S:state) (k:Canvas.key) : state option =
    match getKey k with
        | LeftArrow ->
            s <- s - rotationRadians
            Moving S |> Some
        | RightArrow ->
            s <- s + rotationRadians
            Moving S |> Some
        | _ -> None


do runApp "2g1" w h draw react Start
