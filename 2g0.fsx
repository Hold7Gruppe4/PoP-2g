
type vec = float * float

///<summary>
/// Given parameters v1 and v2, this function will add them together and output a new vec.
/// </summary> 
/// <param name = "v1" > A tuple in the type vec. </param>
/// <param name = "v2" > A 2nd tuple in the type vec. </param>
/// <returns> The added vector, named v3, which is the two parameters's added together. </param>

let add (v1:vec) (v2:vec) : (vec) =
    let v3A = fst v1 + fst v2
    let v3B = snd v1 + snd v2
    let v3 = vec (v3A,v3B)
    v3

///<summary>
/// Given parameters v1 and c, this function will multiply them together and output a new vec.
/// </summary> 
/// <param name = "v1" > A tuple in the type vec. </param>
/// <param name = "c" > A constant, with the datatype float, used to multiply v1. </param>
/// <returns> The multiplied vector, named v2, which is the two parameters's multiplied together. </param>

let mul (v1:vec) (c:float) : (vec) =
    let v2A = fst v1 * c
    let v2B = snd v1 * c
    let v2 = vec (v2A,v2B)
    v2


///<summary>
/// Given parameters v1 and rc, this function will rotate the v1 based on the radians given by rc.
/// </summary> 
/// <param name = "v1" > A tuple in the type vec. </param>
/// <param name = "rc" > A constant, with the datatype float, used to multiply v1. This is given in radians </param>
/// <returns> The rotated vector, named v2, which is result of the rotation. Here is the following formula used (rc*v1=(xcos(rc)-ysin(rc),xsin(rc)+ycos(rc))) </param>


let rot (v1:vec) (rc:float) : (vec) =
    let v2A = fst v1 * cos rc - snd v1 * sin rc 
    let v2B = fst v1 * sin rc +  snd v1 * cos rc 
    let v2 = vec (v2A,v2B)
    v2

//Here are the different parameters, which can be changed in order to calculate the different things.

//For the add function
let vector1ADD = vec (1.2 , 1.5)
let vector2ADD = vec (1.2 , 1.5)
//For the multiply function
let vectorMUL = vec (1.2 , 1.5)
let c = 2.0
//For the rotate function
let vectorROT = vec (1.2 , 1.5)
let rc : float = 10.0*System.Math.PI/180.0

//Here are the different calls for the various functions.
let A = add (vector1ADD) (vector2ADD)
let M = mul (vectorMUL) (c)
let R = rot (vectorROT) (rc)

//Here they are printed. 
printfn "Add: %A, mul: %A and rot: %A" A M R