
type vec = float * float

///<summary>
/// Given parameters vectorA and vectorB, this function will add them together and output a new vec.
/// when vectorA = (1.1 , 1.2) and vectorB = (1.3 , 1.5), the output of add will be v3 = (1.4 , 1.7)
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
/// when v1 = (1.1 , 1.2) and c = 2, the output of add will be v2 = (1.2 , 1.4)
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
/// when v1 = (1.2 , 1.5) and rc = pi, the output of rotated vec will be v2 = (-1.2 , -1.5), since this is a 180 degrees rotation.
/// </summary> 
/// <param name = "v1" > A tuple in the type vec. </param>
/// <param name = "rc" > A constant, with the datatype float, used to multiply v1. This is given in radians </param>
/// <returns> The added vector, named v2, which is result of the rotation. Here is the following formula used (rc*v1=(xcos(rc)-ysin(rc),xsin(rc)+ycos(rc))) </param>


let rot (v1:vec) (rc:float) : (vec) =
    let v2A = fst v1 * cos rc - snd v1 * sin rc 
    let v2B = fst v1 * sin rc +  snd v1 * cos rc 
    let v2 = vec (v2A,v2B)
    v2




let vectorA = vec (1.2 , 1.5)
let vectorB = vec (1.2 , 1.5)
let c = 2.0
let rc : float = System.Math.PI
let x = add (vectorA) (vectorB)
let z = mul (vectorA) (c)
let p = rot (vectorA) (rc)

printfn "%A and %A and %A" x z p 