
type vec = float * float


///<summary>
/// Given parameters vectorA and vectorB, this function will add them together and output a new vec.
/// when vectorA = (1.1 , 1.2) and vectorB = (1.3 , 1.5), the output of add will be v3 = (1.4 , 1.7)
/// </summary> 
/// <param name = "vectorA" > A tuple in the type vec. </param>
/// <param name = "vectorB" > A 2nd tuple in the type vec. </param>
/// <returns> The added vector, named v3, which is the two param's added together. </param>

let add (v1:vec) (v2:vec) : (vec) =
    let v3A = fst v1 + fst v2
    let v3B = snd v1 + snd v2
    let v3 = vec (v3A,v3B)
    v3

let multiply (v1:vec) (v2:vec) : (vec) =
    let v3A = fst v1 * fst v2
    let v3B = snd v1 * snd v2
    let v3 = vec (v3A,v3B)
    v3

let vectorA = vec (1.2 , 1.5)
let vectorB = vec (1.2 , 1.5)
let x = add (vectorA) (vectorB)
let z = multiply (vectorA) (vectorB)

printfn "%A and %A" x z