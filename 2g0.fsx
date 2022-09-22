type vec = float * float

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