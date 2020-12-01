(*Treba je le pravilno nastimati a*)
let a = [|100; 2; 3; 4; 6; 9; 10; 23; 30|]

let elementa_z_vsoto array sum= 
    let rec aux i j = 
        if i >= j then None
        else let s = array.(i) + array.(j) in 
            if s > sum then aux i (j - 1)
            else if s < sum then aux (i + 1) j
            else Some (array.(i), array.(j))
    in aux 0 (Array.length array - 1)

let resi array = 
    Array.sort (-) array;
    let pair = elementa_z_vsoto array 10 in
    match pair with
    | None -> failwith "Napaka"
    | Some (a, b) -> a * b

let resitev = resi a