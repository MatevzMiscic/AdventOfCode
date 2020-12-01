
let a = [|2; 3; 4; 6; 9; 10; 23; 30|]

let elementa_z_vsoto sum array = 
    let rec aux i j = 
        if i >= j then None
        else let s = array.(i) + array.(j) in 
            if s > sum then aux i (j - 1)
            else if s < sum then aux (i + 1) j
            else Some (i, j)
    in aux 0 (Array.length array - 1)

let r = elementa_z_vsoto a 10;;

let resi array = ()
    