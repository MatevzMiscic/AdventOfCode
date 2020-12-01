(*
Kako razdelimo string na dele glede na nek znak, sem pogledal na stackoverfov:
https://stackoverflow.com/questions/23204953/does-ocaml-have-string-split-function-like-python
*)

let elementa_z_vsoto array sum = 
    let rec aux i j = 
        if i >= j then None
        else let s = array.(i) + array.(j) in 
            if s > sum then aux i (j - 1)
            else if s < sum then aux (i + 1) j
            else Some (array.(i), array.(j))
    in aux 0 (Array.length array - 1)

let resi array = 
    Array.sort (-) array;
    let pair = elementa_z_vsoto array 2020 in
    match pair with
    | None -> failwith "Napaka"
    | Some (a, b) -> a * b


let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
    |> List.map int_of_string
    |> Array.of_list
    |> resi
    |> string_of_int

let seznam array = 
    let n = Array.length array in
    let vsota_brez_el k = 
        let na_mestu j = if j < k then array.(j) else array.(j + 1)
        in
        let polje_brez_k = Array.init (n - 1) na_mestu
        in elementa_z_vsoto polje_brez_k (2020 - array.(k))
    in
    Array.init n vsota_brez_el

let naloga2 vsebina_datoteke =
    "kmalu"
    

let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko "day_1/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_1/day_1_1.out" odgovor1;
    izpisi_datoteko "day_1/day_1_2.out" odgovor2