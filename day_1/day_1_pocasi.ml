(*
Kako razdeliti string na dele glede na nek znak, sem pogledal na stackoverfov:
https://stackoverflow.com/questions/23204953/does-ocaml-have-string-split-function-like-python
*)


let rec vsota1 list sum = 
    match list with
    | [] -> None
    | x::xs -> if x = sum then (Some x) else vsota1 xs sum

let rec vsota2 list sum = 
    match list with
    | [] -> None
    | x::xs -> let out = vsota1 xs (sum - x) in
        match out with
        | None -> vsota2 xs sum
        | Some y -> Some (x, y)

let rec vsota3 list sum = 
    match list with
    | [] -> None
    | x::xs -> let out = vsota2 xs (sum - x) in
        match out with
        | None -> vsota3 xs sum
        | Some (y, z) -> Some (x, y, z)

let resi1 list = 
    let pair = vsota2 list 2020 in
    match pair with
    | None -> failwith "Napaka"
    | Some (x, y) -> x * y

let resi2 list = 
    let pair = vsota3 list 2020 in
    match pair with
    | None -> failwith "Napaka"
    | Some (x, y, z) -> x * y * z

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
    |> List.map int_of_string
    |> resi1
    |> string_of_int

let naloga2 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
    |> List.map int_of_string
    |> resi2
    |> string_of_int

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