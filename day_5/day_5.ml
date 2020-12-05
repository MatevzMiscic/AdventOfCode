
let one char = char = 'B' || char = 'R'

let to_decimal str = 
    let n = String.length str in
    let rec aux a b i = 
        if i = n then a
        else let c = (a + b) / 2 in
            if one str.[i] then aux c b (i + 1)
            else aux a c (i + 1)
    in
    aux 0 1024 0

let max a b = if a >= b then a else b

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> List.map to_decimal
    |> List.fold_left max 0
    |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "day_5/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_5/task_1.out" odgovor1;
    izpisi_datoteko "day_5/task_2.out" odgovor2
