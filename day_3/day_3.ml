

let count map = 
    let rec aux acc rest row = 
        match rest with
        | [] -> acc
        | l::ls -> 
            if l.[(3 * row) mod (String.length l)] = '.' then aux acc ls (row + 1)
            else aux (acc + 1) ls (row + 1)
    in
    aux 0 map 0

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> count
    |> string_of_int

let naloga2 vsebina_datoteke =
    "1"

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
    let vsebina_datoteke = preberi_datoteko "day_3/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_3/task_1.out" odgovor1;
    izpisi_datoteko "day_3/task_2.out" odgovor2
