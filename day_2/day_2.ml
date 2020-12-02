

let a = "whyasajsajfjaf"
let b = a.[0]

let count str char = 
    let rec aux count index = 
        if index < 0 then count
        else if str.[index] = char then aux (count + 1) (index - 1)
        else aux count (index - 1)
    in
    aux 0 ((String.length str) - 1)

let is_correct char min max pass = 
    let num = count pass char in
    min <= num && num <= max

let naloga1 vsebina_datoteke =
    vsebina_datoteke

let naloga2 vsebina_datoteke =
    string_of_int (String.length vsebina_datoteke)

(*
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
    let vsebina_datoteke = preberi_datoteko "day_2/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_2/task_1.out" odgovor1;
    izpisi_datoteko "day_2/task_2.out" odgovor2
*)