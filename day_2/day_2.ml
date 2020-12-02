

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

let process_line line = 
    let array = Array.of_list (String.split_on_char ' ' line) in
    let pass = array.(2) in
    let char = array.(1).[0] in
    let range = Array.of_list (String.split_on_char '-' array.(0)) in
    let min = int_of_string range.(0) in
    let max = int_of_string range.(1) in
    is_correct char min max pass

let rec count el list = 
    match list with
    | [] -> 0
    | x::xs -> if x = el then 1 + count el xs else count el xs

let arr = process_line "8-16 w: vwxmrfwwlwtswwtcww"

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> List.map process_line
    |> count true
    |> string_of_int

let naloga2 vsebina_datoteke =
    string_of_int (String.length vsebina_datoteke)

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
