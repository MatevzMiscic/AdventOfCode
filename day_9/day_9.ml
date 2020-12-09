module S = Set.Make(Int);;
module M = Map.Make(String);;

let rec is_sum num array = 
    (*print_endline (string_of_int (Array.length array));*)
    let rec aux i j = 
        (*print_endline ((string_of_int i) ^ ", " ^ (string_of_int j));*)
        if array.(i) + array.(j) = num then
            (*let _ = print_endline "true" in*)
            true
        else if j + 1 < Array.length array then
            aux i (j + 1)
        else if i + 2 < Array.length array then
            aux (i + 1) (i + 2)
        else false
    in
    aux 0 1

let solve array = 
    let size = 25 in
    let rec find index = 
        if is_sum array.(index) (Array.sub array (index - size) size) then find (index + 1)
        else array.(index)
    in
    find size

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> List.map int_of_string
    |> Array.of_list
    |> solve
    |> string_of_int

let naloga2 vsebina_datoteke =
    "bo enkrat"

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
    let vsebina_datoteke = preberi_datoteko "day_9/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_9/task_1.out" odgovor1;
    izpisi_datoteko "day_9/task_2.out" odgovor2
