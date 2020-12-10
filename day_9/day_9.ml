
let rec is_sum num array = 
    let rec aux i j = 
        if array.(i) + array.(j) = num then
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

let rec find_sum num array = 
    let rec aux sum i j = 
        if sum = num then
            (i, j)
        else if j + 1 < Array.length array then
            aux (sum + array.(j + 1)) i (j + 1)
        else if i + 2 < Array.length array then
            aux (array.(i + 1) + array.(i + 2)) (i + 1) (i + 2)
        else
            failwith "Napaka"
    in
    aux (array.(0) + array.(1)) 0 1

(* tukaj je treba vstaviti rešitev prve naloge namesto 393911906*)
let solve2 odgovor1 array = 
    let i, j = find_sum odgovor1 array in
    let sub = Array.sub array i (j - i + 1) in
    let min = Array.fold_left min sub.(0) sub in
    let max = Array.fold_left max sub.(0) sub in
    min + max

let naloga2 odgovor1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> List.map int_of_string
    |> Array.of_list
    |> solve2 (int_of_string odgovor1)
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
    let vsebina_datoteke = preberi_datoteko "day_9/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke in
    let odgovor2 = naloga2 odgovor1 vsebina_datoteke in
    izpisi_datoteko "day_9/task_1.out" odgovor1;
    izpisi_datoteko "day_9/task_2.out" odgovor2
