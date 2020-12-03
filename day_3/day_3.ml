

let count dx dy map = 
    let rec aux acc rest row = 
        match rest with
        | [] -> acc
        | l::ls -> 
            if row mod dy = 0 then
                let moves_right = row / dy in
                if l.[(dx * moves_right) mod (String.length l)] = '.' then aux acc ls (row + 1)
                else aux (acc + 1) ls (row + 1)
            else aux acc ls (row + 1)
    in
    aux 0 map 0

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> count 3 1
    |> string_of_int

let rec mul list = 
    match list with
    | [] -> 1
    | x::xs -> x * (mul xs)

let solve map = 
    let slopes = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)] in
    let f (dx, dy) = count dx dy map
    in
    slopes |> List.map f |> mul

let naloga2 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> solve 
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
    let vsebina_datoteke = preberi_datoteko "day_3/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_3/task_1.out" odgovor1;
    izpisi_datoteko "day_3/task_2.out" odgovor2
