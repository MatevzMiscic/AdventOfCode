module S = Set.Make(Char);;

let count_chars list = 
    let str = String.concat "" list in
    let rec aux set count i = 
        if i < 0 then count
        else if S.mem str.[i] set then 
            aux set count (i - 1)
        else aux (S.add str.[i] set) (count + 1) (i - 1)
    in
    aux S.empty 0 ((String.length str) - 1)

let solve f list = 
    let rec aux sum acc rest = 
        match rest with
        | [] -> sum + f acc
        | x::xs ->
            if x = "" then aux (sum + f acc) [] xs
            else aux sum (x::acc) xs
    in
    aux 0 [] list

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> solve count_chars
    |> string_of_int

let intersection list = 
    let set_from_string str = 
        let rec aux set i = 
            if i < 0 then set
            else aux (S.add str.[i] set) (i - 1)
        in
        aux S.empty ((String.length str) - 1)
    in
    let sets = List.map set_from_string list in
    match sets with
    | [] -> 0
    | x::xs -> S.cardinal (List.fold_left S.inter x xs)


let naloga2 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> solve intersection
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
    let vsebina_datoteke = preberi_datoteko "day_6/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_6/task_1.out" odgovor1;
    izpisi_datoteko "day_6/task_2.out" odgovor2
