module S = Set.Make(Char);;

let count_chars str = 
    let rec aux set count i = 
        if i < 0 then count
        else if S.mem str.[i] set then 
            aux set count (i - 1)
        else aux (S.add str.[i] set) (count + 1) (i - 1)
    in
    aux S.empty 0 ((String.length str) - 1)

let solve list = 
    let rec aux sum acc rest = 
        match rest with
        | [] -> sum + count_chars (String.concat "" acc)
        | x::xs ->
            if x = "" then aux (sum + count_chars (String.concat "" acc)) [] xs
            else aux sum (x::acc) xs
    in
    aux 0 [] list

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> solve
    |> string_of_int

let naloga2 vsebina_datoteke =
    "lol"

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
