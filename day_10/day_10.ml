

let solve list = 
    let sorted = List.sort compare list in
    let rec aux (one, three) list2 = 
        match list2 with
        | [] -> failwith "prekratek seznam"
        | [a] -> (one, three)
        | a::b::rest -> 
            if b - a = 1 then 
                (*let _ = print_endline ((string_of_int a) ^ " -> " ^ (string_of_int b) ^ "   1") in*)
                aux (one + 1, three) (b::rest)
            else if b - a = 3 then 
                (*let _ = print_endline ((string_of_int a) ^ " -> " ^ (string_of_int b) ^ "   3") in*)
                aux (one, three + 1) (b::rest)
            else aux (one, three) (b::rest)
    in
    let (one, three) = aux (0, 0) (0::sorted) in
    (*print_endline ((string_of_int one) ^ ", " ^ (string_of_int three));*)
    one * (three + 1)

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> List.map int_of_string
    |> solve
    |> string_of_int


let naloga2 vsebina_datoteke =
    "še malo"

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
    let vsebina_datoteke = preberi_datoteko "day_10/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_10/task_1.out" odgovor1;
    izpisi_datoteko "day_10/task_2.out" odgovor2
