module M = Map.Make(Int);;

let solve list = 
    let sorted = List.sort compare list in
    let rec aux (one, three) list2 = 
        match list2 with
        | [] -> failwith "prekratek seznam"
        | [a] -> (one, three)
        | a::b::rest -> 
            if b - a = 1 then 
                aux (one + 1, three) (b::rest)
            else if b - a = 3 then 
                aux (one, three + 1) (b::rest)
            else aux (one, three) (b::rest)
    in
    let (one, three) = aux (0, 0) (0::sorted) in
    one * (three + 1)

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> List.map int_of_string
    |> solve
    |> string_of_int

let change el delta map = 
    if M.mem el map then M.add el (M.find el map + delta) map
    else M.add el delta map

let solve2 list = 
    let sorted = List.sort compare list in
    let list2 = (0::sorted) @ [List.hd (List.rev sorted) + 3] in
    let rec count_paths map list = 
        match list with
        | [] -> failwith "prekratek seznam"
        | [x] -> M.find x map
        | a::b::rest -> 
            let map = change a 0 map in
            let delta = M.find a map in
            let map = map |> change (a+1) delta |> change (a+2) delta |> change (a+3) delta in 
            count_paths map (b::rest)
    in
    count_paths (M.add 0 1 M.empty) list2

let naloga2 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> List.map int_of_string
    |> solve2
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
    let vsebina_datoteke = preberi_datoteko "day_10/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_10/task_1.out" odgovor1;
    izpisi_datoteko "day_10/task_2.out" odgovor2
