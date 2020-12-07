#load "str.cma";;

module S = Set.Make(String);;
module M = Map.Make(String);;

let bag str = 
    let array = Array.of_list (String.split_on_char ' ' (String.trim str)) in
    let sub = Array.sub array 1 (Array.length array - 2) in
    String.concat " " (Array.to_list sub)

let bags f str = 
    match Str.split (Str.regexp " bags contain ") str with
    | x1::x2::[] when x2 = "no other bags." -> (x1, [])
    | x1::x2::[] -> 
        let trimmed = String.trim (String.sub x2 0 ((String.length x2) - 1)) in
        let list = String.split_on_char ',' trimmed in
        (x1, List.map f list)
    | _ -> failwith "Napaka"

let rec make_graph2 list = 
    match list with
    | [] -> M.empty
    | line::rest -> 
        let x, xs = bags bag line in
        let rec add_edges graph vertex verteces = 
            match verteces with
            | [] -> graph
            | x::xs -> let new_graph = 
                if M.mem x graph then M.add x (vertex::(M.find x graph)) graph
                else M.add x [vertex] graph
                in
                add_edges new_graph vertex xs
        in
        add_edges (make_graph2 rest) x xs 

let rec dfs graph visited v = 
    (*print_endline v;*)
    let visited = S.add v visited in
    if not (M.mem v graph) then visited else
    let adj = M.find v graph in
    match adj with
    | [] -> visited
    | u::us when not (S.mem u visited) -> 
        let visited = dfs (M.add v us graph) visited u in
        dfs (M.add v us graph) visited v
    | u::us -> dfs (M.add v us graph) visited v

let solve list = 
    S.cardinal (dfs (make_graph2 list) S.empty "shiny gold") - 1

let bag_count str = 
    let array = Array.of_list (String.split_on_char ' ' (String.trim str)) in
    let sub = Array.sub array 1 (Array.length array - 2) in
    (String.concat " " (Array.to_list sub), int_of_string array.(0))

let rec make_graph list = 
    match list with
    | [] -> M.empty
    | line::rest -> 
        let x, xs = bags bag_count line in
        M.add x xs (make_graph rest)

(* graf mora biti usmerjen acikličen graf*)
let rec dfs2 graph numbers v = 
    (*print_endline v;*)
    let numbers = if M.mem v numbers then numbers else M.add v 1 numbers in
    if not (M.mem v graph) then numbers else
    let adj = M.find v graph in
    match adj with
    | [] -> numbers
    | (u, n)::us when not (M.mem u numbers) -> 
        let numbers = dfs2 graph numbers u in
        dfs2 (M.add v us graph) (M.add v ((M.find v numbers) + (M.find u numbers) * n) numbers) v
    | (u, n)::us ->
        dfs2 (M.add v us graph) (M.add v ((M.find v numbers) + (M.find u numbers) * n) numbers) v

let solve2 list = 
    M.find "shiny gold" (dfs2 (make_graph list) M.empty "shiny gold") - 1

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> solve
    |> string_of_int

let naloga2 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
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
    let vsebina_datoteke = preberi_datoteko "day_7/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_7/task_1.out" odgovor1;
    izpisi_datoteko "day_7/task_2.out" odgovor2
