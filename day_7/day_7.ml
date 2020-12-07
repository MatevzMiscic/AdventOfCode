#load "str.cma";;

module S = Set.Make(String);;
module M = Map.Make(String);;

let bag str = 
    let array = Array.of_list (String.split_on_char ' ' (String.trim str)) in
    let sub = Array.sub array 1 (Array.length array - 2) in
    String.concat " " (Array.to_list sub)

let bags str = 
    match Str.split (Str.regexp " bags contain ") str with
    | x1::x2::[] when x2 = "no other bags." -> (x1, [])
    | x1::x2::[] -> 
        let trimmed = String.trim (String.sub x2 0 ((String.length x2) - 1)) in
        let list = String.split_on_char ',' trimmed in
        (x1, List.map bag list)
    | _ -> failwith "Napaka"

let a = bags "dotted silver bags contain 2 dotted orange bags, 3 bright fuchsia bags, 5 bright tomato bags, 3 faded turquoise bags."

let rec make_graph list = 
    match list with
    | [] -> M.empty
    | line::rest -> 
        let x, xs = bags line in
        M.add x xs (make_graph rest)

let rec make_graph2 list = 
    match list with
    | [] -> M.empty
    | line::rest -> 
        let x, xs = bags line in
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
    print_endline v;
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

let list = [
    "light red bags contain 1 bright white bag, 2 muted yellow bags.";
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.";
    "bright white bags contain 1 shiny gold bag.";
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.";
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.";
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.";
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.";
    "faded blue bags contain no other bags.";
    "dotted black bags contain no other bags.";
]
(*
let graph = make_graph2 list
let s = dfs (make_graph2 list) S.empty "shiny gold"



    let visited = S.add v visited in
    let rec traverse adj visited = 
        match adj with
        | [] -> visited
        | v::vs when not S.mem v visited -> traverse vs

    let aux map graph list = 
        match list with
        | [] -> (map, graph)
        | line::rest -> 
            let x, xs = bags line in
*)

let m = M.empty
let m = M.add "a" [] m 
let m = M.add "a" (1::(M.find "a" m)) m

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> solve
    |> string_of_int

let naloga2 vsebina_datoteke =
    "kmalu"

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
