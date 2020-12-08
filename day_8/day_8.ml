#load "str.cma";;

module S = Set.Make(Int);;
module M = Map.Make(String);;

let to_inst str = 
    match String.split_on_char ' ' str with
    | oper::arg::[] -> 
        let arg = int_of_string arg in
        let oper = match oper with
        | "acc" -> 0
        | "jmp" -> 1
        | _ -> 2
        in
        (oper, arg)
    | _ -> failwith "Napaka"

let rec run acc index seen instructions = 
    if S.mem index seen then acc
    else
        let seen = S.add index seen in
        match instructions.(index) with
        | (oper, arg) when oper = 0 -> run (acc + arg) (index + 1) seen instructions
        | (oper, arg) when oper = 1 -> run acc (index + arg) seen instructions
        | (oper, arg) -> run acc (index + 1) seen instructions

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> List.map to_inst
    |> Array.of_list
    |> run 0 0 S.empty
    |> string_of_int

let rec terminates change acc index seen instructions = 
    if S.mem index seen then -1
    else if index < 0 || (Array.length instructions) < index then
        -1
    else if index = Array.length instructions then
        acc
    else
        let seen = S.add index seen in
        match instructions.(index) with
        | (oper, arg) when oper = 0 -> terminates change (acc + arg) (index + 1) seen instructions
        | (oper, arg) when oper = 1 -> 
            if index = change then terminates change acc (index + 1) seen instructions
            else terminates change acc (index + arg) seen instructions
        | (oper, arg) -> 
            if index = change then terminates change acc (index + arg) seen instructions
            else terminates change acc (index + 1) seen instructions

let repair instructions = 
    let rec aux index = 
        let ter = terminates index 0 0 S.empty instructions in
        if ter <> -1 then ter
        else aux (index - 1)
    in
    aux (Array.length instructions - 1)

let naloga2 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> List.map to_inst
    |> Array.of_list
    |> repair
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
    let vsebina_datoteke = preberi_datoteko "day_8/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_8/task_1.out" odgovor1;
    izpisi_datoteko "day_8/task_2.out" odgovor2
