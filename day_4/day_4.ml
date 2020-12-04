let rec contains el list = 
    match list with
    | [] -> false
    | x::xs -> if x = el then true else contains el xs

let rec contains_all els list = 
    match els with
    | [] -> true
    | x::xs -> if contains x list then contains_all xs list else false

let is_valid passport = 
    let sep = List.map (String.split_on_char ':') (String.split_on_char ' ' passport) in
    let fields = List.map List.hd sep in
    let required = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"] in
    let valid = contains_all required fields in
    if valid then 1
    else 0

let count_valid list = 
    let rec aux sum acc rest = 
        match rest with
        | [] -> sum + is_valid (String.concat " " acc)
        | x::xs ->
            if x = "" then aux (sum + is_valid (String.concat " " acc)) [] xs
            else aux sum (x::acc) xs
    in
    aux 0 [] list

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> count_valid
    |> string_of_int

let is_valid field str = 
    if field = "byr" then
        let num = int_of_string str in
        String.length str = 4 && 1920 <= num && num <= 2002
    else if field = "iyr" then
        let num = int_of_string str in
        String.length str = 4 && 2010 <= num && num <= 2020
    else if field = "eyr" then
        let num = int_of_string str in
        String.length str = 4 && 2020 <= num && num <= 2030
    else if field = "hgt" then (*TODO*)
        let num = int_of_string str in
        String.length str = 4 && 2020 <= num && num <= 2030
    else if field = "hcl" then
        let rec hcl_ok str i = 
            if i = 0 then str.[0] = '#'
            else contains str.[i] ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'a';'b';'c';'d';'e';'f'] && hcl_ok str (i-1)
        in hcl_ok str ((String.length str) - 1)
    else if field = "ecl" then
        contains str ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl;" "oth"]
    else if field = "pid" then
        let rec hcl_ok str i = 
            if i = 0 then str.[0] = '#'
            else contains str.[i] ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'a';'b';'c';'d';'e';'f'] && hcl_ok str (i-1)
        in hcl_ok str ((String.length str) - 1)

    

let naloga2 vsebina_datoteke =
    "skor"

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
    let vsebina_datoteke = preberi_datoteko "day_4/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_4/task_1.out" odgovor1;
    izpisi_datoteko "day_4/task_2.out" odgovor2
