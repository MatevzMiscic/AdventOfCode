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

let count_valid f list = 
    let rec aux sum acc rest = 
        match rest with
        | [] -> sum + is_valid (String.concat " " acc)
        | x::xs ->
            if x = "" then aux (sum + f (String.concat " " acc)) [] xs
            else aux sum (x::acc) xs
    in
    aux 0 [] list

let naloga1 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> count_valid is_valid
    |> string_of_int

let is_field_valid field str = 
    if field = "byr" then
        let num = int_of_string str in
        String.length str = 4 && 1920 <= num && num <= 2002
    else if field = "iyr" then
        let num = int_of_string str in
        String.length str = 4 && 2010 <= num && num <= 2020
    else if field = "eyr" then
        let num = int_of_string str in
        String.length str = 4 && 2020 <= num && num <= 2030
    else if field = "hgt" then
        let n = String.length str in
        if n < 3 then false
        else 
            let num = int_of_string (String.sub str 0 (n-2)) in
            if str.[n-2] = 'c' && str.[n-1] = 'm' then
                150 <= num && num <= 193
            else if str.[n-2] = 'i' && str.[n-1] = 'n' then
                59 <= num && num <= 76
            else false
    else if field = "hcl" then
        let rec hcl_ok str i = 
            if i = 0 then str.[0] = '#'
            else contains str.[i] ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'a';'b';'c';'d';'e';'f'] && hcl_ok str (i-1)
        in (String.length str = 7) && hcl_ok str 6
    else if field = "ecl" then
        contains str ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
    else if field = "pid" then
        let rec pid_ok str i = 
            if i < 0 then true
            else contains str.[i] ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'] && pid_ok str (i-1)
        in String.length str = 9 && pid_ok str 8
    else true

let rec all_valid list = 
    match list with
    | [] -> true
    | x::xs ->
        if is_field_valid (List.hd x) (List.hd (List.tl x)) then all_valid xs
        else false

let is_valid2 passport = 
    let sep = List.map (String.split_on_char ':') (String.split_on_char ' ' passport) in
    let fields = List.map List.hd sep in
    let required = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"] in
    let valid = contains_all required fields && all_valid sep in
    if valid then 1
    else 0

let naloga2 vsebina_datoteke =
    vsebina_datoteke
    |> String.split_on_char '\n'
    |> count_valid is_valid2
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
    let vsebina_datoteke = preberi_datoteko "day_4/input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_4/task_1.out" odgovor1;
    izpisi_datoteko "day_4/task_2.out" odgovor2
