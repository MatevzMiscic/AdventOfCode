(*
Ta koda je vzeta iz spletne učilnice za programiranje 1. 
Uporablja se za branje iz datotek in pisanje v datoteke v rešitvah nalog za vsak dan, tudi če to ni eksplicitno napisano.
*)

let naloga1 vsebina_datoteke =
    vsebina_datoteke

let naloga2 vsebina_datoteke =
    string_of_int (String.length vsebina_datoteke)

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
    let vsebina_datoteke = preberi_datoteko "input.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_0_1.out" odgovor1;
    izpisi_datoteko "day_0_2.out" odgovor2