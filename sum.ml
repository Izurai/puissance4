open Base
open Stdio

let rec read_in_stream accum =
        let line = In_channel.input_line In_channel.stdin in
        match line with
        | None -> accum
        | Some x -> read_in_stream (accum +. Float.of_string x)
let () = (* like the main() function *)
        printf "Total: %F\n" (read_in_stream 0.0)
