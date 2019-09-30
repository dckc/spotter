open Spotter

let safeScope = Dict.add "null" (bindingObj (finalSlotObj nullObj)) Dict.empty

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    Printf.printf "[%i] %s\n" i Sys.argv.(i);
    let filename = Sys.argv.(i) in
    let expr = read_mast filename in
    try
      let (result, _) = (expr safeScope) in
      Printf.printf "==> %s\n" (result#stringOf)
    with
    | Refused (verb, args, namedArgs) -> Printf.printf "Refused: XXX.%s(...)\n" verb
    | UserException (span) -> Printf.printf "UserException at %s\n" (string_of_span span)
  done
