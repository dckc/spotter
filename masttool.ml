module Record = Map.Make(String)

type json =
  | JNull
  | JBool of bool
  | JInt of int (* XXX ZZ *)
  | JStr of string
  | JArray of json list
  | JObject of json Record.t

let jprops ps = JObject (Record.of_seq (List.to_seq ps))

let reader inch =
  object
    method read_char = input_char inch
    method read_string length = really_input_string length
  end

let mast_magic = "Mont\xe0MAST"

exception InvalidMagic

let check_magic rd =
  for i = 0 to String.length mast_magic - 1 do
    if rd#read_char <> mast_magic.[i] then raise InvalidMagic
  done;
  let the_version = 1 in
  if rd#read_char <> (Char.chr the_version) then raise InvalidMagic;
  jprops [(mast_magic, JInt the_version)]

(*
let eat_tag rd: json =
  match rd#read_char with
  | 'L'
 *)

let decode_in inch = check_magic (reader inch)

let decode open_in_bin path = decode_in (open_in_bin path)

let main () =
  for i = 1 to Array.length Sys.argv - 1 do
    let path = Sys.argv.(i) in
    Printf.printf "[%i] %s\n" i path ;
    decode open_in_bin path
  done
