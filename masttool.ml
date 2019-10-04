
type json =
  | JNull
  | JBool of bool
  | JInt of Z.t
  | JStr of string
  | JArray of json list
  | JObject of props
and props = (string * json) list

let rec print_json ff item: unit =
  let open Format in
  match item with
  | JNull -> fprintf ff "null"
  | JBool b -> fprintf ff "%s" (if b then "true" else "false")
  | JInt i -> fprintf ff "%d" (Z.to_int i) (* XXX big int *)
  | JStr s -> fprintf ff "\"%s\"" s (* XXX escaping *)
  | JArray vs ->
     let rec fmt_list ff vs = match vs with
       | [] -> ()
       | [v] -> print_json ff v
       | v::vs -> fprintf ff "%a, %a" print_json v fmt_list vs
     in fprintf ff "[%a]" fmt_list vs
  | JObject ps ->
     let rec fmt_props ff ps = match ps with
       | [] -> ()
       | [(k, v)] -> fprintf ff "\"%s\": %a" k print_json v
       | (k, v)::rest -> fprintf ff "\"%s\": %a, %a" k print_json v fmt_props rest
     in fprintf ff "{%a}" fmt_props ps

let reader inch =
  object
    method read_char = input_char inch
    method read_char_opt =
      try
        Some (input_char inch)
      with
      | End_of_file -> None
    method read_byte = input_byte inch
    method read_string length = really_input_string length
  end

let mast_magic = "Mont\xe0MAST"

exception NotImplemented of string
let todo s = raise (NotImplemented s)

exception InvalidMagic
exception InvalidMAST

let check_magic rd: props =
  for i = 0 to String.length mast_magic - 1 do
    if rd#read_char <> mast_magic.[i] then raise InvalidMagic
  done;
  let the_version = 1 in
  if rd#read_char <> (Char.chr the_version) then raise InvalidMagic;
  [("magic", JStr mast_magic); ("version", JInt (Z.of_int the_version))]

let varint rd =
  let rec go shift acc =
    let b = Z.of_int rd#read_byte in
    let n = Z.logor acc (Z.shift_left (Z.logand b (Z.of_int 0x7f)) shift) in
    if not (Z.testbit b 7) then n else go (shift + 7) n in
  go 0 Z.zero

let eat_span rd: json =
  Printf.printf "(eat_span)\n";
  let tag = rd#read_char in
  let v4 =
    let i1 = varint rd in
    let i2 = varint rd in
    let i3 = varint rd in
    let i4 = varint rd in
    JArray [JInt i1; JInt i2; JInt i3; JInt i4]
  and key = match tag with
    | 'S' -> "str"
    | 'B' -> "blob"
    | stag -> raise InvalidMAST
  in JObject [(key, v4)]

let eat_literal rd: json =
  match rd#read_char with
  | 'N' -> ignore (eat_span rd); JNull
  | lit_tag ->
     let value = match lit_tag with
       | 'I' ->
          let i = varint rd
          and zz i =
            let shifted = Z.shift_right i 1 in
            if Z.testbit i 0 then
              Z.logxor (Z.of_int (-1)) shifted
            else shifted
          in JInt (zz i)
       | ch -> todo ("literal tag:" ^ (Char.escaped ch))
     and s = (eat_span rd)
     in JObject [("literal", value); ("span", s)]

let eat_tags rd: (json list * json list) =
  let rec go exprs patts =
    let go_expr e = go (e::exprs) patts
    and go_patt p = go exprs (p::patts)
    in match rd#read_char_opt with
       | None -> (exprs, patts)
       | Some tag ->
          (match tag with
           | 'L' -> go_expr (eat_literal rd)
           | ch -> todo ("tag:" ^ (Char.escaped ch)))
  in go [] []

let decode_in inch: json =
  let rd =  (reader inch) in
  let m = check_magic rd
  and (exprs, patts) = eat_tags rd
  in JObject (m @ [("exprs", JArray exprs); ("patts", JArray patts)])

let decode open_in_bin path = decode_in (open_in_bin path)

let main () =
  for i = 1 to Array.length Sys.argv - 1 do
    let path = Sys.argv.(i) in
    Printf.printf "[%i] %s\n" i path ;
    decode open_in_bin path
  done
