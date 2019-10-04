type json =
  | JNull
  | JBool of bool
  | JInt of Z.t
  | JStr of string
  | JArray of json list
  | JObject of props

and props = (string * json) list

let rec print_json ff item : unit =
  let open Format in
  match item with
  | JNull -> fprintf ff "null"
  | JBool b -> fprintf ff "%s" (if b then "true" else "false")
  | JInt i -> fprintf ff "%d" (Z.to_int i) (* XXX big int *)
  | JStr s -> fprintf ff "\"%s\"" s (* XXX escaping *)
  | JArray vs ->
      let rec fmt_list ff vs =
        match vs with
        | [] -> ()
        | [v] -> print_json ff v
        | v :: vs -> fprintf ff "%a,@ %a" print_json v fmt_list vs in
      fprintf ff "[@[<hov 0>%a@]]" fmt_list vs
  | JObject ps ->
      let rec fmt_props ff ps =
        match ps with
        | [] -> ()
        | [(k, v)] -> fprintf ff "\"%s\": %a" k print_json v
        | (k, v) :: rest ->
            fprintf ff "\"%s\": %a,@ %a" k print_json v fmt_props rest in
      fprintf ff "{@[<hv 0>%a@]}" fmt_props ps

let reader inch =
  object
    method read_char = input_char inch

    method read_char_opt =
      try Some (input_char inch) with End_of_file -> None

    method read_byte = input_byte inch

    method read_string length = really_input_string inch length
  end

let mast_magic = "Mont\xe0MAST"

exception NotImplemented of string

let todo s = raise (NotImplemented s)

exception InvalidMagic
exception InvalidMAST

let check_magic rd : props =
  for i = 0 to String.length mast_magic - 1 do
    if rd#read_char <> mast_magic.[i] then raise InvalidMagic
  done ;
  let the_version = 1 in
  if rd#read_char <> Char.chr the_version then raise InvalidMagic ;
  [("magic", JStr mast_magic); ("version", JInt (Z.of_int the_version))]

let varint rd =
  let rec go shift acc =
    let b = Z.of_int rd#read_byte in
    let n = Z.logor acc (Z.shift_left (Z.logand b (Z.of_int 0x7f)) shift) in
    if not (Z.testbit b 7) then n else go (shift + 7) n in
  go 0 Z.zero

let input_str rd = rd#read_string (Z.to_int (varint rd))

let eat_span rd : json =
  let tag = rd#read_char in
  let v4 =
    let i1 = varint rd in
    let i2 = varint rd in
    let i3 = varint rd in
    let i4 = varint rd in
    JArray [JInt i1; JInt i2; JInt i3; JInt i4]
  and key =
    match tag with 'S' -> "str" | 'B' -> "blob" | stag -> raise InvalidMAST
  in
  JObject [(key, v4)]

let eat_literal rd : json =
  match rd#read_char with
  | 'N' ->
      ignore (eat_span rd) ;
      JNull
  | lit_tag ->
      let value =
        match lit_tag with
        | 'I' ->
            let i = varint rd
            and zz i =
              let shifted = Z.shift_right i 1 in
              if Z.testbit i 0 then Z.logxor (Z.of_int (-1)) shifted
              else shifted in
            JInt (zz i)
        | 'S' -> JStr (input_str rd)
        | ch -> todo ("literal tag:" ^ Char.escaped ch)
      and s = eat_span rd in
      JObject [("literal", value); ("span", s)]

let eat_tags rd : json list * json list =
  let rec go exprs patts =
    let go_expr e = go (e :: exprs) patts
    and go_patt p = go exprs (p :: patts)
    and many f rd =
      let l = Z.to_int (varint rd) in
      List.init l (fun _ -> f rd)
    and eat_expr rd = JInt (varint rd)
    and eat_patt rd = JInt (varint rd) in
    match rd#read_char_opt with
    | None -> (exprs, patts)
    | Some 'L' -> go_expr (eat_literal rd)
    | Some 'P' ->
        let patt =
          match rd#read_char with
          | 'F' ->
              let n = JStr (input_str rd) and g = JInt (varint rd) in
              [("FinalPatt", n); ("guard", g)]
          | ch -> todo ("patt tag:" ^ Char.escaped ch)
        and s = eat_span rd in
        go_patt (JObject (patt @ [("span", s)]))
    | Some tag ->
        let expr : (string * json) list =
          match tag with
          | 'N' -> [("NounExpr", JStr (input_str rd))]
          | 'S' ->
              let ixs = many eat_expr rd in
              [("SeqExpr", JArray ixs)]
          | 'C' ->
              let eat_narg rd =
                let n = eat_expr rd and v = eat_expr rd and s = eat_span rd in
                JObject [("arg name", n); ("value", v); ("span", s)]
              and t = eat_expr rd
              and v = JStr (input_str rd)
              and a = JArray (many eat_expr rd) in
              let na = JArray (many eat_narg rd) in
              [("CallExpr", v); ("target", t); ("args", a); ("named args", na)]
          | 'D' ->
              let p = eat_patt rd and ex = eat_expr rd and rhs = eat_expr rd in
              [("DefExpr", p); ("exit", ex); ("rhs", rhs)]
          | ch -> todo ("expr tag:" ^ Char.escaped ch)
        and s = eat_span rd in
        go_expr (JObject (expr @ [("span", s)])) in
  go [] []

let decode_in inch : json =
  let rd = reader inch in
  let m = check_magic rd
  and exprs, patts = eat_tags rd
  and rev l = List.rev_append l [] in
  JObject (m @ [("exprs", JArray (rev exprs)); ("patts", JArray (rev patts))])

let decode open_in_bin path = decode_in (open_in_bin path)

let main () =
  for i = 1 to Array.length Sys.argv - 1 do
    let path = Sys.argv.(i) in
    Printf.printf "[%i] %s\n" i path ;
    decode open_in_bin path
  done
