(* MAST to JSON converter

Example:

  def x := [1, "abc", null]; 3

compiles to 122 bytes of mast. This JSON representation can be handy:

{"magic": "Mont\xe0MAST",
 "version": 1,
 "exprs": [null, null,
           {"NounExpr": "_makeList", "span": {"blob": [1, 6, 1, 25]}},
           {"literal": 1, "span": {"str": [1, 10, 1, 11]}},
           {"literal": "abc", "span": {"str": [1, 13, 1, 18]}},
           {"NounExpr": "null", "span": {"str": [1, 20, 1, 24]}},
           {"CallExpr": "run",
            "target": 2,
            "args": [3, 4, 5],
            "named args": [],
            "span": {"blob": [1, 6, 1, 25]}},
           {"DefExpr": 0,
            "exit": 1,
            "rhs": 6,
            "span": {"blob": [1, 0, 1, 25]}},
           {"literal": 3, "span": {"blob": [1, 27, 2, 28]}},
           {"SeqExpr": [7, 8], "span": {"blob": [1, 0, 2, 0]}}],
 "patts": [{"FinalPatt": "x", "guard": 0, "span": {"str": [1, 0, 1, 5]}}]}

 *)

exception NotImplemented of string

let todo s = raise (NotImplemented s)

module JSON = struct
  type t =
    | JNull
    | JBool of bool
    | JInt of Z.t
    | JStr of string
    | JArray of t list
    | JObject of props

  and props = (string * t) list

  (* http://json.org/ *)
  let char_lit ch : char Seq.t =
    match ch with
    | '"' -> List.to_seq ['\\'; '"']
    | '\\' -> List.to_seq ['\\'; '\\']
    | '\x00' .. '\x1f' ->
        let hex4 = Printf.sprintf "\\u%04x" (Char.code ch) in
        String.to_seq hex4
    (* "Any codepoint except " or \ or control characters *)
    | _ -> Seq.return ch

  let str_lit_body s = String.of_seq (Seq.flat_map char_lit (String.to_seq s))

  let rec print_json ff item : unit =
    let open Format in
    match item with
    | JNull -> fprintf ff "null"
    | JBool b -> fprintf ff "%s" (if b then "true" else "false")
    | JInt i -> fprintf ff "%s" (Z.to_string i)
    | JStr s -> fprintf ff "\"%s\"" (str_lit_body s)
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
end

let reader inch =
  object
    method read_char = input_char inch

    method read_char_opt =
      try Some (input_char inch) with End_of_file -> None

    method read_byte = input_byte inch

    method read_string length = really_input_string inch length
  end

let mast_magic = "Mont\xe0MAST"

exception InvalidMagic
exception InvalidMAST

open JSON

let check_magic rd : JSON.props =
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

let eat_span rd : JSON.t =
  let tag = rd#read_char and i4 = List.map varint [rd; rd; rd; rd] in
  let key =
    match tag with 'S' -> "str" | 'B' -> "blob" | stag -> raise InvalidMAST
  in
  JObject [(key, JArray (List.map (fun i -> JInt i) i4))]

let eat_literal rd : JSON.t =
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

let eat_tags rd : JSON.t list * JSON.t list =
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
        let expr : (string * JSON.t) list =
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

let decode_in inch : JSON.t =
  let rd = reader inch in
  let m = check_magic rd
  and exprs, patts = eat_tags rd
  and rev l = List.rev_append l [] in
  JObject (m @ [("exprs", JArray (rev exprs)); ("patts", JArray (rev patts))])

let decode open_in_bin path = decode_in (open_in_bin path)

let main () =
  for i = 1 to Array.length Sys.argv - 1 do
    let path = Sys.argv.(i) in
    Printf.eprintf "[%i] %s\n" i path ;
    print_json Format.std_formatter (decode open_in_bin path)
  done
