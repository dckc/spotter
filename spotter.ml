open CamomileLibraryDefault.Camomile;;

type monte = <
    call : string
        -> monte list
        -> (monte * monte) list
        -> monte option;
    stringOf : string;
>;;

let nullObj : monte = object
    method call verb args namedArgs = None
    method stringOf = "<null>"
end;;

let rec intObj i : monte = object
    method call verb args namedArgs = match (verb, args) with
        | ("next", []) -> Some (intObj (i + 1))
        | ("previous", []) -> Some (intObj (i - 1))
        | _ -> None
    method stringOf = string_of_int i
end;;

exception Refused of (string * monte list * monte list);;

let call_exn target verb args namedArgs : monte =
    match target#call verb args namedArgs with
        | Some rv -> rv
        | None -> raise (Refused (verb, args, (List.map fst namedArgs)));;

let prettyPrint formatter obj = Format.pp_print_string formatter obj#stringOf;;

type
mastexpr = NullExpr
         | LiteralExpr of monte
         | CharExpr of string
         | DoubleExpr of float
         | IntExpr of Z.t
         | StringExpr of string
         | NounExpr of string
         | BindingExpr of string
         | SeqExpr of mastexpr list
         | CallExpr of (mastexpr * string * mastexpr list * mastnarg list)
         | DefExpr of (mastpatt * mastexpr * mastexpr)
         | EscapeExpr of (mastpatt * mastexpr)
         | EscapeCatchExpr of (mastpatt * mastexpr * mastpatt * mastexpr)
         | ObjectExpr of (string * mastpatt * mastexpr * mastexpr list * mastmethod list * mastmatcher list)
         | AssignExpr of (string * mastexpr)
         | FinallyExpr of (mastexpr * mastexpr)
         | TryExpr of (mastexpr * mastpatt * mastexpr)
         | HideExpr of mastexpr
         | IfExpr of (mastexpr * mastexpr * mastexpr)
         | MetaStateExpr
         | MetaContextExpr
and
mastmethod = (string * string * mastpatt list * mastnpatt list * mastpatt * mastpatt)
and
mastmatcher = (mastpatt * mastexpr)
and
mastnarg = (mastexpr * mastexpr)
and
mastnpatt = (mastexpr * mastpatt * mastexpr)
and
mastpatt = IgnorePattern of mastexpr
         | FinalPattern of (string * mastexpr)
         | VarPattern of (string * mastexpr)
         | ListPattern of mastpatt list
         | ViaPattern of (mastexpr * mastpatt)
         | BindingPatt of string
;;

exception InvalidMAST of (string * char * int);;
let throw_invalid_mast ic c message = raise (InvalidMAST (message, c, pos_in ic))

let input_varint ic =
    let rec go shift acc =
        let b = Z.of_int (input_byte ic) in
        let n = Z.logor acc (Z.shift_left (Z.logand b (Z.of_int 0x7f)) shift) in
        if not (Z.testbit b 7) then n else go (shift + 7) n
    in go 0 Z.zero;;

type span = OneToOne of (Z.t * Z.t * Z.t * Z.t)
          | Blob of (Z.t * Z.t * Z.t * Z.t)
;;
let input_span ic = match input_char ic with
    | 'S' -> OneToOne (input_varint ic, input_varint ic, input_varint ic, input_varint ic)
    | 'B' -> Blob (input_varint ic, input_varint ic, input_varint ic, input_varint ic)
    |  x  -> throw_invalid_mast ic x "input_span"
;;

let input_str ic = really_input_string ic (Z.to_int (input_varint ic));;

(* A growing mutable list that is indexed backwards. Simulates a portion of
 * the Python list API. *)
let backlist () = object
    val mutable l = []
    val mutable len = 0
    method push x = l <- x :: l; len <- len + 1
    method get i = List.nth l (len - 1 - i)
    method tl = List.hd l
end;;

let mast_context ic = object (self)
    (* Compared to the classic MAST context, we store the exprs and patts
     * backwards, so that we can build them quickly. *)
    val exprs = backlist ()
    val patts = backlist ()

    method private input_expr_ref = fst (exprs#get (Z.to_int (input_varint ic)))
    method private input_expr c = match c with
        | 'L' -> (match input_char ic with
            | 'N' -> NullExpr
            |  x  -> throw_invalid_mast ic x "literal"
        )
        | 'N' -> NounExpr (input_str ic)
        |  x  -> throw_invalid_mast ic x "input_expr"
    method private input_patt = match input_char ic with
        | 'I' -> IgnorePattern self#input_expr_ref
        | 'F' -> let n = input_str ic in FinalPattern (n, self#input_expr_ref)
        | 'V' -> let n = input_str ic in VarPattern (n, self#input_expr_ref)
        |  x  -> throw_invalid_mast ic x "input_patt"
    method private input_expr_and_span c =
        let e = self#input_expr c in (e, input_span ic)
    method private input_patt_and_span =
        let p = self#input_patt in (p, input_span ic)

    method input_all_exprs =
        try while true do
            match input_char ic with
                | 'P' -> patts#push self#input_patt_and_span
                |  c  -> exprs#push (self#input_expr_and_span c)
        done with | End_of_file -> ()
    method input_last_expr = self#input_all_exprs; exprs#tl
end;;

exception InvalidMagic;;
let mast_magic = "Mont\xe0MAST\x01";;
let open_in_mast path = let ic = open_in_bin path in
    (* Check the magic number. *)
    for i = 0 to String.length mast_magic - 1 do
        if input_char ic <> String.get mast_magic i then
            (close_in ic; raise InvalidMagic)
    done; ic
;;

let read_mast filename =
    let ic = open_in_mast filename in
    let context = mast_context ic in
    let rv = context#input_last_expr in
    close_in ic;
    rv;;
