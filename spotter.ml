open CamomileLibraryDefault.Camomile

type monte =
  < call: string -> monte list -> (monte * monte) list -> monte option
  ; stringOf: string
  ; unwrap: monteprim option >

and monteprim =
  | MNull
  | MBool of bool
  | MChar of int
  | MDouble of float
  | MInt of Z.t
  | MStr of string
  | MList of monte list
  | MMap of (monte * monte) list

let prim_name p =
  match p with
  | MNull -> "Null"
  | MBool _ -> "Bool"
  | MChar _ -> "Char"
  | MDouble _ -> "Double"
  | MInt _ -> "Int"
  | MStr _ -> "Str"
  | MList _ -> "List"
  | MMap _ -> "Map"

(* Narrowing: Cast away extra non-private methods of Monte objects. *)
let to_monte
    (m :
      < call: string -> monte list -> (monte * monte) list -> monte option
      ; stringOf: string
      ; unwrap: monteprim option
      ; .. >) : monte =
  (m :> monte)

module UTF8D = struct
  (** `decode bs` gives either Ok (code, rest) or Error (consumed_bytes, rest) *)
  let decode1 (bs : int Seq.t) : (int * int Seq.t, int list * int Seq.t) result
      =
    let open Int in
    let mask2 = 0b11000000
    and mask3 = 0b11100000
    and mask4 = 0b11110000
    and mask5 = 0b11111000
    and mask b m = logand b (lognot m) in
    let is_0xxxxxxx b = shift_right b 7 = 0b0
    and is_110xxxxx b = shift_right b 5 = 0b110
    and is_1110xxxx b = shift_right b 4 = 0b1110
    and is_11110xxx b = shift_right b 3 = 0b11110
    and cat hi n lo = logor (shift_left hi n) lo in
    match bs () with
    | Nil -> Error ([], bs)
    | Cons (b0, b1n) when is_0xxxxxxx b0 -> Ok (b0, b1n)
    | Cons (b0, b1n) -> (
      match b1n () with
      | Nil -> Error ([b0], b1n)
      | Cons (b1, b2n) when is_110xxxxx b0 ->
          let code = cat (mask b0 mask3) 6 (mask b1 mask2) in
          Ok (code, b2n)
      | Cons (b1, b2n) -> (
        match b2n () with
        | Nil -> Error ([b0; b1], b2n)
        | Cons (b2, b3n) when is_1110xxxx b0 ->
            let code =
              cat (cat (mask b0 mask4) 6 (mask b1 mask2)) 6 (mask b2 mask2)
            in
            Ok (code, b3n)
        | Cons (b2, b3n) -> (
          match b3n () with
          | Nil -> Error ([b0; b1; b2], b3n)
          | Cons (b3, b4n) when is_11110xxx b0 ->
              let code =
                cat
                  (cat
                     (cat (mask b0 mask5) 6 (mask b1 mask2))
                     6 (mask b2 mask2))
                  6 (mask b3 mask2) in
              Ok (code, b4n)
          | Cons (b3, b4n) -> Error ([b0; b1; b2; b3], b4n) ) ) )
end

let rec seq_of_chan inch : int Seq.t =
 fun () ->
  try
    let b0 = input_byte inch in
    Seq.Cons (b0, seq_of_chan inch)
  with End_of_file -> Nil

module type MAST = sig
  type span

  val oneToOne : Z.t * Z.t * Z.t * Z.t -> span
  val blob : Z.t * Z.t * Z.t * Z.t -> span

  type t
  type patt
  type narg
  type nparam
  type meth
  type matcher

  val charExpr : int -> span -> t
  val doubleExpr : float -> span -> t
  val intExpr : Z.t -> span -> t
  val strExpr : string -> span -> t
  val nounExpr : string -> span -> t
  val bindingExpr : string -> span -> t
  val seqExpr : t list -> span -> t
  val callExpr : t -> string -> t list -> narg list -> span -> t
  val defExpr : patt -> t option -> t -> span -> t
  val escapeExpr : patt -> t -> span -> t
  val escapeCatchExpr : patt -> t -> patt -> t -> span -> t

  val objectExpr :
       string
    -> patt
    -> t option
    -> t list
    -> meth list
    -> matcher list
    -> span
    -> t

  val assignExpr : string -> t -> span -> t
  val tryExpr : t -> patt -> t -> span -> t
  val finallyExpr : t -> t -> span -> t
  val hideExpr : t -> span -> t
  val ifExpr : t -> t -> t option -> span -> t
  val metaStateExpr : span -> t
  val metaContextExpr : span -> t

  val metho :
       string
    -> string
    -> patt list
    -> nparam list
    -> t option
    -> t
    -> span
    -> meth

  val matche : patt -> t -> span -> matcher
  val namedArg : t -> t -> span -> narg
  val namedParam : t -> patt -> t option -> span -> nparam
  val ignorePatt : t option -> span -> patt
  val finalPatt : string -> t option -> span -> patt
  val varPatt : string -> t option -> span -> patt
  val listPatt : patt list -> span -> patt
  val viaPatt : t -> patt -> span -> patt
  val bindingPatt : string -> span -> patt
end

module Dict = Map.Make (String)

module AtomDict = Map.Make (struct
  type t = string * int

  let compare = compare
end)

let nullObj : monte =
  object
    method call verb args namedArgs = None

    method stringOf = "<null>"

    method unwrap = Some MNull
  end

let rec boolObj b : monte =
  object
    method call verb args namedArgs =
      match (verb, args) with
      | "pick", [x; y] -> Some (if b then x else y)
      | "not", [] -> Some (boolObj (not b))
      | _ -> None

    method stringOf = if b then "true" else "false"

    method unwrap = Some (MBool b)
  end

let char_lit c =
  match c with
  | _ when c > 0xffff -> Printf.sprintf "\\U%08x" c
  | _ when c > 0x7f -> Printf.sprintf "\\u%04x" c
  | _ when c < 0x20 -> Printf.sprintf "\\u%04x" c
  | _ when c = Char.code '\\' -> "\"\\\""
  | _ when c = Char.code '\"' -> "\"\"\""
  | _ -> Char.escaped (char_of_int c)

let charObj (c : int) : monte =
  object
    method call verb args namedArgs = match (verb, args) with _ -> None

    (* XXX quotes? *)
    method stringOf = "'" ^ char_lit c ^ "'"

    method unwrap = Some (MChar c)
  end

let doubleObj d : monte =
  object
    method call verb args namedArgs = match (verb, args) with _ -> None

    method stringOf = string_of_float d

    method unwrap = Some (MDouble d)
  end

let rec intObj i : monte =
  object
    method call verb args namedArgs =
      match (verb, args) with
      | "next", [] -> Some (intObj (Z.succ i))
      | "previous", [] -> Some (intObj (Z.pred i))
      | "add", [jj] -> (
        match jj#unwrap with
        | Some (MInt j) -> Some (intObj (Z.add i j))
        | _ -> None )
      | "multiply", [jj] -> (
        match jj#unwrap with
        | Some (MInt j) -> Some (intObj (Z.mul i j))
        | _ -> None )
      | _ -> None

    method stringOf = Z.to_string i

    method unwrap = Some (MInt i)
  end

let rec strObj s : monte =
  object
    method call verb (args : monte list) namedArgs : monte option =
      match (verb, args) with
      | "add", [otherObj] -> (
        match otherObj#unwrap with
        | Some (MStr other) -> Some (strObj (s ^ other))
        | Some (MChar other) ->
            let utf8 code = String.make 0 (Char.chr code) (* XXX *) in
            Some (strObj (s ^ utf8 other))
        | _ -> None (* WrongType? fwd ref *) )
      | "size", [] -> Some (intObj (Z.of_int (UTF8.length s)))
      | _ -> None

    method stringOf =
      let parts =
        List.map
          (fun ch -> char_lit (Char.code ch))
          (List.of_seq (String.to_seq s)) in
      "\"" ^ String.concat "" parts ^ "\""

    method unwrap = Some (MStr s)
  end

let bindingObj slot : monte =
  object
    method call verb args namedArgs =
      match (verb, args) with "get", [] -> Some slot | _ -> None

    method stringOf = "<binding>"

    method unwrap = None
  end

let finalSlotObj value : monte =
  object
    method call verb args namedArgs =
      match (verb, args) with "get", [] -> Some value | _ -> None

    method stringOf = "<final slot>"

    method unwrap = None
  end

let varSlotObj value : monte =
  object
    val mutable cell = value

    method call verb args namedArgs =
      match (verb, args) with
      | "get", [] -> Some cell
      | "put", [v] ->
          cell <- v ;
          Some nullObj
      | _ -> None

    method stringOf = "<var slot>"

    method unwrap = None
  end

type mspan =
  | OneToOne of (Z.t * Z.t * Z.t * Z.t)
  | Blob of (Z.t * Z.t * Z.t * Z.t)

let string_of_span span =
  let sos (x1, y1, x2, y2) =
    String.concat ":" (List.map Z.to_string [x1; y1; x2; y2]) in
  match span with OneToOne t -> "str:" ^ sos t | Blob t -> "blob:" ^ sos t

type mexn =
  | NotImplemented of (monte * string * monte list)
  | Refused of (monte * string * monte list * monte list)
  | Ejecting of (monte * monte)
  | DoubleThrown
  | WrongType of (monteprim * monte)
  | NameError of (string * mspan)
  | MissingNamedArg of (monte * mspan)
  | UserException of monte

let string_of_mexn m =
  match m with
  | NotImplemented (target, verb, args) ->
      "XXX not implemented: " ^ target#stringOf ^ "." ^ verb ^ "/"
      ^ string_of_int (List.length args)
  | Refused (target, verb, args, namedArgs) ->
      "Message refused: " ^ target#stringOf ^ "." ^ verb ^ "/"
      ^ string_of_int (List.length args)
  | Ejecting (payload, ej) ->
      "Ejector: " ^ ej#stringOf ^ "(" ^ payload#stringOf ^ ")"
  | DoubleThrown ->
      "An ejector has come forward with a complaint of being thrown...twice!"
  | WrongType (expected, actual) ->
      "Wrong type while unwrapping " ^ prim_name expected ^ ": "
      ^ actual#stringOf
  | NameError (name, span) ->
      "name error at " ^ string_of_span span ^ ": " ^ name
  | MissingNamedArg (k, span) ->
      "Named arg " ^ k#stringOf ^ " missing in call at span "
      ^ string_of_span span
  | UserException value -> "User-created exception : " ^ value#stringOf

exception MonteException of mexn

let loaderObj =
  object
    method call verb args namedArgs =
      match (verb, args) with
      | "import", [_] ->
          raise (MonteException (UserException (strObj "XXX loader not impl")))
      | _ -> None

    method stringOf = "<import>"

    method unwrap = None
  end

(* The main calling interface. Handles Miranda methods. Propagates exceptions
 * on failure. *)
let call_exn target verb args namedArgs : monte =
  match target#call verb args namedArgs with
  | Some rv -> rv
  | None -> (
    match (verb, args) with
    (* Miranda behaviors. *)
    | "_conformTo", [_] -> nullObj
    | "_sealedDispatch", [_] -> nullObj
    | "_uncall", [] -> nullObj
    | _ ->
        raise
          (MonteException
             (Refused (target, verb, args, List.map fst namedArgs))) )

let throwObj : monte =
  object
    method call verb args nargs =
      match (verb, args) with
      | "run", [value] -> raise (MonteException (UserException value))
      | "eject", [ej; value] ->
          let _ = call_exn ej "run" [value] [] in
          raise (MonteException (UserException value))
      | _ -> None

    method stringOf = "throw"

    method unwrap = None
  end

let dataGuardObj example : monte =
  let name = prim_name example in
  object
    method call verb args nargs =
      match (verb, args) with
      | "coerce", [specimen; exit] -> (
        match specimen#unwrap with
        | Some sp when prim_name sp = name -> Some specimen
        | _ ->
            Some
              (call_exn exit "run"
                 [strObj (specimen#stringOf ^ "does not conform to " ^ name)]
                 []) )
      | _, _ -> None

    method stringOf = name

    method unwrap = None
  end

let voidGuardObj : monte =
  object
    method call verb args nargs =
      match (verb, args) with
      | "coerce", [specimen; exit] ->
          Some
            (call_exn exit "run"
               [strObj (specimen#stringOf ^ "does not conform to Void")]
               [])
      | _, _ -> None

    method stringOf = "Void"

    method unwrap = None
  end

let todoGuardObj name : monte =
  object
    method call verb args nargs =
      match (verb, args) with
      | "coerce", [specimen; exit] ->
          Printf.printf "\nXXX %s.coerce(...) not implemented\n" name ;
          Some specimen
      | _ -> None

    method stringOf = "DeepFrozenStamp"

    method unwrap = None
  end

let flexMapObj (init : (monte * monte) list) : monte =
  object
    val mutable pairs = init

    method call verb args nargs = None

    (* XXX *)
    method stringOf = "<FlexMap>"

    method unwrap = None
  end

let sameEver leftObj rightObj : bool =
  if leftObj == rightObj then true
  else
    match (leftObj#unwrap, rightObj#unwrap) with
    | Some leftPrim, Some rightPrim -> leftPrim = rightPrim
    | _ ->
        raise
          (MonteException
             (NotImplemented (nullObj, "sameEver", [leftObj; rightObj])))

let rec listObj l : monte =
  object
    method call verb args namedArgs =
      match (verb, args) with
      | "asMap", [] when l = [] -> Some (mapObj [])
      | "diverge", [] -> Some (flexListObj l)
      | "size", [] -> Some (intObj (Z.of_int (List.length l)))
      | _ -> None

    method stringOf =
      "[" ^ String.concat " " (List.map (fun o -> o#stringOf) l) ^ "]"

    method unwrap = Some (MList l)
  end

and flexListObj (init : monte list) : monte =
  object
    val mutable items = init

    method call verb args nargs =
      match (verb, args) with
      | "push", [item] ->
          items <- item :: items ;
          Some nullObj
      | "snapshot", [] -> Some (listObj items)
      | _ -> None

    (* XXX *)
    method stringOf = "<FlexList>"

    method unwrap = None
  end

and mapObj (pairs : (monte * monte) list) : monte =
  let throwStr ej msg = call_exn throwObj "eject" [ej; strObj msg] [] in
  (* An iterator on a map, producing its keys and values. *)
  let _makeIterator () =
    object
      val mutable _index = 0

      method call verb args nargs =
        match (verb, args) with
        | "next", [ej] ->
            if _index < List.length pairs then (
              let k, v = List.nth pairs _index in
              let rv = listObj [k; v] in
              _index <- _index + 1 ;
              Some rv )
            else Some (throwStr ej "next/1: Iterator exhausted")
        | _ -> None

      method stringOf = "<mapIterator>"

      method unwrap = None
    end in
  object
    method call (verb : string) (args : monte list) namedArgs : monte option =
      match (verb, args) with
      (* XXX O(n) *)
      | "contains", [needle] ->
          Some (boolObj (List.exists (fun (k, _) -> sameEver k needle) pairs))
      | "diverge", [] -> Some (flexMapObj pairs)
      | "get", [needle] ->
          Some
            ( match List.find_opt (fun (k, _) -> sameEver k needle) pairs with
            | Some (_, v) -> v
            | _ -> throwStr throwObj "not found" )
      | "getKeys", [] -> Some (listObj (List.map fst pairs))
      | "getValues", [] -> Some (listObj (List.map snd pairs))
      | "_makeIterator", [] -> Some (_makeIterator ())
      | _ ->
          Printf.printf "\nXXX Map verb todo? %s\n" verb ;
          None

    method stringOf =
      let item (k, v) = String.concat " => " [k#stringOf; v#stringOf] in
      let items = String.concat ", " (List.map item pairs) in
      "[" ^ items ^ "]"

    method unwrap = Some (MMap pairs)
  end

let _makeList : monte =
  object
    method call verb args namedArgs =
      match verb with "run" -> Some (listObj args) | _ -> None

    method stringOf = "_makeList"

    method unwrap = None
  end

let unwrapList specimen =
  match specimen#unwrap with
  | Some (MList l) -> l
  | _ -> raise (MonteException (WrongType (MList [], specimen)))

let unwrapPair specimen =
  match specimen#unwrap with
  | Some (MList [a; b]) -> (a, b)
  (* XXX bad airity exception arm? *)
  | _ ->
      raise
        (MonteException
           (WrongType (MList [strObj "key"; strObj "val"], specimen)))

let unwrapMap specimen =
  match specimen#unwrap with
  | Some (MMap pairs) -> pairs
  | _ -> raise (MonteException (WrongType (MMap [], specimen)))

let unwrapBool specimen =
  match specimen#unwrap with
  | Some (MBool b) -> b
  | _ -> raise (MonteException (WrongType (MBool true, specimen)))

let unwrapStr specimen : string =
  match specimen#unwrap with
  | Some (MStr s) -> s
  | _ -> raise (MonteException (WrongType (MStr "", specimen)))

let _makeMap : monte =
  object
    method call verb (args : monte list) namedArgs : monte option =
      match (verb, args) with
      | "fromPairs", [pairsObj] ->
          let pairs = List.map unwrapPair (unwrapList pairsObj) in
          Some (mapObj pairs)
      | _ -> None

    method stringOf = "_makeMap"

    method unwrap = None
  end

let _equalizer : monte =
  object (self)
    method call verb args nargs =
      match (verb, args) with
      | "sameEver", [leftObj; rightObj] ->
          Some (boolObj (sameEver leftObj rightObj))
      | _ -> None

    method stringOf = "_equalizer"

    method unwrap = None
  end

let theMObj : monte =
  object
    method stringOf = "M"

    method call verb args nargs =
      match (verb, args) with
      | "callWithMessage", [target; message] ->
          let unwrapMessage specimen =
            match specimen#unwrap with
            | Some (MList [verbObj; argsObj; nargsObj]) ->
                (unwrapStr verbObj, unwrapList argsObj, unwrapMap nargsObj)
            | _ -> raise (MonteException (WrongType (MStr "", specimen))) in
          let verb, args, nargs = unwrapMessage message in
          Some (call_exn target verb args nargs)
      | _ ->
          Printf.printf "XXX TODO? M.%s/%d\n" verb (List.length args) ;
          None

    method unwrap = None
  end

let theRefObj : monte =
  object
    method stringOf = "Ref"

    method call verb args nargs =
      match (verb, args) with
      | "isNear", [specimen] ->
          Printf.printf "\nXXX TODO: Ref.isNear with promises and such\n" ;
          Some (boolObj true)
      | _ ->
          Printf.printf "\nXXX TODO? Ref.%s/%d\n" verb (List.length args) ;
          None

    method unwrap = None
  end

let todoObj name : monte =
  object
    method call verb args nargs = None

    method stringOf = name

    method unwrap = None
  end

let funObj name f : monte =
  object
    method call verb args nargs =
      match verb with "run" -> f args nargs | _ -> None

    method stringOf = name

    method unwrap = None
  end

let traceObj name suffix (print_str : string -> unit) =
  funObj name (fun args nargs ->
      print_str " ~ " ;
      List.iter (fun obj -> print_str obj#stringOf ; print_str ", ") args ;
      print_str suffix ;
      Some nullObj)

let calling verb args namedArgs target = call_exn target verb args namedArgs
let prettyPrint formatter obj = Format.pp_print_string formatter obj#stringOf

let input_varint ic =
  let rec go shift acc =
    let b = Z.of_int (input_byte ic) in
    let n = Z.logor acc (Z.shift_left (Z.logand b (Z.of_int 0x7f)) shift) in
    if not (Z.testbit b 7) then n else go (shift + 7) n in
  go 0 Z.zero

exception InvalidMAST of (string * int)

let throw_invalid_mast ic message = raise (InvalidMAST (message, pos_in ic))

let input_span ic =
  match input_char ic with
  | 'S' ->
      OneToOne
        (input_varint ic, input_varint ic, input_varint ic, input_varint ic)
  | 'B' ->
      Blob (input_varint ic, input_varint ic, input_varint ic, input_varint ic)
  | _ -> throw_invalid_mast ic "input_span"

let ejectTo span =
  let ej =
    object (self)
      val mutable thrown = false

      method disable =
        if thrown then raise (MonteException DoubleThrown) ;
        thrown <- true

      method private throw v =
        self#disable ;
        raise (MonteException (Ejecting (v, to_monte self)))

      method call verb args namedArgs =
        match (verb, args) with
        | "run", [v] -> self#throw v
        | "run", [] -> self#throw nullObj
        | _ -> None

      method stringOf = "<ejector at " ^ string_of_span span ^ ">"

      method unwrap = None
    end in
  (to_monte ej, fun () -> ej#disable)

let _loop : monte =
  object
    method call verb (args : monte list) nargs =
      let run iterable consumer : monte =
        let iterator = call_exn iterable "_makeIterator" [] [] in
        let no_span : mspan = Blob (Z.zero, Z.zero, Z.zero, Z.zero) in
        let ej, _ = ejectTo no_span in
        let rec next () =
          try
            let values = call_exn iterator "next" [ej] [] in
            ignore (call_exn consumer "run" (unwrapList values) []) ;
            next ()
          with MonteException (Ejecting (_, thrower)) as ex ->
            if thrower == ej then nullObj else raise ex in
        next () in
      match (verb, args) with
      | "run", [iterable; consumer] -> Some (run iterable consumer)
      | _ -> None

    method stringOf = "_loop"

    method unwrap = None
  end

let makeScope (pairs : (string * monte) list) : monte Dict.t =
  Dict.of_seq
    (List.to_seq
       (List.map (fun (k, v) -> (k, bindingObj (finalSlotObj v))) pairs))

let unwrapScope obj =
  let pairs = unwrapMap obj in
  let no_amp s =
    String.(if sub s 0 2 = "&&" then sub s 2 (length s - 2) else s) in
  let key_names = List.map (fun (k, v) -> (no_amp (unwrapStr k), v)) pairs in
  Dict.of_seq (List.to_seq key_names)

let safeScope print_str =
  makeScope
    [ ("Bool", dataGuardObj (MBool true)); ("Bytes", todoGuardObj "Bytes")
    ; ("Char", dataGuardObj (MChar 32))
    ; ("DeepFrozen", todoGuardObj "DeepFrozen")
    ; ("DeepFrozenStamp", todoGuardObj "DeepFrozenStamp")
    ; ("Double", dataGuardObj (MDouble 1.0)); ("Infinity", doubleObj infinity)
    ; ("NaN", doubleObj nan); ("Int", dataGuardObj (MInt Z.zero))
    ; ("Near", todoGuardObj "Near")
    ; ("KernelAstStamp", todoObj "KernelAstStamp")
    ; ("Same", todoGuardObj "Same"); ("Ref", theRefObj)
    ; ("astEval", todoObj "astEval"); ("Selfless", todoGuardObj "Selfless")
    ; ("Str", todoGuardObj "Str")
    ; ("SemitransparentStamp", todoObj "SemitransparentStamp")
    ; ("SubrangeGuard", todoGuardObj "SubrangeGuard")
    ; ("TransparentStamp", todoObj "TransparentStamp"); ("Void", voidGuardObj)
    ; ("_auditedBy", todoObj "_auditedBy"); ("_equalizer", _equalizer)
    ; ("_loop", _loop); ("_makeBytes", todoObj "_makeBytes")
    ; ("_makeDouble", todoObj "_makeDouble")
    ; ("_makeFinalSlot", todoObj "_makeFinalSlot")
    ; ("_makeInt", todoObj "_makeInt"); ("_makeList", _makeList)
    ; ("_makeInt", todoObj "_makeInt"); ("_makeMap", _makeMap)
    ; ("false", boolObj false); ("null", nullObj)
    ; ("_makeSourceSpan", todoObj "_makeSourceSpan")
    ; ("_makeStr", todoObj "_makeStr"); ("_makeVarSlot", todoObj "_makeVarSlot")
    ; ("M", theMObj); ("_slotToBinding", todoObj "_slotToBinding")
    ; ("loadMAST", todoObj "loadMAST"); ("makeLazySlot", todoObj "makeLazySlot")
    ; ("promiseAllFulfilled", todoObj "promiseAllFulfilled")
    ; ("throw", throwObj); ("Any", todoGuardObj "Any")
    ; ("traceln", traceObj "traceln" "\n" print_str); ("true", boolObj true)
    ; ("trace", traceObj "trace" "" print_str)
    ; ("typhonAstBuilder", todoObj "typhonAstBuilder" (* XXX typhon objects? *))
    ; ("typhonAstEval", todoObj "typhonAstEval" (* XXX typhon objects? *)) ]

let const k _ = k

let rec sequence actions =
  match actions with
  | f :: fs ->
      State.bind f (fun x ->
          State.bind (sequence fs) (fun xs -> State.return (x :: xs)))
  | [] -> State.return []

let lazyState f s = f () s

module Compiler = struct
  type span = mspan

  let oneToOne t = OneToOne t
  let blob t = Blob t

  type menv = monte Dict.t
  type t = (monte, menv) State.t
  type patt = monte -> monte -> (unit, menv) State.t
  type narg = (monte * monte, menv) State.t
  type nparam = (monte * monte) list -> (unit, menv) State.t
  type meth = string * patt list * nparam list * t
  type matcher = patt * t

  let charExpr c _ = State.return (charObj c)
  let doubleExpr d _ = State.return (doubleObj d)
  let intExpr i _ = State.return (intObj i)
  let strExpr s _ = State.return (strObj s)

  let nounExpr n span =
    let get = calling "get" [] [] in
    State.bind State.get (fun env ->
        match Dict.find_opt n env with
        | Some b -> State.return (get (get b))
        | None -> raise (MonteException (NameError (n, span))))

  let nullExpr span = nounExpr "null" span

  let bindingExpr n span =
    State.bind State.get (fun env ->
        match Dict.find_opt n env with
        | Some b -> State.return b
        | None -> raise (MonteException (NameError ("&&" ^ n, span))))

  let seqExpr exprs _ =
    List.fold_left
      (fun ma expr -> State.bind ma (fun _ -> expr))
      (State.return nullObj) exprs

  let callExpr target verb args namedArgs span =
    State.bind target (fun t ->
        State.bind (sequence args) (fun a ->
            State.bind (sequence namedArgs) (fun na ->
                State.return (call_exn t verb a na))))

  let defExpr patt exitOpt expr span =
    let withOptionalExpr exprOpt d f =
      match exprOpt with Some expr -> State.bind expr f | None -> f d in
    withOptionalExpr exitOpt throwObj (fun exit ->
        State.bind expr (fun e ->
            State.and_then (patt e exit) (State.return e)))

  let escapeExpr patt body span =
    lazyState (fun () ->
        let ej, disable = ejectTo span in
        State.bind
          (State.and_then (patt ej nullObj) State.get)
          (fun s ->
            try
              let x, _ = body s in
              disable () ; State.return x
            with MonteException (Ejecting (o, thrower)) when thrower == ej ->
              State.return o))

  let escapeCatchExpr patt body cpatt cbody span =
    lazyState (fun () ->
        let ej, disable = ejectTo span in
        State.bind
          (State.and_then (patt ej nullObj) State.get)
          (fun s ->
            try
              let x, _ = body s in
              disable () ; State.return x
            with MonteException (Ejecting (o, thrower)) when thrower == ej ->
              State.and_then (cpatt o nullObj) cbody))

  let objectExpr doc (namePatt : patt) (asOpt : t option) (auditors : t list)
      (meths : meth list) (matchs : matcher list) (span : mspan) : t =
    let methdict =
      List.fold_left
        (fun d (v, ps, nps, body) ->
          AtomDict.add (v, List.length ps) (ps, nps, body) d)
        AtomDict.empty meths in
    let runMatcher env message matchPatt body ej : monte =
      let _, env' = (matchPatt message ej) env in
      let rv, _ = State.run body env' in
      rv in
    let runMatchers env verb args namedArgs : monte option =
      let message = listObj [strObj verb; listObj args; mapObj namedArgs]
      and ej, disable = ejectTo span in
      let rec loop (ms : matcher list) : monte option =
        match ms with
        | [] -> None
        | (patt, body) :: rest -> (
          try
            let rv = runMatcher env message patt body ej in
            disable () ; Some rv
          with MonteException (Ejecting (o, thrower)) when thrower == ej ->
            loop rest ) in
      loop matchs in
    State.bind
      (Option.value asOpt ~default:(State.return nullObj))
      (fun ase ->
        State.bind (sequence auditors) (fun auds (* XXX rebind into env *) s ->
            let userObj =
              object (self)
                (* XXX method dispatch, matcher dispatch *)
                method call verb args namedArgs : monte option =
                  let runMethod params nParams body =
                    let exit = throwObj in
                    (* XXX duplicate code with listPatt, refactor! *)
                    let env' =
                      List.fold_left2
                        (fun ma p s -> State.and_then ma (p s exit))
                        (State.return ()) params args in
                    let env'' = State.and_then (namePatt self throwObj) env' in
                    Printf.printf "\n(executing %s(" verb ;
                    List.iter (fun a -> Printf.printf "%s, " a#stringOf) args ;
                    Printf.printf ") at %s)" (string_of_span span) ;
                    let o, _ = State.and_then env'' body s in
                    Some o in
                  match
                    AtomDict.find_opt (verb, List.length args) methdict
                  with
                  | Some (params, nParams, body) ->
                      runMethod params nParams body
                  | None ->
                      (* XXX miranda *)
                      runMatchers s verb args namedArgs

                (* XXX call printOn *)
                method stringOf = "<user>"

                method unwrap = None
              end in
            let _, s' = (namePatt userObj throwObj) s in
            (userObj, s')))

  let assignExpr name rhs span =
    State.bind rhs (fun rv ->
        State.and_then (State.modify (Dict.add name rv)) (State.return rv))

  let tryExpr body patt catcher span s =
    try body s
    with MonteException ex -> (
      match ex with
      (* Ejectors unwind at try-exprs, but do not run their catchers. *)
      | Ejecting _ -> raise (MonteException ex)
      (* XXX sealed *)
      | _ -> State.and_then (patt nullObj nullObj) catcher s )

  let finallyExpr body unwinder span env =
    try body env
    with MonteException m -> unwinder env ; raise (MonteException m)

  let hideExpr expr _ = expr

  let ifExpr test cons alt span =
    let alt' = Option.value alt ~default:(nullExpr span) in
    State.bind test (fun t -> if unwrapBool t then cons else alt')

  let metaStateExpr span =
    State.return
      (object
         method call verb args namedArgs = None

         method stringOf = "<meta.getState()>"

         method unwrap = None
      end)

  let metaContextExpr span =
    State.return
      (object
         method call verb args namedArgs = None

         method stringOf = "<meta.context()>"

         method unwrap = None
      end)

  let metho doc verb patts nparams rguard body span =
    (* XXX rguard? signature synthesis? *)
    (verb, patts, nparams, body)

  let matche patt body span = (patt, body)

  let namedArg key value span =
    State.bind key (fun k -> State.bind value (fun v -> State.return (k, v)))

  let namedParam key patt defaultOpt span map =
    State.bind key (fun k ->
        (* XXX uses OCaml equality!! *)
        match (List.assoc_opt k map, defaultOpt) with
        | Some value, _ -> patt value throwObj
        | None, Some default -> State.bind default (const (State.return ()))
        | None, None -> raise (MonteException (MissingNamedArg (k, span))))

  let coerceOpt guardOpt specimen exit =
    match guardOpt with
    | None -> State.return specimen
    | Some guard ->
        State.bind guard (fun g ->
            let s = call_exn g "coerce" [specimen; exit] [] in
            State.return s)

  let ignorePatt guardOpt span specimen exit =
    State.map (fun _ -> ()) (coerceOpt guardOpt specimen exit)

  let finalPatt noun guard span specimen exit =
    State.bind (coerceOpt guard specimen exit) (fun s ->
        Printf.printf "(finalPatt: %s := %s)" noun s#stringOf ;
        (* XXX guards *)
        State.modify (Dict.add noun (bindingObj (finalSlotObj s))))

  let varPatt noun guard span specimen exit =
    State.bind (coerceOpt guard specimen exit) (fun s ->
        (* XXX guards *)
        State.modify (Dict.add noun (bindingObj (varSlotObj s))))

  let listPatt patts span specimen exit =
    let specimens = unwrapList specimen in
    List.fold_left2
      (fun ma p s -> State.and_then ma (p s exit))
      (State.return ()) patts specimens

  let viaPatt transformer patt span specimen exit =
    State.bind transformer (fun trans ->
        patt (call_exn trans "run" [specimen; exit] []) exit)

  let bindingPatt noun span specimen exit =
    State.modify (Dict.add noun specimen)
end

let input_str ic = really_input_string ic (Z.to_int (input_varint ic))

let input_many f ic =
  let l = Z.to_int (input_varint ic) in
  List.init l (fun _ -> f ic)

(* A growing mutable list that is indexed backwards. Simulates a portion of
 * the Python list API. *)
let backlist () =
  object
    val mutable l = []

    val mutable len = 0

    method push x =
      l <- x :: l ;
      len <- len + 1

    method get i = List.nth l (len - 1 - i)

    method tl = List.hd l
  end

exception InvalidMagic

let mast_magic = "Mont\xe0MAST\x01"

let open_in_mast path =
  let ic = open_in_bin path in
  (* Check the magic number. *)
  for i = 0 to String.length mast_magic - 1 do
    if input_char ic <> mast_magic.[i] then (close_in ic ; raise InvalidMagic)
  done ;
  ic

module MASTContext (Monte : MAST) = struct
  type masthack =
    | HNone
    | HExpr of Monte.t
    | HMeth of Monte.meth
    | HMatch of Monte.matcher

  let logged label ch =
    (* XXX Printf.printf "%s%c..." label ch; *)
    ch

  let make () =
    object (self)
      (* Compared to the classic MAST context, we store the exprs and patts
         * backwards, so that we can build them quickly. *)
      val exprs = backlist ()

      val patts = backlist ()

      method private eat_span ic =
        let v4 ic =
          let i1 = input_varint ic in
          let i2 = input_varint ic in
          let i3 = input_varint ic in
          let i4 = input_varint ic in
          (i1, i2, i3, i4) in
        match input_char ic with
        | 'S' -> Monte.oneToOne (v4 ic)
        | 'B' -> Monte.blob (v4 ic)
        | _ -> throw_invalid_mast ic "input_span"

      method private eat_expr ic =
        match exprs#get (Z.to_int (input_varint ic)) with
        | HExpr e -> e
        | _ -> throw_invalid_mast ic "eat_expr"

      method eat_expr_opt ic =
        match exprs#get (Z.to_int (input_varint ic)) with
        | HExpr e -> Some e
        | HNone -> None
        | _ -> throw_invalid_mast ic "eat_expr_opt"

      method private eat_method ic =
        match exprs#get (Z.to_int (input_varint ic)) with
        | HMeth m -> m
        | _ -> throw_invalid_mast ic "eat_method"

      method private eat_matcher ic =
        match exprs#get (Z.to_int (input_varint ic)) with
        | HMatch m -> m
        | _ -> throw_invalid_mast ic "eat_matcher"

      method private eat_patt ic = patts#get (Z.to_int (input_varint ic))

      method private eat_tag ic =
        match logged "eat_tag" (input_char ic) with
        | 'P' ->
            patts#push
              ( match logged "Pattern tag" (input_char ic) with
              | 'I' ->
                  let g = self#eat_expr_opt ic in
                  Monte.ignorePatt g (self#eat_span ic)
              | 'F' ->
                  let n = input_str ic in
                  let g = self#eat_expr_opt ic in
                  Monte.finalPatt n g (self#eat_span ic)
              | 'V' ->
                  let n = input_str ic in
                  let g = self#eat_expr_opt ic in
                  Monte.varPatt n g (self#eat_span ic)
              | 'L' ->
                  let ps = input_many self#eat_patt ic in
                  Monte.listPatt ps (self#eat_span ic)
              | 'A' ->
                  let trans = self#eat_expr ic in
                  let p = self#eat_patt ic in
                  Monte.viaPatt trans p (self#eat_span ic)
              | 'B' ->
                  let s = input_str ic in
                  Monte.bindingPatt s (self#eat_span ic)
              | x -> throw_invalid_mast ic "patt" )
        | 'M' ->
            let eat_nparam ic =
              let ek = self#eat_expr ic in
              let pv = self#eat_patt ic in
              let ed = self#eat_expr_opt ic in
              Monte.namedParam ek pv ed in
            let doc = input_str ic in
            let verb = input_str ic in
            let ps = input_many self#eat_patt ic in
            let nps = input_many eat_nparam ic in
            let g = self#eat_expr_opt ic in
            let b = self#eat_expr ic in
            let span = self#eat_span ic in
            exprs#push
              (HMeth
                 (Monte.metho doc verb ps
                    (List.map (fun np -> np span) nps)
                    g b span))
        | 'R' ->
            let p = self#eat_patt ic and e = self#eat_expr ic in
            exprs#push (HMatch (Monte.matche p e (self#eat_span ic)))
        | 'L' ->
            exprs#push
              ( match logged "literal tag" (input_char ic) with
              | 'N' ->
                  ignore (self#eat_span ic) ;
                  HNone
              | tag ->
                  let e =
                    match tag with
                    | 'C' -> (
                      match UTF8D.decode1 (seq_of_chan ic) with
                      | Ok (code, _) -> Monte.charExpr code
                      | Error (bs, _) -> throw_invalid_mast ic "bad utf8" )
                    | 'I' ->
                        let i = input_varint ic in
                        let shifted = Z.shift_right i 1 in
                        Monte.intExpr
                          ( if Z.testbit i 0 then
                            Z.logxor (Z.of_int (-1)) shifted
                          else shifted )
                    | 'S' -> Monte.strExpr (input_str ic)
                    | x -> throw_invalid_mast ic ("literal:" ^ Char.escaped x)
                  in
                  HExpr (e (self#eat_span ic)) )
        | tag ->
            let expr =
              match logged "expr tag" tag with
              | 'N' -> Monte.nounExpr (input_str ic)
              | 'B' -> Monte.bindingExpr (input_str ic)
              | 'S' -> Monte.seqExpr (input_many self#eat_expr ic)
              | 'C' ->
                  let eat_narg ic =
                    let n = self#eat_expr ic in
                    let v = self#eat_expr ic in
                    Monte.namedArg n v (self#eat_span ic) in
                  let t = self#eat_expr ic in
                  let v = input_str ic in
                  let a = input_many self#eat_expr ic in
                  let na = input_many eat_narg ic in
                  Monte.callExpr t v a na
              | 'D' ->
                  let p = self#eat_patt ic in
                  let ex = self#eat_expr_opt ic in
                  Monte.defExpr p ex (self#eat_expr ic)
              | 'e' ->
                  let p = self#eat_patt ic in
                  Monte.escapeExpr p (self#eat_expr ic)
              | 'E' ->
                  let p = self#eat_patt ic in
                  let e = self#eat_expr ic in
                  let pc = self#eat_patt ic in
                  let ec = self#eat_expr ic in
                  Monte.escapeCatchExpr p e pc ec
              | 'O' ->
                  (* Object with no script, just direct methods and matchers. *)
                  let doc = input_str ic in
                  let patt = self#eat_patt ic in
                  let asExpr = self#eat_expr_opt ic in
                  let implements = input_many self#eat_expr ic in
                  let methods = input_many self#eat_method ic in
                  let matchers = input_many self#eat_matcher ic in
                  Monte.objectExpr doc patt asExpr implements methods matchers
              | 'A' ->
                  let target = input_str ic in
                  Monte.assignExpr target (self#eat_expr ic)
              | 'F' ->
                  let eb = self#eat_expr ic in
                  let ec = self#eat_expr ic in
                  Monte.finallyExpr eb ec
              | 'Y' ->
                  let eb = self#eat_expr ic in
                  let p = self#eat_patt ic in
                  let ec = self#eat_expr ic in
                  Monte.tryExpr eb p ec
              | 'H' -> Monte.hideExpr (self#eat_expr ic)
              | 'I' ->
                  let test = self#eat_expr ic in
                  let cons = self#eat_expr ic in
                  let alt = self#eat_expr_opt ic in
                  Monte.ifExpr test cons alt
              | 'T' -> Monte.metaStateExpr
              | 'X' -> Monte.metaContextExpr
              | x -> throw_invalid_mast ic ("eat_tag:" ^ Char.escaped x) in
            exprs#push (HExpr (expr (self#eat_span ic)))

      method eat_all_exprs ic =
        try
          while true do
            self#eat_tag ic
          done
        with End_of_file -> ()

      method eat_last_expr ic =
        self#eat_all_exprs ic ;
        match exprs#tl with
        | HExpr e -> e
        | _ -> throw_invalid_mast ic "eat_last_expr"
    end
end

module M = MASTContext (Compiler)

let load read_mast (filename : string) (scope : monte Dict.t) : monte =
  Printf.printf "%s: read mast\n" filename ;
  let expr = read_mast filename in
  Printf.printf "%s: evaluate module\n" filename ;
  let mmod, _ = expr scope in
  Printf.printf "=mod=> %s\n" mmod#stringOf ;
  mmod

let getMonteFileObj read_mast : monte =
  object
    method call verb (args : monte list) nargs =
      match (verb, args) with
      | "run", [filenameObj; scopeObj] ->
          let filename = unwrapStr filenameObj ^ ".mast" in
          Some (load read_mast filename (unwrapScope scopeObj))
      | _ -> None

    method stringOf = "<getMonteFile>"

    method unwrap = None
  end

let typhonUtils =
  let isMapObj =
    funObj "isMap" (fun args nargs ->
        match args with
        | [specimen] ->
            Some
              (boolObj
                 ( match specimen#unwrap with
                 | Some (MMap _) -> true
                 | _ -> false ))
        | _ -> None) in
  [("isMap", isMapObj)]

(* limit use of ambient authority to this top-level expression. *)
let () =
  let read_mast filename : Compiler.t =
    let ic = open_in_mast filename in
    let context = M.make () in
    let rv = context#eat_last_expr ic in
    close_in ic ; rv in
  let ioScope : monte Dict.t =
    let pick k x y = None in
    Dict.union pick
      (safeScope (fun s -> Printf.printf "%s" s))
      (makeScope (typhonUtils @ [("getMonteFile", getMonteFileObj read_mast)]))
  in
  for i = 1 to Array.length Sys.argv - 1 do
    let filename = Sys.argv.(i) in
    try
      let mmod = load read_mast filename ioScope in
      Printf.printf "[%i] %s: run module\n" i filename ;
      let result = call_exn mmod "run" [loaderObj] [] in
      Printf.printf "=out=> %s\n" result#stringOf
    with MonteException m -> Printf.printf "\n%s\n" (string_of_mexn m)
  done
