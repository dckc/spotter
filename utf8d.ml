(** `decode bs` gives either Ok (code, rest) or Error (consumed_bytes, rest) *)
let decode1 (bs : int Seq.t) : (int * int Seq.t, int list * int Seq.t) result =
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
            cat (cat (mask b0 mask4) 6 (mask b1 mask2)) 6 (mask b2 mask2) in
          Ok (code, b3n)
      | Cons (b2, b3n) -> (
        match b3n () with
        | Nil -> Error ([b0; b1; b2], b3n)
        | Cons (b3, b4n) when is_11110xxx b0 ->
            let code =
              cat
                (cat (cat (mask b0 mask5) 6 (mask b1 mask2)) 6 (mask b2 mask2))
                6 (mask b3 mask2) in
            Ok (code, b4n)
        | Cons (b3, b4n) -> Error ([b0; b1; b2; b3], b4n) ) ) )
