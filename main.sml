structure Main =
struct
  structure W = Word8

  (* ---------- hex <-> byte helpers ---------- *)

  (* read two hex chars at positions i,i+1 of string s *)
  fun readHexPair (s, i) : W.word =
    let
      val hi = Char.ord (String.sub (s, i))
      val lo = Char.ord (String.sub (s, i + 1))

      fun nib c =
        if c >= Char.ord #"0" andalso c <= Char.ord #"9" then
          c - Char.ord #"0"
        else if c >= Char.ord #"a" andalso c <= Char.ord #"f" then
          c - Char.ord #"a" + 10
        else if c >= Char.ord #"A" andalso c <= Char.ord #"F" then
          c - Char.ord #"A" + 10
        else
          raise Fail "Invalid hex digit"
    in
      W.fromInt (16 * nib hi + nib lo)
    end

  (* string → Seq of bytes *)
  fun fromHexSeq s =
    let
      val n        = String.size s div 2
    in
      Seq.tabulate (fn i => readHexPair (s, i * 2)) n
    end

  (* one byte → two upper‑case hex chars *)
  fun byteToHex (b: W.word) : string =
    let
      val digits = "0123456789ABCDEF"
      val v      = W.toInt b
    in
      String.substring (digits, v div 16, 1)
      ^ String.substring (digits, v mod 16, 1)
    end

  (* Seq of bytes → hex string *)
  fun toHexSeq (bs : W.word Seq.seq) =
    let
      val n = Seq.length bs
      fun loop (i, acc) =
        if i = n then
          String.concat (List.rev acc)
        else
          loop (i + 1, byteToHex (Seq.nth bs i) :: acc)
    in
      loop (0, [])
    end

  (* ---------- test data ---------- *)

  val keyHex = "2b7e151628aed2a6abf7158809cf4f3c"

  val ptHexes = [
    "6bc1bee22e409f96e93d7e117393172a",
    "ae2d8a571e03ac9c9eb76fac45af8e51",
    "30c81c46a35ce411e5fbc1191a0a52ef",
    "f69f2445df4f9b17ad2b417be66c3710"
  ]

  (* key and plaintexts as Seq.seq *)
  val key  : W.word Seq.seq = fromHexSeq keyHex
  val pts  : (W.word Seq.seq) Seq.seq =
               Seq.map fromHexSeq (Seq.fromList ptHexes)

  (* encrypt each block with purely functional AES *)
  val ciphers : (W.word Seq.seq) Seq.seq =
                 Seq.map (fn pt => AES.encrypt_block key pt) pts

  (* ---------- pretty print ---------- *)
  val _ =
    ( print "ECB-AES128 (pure-Seq version)\n\n"
    ; print "Key:\n  " ; print keyHex ; print "\n\n"
    ; print "Plaintext blocks:\n"
    ; Seq.applyIdx pts
        (fn (_, p) => print ("  " ^ toHexSeq p ^ "\n"))
    ; print "\nResulting ciphertext:\n"
    ; Seq.applyIdx ciphers
        (fn (_, ct) => print ("  " ^ toHexSeq ct ^ "\n"))
    ; print "\n"
    )
end
