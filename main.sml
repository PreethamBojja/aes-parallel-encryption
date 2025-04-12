structure Main =
struct
  open AES  (* brings encrypt_block into scope *)

  structure W = Word8

  (* -----------------------------------------------------------
     1) Utilities to convert between strings of hex and vectors
     ----------------------------------------------------------- *)

  (* Convert two hex characters into one Word8 byte. *)
  fun readHexPair (s, i) =
    let
      val hi = Char.ord (String.sub (s, i))
      val lo = Char.ord (String.sub (s, i+1))

      fun nib c =
        if c >= Char.ord #"0" andalso c <= Char.ord #"9"
          then c - Char.ord #"0"
        else if c >= Char.ord #"a" andalso c <= Char.ord #"f"
          then c - Char.ord #"a" + 10
        else if c >= Char.ord #"A" andalso c <= Char.ord #"F"
          then c - Char.ord #"A" + 10
        else raise Fail "Invalid hex digit"
    in
      W.fromInt (16 * nib hi + nib lo)
    end

  (* Convert a hex string like "2b7e1516..." into a Word8Vector.vector. *)
  fun fromHex s =
    let
      val n = String.size s
      fun lp (i, acc) =
        if i >= n then
          List.rev acc
        else
          lp (i + 2, readHexPair (s, i) :: acc)
    in
      Word8Vector.fromList (lp (0, []))
    end

  (* Convert a single byte to two hex characters, uppercase. *)
  fun byteToHex (b:W.word) =
    let
      val digits = "0123456789ABCDEF"
      val v = W.toInt b
      val hi = String.substring (digits, v div 16, 1)
      val lo = String.substring (digits, v mod 16, 1)
    in
      hi ^ lo
    end

    (* Convert an entire Word8Vector (e.g. 16 bytes) to a hex string. *)
  fun toHex (vec: Word8Vector.vector) =
    let
      val n = Word8Vector.length vec
      fun lp (i:int, acc:string list) =
        if i = n then
          String.concat (List.rev acc)
        else
          lp (i + 1, byteToHex (Word8Vector.sub (vec, i)) :: acc)
    in
      lp (0, [])
    end

  (* -----------------------------------------------------------
     2) Hardcode the key and plaintext blocks, then encrypt
     ----------------------------------------------------------- *)

  val keyHex = "2b7e151628aed2a6abf7158809cf4f3c"

  val ptHexes = [
    "6bc1bee22e409f96e93d7e117393172a",
    "ae2d8a571e03ac9c9eb76fac45af8e51",
    "30c81c46a35ce411e5fbc1191a0a52ef",
    "f69f2445df4f9b17ad2b417be66c3710"
  ]

  val key = fromHex keyHex
  val pts = List.map fromHex ptHexes

  (* Encrypt each 16‑byte block in ECB style. *)
  val ciphers = List.map (fn block => encrypt_block key block) pts

  (* -----------------------------------------------------------
     3) Print everything
     ----------------------------------------------------------- *)

  val _ =
    (print "ECB-AES128\n\n";
     print "Key:\n  "; print (keyHex ^ "\n\n");
     print "Plaintext blocks:\n";
     List.app (fn ph => print ("  " ^ ph ^ "\n")) ptHexes;
     print "\nResulting ciphertext:\n";
     List.app (fn ctvec => print ("  " ^ toHex ctvec ^ "\n")) ciphers;
     print "\n")
end

