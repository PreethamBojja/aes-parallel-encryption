structure Main =
struct
  structure W = Word8

  (* Convert two hex characters into one Word8 byte *)
  fun readHexPair (s, i) =
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

  (* Convert a hex string like "2b7e..." into a Word8Vector.vector using Seq *)
  fun fromHexSeq s =
    let
      val n = String.size s
      val numBytes = n div 2
      val seq = Seq.tabulate (fn i => readHexPair (s, i * 2)) numBytes
    in
      Word8Vector.tabulate (numBytes, fn i => Seq.nth seq i)
    end

  (* Convert a single byte to two hex characters, uppercase *)
  fun byteToHex (b: W.word) =
    let
      val digits = "0123456789ABCDEF"
      val v = W.toInt b
      val hi = String.substring (digits, v div 16, 1)
      val lo = String.substring (digits, v mod 16, 1)
    in
      hi ^ lo
    end

  (* Convert a Word8Vector to hex string *)
  fun toHex (vec: Word8Vector.vector) =
    let
      val n = Word8Vector.length vec
      fun loop (i, acc) =
        if i = n then
          String.concat (List.rev acc)
        else
          loop (i + 1, byteToHex (Word8Vector.sub (vec, i)) :: acc)
    in
      loop (0, [])
    end

  (* Input key and plaintexts in hex *)
  val keyHex = "2b7e151628aed2a6abf7158809cf4f3c"

  val ptHexes = [
    "6bc1bee22e409f96e93d7e117393172a",
    "ae2d8a571e03ac9c9eb76fac45af8e51",
    "30c81c46a35ce411e5fbc1191a0a52ef",
    "f69f2445df4f9b17ad2b417be66c3710"
  ]

  (* Convert input hex to byte vectors using Seq *)
  val key = fromHexSeq keyHex
  val pts = Seq.toList (Seq.map fromHexSeq (Seq.fromList ptHexes))

  (* Encrypt each block using AES.encrypt_block *)
  val ciphers = Seq.toList (Seq.map (fn pt => AES.encrypt_block key pt) (Seq.fromList pts))

  (* Print output *)
  val _ =
    (print "ECB-AES128\n\n";
     print "Resulting ciphertext:\n";
     List.app (fn ct => print ("  " ^ toHex ct ^ "\n")) ciphers;
     print "\n")
end
