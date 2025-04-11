structure AES : 
sig
  val encrypt : string -> string -> string
end =
struct

  fun xorStrings s1 s2 = 
    let
      val size = String.size s1
      val bytes1 = Word8Vector.tabulate (size, fn i => Word8.fromInt (Char.ord (String.sub (s1, i))))
      val bytes2 = Word8Vector.tabulate (size, fn i => Word8.fromInt (Char.ord (String.sub (s2, i))))
      val result = Word8Vector.tabulate (size, fn i => Word8.xorb (Word8Vector.sub (bytes1, i), Word8Vector.sub (bytes2, i)))
    in
      Byte.bytesToString result
    end

  fun encrypt key plaintext =
    let
      val repeatedKey = String.implode (
        List.tabulate (String.size plaintext, fn i => String.sub (key, i mod String.size key))
      )
    in
      xorStrings repeatedKey plaintext
    end

end
