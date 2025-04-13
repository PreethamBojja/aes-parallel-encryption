structure Main =
struct
  structure W = Word8

  (* ---------- Hex <-> Byte Helpers ---------- *)

  (* Read two hex chars at positions i and i+1 from string s *)
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

  (* Convert a hex string into a sequence of bytes *)
  fun fromHexSeq s =
    let
      val n = String.size s div 2
    in
      Seq.tabulate (fn i => readHexPair (s, i * 2)) n
    end

  (* Convert one byte into two uppercase hex characters *)
  fun byteToHex (b: W.word) : string =
    let
      val digits = "0123456789ABCDEF"
      val v      = W.toInt b
    in
      String.substring (digits, v div 16, 1) ^
      String.substring (digits, v mod 16, 1)
    end

  (* Convert a sequence of bytes to a hex string *)
  fun toHexSeq (bs: W.word Seq.seq) =
    let
      val n = Seq.length bs
      fun loop (i, acc) =
        if i = n then String.concat (List.rev acc)
        else loop (i + 1, byteToHex (Seq.nth bs i) :: acc)
    in
      loop (0, [])
    end

  (* ---------- File and Block Processing Helpers ---------- *)

  (* Read the entire file into a string *)
  fun readFile filename =
    let
      val instream = TextIO.openIn filename
      val content  = TextIO.inputAll instream
      val _        = TextIO.closeIn instream
    in
      content
    end

  (* Remove whitespace (spaces, newlines, carriage returns) *)
  fun removeWhitespace s =
    String.translate (fn c =>
         if c = #" " orelse c = #"\n" orelse c = #"\r"
         then ""
         else String.implode [c]
    ) s

  (* Pad the hex string with '0' so its length is a multiple of 32 (128 bits) *)
  fun padHex s =
    let
      val len       = String.size s
      val remainder = len mod 32
      val padAmount = if remainder = 0 then 0 else 32 - remainder
      val padding   = String.implode (List.tabulate (padAmount, fn _ => #"0"))
    in
      s ^ padding
    end

  (* Split the padded hex string into blocks of 32 characters each *)
  fun splitIntoBlocks s =
    let
      fun loop (i, acc) =
         if i >= String.size s then List.rev acc
         else
           let
             val block = String.substring (s, i, 32)
           in
             loop (i + 32, block :: acc)
           end
    in
      loop (0, [])
    end

  (* Read the plaintext from file, remove whitespace, pad,
     and split it into blocks, converting each block into a sequence of bytes *)
  fun readPlaintextBlocks filename =
    let
      val raw     = readFile filename
      val cleaned = removeWhitespace raw
      val padded  = padHex cleaned
      val blocks  = splitIntoBlocks padded
    in
      List.map fromHexSeq blocks
    end

  (* ---------- AES Key and Benchmark ---------- *)

  (* Hardcoded AES key (same as before) *)
  val keyHex = "2b7e151628aed2a6abf7158809cf4f3c"
  val key    = fromHexSeq keyHex

  (* Main function:
       1. Reads the plaintext filename from command-line arguments.
       2. Loads and processes the plaintext file (timing that step).
       3. Encrypts all 128-bit blocks using AES.encrypt_block (timed using Util.getTime).
       4. Prints the key, plaintext blocks, encryption time, and resulting ciphertext. *)
  fun main () =
    let
      (* Get filename from command-line arguments or die with usage *)
      val filename =
            List.hd (CommandLineArgs.positional ())
            handle _ => Util.die "Usage: ./main -- <PLAINTEXT_FILENAME>"

      val _ = print "Loading plaintext file... (if large, this might take a while)\n"
      (* Benchmark the file-loading and block-splitting process *)
      val (pts, tm_load) = Util.getTime (fn () => readPlaintextBlocks filename)
      val _ = print ("Loaded plaintext in " ^ Time.fmt 4 tm_load ^ "s\n\n")

      val _ = (print "Key:\n  "; print keyHex; print "\n\n")
      val _ = (print "Plaintext blocks:\n")
      val _ = List.app (fn p => print ("  " ^ toHexSeq p ^ "\n")) pts

      (* Benchmark the encryption process using Util.getTime *)
      val (ciphers, tm_enc) = Util.getTime (fn () => List.map (fn pt => AES.encrypt_block key pt) pts)
      val _ = print ("\nEncryption took " ^ Time.fmt 4 tm_enc ^ "s\n\n")
      (* val _ = print "Resulting ciphertext:\n"
      val _ = List.app (fn ct => print ("  " ^ toHexSeq ct ^ "\n")) ciphers *)
    in
      ()
    end

end

(* Automatically run main upon starting the program *)
val _ = Main.main ();