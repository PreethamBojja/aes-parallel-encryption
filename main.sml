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
  fun toHexSeq (bs : W.word Seq.seq) : string = Seq.reduce (op ^) "" (Seq.map byteToHex bs)  

  (* ---------- File and Block Processing Helpers ---------- *)
fun readFile filename =
    let
      val ins  = TextIO.openIn filename
      val data = TextIO.inputAll ins
      val _    = TextIO.closeIn ins
    in
      data
    end

  fun checkHexMultiple s = if String.size s mod 32 = 0 then s else raise Fail "hex data length is not a multiple of 16 bytes"

  fun splitIntoBlocks s =
    let
      fun lp (i, acc) =
        if i >= String.size s then List.rev acc
        else lp (i + 32, String.substring (s, i, 32) :: acc)
    in
      lp (0, [])
    end

  (* ---------- load plaintext as a Seq.seq ---------- *)

  fun readPlaintextBlocks filename : (W.word Seq.seq) Seq.seq =
    let
      val raw    = readFile filename
      val valid  = checkHexMultiple raw
      val blocks = splitIntoBlocks valid              (* string list *)
    in
      Seq.fromList (List.map fromHexSeq blocks)       (* Seq.seq of byte‑Seqs *)
    end

  (* ---------- AES key ---------- *)

  val keyHex = "2b7e151628aed2a6abf7158809cf4f3c"
  val key    = fromHexSeq keyHex                     (* byte Seq.seq *)

  (* ---------- main ---------- *)

  fun main () =
    let
      val filename =
        List.hd (CommandLineArgs.positional ())
        handle _ => Util.die "Usage: ./main -- <PLAINTEXT_FILE>"

      val _ = print "Loading plaintext file...\n"
      val (pts, tm_load) = Util.getTime (fn () => readPlaintextBlocks filename)
      val _ = print ("Loaded in " ^ Time.fmt 4 tm_load ^ " s\n\n")
      val roundKeys = AES.keyExpansion key
      val nBlocks = Seq.length pts

      val (ciphers, tm_enc) = Util.getTime (fn () => Seq.tabulate (fn i => AES.encrypt_block roundKeys (Seq.nth pts i)) nBlocks)
      val _ = print ("Encryption took " ^ Time.fmt 4 tm_enc ^ " s\n\n")
      (* val _ = print "Ciphertext blocks:\n"
      val _ = Seq.applyIdx ciphers (fn (_, ct) => print ("  " ^ toHexSeq ct ^ "\n")) *)
    in
      ()
    end
end

(* auto‑run *)
val _ = Main.main ()