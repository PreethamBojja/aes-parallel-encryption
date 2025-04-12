signature AES =
  sig
    (* Encrypt one 16‑byte block with a 16‑byte key *)
    val encrypt_block : Word8Vector.vector -> Word8Vector.vector -> Word8Vector.vector
  end

structure AES : AES =
struct
  structure W = Word8
  type byte = W.word

  (* AES parameters *)
  val Nb = 4               (* state columns            *)
  val Nk = 4               (* 128‑bit key ⇒ 4 words    *)
  val Nr = 10              (* rounds                   *)

  (* helpers for type conversion *)
  fun toByte (i:int): byte = W.fromInt i
  fun toInt  (b:byte): int = W.toInt b

  (* Rijndael S‑box *)
  val sbox: byte Vector.vector =
    Vector.fromList (List.map toByte
      [0x63,0x7c,0x77,0x7b,0xf2,0x6b,0x6f,0xc5,0x30,0x01,0x67,0x2b,0xfe,0xd7,0xab,0x76,
       0xca,0x82,0xc9,0x7d,0xfa,0x59,0x47,0xf0,0xad,0xd4,0xa2,0xaf,0x9c,0xa4,0x72,0xc0,
       0xb7,0xfd,0x93,0x26,0x36,0x3f,0xf7,0xcc,0x34,0xa5,0xe5,0xf1,0x71,0xd8,0x31,0x15,
       0x04,0xc7,0x23,0xc3,0x18,0x96,0x05,0x9a,0x07,0x12,0x80,0xe2,0xeb,0x27,0xb2,0x75,
       0x09,0x83,0x2c,0x1a,0x1b,0x6e,0x5a,0xa0,0x52,0x3b,0xd6,0xb3,0x29,0xe3,0x2f,0x84,
       0x53,0xd1,0x00,0xed,0x20,0xfc,0xb1,0x5b,0x6a,0xcb,0xbe,0x39,0x4a,0x4c,0x58,0xcf,
       0xd0,0xef,0xaa,0xfb,0x43,0x4d,0x33,0x85,0x45,0xf9,0x02,0x7f,0x50,0x3c,0x9f,0xa8,
       0x51,0xa3,0x40,0x8f,0x92,0x9d,0x38,0xf5,0xbc,0xb6,0xda,0x21,0x10,0xff,0xf3,0xd2,
       0xcd,0x0c,0x13,0xec,0x5f,0x97,0x44,0x17,0xc4,0xa7,0x7e,0x3d,0x64,0x5d,0x19,0x73,
       0x60,0x81,0x4f,0xdc,0x22,0x2a,0x90,0x88,0x46,0xee,0xb8,0x14,0xde,0x5e,0x0b,0xdb,
       0xe0,0x32,0x3a,0x0a,0x49,0x06,0x24,0x5c,0xc2,0xd3,0xac,0x62,0x91,0x95,0xe4,0x79,
       0xe7,0xc8,0x37,0x6d,0x8d,0xd5,0x4e,0xa9,0x6c,0x56,0xf4,0xea,0x65,0x7a,0xae,0x08,
       0xba,0x78,0x25,0x2e,0x1c,0xa6,0xb4,0xc6,0xe8,0xdd,0x74,0x1f,0x4b,0xbd,0x8b,0x8a,
       0x70,0x3e,0xb5,0x66,0x48,0x03,0xf6,0x0e,0x61,0x35,0x57,0xb9,0x86,0xc1,0x1d,0x9e,
       0xe1,0xf8,0x98,0x11,0x69,0xd9,0x8e,0x94,0x9b,0x1e,0x87,0xe9,0xce,0x55,0x28,0xdf,
       0x8c,0xa1,0x89,0x0d,0xbf,0xe6,0x42,0x68,0x41,0x99,0x2d,0x0f,0xb0,0x54,0xbb,0x16])

  (* round constants *)
  val rcon: byte Vector.vector =
    Vector.fromList (List.map toByte [0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x1B,0x36])

  (* multiply by x in GF(2^8) *)
  fun xtime (b:byte): byte =
    let
      val shifted = W.<<(b, 0w1)
      val hi      = W.andb(b, 0wx80)
    in if hi = 0w0 then shifted else W.xorb(shifted, 0wx1B) end

  (* index into the state (column‑major) *)
  fun idx (r:int, c:int): int = Int.+(Int.*(c,4), r)

  (* in‑place map over a Word8Array *)
  fun mapArr f arr =
    let
      val n:int = Word8Array.length arr
      fun lp (i:int) =
        if i = n then ()
        else (Word8Array.update(arr, i, f (Word8Array.sub(arr,i)));
              lp (i + 1))
    in lp 0 end

  (* === core round operations === *)
  fun subBytes st = mapArr (fn b => Vector.sub(sbox, toInt b)) st

  fun shiftRows st =
    let
      fun g (r:int, c:int) = Word8Array.sub(st, idx(r,c))
      fun s (r:int, c:int) v = Word8Array.update(st, idx(r,c), v)
      (* row 1 left rotate 1 *)
      val t10 = g(1,0)
      val _ = (s(1,0) (g(1,1)); s(1,1) (g(1,2)); s(1,2) (g(1,3)); s(1,3) t10)
      (* row 2 left rotate 2 *)
      val t20 = g(2,0)
      val t21 = g(2,1)
      val _ = (s(2,0) (g(2,2)); s(2,1) (g(2,3)); s(2,2) t20; s(2,3) t21)
      (* row 3 left rotate 3 (right rotate 1) *)
      val t3 = g(3,3)
      val _ = (s(3,3) (g(3,2)); s(3,2) (g(3,1)); s(3,1) (g(3,0)); s(3,0) t3)
    in () end

  fun mixSingle (c0,c1,c2,c3) =
    let
      val t  = W.xorb(c0, W.xorb(c1, W.xorb(c2,c3)))
      val c0' = W.xorb(c0, xtime (W.xorb(c0,c1)))
      val c1' = W.xorb(c1, xtime (W.xorb(c1,c2)))
      val c2' = W.xorb(c2, xtime (W.xorb(c2,c3)))
      val c3' = W.xorb(c3, xtime (W.xorb(c3,c0)))
    in (W.xorb(c0',t), W.xorb(c1',t), W.xorb(c2',t), W.xorb(c3',t)) end

  fun mixColumns st =
    let
      fun col (c:int) =
        let
          val c0 = Word8Array.sub(st, idx(0,c))
          val c1 = Word8Array.sub(st, idx(1,c))
          val c2 = Word8Array.sub(st, idx(2,c))
          val c3 = Word8Array.sub(st, idx(3,c))
          val (r0,r1,r2,r3) = mixSingle(c0,c1,c2,c3)
          val _ = (Word8Array.update(st, idx(0,c), r0);
                   Word8Array.update(st, idx(1,c), r1);
                   Word8Array.update(st, idx(2,c), r2);
                   Word8Array.update(st, idx(3,c), r3))
        in () end
      val _ = (col 0; col 1; col 2; col 3)
    in () end

  fun addRoundKey (st, rk:Word8Vector.vector, rnd:int) =
    let
      val off:int = rnd * 16
      fun lp (i:int) =
        if i = 16 then ()
        else (Word8Array.update(st, i,
              W.xorb(Word8Array.sub(st,i), Word8Vector.sub(rk, off + i)));
              lp (i + 1))
    in lp 0 end

  (* === key schedule === *)
  fun keyExpansion (key:Word8Vector.vector): Word8Vector.vector =
    let
      val out = Word8Array.array(176, 0w0)
      val _ =
        let
          val n = Word8Vector.length key
          fun lp i = if i = n then ()
                     else (Word8Array.update(out, i, Word8Vector.sub(key,i)); lp (i+1))
        in lp 0 end

      fun subByte b = Vector.sub(sbox, toInt b)
      fun rot (a,b,c,d) = (b,c,d,a)

      fun loop (i:int, rcix:int) =
        if i = 176 then ()
        else
          let
            val t0 = Word8Array.sub(out, i-4)
            val t1 = Word8Array.sub(out, i-3)
            val t2 = Word8Array.sub(out, i-2)
            val t3 = Word8Array.sub(out, i-1)
            val (u0,u1,u2,u3, nextRC) =
              if Int.mod(i,16) = 0 then
                let
                  val (b,c,d,a) = rot(t0,t1,t2,t3)
                  val sb0 = subByte b
                  val sb1 = subByte c
                  val sb2 = subByte d
                  val sb3 = subByte a
                  val rc   = Vector.sub(rcon, rcix)
                in (W.xorb(sb0, rc), sb1, sb2, sb3, rcix + 1) end
              else (t0,t1,t2,t3, rcix)
            val p = i - 16
            val o0 = W.xorb(u0, Word8Array.sub(out, p))
            val o1 = W.xorb(u1, Word8Array.sub(out, p + 1))
            val o2 = W.xorb(u2, Word8Array.sub(out, p + 2))
            val o3 = W.xorb(u3, Word8Array.sub(out, p + 3))
            val _ = (Word8Array.update(out, i,     o0);
                     Word8Array.update(out, i + 1, o1);
                     Word8Array.update(out, i + 2, o2);
                     Word8Array.update(out, i + 3, o3))
          in
            loop(i + 4, nextRC)
          end
      val _ = loop(16, 0)
    in
      Word8Array.vector out
    end

  (* === helpers === *)
  fun vecToArr v =
    let
      val a = Word8Array.array(16, 0w0)
      fun lp (i:int) =
        if i = 16 then ()
        else (Word8Array.update(a, i, Word8Vector.sub(v, i)); lp(i + 1))
    in lp 0; a end

  fun arrToVec a = Word8Vector.tabulate(16, fn i => Word8Array.sub(a, i))

  (* === public entry === *)
  fun encrypt_block key pt =
    let
      val rks = keyExpansion key
      val st  = vecToArr pt
      val _   = addRoundKey(st, rks, 0)
      fun rounds (r:int) =
        if r = Nr then ()
        else (subBytes st; shiftRows st; mixColumns st; addRoundKey(st, rks, r); rounds (r + 1))
      val _ = rounds 1                      (* rounds 1‑9 *)
      val _ = (subBytes st; shiftRows st; addRoundKey(st, rks, Nr))  (* final round *)
    in
      arrToVec st
    end
end