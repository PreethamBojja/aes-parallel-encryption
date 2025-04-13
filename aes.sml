structure AES : sig
  val encrypt_block : (Word8.word Seq.seq) -> (Word8.word Seq.seq) -> (Word8.word Seq.seq)
end =
struct
  structure W = Word8
  type byte = W.word
  type state = byte Seq.seq  (* We'll store each 128-bit block as a seq of 16 bytes *)

  (* AES parameters *)
  val Nb = 4  (* columns in the state *)
  val Nk = 4  (* 128-bit key => 4 words => 16 bytes *)
  val Nr = 10 (* rounds *)

  (* === Helpers for type conversion === *)
  fun toInt (b: byte) : int = W.toInt b

  (*** Convert the S-box and Rcon to Seq instead of Vector ***)
  val sbox : byte Seq.seq =
    Seq.fromList (List.map W.fromInt [
      0x63,0x7c,0x77,0x7b,0xf2,0x6b,0x6f,0xc5,0x30,0x01,0x67,0x2b,0xfe,0xd7,0xab,0x76,
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
      0x8c,0xa1,0x89,0x0d,0xbf,0xe6,0x42,0x68,0x41,0x99,0x2d,0x0f,0xb0,0x54,0xbb,0x16
    ])

  val rcon : byte Seq.seq =
    Seq.fromList (List.map W.fromInt [
      0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x1B,0x36
    ])

  (* multiply by x in GF(2^8) *)
  fun xtime (b: byte): byte =
    let
      val shifted = W.<<(b, 0w1)
      val hi      = W.andb(b, 0wx80)
    in
      if hi = 0w0 then shifted else W.xorb(shifted, 0wx1B)
    end

  (* We'll store our 16-byte state as a 'byte seq', with indices 0..15 in row-major or column-major. *)
  (* Original code uses idx(r,c) = 4*c + r for column-major. We will keep that approach. *)
  fun idx (r, c) = 4*c + r

  (* ========== Seq-based "get" and "set" utilities ========== *)

  fun get (st: state, i: int) : byte = Seq.nth st i

  (* single-element injection: set st[i] = v in purely functional style *)
  fun set (st: state, i: int, v: byte) : state = Seq.inject(st, Seq.singleton (i, v))

  (* multiple assignments at once, e.g. set st at [(i1,v1), (i2,v2), ...] *)
  fun setMany (st: state, updates: (int * byte) list) : state = Seq.inject(st, Seq.fromList updates)

  (* ========== Round operations in a purely functional style ========== *)

  fun subBytes (st: state) : state =
    Seq.map (fn b => Seq.nth sbox (W.toInt b)) st

  fun shiftRows (st: state) : state =
    let
      fun g (r, c) = get(st, idx(r, c))
      (* row 1: left rotate 1 => st[1,0] <- old st[1,1], etc. *)
      val t10 = g(1,0)
      val st1 = setMany (st, [
          (idx(1,0), g(1,1)),
          (idx(1,1), g(1,2)),
          (idx(1,2), g(1,3)),
          (idx(1,3), t10)
        ])

      (* row 2: left rotate 2 *)
      val t20 = get(st1, idx(2,0))
      val t21 = get(st1, idx(2,1))
      val st2 = setMany (st1, [
          (idx(2,0), get(st1, idx(2,2))),
          (idx(2,1), get(st1, idx(2,3))),
          (idx(2,2), t20),
          (idx(2,3), t21)
        ])

      (* row 3: left rotate 3 => right rotate 1 *)
      val t3 = get(st2, idx(3,3))
      val st3 = setMany (st2, [
          (idx(3,3), get(st2, idx(3,2))),
          (idx(3,2), get(st2, idx(3,1))),
          (idx(3,1), get(st2, idx(3,0))),
          (idx(3,0), t3)
        ])
    in
      st3
    end

  fun mixSingle (c0, c1, c2, c3) =
    let
      val t = W.xorb(c0, W.xorb(c1, W.xorb(c2, c3)))
      val c0' = W.xorb(c0, xtime (W.xorb(c0, c1)))
      val c1' = W.xorb(c1, xtime (W.xorb(c1, c2)))
      val c2' = W.xorb(c2, xtime (W.xorb(c2, c3)))
      val c3' = W.xorb(c3, xtime (W.xorb(c3, c0)))
    in
      (W.xorb(c0', t), W.xorb(c1', t), W.xorb(c2', t), W.xorb(c3', t))
    end

  fun mixColumns (st: state) : state =
    let
      fun mixCol (st0, c) =
        let
          val c0 = get(st0, idx(0, c))
          val c1 = get(st0, idx(1, c))
          val c2 = get(st0, idx(2, c))
          val c3 = get(st0, idx(3, c))
          val (r0, r1, r2, r3) = mixSingle(c0, c1, c2, c3)
          val st1 = setMany(st0, [
            (idx(0, c), r0),
            (idx(1, c), r1),
            (idx(2, c), r2),
            (idx(3, c), r3)
          ])
        in
          st1
        end
      (* fold over c=0..3, updating state each time *)
      fun loopCol (c, stNow) =
        if c = 4 then stNow
        else loopCol (c + 1, mixCol(stNow, c))

    in
      loopCol (0, st)
    end

  fun addRoundKey (st: state, rk: byte Seq.seq, rnd: int) : state =
    let
      val off = rnd * 16
      fun f i stNow =
        if i = 16 then stNow
        else
          let
            val updated = set(stNow, i, W.xorb( get(stNow, i), Seq.nth rk (off + i) ))
          in
            f (i + 1) updated
          end
    in
      f 0 st
    end

  (*** Key expansion using pure Seq ***)

  (* We'll produce a 176-byte sequence for the round keys. *)
  fun keyExpansion (key: byte Seq.seq) : byte Seq.seq =
    let
      (* Start with a 176-element seq of 0w0, then copy key into [0..15]. *)
      val out0 = Seq.tabulate (fn _ => 0w0) 176

      fun copyKey (out, i) =
        if i = Seq.length key then out
        else copyKey ( set(out, i, Seq.nth key i), i+1 )

      val out1 = copyKey (out0, 0)

      fun subByte b = Seq.nth sbox (W.toInt b)
      fun rot (a, b, c, d) = (b, c, d, a)

      fun loop (out, i, rcix) =
        if i = 176 then out
        else
          let
            val t0 = Seq.nth out (i-4)
            val t1 = Seq.nth out (i-3)
            val t2 = Seq.nth out (i-2)
            val t3 = Seq.nth out (i-1)
            val (u0, u1, u2, u3, nextRC) =
              if (i mod 16) = 0 then
                let
                  val (b, c, d, a) = rot(t0, t1, t2, t3)
                  val sb0 = subByte b
                  val sb1 = subByte c
                  val sb2 = subByte d
                  val sb3 = subByte a
                  val rc = Seq.nth rcon rcix
                in
                  (W.xorb(sb0, rc), sb1, sb2, sb3, rcix + 1)
                end
              else
                (t0, t1, t2, t3, rcix)

            val p = i - 16
            val o0 = W.xorb(u0, Seq.nth out (p))
            val o1 = W.xorb(u1, Seq.nth out (p + 1))
            val o2 = W.xorb(u2, Seq.nth out (p + 2))
            val o3 = W.xorb(u3, Seq.nth out (p + 3))

            val out2 = setMany(out, [
              (i,     o0),
              (i + 1, o1),
              (i + 2, o2),
              (i + 3, o3)
            ])
          in
            loop (out2, i + 4, nextRC)
          end

      val out2 = loop (out1, 16, 0)
    in
      out2
    end

  (* ========== The 16-byte block "get" and "set" for the plaintext/ciphertext ========== *)

  (* encrypt_block: purely functional ECS block encryption *)
  fun encrypt_block (keySeq: byte Seq.seq) (ptSeq: byte Seq.seq) : byte Seq.seq =
    let
      (* Expand the key to 176 bytes. *)
      val roundKeys = keyExpansion keySeq

      (* We'll keep the state in a 'byte seq of length 16'. *)
      fun rounds (st, r) =
        if r = Nr then st
        else
          let
            val st1 = subBytes st
            val st2 = shiftRows st1
            val st3 = mixColumns st2
            val st4 = addRoundKey(st3, roundKeys, r)
          in
            rounds (st4, r+1)
          end

      (* Round 0: add round key, then do rounds 1..9, then final round. *)
      val st0 = addRoundKey(ptSeq, roundKeys, 0)
      val stX = rounds (st0, 1)
      val stY = subBytes stX
      val stZ = shiftRows stY
      val stF = addRoundKey(stZ, roundKeys, Nr)
    in
      stF
    end
end