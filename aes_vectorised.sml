structure AES_vectorised : sig
  val encrypt_block : Word8.word Seq.seq -> Word8.word Seq.seq -> Word8.word Seq.seq
  val keyExpansion  : Word8.word Seq.seq -> Word8.word Seq.seq
  val setIspc       : bool -> unit
end = struct
  structure W = Word8
  type byte  = W.word
  type state = byte Array.array

  val Nb = 4
  val Nk = 4
  val Nr = 10

  val useIspc = ref false
  fun setIspc b = useIspc := b

  (* --- Flat S-box and RCON tables --- *)
  val sboxArr = Array.fromList (List.map W.fromInt [
    0x63,0x7C,0x77,0x7B,0xF2,0x6B,0x6F,0xC5,0x30,0x01,0x67,0x2B,0xFE,0xD7,0xAB,0x76,
    0xCA,0x82,0xC9,0x7D,0xFA,0x59,0x47,0xF0,0xAD,0xD4,0xA2,0xAF,0x9C,0xA4,0x72,0xC0,
    0xB7,0xFD,0x93,0x26,0x36,0x3F,0xF7,0xCC,0x34,0xA5,0xE5,0xF1,0x71,0xD8,0x31,0x15,
    0x04,0xC7,0x23,0xC3,0x18,0x96,0x05,0x9A,0x07,0x12,0x80,0xE2,0xEB,0x27,0xB2,0x75,
    0x09,0x83,0x2C,0x1A,0x1B,0x6E,0x5A,0xA0,0x52,0x3B,0xD6,0xB3,0x29,0xE3,0x2F,0x84,
    0x53,0xD1,0x00,0xED,0x20,0xFC,0xB1,0x5B,0x6A,0xCB,0xBE,0x39,0x4A,0x4C,0x58,0xCF,
    0xD0,0xEF,0xAA,0xFB,0x43,0x4D,0x33,0x85,0x45,0xF9,0x02,0x7F,0x50,0x3C,0x9F,0xA8,
    0x51,0xA3,0x40,0x8F,0x92,0x9D,0x38,0xF5,0xBC,0xB6,0xDA,0x21,0x10,0xFF,0xF3,0xD2,
    0xCD,0x0C,0x13,0xEC,0x5F,0x97,0x44,0x17,0xC4,0xA7,0x7E,0x3D,0x64,0x5D,0x19,0x73,
    0x60,0x81,0x4F,0xDC,0x22,0x2A,0x90,0x88,0x46,0xEE,0xB8,0x14,0xDE,0x5E,0x0B,0xDB,
    0xE0,0x32,0x3A,0x0A,0x49,0x06,0x24,0x5C,0xC2,0xD3,0xAC,0x62,0x91,0x95,0xE4,0x79,
    0xE7,0xC8,0x37,0x6D,0x8D,0xD5,0x4E,0xA9,0x6C,0x56,0xF4,0xEA,0x65,0x7A,0xAE,0x08,
    0xBA,0x78,0x25,0x2E,0x1C,0xA6,0xB4,0xC6,0xE8,0xDD,0x74,0x1F,0x4B,0xBD,0x8B,0x8A,
    0x70,0x3E,0xB5,0x66,0x48,0x03,0xF6,0x0E,0x61,0x35,0x57,0xB9,0x86,0xC1,0x1D,0x9E,
    0xE1,0xF8,0x98,0x11,0x69,0xD9,0x8E,0x94,0x9B,0x1E,0x87,0xE9,0xCE,0x55,0x28,0xDF,
    0x8C,0xA1,0x89,0x0D,0xBF,0xE6,0x42,0x68,0x41,0x99,0x2D,0x0F,0xB0,0x54,0xBB,0x16
  ])
  val rconArr = Array.fromList (List.map W.fromInt [0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x1B,0x36])


  fun aes_encrypt_ispc (st: state, rk: state, n: Int.int) = 
    (_import "aes_encrypt_ispc" : state * state * Int.int -> unit;) 
      (st, rk, n)

  fun xtime b =
    let
      val bb      = W.toInt b
      val shifted = (bb * 2) mod 256
      val word    = W.fromInt shifted
    in
      if bb >= 0x80 then W.xorb (word, W.fromInt 0x1B)
      else word
    end


  fun shiftRowsPure_arr st =
    let
      val len   = Array.length st
      val tmp   = Array.tabulate (len, fn i => Array.sub (st, i))
      val cref  = ref 0
    in
      while !cref < Nb do
        ( let val rref = ref 0 in
            while !rref < Nb do
              ( let
                  val srcIdx = 4 * (((!cref + !rref) mod Nb)) + !rref
                  val dstIdx = 4 * !cref + !rref
                in
                  Array.update (st, dstIdx, Array.sub (tmp, srcIdx))
                end;
                rref := !rref + 1
              )
          end;
          cref := !cref + 1
        );
      st
    end

  fun subBytesPure_arr st =
    let val len = Array.length st
        val iref = ref 0
    in
      while !iref < len do
        ( let val b = Array.sub (st, !iref)
          in Array.update (st, !iref, Array.sub (sboxArr, W.toInt b)) end;
          iref := !iref + 1
        );
      st
    end

  fun mixColumnsPure_arr st =
    let
      fun idx (r,c) = 4*c + r
      fun mixCol c =
        let
          val c0 = Array.sub (st, idx (0,c))
          val c1 = Array.sub (st, idx (1,c))
          val c2 = Array.sub (st, idx (2,c))
          val c3 = Array.sub (st, idx (3,c))
          val t  = W.xorb (c0, W.xorb (c1, W.xorb (c2, c3)))
          val r0 = W.xorb (W.xorb (c0, xtime (W.xorb (c0,c1))), t)
          val r1 = W.xorb (W.xorb (c1, xtime (W.xorb (c1,c2))), t)
          val r2 = W.xorb (W.xorb (c2, xtime (W.xorb (c2,c3))), t)
          val r3 = W.xorb (W.xorb (c3, xtime (W.xorb (c3,c0))), t)
        in
          Array.update (st, idx (0,c), r0);
          Array.update (st, idx (1,c), r1);
          Array.update (st, idx (2,c), r2);
          Array.update (st, idx (3,c), r3)
        end
      val cref = ref 0
    in
      while !cref < Nb do
        ( mixCol (!cref); cref := !cref + 1 );
      st
    end

  fun addRoundKeyPure_arr (st, rk, rnd) =
    let
      val offset = rnd * Nb * 4
      val len    = Array.length st
      val iref   = ref 0
    in
      while !iref < len do
        ( let
            val v = Array.sub (rk, offset + !iref)
            val x = W.xorb (Array.sub (st, !iref), v)
          in Array.update (st, !iref, x) end;
          iref := !iref + 1
        );
      st
    end


  fun keyExpansion_arr keySeq =
    let
      val total = Nb * (Nr + 1) * 4
      val out   = Array.array (total, W.fromInt 0)
      val len   = Seq.length keySeq
      fun copyKey i =
        if i < len then
          ( Array.update (out, i, Seq.nth keySeq i);
            copyKey (i + 1) )
        else ()
      val _ = copyKey 0
      fun loop (i, rcix) =
        if i < total then
          let
            val t0 = Array.sub (out, i-4)
            val t1 = Array.sub (out, i-3)
            val t2 = Array.sub (out, i-2)
            val t3 = Array.sub (out, i-1)
            val (u0,u1,u2,u3,rc') =
              if i mod (Nk * 4) = 0 then
                let
                  val sb  = fn x => Array.sub (sboxArr, W.toInt x)
                  val rcv = Array.sub (rconArr, rcix)
                in
                  ( W.xorb (sb t1, rcv), sb t2, sb t3, sb t0, rcix + 1 )
                end
              else (t0, t1, t2, t3, rcix)
            val p  = i - (Nk * 4)
            val o0 = W.xorb (u0, Array.sub (out, p))
            val o1 = W.xorb (u1, Array.sub (out, p + 1))
            val o2 = W.xorb (u2, Array.sub (out, p + 2))
            val o3 = W.xorb (u3, Array.sub (out, p + 3))
          in
            Array.update (out, i,   o0);
            Array.update (out, i+1, o1);
            Array.update (out, i+2, o2);
            Array.update (out, i+3, o3);
            loop (i + 4, rc')
          end
        else ()
    in
      loop (Nk * 4, 0);
      out
    end


  fun encrypt_block_arr rk ptSeq =
    let
      val st = Array.tabulate (Nb*4, fn i => Seq.nth ptSeq i)
    in
      if !useIspc then
        ( aes_encrypt_ispc (st, rk, 1); st )
      else
        let
          val _    = addRoundKeyPure_arr (st, rk, 0)
          val rref = ref 1
        in
          while !rref < Nr do
            ( subBytesPure_arr st;
              shiftRowsPure_arr st;
              mixColumnsPure_arr st;
              addRoundKeyPure_arr (st, rk, !rref);
              rref := !rref + 1
            );
          subBytesPure_arr st;
          shiftRowsPure_arr st;
          addRoundKeyPure_arr (st, rk, Nr);
          st
        end
    end


  fun keyExpansion keySeq =
    let
      val arr = keyExpansion_arr keySeq
      val len = Array.length arr
    in
      Seq.tabulate (fn i => Array.sub(arr,i)) len
    end

  fun encrypt_block rkSeq ptSeq =
    let
      val rkLen = Seq.length rkSeq
      val rkArr = Array.tabulate (rkLen, fn i => Seq.nth rkSeq i)
      val outArr = encrypt_block_arr rkArr ptSeq
      val len = Array.length outArr
    in
      Seq.tabulate (fn i => Array.sub(outArr,i)) len
    end

end