structure Graph :>
sig
  type t
  type graph = t

  type vertex = int

  (* =======================================================================
   * All of these functions require O(1) work and span
   *)

  val in_degree: graph * vertex -> int
  val out_degree: graph * vertex -> int

  val in_neighbors: graph * vertex -> vertex Seq.t
  val out_neighbors: graph * vertex -> vertex Seq.t

  val num_vertices: graph -> int
  val num_edges: graph -> int

  (* ======================================================================= *)

  val load_from_file: string -> graph
  val load_from_snap_file: string -> graph
end =
struct
  structure A = AdjacencyGraph(Int)


  type vertex = int

  datatype graph =
    G of {inn: int Seq.t, outn: int Seq.t, inoff: int Seq.t, outoff: int Seq.t}

  type t = graph


  fun edge_cmp ((u1, v1), (u2, v2)) =
    case Int.compare (u1, u2) of
      EQUAL => Int.compare (v1, v2)
    | other => other


  fun compress (s: vertex Seq.t Seq.t) : A.graph =
    let
      val degrees = Seq.map Seq.length s
      val (offsets, _) = Seq.scan op+ 0 degrees
    in
      (offsets, degrees, Seq.flatten s) : A.graph
    end


  fun filter_graph (ag: A.graph) p =
    let
      val nbrs' =
        Seq.tabulate (fn u => Seq.filter (fn v => p (u, v)) (A.neighbors ag u))
          (A.numVertices ag)
    in
      compress nbrs'
    end


  fun inject_cycles (ag: A.graph) k =
    let
      val nbrs' =
        Seq.tabulate
          (fn u =>
             if u > k + 1 then
               A.neighbors ag u
             else
               Merge.merge Int.compare
                 ( A.neighbors ag u
                 , Seq.flatten (Seq.fromList
                     [ if u >= 2 then Seq.singleton (u - 2) else Seq.empty ()
                     , if u <= k then Seq.singleton (u + 1) else Seq.empty ()
                     ])
                 )) (A.numVertices ag)
    in
      compress nbrs'
    end


  fun load_from_snap_file filename =
    let
      val chars = ReadFile.contentsSeq filename
      val nl_pos =
        ArraySlice.full
          (SeqBasis.filter 1000 (0, Seq.length chars) (fn i => i) (fn i =>
             Seq.nth chars i = #"\n"))

      (* val _ = Util.for (0, 10) (fn i =>
        print (Int.toString (Seq.nth nl_pos i) ^ " "))
      val _ = print "...\n" *)


      val num_lines = Seq.length nl_pos + 1
      fun line_start i =
        if i = 0 then 0 else 1 + Seq.nth nl_pos (i - 1)
      fun line_end i =
        if i = Seq.length nl_pos then Seq.length chars else Seq.nth nl_pos i
      fun line i =
        Seq.subseq chars (line_start i, line_end i - line_start i)


      (* val _ = Util.for (0, 10) (fn i =>
        print
          (Int.toString (line_start i) ^ " " ^ Int.toString (line_end i) ^ " "))
      val _ = print "...\n" *)


      val first_not_comment =
        valOf (FindFirst.findFirstSerial (0, num_lines) (fn i =>
          Seq.nth chars (line_start i) <> #"#"))

      val num_edges = num_lines - first_not_comment
      val num_edges =
        if line_start (num_lines - 1) = Seq.length chars then num_edges - 1
        else num_edges

      fun parse_edge i =
        let
          val lo = line_start (first_not_comment + i)
          val hi = line_end (first_not_comment + i)

          (* val _ = print
            ("line " ^ Int.toString i ^ " " ^ Int.toString lo ^ " "
             ^ Int.toString hi ^ "\n")
          val _ = print
            (Seq.toString
               (fn c => if Char.isSpace c then "_" else Char.toString c)
               (line (first_not_comment + i)) ^ "\n") *)

          val mid1 = valOf (FindFirst.findFirstSerial (lo, hi) (fn j =>
            Char.isSpace (Seq.nth chars j)))
          (* val _ = print (Int.toString mid1 ^ "\n") *)
          val mid2 = valOf (FindFirst.findFirstSerial (mid1, hi) (fn j =>
            not (Char.isSpace (Seq.nth chars j))))
          val mid3 = Option.getOpt
            ( FindFirst.findFirstSerial (mid2, hi) (fn j =>
                Char.isSpace (Seq.nth chars j))
            , hi
            )
          (* val _ = print (Int.toString mid2 ^ "\n") *)

          val from = valOf (Parse.parseInt (Seq.subseq chars (lo, mid1 - lo)))
          (* val _ = print ("from: " ^ Int.toString from ^ "\n") *)
          val to = valOf (Parse.parseInt (Seq.subseq chars (mid2, mid3 - mid2)))
        (* val _ = print ("to: " ^ Int.toString to ^ "\n") *)
        in
          (from, to)
        end
        handle exn =>
          raise Fail
            ("Graph.parse_edge: line "
             ^ Int.toString (1 + i + first_not_comment)
             ^ ": error during parsing: " ^ exnMessage exn)

      val e = Seq.tabulate parse_edge num_edges

      val self = Parallel.reduce (op+) 0 (0, Seq.length e) (fn i =>
        let val (u, v) = Seq.nth e i
        in if u = v then 1 else 0
        end)
      val _ =
        if self = 0 then
          ()
        else
          print
            ("  Warning: graph contains " ^ Int.toString self ^ " self-loops\n")


      val (min_v, max_v) =
        Parallel.reduce
          (fn ((a, b), (c, d)) => (Int.min (a, c), Int.max (b, d)))
          (valOf Int.maxInt, valOf Int.minInt) (0, Seq.length e)
          (fn i => let val (u, v) = Seq.nth e i
                   in (Int.min (u, v), Int.max (u, v))
                   end)

      val name_range_size = max_v - min_v + 1
      val used = SeqBasis.tabulate 1000 (0, name_range_size) (fn i =>
        0w0 : Word8.word)
      val _ = Seq.foreach e (fn (_, (u, v)) =>
        ( Array.update (used, u - min_v, 0w1)
        ; Array.update (used, v - min_v, 0w1)
        ))

      val new_names = SeqBasis.scan 1000 (op+) 0 (0, name_range_size) (fn i =>
        if Array.sub (used, i) = 0w0 then 0 else 1)

      val e =
        Seq.map
          (fn (u, v) =>
             ( Array.sub (new_names, u - min_v)
             , Array.sub (new_names, v - min_v)
             )) e

      val num_vertices = Array.sub (new_names, name_range_size)

      val e_sorted = Mergesort.sort edge_cmp e
      val be = Seq.map (fn (u, v) => (v, u)) e
      val be_sorted = Mergesort.sort edge_cmp be

      val outg as (outoff, _, outn) = A.fromSortedEdges e_sorted
      val ing as (inoff, _, inn) = A.fromSortedEdges be_sorted
    in
      if
        Seq.length inoff = num_vertices + 1
        andalso Seq.length outoff = num_vertices + 1
      then (* annoying but expected behavior *) ()
      else raise Fail "edge reversal failed";

      G { inn = inn
        , outn = outn
        , inoff = Seq.take inoff num_vertices
        , outoff = Seq.take outoff num_vertices
        }
    end


  fun load_from_file filename =
    let
      val outg as (outoff, _, outn) = A.parseFile filename
      val _ = print (Int.toString (Seq.length outoff) ^ "\n")
      val _ = print (Int.toString (Seq.length outn) ^ "\n")

      val _ = print
        ("parity check? " ^ (if A.parityCheck outg then "yes" else "no") ^ "\n")

      (* val _ = print ("filtering...\n")
      val outg as (outoff, _, outn) = filter_graph outg (fn (u, v) => u > v)
      
      val _ = print (Int.toString (Seq.length outoff) ^ "\n")
      val _ = print (Int.toString (Seq.length outn) ^ "\n")
      
      val _ = print
        ("parity check? " ^ (if A.parityCheck outg then "yes" else "no") ^ "\n") *)

      (* val _ = print ("injecting a few cycles...\n")
      val outg as (outoff, _, outn) = inject_cycles outg 100 *)

      val be = Seq.flatten
        (Seq.tabulate (fn u => Seq.map (fn v => (v, u)) (A.neighbors outg u))
           (A.numVertices outg))

      (* val be_sorted =
        Seq.map (Seq.nth be)
          (Mergesort.sort (fn (i, j) => edge_cmp (Seq.nth be i, Seq.nth be j))
             (Seq.tabulate (fn i => i) (Seq.length be))) *)

      val be_sorted = Mergesort.sort edge_cmp be

      val ing as (inoff, _, inn) = A.fromSortedEdges be_sorted
      val inoff =
        if Seq.length inoff = Seq.length outoff + 1 then
          (* this is annoying but expected *)
          Seq.take inoff (Seq.length outoff)
        else
          raise Fail "edge reversal failed"

    (* val _ = print (Int.toString (Seq.length inoff) ^ "\n")
    val _ = print (Int.toString (Seq.length inn) ^ "\n") *)
    in
      G {inn = inn, outn = outn, inoff = inoff, outoff = outoff}
    end


  fun num_edges (G {outn, inn, ...}) =
    (* if Seq.length outn = Seq.length inn then Seq.length outn
    else raise Fail "uh oh" *)
    Seq.length outn

  fun num_vertices (G {outoff, inoff, ...}) =
    (* if Seq.length outoff = Seq.length inoff then Seq.length outoff
    else raise Fail "oh no" *)
    Seq.length outoff


  fun in_degree (g as G {inoff, ...}, v) =
    let
      val lo = Seq.nth inoff v
      val hi =
        if v = num_vertices g - 1 then num_edges g else Seq.nth inoff (v + 1)
    in
      hi - lo
    end


  fun out_degree (g as G {outoff, ...}, v) =
    let
      val lo = Seq.nth outoff v
      val hi =
        if v = num_vertices g - 1 then num_edges g else Seq.nth outoff (v + 1)
    in
      hi - lo
    end


  fun in_neighbors (g as G {inoff, inn, ...}, v) =
    Seq.subseq inn (Seq.nth inoff v, in_degree (g, v))


  fun out_neighbors (g as G {outoff, outn, ...}, v) =
    Seq.subseq outn (Seq.nth outoff v, out_degree (g, v))


end
