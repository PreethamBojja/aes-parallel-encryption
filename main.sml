val filename =
  List.hd (CommandLineArgs.positional ())
  handle _ => Util.die "Usage: ./main @mpl procs <P> -- <PLAINTEXT_FILE>"

val _ = print "Loading plaintext from file...\n"
val (plaintext, loadTime) = Util.getTime (fn _ => TextIO.inputAll (TextIO.openIn filename))
val _ = print ("Loaded plaintext in " ^ Time.fmt 4 loadTime ^ "s\n")

val key = "secretkey"

val _ = print "Encrypting...\n"
val (ciphertext, encTime) = Benchmark.run (fn _ => AES.encrypt key plaintext)
val _ = print ("Encryption took " ^ Time.fmt 4 encTime ^ "s\n")

val _ = print ("Ciphertext preview (first 100 chars):\n" ^
               String.extract (ciphertext, 0, SOME (Int.min (100, String.size ciphertext))) ^ "\n")
