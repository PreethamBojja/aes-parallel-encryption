structure ReadFile =
struct

  fun contentsSeq filename =
    let
      val () =
        if Posix.FileSys.ST.isReg (Posix.FileSys.stat filename) then ()
        else raise Fail ("Could not read " ^ filename)
    in
      ReadFile.contentsSeq filename
    end

end
