Control.Print.printLength := 100;
Control.Print.linewidth := 80;

fun readFileByCharacters (infile: string) =
    let
	val f = TextIO.openIn infile
	fun helper (copt: char option) : char list =
	    case copt of
		NONE => (TextIO.closeIn f; [])
	      | SOME(c) => c :: helper(TextIO.input1 f)
    in
	helper(TextIO.input1 f)
    end

fun readFileByLines (infile: string) =
    let
	val f = TextIO.openIn infile
	fun helper () =
	    case TextIO.inputLine f of
		SOME line => line :: helper ()
	      | NONE => (TextIO.closeIn; [])
    in
	helper ()
    end

fun stringListToIntList (x : string list) =
    case x of
	[] => []
      | h :: t => valOf (Int.fromString h) :: stringListToIntList (t)
