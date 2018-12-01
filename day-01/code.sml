Control.Print.printLength := 100;
Control.Print.linewidth := 80;

(* UNUSED for this task (First attempt to read a file) *)
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

fun star1 (input) =
    foldl op+ 0 input


fun findFirstDuplicate (curFreq, x, foundFreqs, wholeList) =
    case x of
	[] => findFirstDuplicate (curFreq, wholeList, foundFreqs, wholeList)
      | h :: t =>
	let
	    val newFreq = curFreq + h
	in
	    if List.exists (fn x => x = newFreq) foundFreqs
	    then
		newFreq
	    else
		findFirstDuplicate(newFreq, t, newFreq :: foundFreqs, wholeList)
	end

fun star2 (input) =
    findFirstDuplicate (0, input, [0], input)

fun stars (file: string) =
    let
	val input = stringListToIntList (readFileByLines(file))
	val s1 = star1 input
	val s2 = star2 input
    in
	s1 :: [s2]
    end
