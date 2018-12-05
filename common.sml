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

fun splitBy delimiter =
    String.tokens (fn x => x = delimiter)


(* LIST *)
fun stringListToIntList (x : string list) =
    case x of
	[] => []
      | h :: t => valOf (Int.fromString h) :: stringListToIntList (t)

fun createList defaultValue length =
    let
	fun helper (idx, x) =
	    if idx = length
	    then x
	    else helper (idx+1, defaultValue :: x)
    in
	helper (0, [])
    end

(* Returns (idxMax, max) *)
fun getMax isGreater minValue x =
    let
	fun helper ([], _, max, idxMax) = (idxMax, max)
	  | helper ((h :: t), idx, maxSoFar, idxMaxSoFar) =
	    if h > maxSoFar
	    then helper (t, idx+1, h, idx)
	    else helper (t, idx+1, maxSoFar, idxMaxSoFar)
    in
	helper (x, 0, minValue, 0)
    end

fun getMaxElement isGreater minValue x =
    #2 (getMax isGreater minValue x)

fun getIdxOfMaxElement isGreater minValue x=
    #1 (getMax isGreater minValue x)

(* MAP *)
structure IntKey =
struct
type ord_key = int
val compare = Int.compare
end

structure intMap = RedBlackMapFn(IntKey)
