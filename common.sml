Control.Print.printLength := 100;
Control.Print.printDepth := 10;
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

fun createList defaultValueFun length =
    List.tabulate (length, defaultValueFun)

fun createGrid defaultValueFun (width, height) =
    createList (fn _ => (createList defaultValueFun width)) height

fun getElementInGrid grid y x =
    List.nth (List.nth (grid, y), x)

fun mapOnGrid f =
    List.map (List.map f)

(* Returns (idxMax, max) -- Asumes that there is only one maximum *)
fun getMax isGreater minValue x =
    let
	fun helper ([], _, max, idxMax) = (idxMax, max)
	  | helper ((h :: t), idx, maxSoFar, idxMaxSoFar) =
	    if isGreater (h, maxSoFar)
	    then helper (t, idx+1, h, idx)
	    else helper (t, idx+1, maxSoFar, idxMaxSoFar)
    in
	helper (x, 0, minValue, 0)
    end

fun getMaxElement isGreater minValue x =
    #2 (getMax isGreater minValue x)

fun getIdxOfMaxElement isGreater minValue x=
    #1 (getMax isGreater minValue x)

fun removeElementFromList _ _ [] = []
  | removeElementFromList compare el (h :: t) =
    if compare (el, h) = General.EQUAL
    then removeElementFromList compare el t
    else h :: (removeElementFromList compare el t)

fun enumerate x =
    List.tabulate (List.length x, (fn n => (n, List.nth (x, n))))



(* MAP *)
structure IntKey =
struct
type ord_key = int
val compare = Int.compare
end

structure intMap = RedBlackMapFn(IntKey)

(* CELL *)
type cell = {x: int, y: int}
