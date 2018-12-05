use "../common.sml";

val getInput = (removeElementFromList Char.compare #"\n") o readFileByCharacters

val smallCapitalDiff = Char.ord #"a" - Char.ord #"A"

fun react c1 c2 =
    abs ((Char.ord c1) - (Char.ord c2)) = smallCapitalDiff

fun makeReactions [] = []
  | makeReactions (e1 :: rest) =
    let
	val tail = makeReactions rest
    in
	case tail of
	    [] => [e1]
	  | h :: t => if react e1 h
		      then t
		      else e1 :: tail
    end

val star1 = List.length o makeReactions

fun isSameLetter c1 c2 =
    Char.compare (c1, c2) = General.EQUAL orelse react c1 c2

fun getPairUnit unit =
    if Char.ord unit > Char.ord #"Z"
    then (Char.chr ((Char.ord unit) - smallCapitalDiff), unit)
    else (unit, Char.chr ((Char.ord unit) + smallCapitalDiff))

fun getUniqueUnits polymer =
    let
	fun cmpr (c1, c2) =
	    let
		val o1 = Char.ord c1
		val o2 = Char.ord c2
		val e1 = if o1 > Char.ord #"Z"
			 then o1 - smallCapitalDiff
			 else o1
		val e2 = if o2 > Char.ord #"Z"
			 then o2 - smallCapitalDiff
			 else o2
	    in
		(e1 - e2) < 0
	    end
	val sorted = ListMergeSort.sort cmpr polymer
	fun helper (new, []) = [new]
	  | helper (new, h :: t) = if isSameLetter new h
	    then h :: t
	    else new :: h :: t
    in
	List.map getPairUnit (List.foldl helper [] sorted)
    end

fun removeUnit polymer (capital, small) =
    removeElementFromList Char.compare capital
			  (removeElementFromList Char.compare small polymer)

fun getAllCleanedPolymers polymer =
    let
	fun helper [] = []
	  | helper (hUnit :: tUnit) =
	    (removeUnit polymer hUnit) :: helper tUnit
    in
	helper (getUniqueUnits polymer)
    end

val getTransformedNewPolymers =
    (List.map makeReactions) o getAllCleanedPolymers

fun star2 polymer =
    getMaxElement op< (List.length polymer)
		  (List.map List.length (getTransformedNewPolymers polymer))

fun starsFile file =
    let
	val input = getInput file
    in
	(star1 input, star2 input)
    end

val stars = starsFile "input.txt"
val starsSmall = starsFile "small_input.txt"
