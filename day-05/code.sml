use "../common.sml";

val getInput = (removeElementFromList Char.compare #"\n") o readFileByCharacters

fun react c1 c2 =
    abs ((Char.ord c1) - (Char.ord c2)) = Char.ord #"a" - Char.ord #"A"

fun makeReactions polymer =
    List.foldr (fn (e, pol) => if null pol
			       then [e]
			       else if react e (hd pol)
			       then tl pol
			       else e :: pol) [] polymer

val star1 = List.length o makeReactions

fun isSameLetter c1 c2 =
    Char.compare (c1, c2) = General.EQUAL orelse react c1 c2

fun getPairUnit unit =
    (Char.toUpper unit, Char.toLower unit)

fun getUniqueUnits polymer =
    let
	fun cmpr (c1, c2) =
	    ((Char.ord (Char.toLower c1)) - (Char.ord (Char.toLower c2))) < 0
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
    List.map (removeUnit polymer) (getUniqueUnits polymer)

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
