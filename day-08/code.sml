use "../common.sml";

datatype node = Leaf of int list | Node of (node list * int list)

fun getInput file =
    List.map (valOf o Int.fromString) (splitBy #" " (hd (readFileByLines file)))

fun createTree (numCh :: numMeta :: tail) =
    if numCh = 0
    then (Leaf (List.take (tail, numMeta)), List.drop (tail, numMeta))
    else
	let
	    fun getChildren n x =
		if n = 0
		then ([], x)
		else
		    let
			val (child, rest) = createTree (x)
			val (children, rest') = getChildren (n-1) rest
		    in
			(child :: children, rest')
		    end
	    val (children, rest) = getChildren numCh tail
	in
	    (Node (children, List.take (rest, numMeta)), List.drop (rest, numMeta))
	end

val sumMeta = List.foldl op+ 0

fun sumTree tree =
    case tree of
	Leaf m => sumMeta m
      | Node (children, m) => (sumMeta m) + (sumMeta (List.map sumTree children))

val star1 = sumTree

fun getTreeValue tree =
    case tree of
	Leaf m => sumMeta m
      | Node (n, m) => sumMeta (List.map (fn i => if i > 0 andalso i-1 < length n
						  then getTreeValue (List.nth (n, i-1))
						  else 0) m)

val star2 = getTreeValue

fun starsFile file =
    let
	val input = getInput file
	val tree = #1 (createTree input)
    in
	(star1 tree, star2 tree)
    end

val stars = starsFile "input.txt"
val starsSmall = starsFile "small_input.txt"
