use "../common.sml";

fun parseLine line =
    let
	val spl = splitBy #" " line
	val cond = valOf (Char.fromString (List.nth (spl, 1)))
	val step = valOf (Char.fromString (List.nth (spl, 7)))
    in
	(cond, step, ~1)
    end

val getInput = (List.map parseLine) o readFileByLines

fun getAllSteps condPairs =
    let
	fun addSteps ((s1, s2, _), lst) =
	    let
		val s1' = if List.exists (fn x => x=s1) lst then [] else [s1]
		val s2' = if List.exists (fn x => x=s2) lst then [] else [s2]
	    in
		s1' @ s2' @ lst
	    end
    in
	ListMergeSort.sort (fn (x, y) => ord x > ord y) (List.foldl addSteps [] condPairs)
    end

fun initConds condPairs =
    let
	val allSteps = getAllSteps condPairs
	fun findCondsFor step =
	    (step, List.map #1 (List.filter (fn (_, c, _) => c=step) condPairs), ~1)
    in
	List.map findCondsFor allSteps
    end

fun getAvaliableStep condList =
    List.find (fn (_, conds, _) => null conds) condList

(* Removes step 'step' from 'conds' of a given step *)
fun removeFromConds step (s, conds, t) =
    (s, List.filter (fn c => Char.compare (c, step) <> General.EQUAL) conds, t)

(* Removes from condList, but not yet from conds for other steps *)
fun removeAvaliableStep condList step =
    (List.filter (fn (s, _, _) => Char.compare (s, step) <> General.EQUAL) condList)

(* Removes from condList and all conds - complete removal *)
fun removeStep condList step =
    List.map (removeFromConds step) (removeAvaliableStep condList step)

fun getSequence [] = ""
  | getSequence condList =
    let
	val nextStep = #1 (valOf (getAvaliableStep condList))
    in
	(String.str nextStep) ^ (getSequence (removeStep condList nextStep))
    end

val star1 = getSequence

fun addDuration add =
    List.map (fn (s, cs, _) => (s, cs, (ord s) - (ord #"A") + 1 + add))

fun getAtMostNSteps condList n =
    let
	val allSteps = List.filter (fn (_, conds, _) => null conds) condList
    in
	if length allSteps <= n
	then allSteps
	else List.take (allSteps, n)
    end

fun tick queue sec condList workers =
    if null queue andalso sec > 0
    then sec
    else
	let
	    val decreaseQ = List.map (fn (s, cs, t) => (s, cs, t-1)) queue
	    val (finishedQ, inProgressQ) = List.partition (fn (_, _, t) => t = 0) decreaseQ
	    val clNew = List.foldl (fn ((s, _, _), cl) => removeStep cl s) condList finishedQ
	    val newSteps = getAtMostNSteps clNew (workers - (length inProgressQ))
	    val clNew' = List.foldl (fn ((s, _, _), cl) => removeAvaliableStep clNew s) clNew newSteps
	    val newQ = inProgressQ @ newSteps
	in
	    tick newQ (sec+1) clNew' workers
	end

fun star2 add condList =
    tick [] ~1 (addDuration add condList)

fun starsFile file add workers =
    let
	val input = initConds (getInput file)
    in
	(star1 input, star2 add input workers)
    end

val stars = starsFile "input.txt" 60 5
val starsSmall = starsFile "small_input.txt" 0 2
