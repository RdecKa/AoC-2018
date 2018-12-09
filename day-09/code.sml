use "../common.sml";

val input_star1 = (459, 71320)
val input_star2 = (459, 7132000)
val input1 = (9, 25)
val input2 = (10, 1618)
val input3 = (13, 7999)
val input4 = (17, 1104)
val input5 = (21, 6111)
val input6 = (30, 5807)

fun addMarble marNum circle curIdx =
    let
	fun helper circ idx acc =
	    if idx = 0
	    then List.revAppend (acc, marNum :: circ)
	    else helper (tl circ) (idx - 1) ((hd circ) :: acc)
    in
	helper circle curIdx []
    end

fun takeMarble circle idx =
    let
	fun helper circ idx' acc =
	    if idx' = 0
	    then (hd circ, List.revAppend (acc, tl circ))
	    else helper (tl circ) (idx' - 1) ((hd circ) :: acc)
    in
	helper circle idx []
    end


fun initScores numPlayers = createList (fn _ => ref 0) numPlayers

fun printProgress marNum lastMar =
    if (marNum mod (lastMar div 100) = 0)
    then print (Real.toString (Real.fromInt(marNum) / Real.fromInt(lastMar)) ^ "\n")
    else ()

fun nextTurn (player, marNum, lastMar, circle, scores, curIdx) =
    let
	val _ = printProgress marNum lastMar;
    in
	if marNum > lastMar
	then circle
	else if marNum mod 23 = 0
	then
	    let
		val plSc = List.nth (scores, player)
		val (takenMarble, newCircle) = takeMarble circle ((curIdx - 7) mod (length circle))
	    in
		plSc := (!plSc) + marNum + takenMarble;
		nextTurn (((player + 1) mod (length scores)), (marNum + 1), lastMar, newCircle, scores, ((curIdx - 7) mod (length circle)))
	    end
	else
	    let
		val newCircle = addMarble marNum circle ((curIdx + 2) mod (length circle))
	    in
		nextTurn (((player + 1) mod (length scores)), (marNum + 1), lastMar, newCircle, scores, ((curIdx + 2) mod (length circle)))
	    end
    end

fun getMaxScore scores =
    getMaxElement (fn (x, y) => (!x) > (!y)) (ref 0) scores

fun play (numPlayers, numMarbles) =
    let
	val scores = initScores numPlayers
    in
	nextTurn (0, 1, numMarbles, [0], scores, 0);
	!(getMaxScore scores)
    end

val star1 = play input_star1
val star2 = play input_star2
