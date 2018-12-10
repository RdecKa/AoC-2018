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

(* -------------------- *)
(* --- OPTIMIZATION --- *)
(* -------------------- *)
fun remove circleEnd =
    let
	fun helper circ idx acc =
	    if idx = 0
	    then (acc, hd circ, tl circ)
	    else helper (tl circ) (idx - 1) ((hd circ) :: acc)
    in
	helper circleEnd 6 []
    end

fun listToString l =
    List.foldl (fn (x, y) => y ^ " " ^ Int.toString(x)) "" l

fun splitOnIdx l i =
    if i = 0
    then ([], l, i)
    else
	let
	    val (f, e, _) = splitOnIdx (tl l) (i - 1)
	in
	    ((hd l) :: f, e, i)
	end

fun moveEndToFront endCircle needed =
    let
	val len = length endCircle
	fun helper e =
	    if null e
	    then ([], [], 0)
	    else
		let
		    val (addFront, newEnd, taken) = helper (tl e)
		in
		    if taken >= needed
		    then ((hd e) :: addFront, newEnd, taken)
		    else (addFront, (hd e) :: newEnd, taken + 1)
		end
	val (newEnd, addFront, _) = if len > needed + 6
				    then splitOnIdx endCircle 7
				    else helper endCircle
    in
	(newEnd, List.rev addFront)
    end

fun printWholeList listFront listEnd =
    print ((listToString listFront) ^ " --- " ^ listToString (rev listEnd) ^ "\n")

(* Current marble is always at index 0 of circleFront *)
fun next (player, marNum, lastMar, circleFront, circleEnd, scores) =
    let
	val _ = printProgress marNum lastMar
	val nextPlayer = (player + 1) mod (length scores)
    in
	if marNum > lastMar
	then circleFront @ (List.rev circleEnd)
	else if marNum mod 23 = 0
	then
	    let
		val plSc = List.nth (scores, player)
		val (addFront, takenMarble, newEnd) = remove circleEnd
	    in
		plSc := (!plSc) + marNum + takenMarble;
		next (nextPlayer, marNum + 1, lastMar, addFront @ circleFront, newEnd, scores)
	    end
	else if length circleFront > 1
	then
	    let
		val first :: second :: tailFront = circleFront
		val newFront = marNum :: tailFront
		val newEnd = second :: first :: circleEnd
	    in
		next (nextPlayer, marNum + 1, lastMar, newFront, newEnd, scores)
	    end
	else
	    let
		val needed = 23 - (marNum mod 23)
		val (newEnd, addFront) = moveEndToFront circleEnd needed
		val newFront = circleFront @ addFront
	    in
		next (player, marNum, lastMar, newFront, newEnd, scores)
	    end
    end

fun game (np, nm) =
    let
	val scores = initScores np
    in
	next (1, 2, nm, [1, 0], [], scores);
	!(getMaxScore scores)
    end

	(*
val star1 = game input_star1
val star2 = game input_star2
	*)
