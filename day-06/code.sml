use "../common.sml";

fun getInput file =
    let
	val lines = readFileByLines file
	fun parseLine line =
	    let
		val xStr :: yStr :: nil = splitBy #"," line
		val x = valOf (Int.fromString xStr)
		val y = valOf (Int.fromString (String.extract (yStr, 1, NONE)))
	    in
		{x=x, y=y}
	    end
    in
	List.map parseLine lines
    end

(* returns (topLeft, bottomRight) *)
fun getBorders listOfPoints =
    let
	fun compareTwo ({x=x, y=y}, ({x=x1, y=y1}, {x=x2, y=y2})) =
	    ({x=Int.min(x, x1), y=Int.min(y, y1)}, ({x=Int.max(x, x2), y=Int.max(y, y2)}))
    in
	List.foldl compareTwo (hd listOfPoints, hd listOfPoints) listOfPoints
    end

fun initGrid size =
    let
	val grid = createGridRef ({x=(~1), y=(~1)}, ~1, ~1) size
	fun setIndicesY ([], _) = []
	  | setIndicesY (row :: tr, idy) =
	    let
		fun setIndicesX ([], _) = []
		  | setIndicesX (cell :: tc, idx) =
		    let
			val ({x=x, y=y}, id, d) = !cell
		    in
			cell := ({x=idx, y=idy}, id, d);
			setIndicesX (tc, idx+1)
		    end
	    in
		setIndicesX (!row, 0);
		setIndicesY (tr, idy+1)
	    end
    in
	setIndicesY (grid, 0); grid
    end

fun markGivenPoints {x=baseX, y=baseY} grid =
    let
	fun updateCell ((id, {x=x, y=y})) =
	    let
		val r = getElementInGridRef grid (y-baseY) (x-baseX)
		val coords = #1 (!r)
	    in
		r := (coords, id, 0)
	    end
    in
	List.app updateCell
    end

fun getDistancesToAllPoints ({x=x, y=y}, _, _) {x=xb, y=yb} =
    List.map (fn (id, {x=xp, y=yp}) => (id, abs (x-(xp-xb)) + abs (y-(yp-yb))))

fun getDistancesToAllPoints' enumPoints baseCell cell =
    getDistancesToAllPoints (!cell) baseCell enumPoints

fun getDistancesToAllPointsForAllCells enumPoints baseCell =
    List.map (fn row => List.map (getDistancesToAllPoints' enumPoints baseCell) (!row))

val getClosestPointForAll =
    let
	fun findMin min [] = min
	  | findMin (minIdSoFar, minSoFar) ((id, d) :: rd) =
	    if d < minSoFar
	    then findMin (id, d) rd
	    else if d = minSoFar
	    then findMin (~1, d) rd
	    else findMin (minIdSoFar, minSoFar) rd
    in
	List.map (List.map (findMin (~1, 10000000)))
    end

fun isInfinite grid id =
    let
	fun compare (id', _) = id = id'
    in
	List.exists compare (List.nth (grid, 0)) (* Top row *)
	orelse List.exists compare (List.nth (grid, ((length grid)-1))) (* Bottom row *)
	orelse List.exists compare (List.map hd grid) (* Left column *)
	orelse List.exists compare (List.map List.last grid) (* Right column *)
    end

fun getSize grid id =
    if isInfinite grid id
    then ~1
    else List.foldl op+ 0 (List.map length (List.map (List.filter (fn (id', _) => id = id')) grid))

fun getSizeAll grid numAreas =
    List.tabulate (numAreas, getSize grid)

fun star1 enumPoints baseCell distances =
    getMaxElement op> ~1 (getSizeAll (getClosestPointForAll distances) (length enumPoints))

fun sumOfDist dist =
    List.foldl (fn ((_, d1), (_, d2)) => (~1, d1 + d2)) (~1, 0) dist

fun sumOfDistForAll dists =
    List.map (List.map sumOfDist) dists

fun star2 distances limit =
    List.foldl op+ 0 (
	List.map length (
	    List.map (
		List.filter (fn (_, sd) => sd < limit)) (sumOfDistForAll distances)))

fun starsFile file limit =
    let
	val input = getInput file
	val ({x=xMin, y=yMin}, {x=xMax, y=yMax}) = getBorders input
	val baseCell = {x=xMin, y=yMin}
	val grid = initGrid (xMax - xMin + 1, yMax - yMin + 1)
	val enumPoints = enumerate input
	val distances = getDistancesToAllPointsForAllCells enumPoints baseCell grid
	val _ = markGivenPoints baseCell grid
    in
	(star1 enumPoints baseCell distances, star2 distances limit)
    end

val stars = starsFile "input.txt" 10000
val starsSmall = starsFile "small_input.txt" 32
