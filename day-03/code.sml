use "../common.sml";

type claim = {id: int, x: int, y: int, w: int, h: int}

datatype claimChange = changeY of int | changeX of int | changeW of int | changeH of int

exception InvalidClaimField

fun splitBy delimiter =
    String.tokens (fn x => x = delimiter)

fun parseLine line =
    let
	val idStr :: _ :: locStr :: sizeStr :: nil = splitBy #" " line
	val id = Int.fromString (String.extract (idStr, 1, NONE))
	val x :: y :: nil = splitBy #"," locStr
	val w :: h :: nil = splitBy #"x" sizeStr
    in
	{id=valOf (id),
	 x=valOf (Int.fromString x),
	 y=valOf (Int.fromString y),
	 w=valOf (Int.fromString w),
	 h=valOf (Int.fromString h)}
    end

fun getInput file =
    let
	val input = readFileByLines file
    in
	List.map parseLine input
    end

fun getMinFabricSize (listOfClaims: claim list) =
    let
	fun helper (restOfClaims: claim list, minWidth, minHeight) =
	    case restOfClaims of
		[] => (minWidth, minHeight)
	      | h :: t =>
		let
		     val newMinWidth = Int.max (minWidth, #x h + #w h)
		     val newMinHeight = Int.max (minHeight, #y h + #h h)
		in
		    helper (t, newMinWidth, newMinHeight)
		end
    in
	helper (listOfClaims, 0, 0)
    end

fun createFabric listOfClaims =
    let
	val (fWidth, fHeight) = getMinFabricSize listOfClaims
	fun createRows (numRows, numCols) =
	    let
		fun createCols numCols =
		    case numCols of
			0 => []
		      | _ => 0 :: createCols (numCols-1)
	    in
		case numRows of
		    0 => []
		  | _ => (createCols numCols) :: createRows (numRows-1, numCols)
	    end
    in
	createRows (fHeight, fWidth)
    end

fun getTransformedClaim (orig:claim, change) =
    case change of
	changeX c => {id=(#id orig), x=((#x orig)+c), y=(#y orig), w=(#w orig), h=(#h orig)}
      | changeY c => {id=(#id orig), x=(#x orig), y=((#y orig)+c), w=(#w orig), h=(#h orig)}
      | changeW c => {id=(#id orig), x=(#x orig), y=(#y orig), w=((#w orig)+c), h=(#h orig)}
      | changeH c => {id=(#id orig), x=(#x orig), y=(#y orig), w=(#w orig), h=((#h orig)+c)}

fun markOneRow (row, claim) =
    if (#w claim) = 0
    then row
    else if (#x claim) > 0
    then (hd row) :: markOneRow ((tl row), getTransformedClaim (claim, changeX ~1))
    else ((hd row)+1) :: markOneRow ((tl row), getTransformedClaim (claim, changeW ~1))

fun markOneClaim (fabric, claim) =
    if (#h claim) = 0
    then fabric
    else if (#y claim) > 0
    then (hd fabric) :: markOneClaim ((tl fabric), getTransformedClaim (claim, changeY ~1))
    else (markOneRow ((hd fabric), claim)) :: (markOneClaim ((tl fabric), getTransformedClaim (claim, changeH ~1)))


fun markClaims fabric [] = fabric
  | markClaims fabric (firstClaim :: restOfClaims) =
    markClaims (markOneClaim (fabric, firstClaim)) restOfClaims

fun countOverlaps [] = 0
  | countOverlaps (fabricH :: fabricT) =
    let
	fun countOverlapsInRow [] = 0
	  | countOverlapsInRow (h :: t) =
	    (if h > 1 then 1 else 0) + countOverlapsInRow t
    in
	(countOverlapsInRow fabricH) + (countOverlaps fabricT)
    end

val star1 = countOverlaps

fun isRowIntacted rowWithMarkedClaims claim =
    if (#w claim) = 0
    then true
    else if (#x claim) > 0
    then isRowIntacted (tl rowWithMarkedClaims) (getTransformedClaim (claim, changeX ~1))
    else ((hd rowWithMarkedClaims) = 1)
	 andalso isRowIntacted (tl rowWithMarkedClaims) (getTransformedClaim (claim, changeW ~1))

fun isIntacted fabricWithMarkedClaims claim =
    if (#h claim) = 0
    then true
    else if (#y claim) > 0
    then isIntacted (tl fabricWithMarkedClaims) (getTransformedClaim (claim, changeY ~1))
    else (isRowIntacted (hd fabricWithMarkedClaims) claim)
	 andalso isIntacted (tl fabricWithMarkedClaims) (getTransformedClaim (claim, changeH ~1))

fun getIntactClaim _ [] = []
  | getIntactClaim fabricWithMarkedClaims (firstClaim :: restOfClaims) =
    let
	val rest = getIntactClaim fabricWithMarkedClaims restOfClaims
    in
	if isIntacted fabricWithMarkedClaims firstClaim
	then firstClaim :: rest
	else rest
    end

fun star2 fabricWithMarkedClaims listOfClaims =
    #id (hd (getIntactClaim fabricWithMarkedClaims listOfClaims))

fun starsFile (file) =
    let
	val listOfClaims = getInput file
	val fabric = createFabric listOfClaims
	val fabricWithMarkedClaims = markClaims fabric listOfClaims
    in
	(star1 fabricWithMarkedClaims, star2 fabricWithMarkedClaims listOfClaims)
    end

fun stars () =
    starsFile "input.txt"

fun starsSmall () =
    starsFile "small_input.txt"
