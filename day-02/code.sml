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

fun increaseCountFor letter ((headLet: char, headCount) :: t) =
    if letter = headLet
    then (headLet, headCount + 1) :: t
    else (headLet, headCount) :: increaseCountFor letter t
  | increaseCountFor _ _ = []

fun countLetterRepetitions ("") = []
  | countLetterRepetitions (word) =
    let
        val h = String.sub (word, 0)
        val t = String.extract (word, 1, NONE)
        val counts = countLetterRepetitions t
        val found = List.find (fn (x, _) => x = h) counts
    in
        case found of
            SOME (letter, count) => increaseCountFor letter counts
         |   NONE => (h, 1) :: counts
    end

fun find23 [] = (0, 0)
  | find23 (h :: t) =
    let
        val (count2, count3) = find23 t
        val counts = countLetterRepetitions h
        val len2 = List.length (List.filter (fn (_, c) => c = 2) counts)
        val len3 = List.length (List.filter (fn (_, c) => c = 3) counts)
        val add2 = if len2 > 0 then 1 else 0
        val add3 = if len3 > 0 then 1 else 0
    in
        (count2 + add2, count3 + add3)
    end

fun star1 words =
    let
        val (c2, c3) = find23 words
    in
        c2 * c3
    end

fun countDiff (id1, id2, diff) =
    if size id1 = 0 orelse size id2 = 0 orelse diff > 1
    then diff
    else if String.sub (id1, 0) = String.sub (id2, 0)
    then countDiff (String.extract (id1, 1, NONE), String.extract (id2, 1, NONE), diff)
    else countDiff (String.extract (id1, 1, NONE), String.extract (id2, 1, NONE), diff + 1)

fun findMatch word =
    List.find (fn w => countDiff (word, w, 0) = 1)

fun findPair [] = ("", "")
|   findPair (word :: restOfWords) =
    let
        val match = findMatch word restOfWords
    in
        if isSome match
        then (word, valOf match)
        else findPair restOfWords
    end

fun getSameLetters ("", "") = ""
  | getSameLetters (id1, id2) =
    let
        val rest = getSameLetters (String.extract (id1, 1, NONE), String.extract (id2, 1, NONE))
    in
        if String.sub (id1, 0) = String.sub (id2, 0)
        then String.substring (id1, 0, 1) ^ rest
        else rest
    end

val star2 = (getSameLetters o findPair)

fun stars file =
    let
        val input = readFileByLines file
    in
        (star1 input, star2 input)
    end
