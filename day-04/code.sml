use "../common.sml";

type timestamp = {year: int, month: int, day: int, hour: int, minute: int}

datatype action = Begins of (timestamp * IntKey.ord_key) | FallsAsleep of timestamp | WakesUp of timestamp

exception InvalidAction
exception InvalidKey
exception InvalidSequence
exception Missed

fun parseLine (line: string) =
    let
	val timeStr :: act :: nil = splitBy #"]" (String.extract (line, 1, NONE))
	val date :: time :: nil = splitBy #" " timeStr
	val year :: month :: day :: nil = splitBy #"-" date
	val hour :: minute :: nil = splitBy #":" time
	val ts = {year=valOf (Int.fromString year),
		  month=valOf (Int.fromString month),
		  day=valOf (Int.fromString day),
		  hour=valOf (Int.fromString hour),
		  minute=valOf (Int.fromString minute)}
	val actionSplit = splitBy #" " act

    in
	case hd actionSplit of
	    "Guard" => Begins (ts, valOf (Int.fromString (String.extract ((hd (tl actionSplit)), 1, NONE))))
	  | "falls" => FallsAsleep ts
	  | "wakes" => WakesUp ts
	  | _ => raise InvalidAction
    end

fun getInput file =
    List.map parseLine (readFileByLines file)

fun getTimestampFromAction action =
    case action of
	Begins (ts, _) => ts
      | FallsAsleep ts => ts
      | WakesUp ts => ts

fun compareTimestamps ({year=y1, month=m1, day=d1, hour=h1, minute=min1},
		       {year=y2, month=m2, day=d2, hour=h2, minute=min2}) =
    if y1 <> y2 then y1 - y2
    else if m1 <> m2 then m1 - m2
    else if d1 <> d2 then d1 - d2
    else if h1 <> h2 then h1 - h2
    else min1 - min2

fun greaterThan (a1, a2) =
    compareTimestamps (getTimestampFromAction a1, getTimestampFromAction a2) > 0

val orderChronologicaly = ListMergeSort.sort greaterThan

(* Guard must already exist *)
fun addActionToGuardInMap (ma, guard, action) =
    let
	val oldList = valOf (intMap.find (ma, guard))
	val newList = oldList @ [action]
    in
	intMap.insert (ma, guard, newList)
    end

fun groupByGuard listOfActions =
    let
	fun helper (ma, [],  _) = ma
	  | helper (ma, (firstAction :: restOfActions), currentGuard) =
	    let
		val (id', ma') = case firstAction of
				     Begins (_, id) => if isSome (intMap.find(ma, id))
						       then (id, addActionToGuardInMap (ma, id, firstAction))
						       else (id, intMap.insert (ma, id, [firstAction]))
				   | _ => (currentGuard, addActionToGuardInMap (ma, currentGuard, firstAction))
	    in
		helper (ma', restOfActions, id')
	    end
    in
	helper (intMap.empty, listOfActions, ~1)
    end

val getMidnightHour = createList (fn _ => 0) 60

fun addSleepingMinutes from to night =
    if null night
    then raise Missed
    else if from = to
    then night
    else
	let
	    val (h :: t) = night
	in
	    if from > 0
	    then h :: addSleepingMinutes (from - 1) (to - 1) t
	    else (h + 1) :: addSleepingMinutes 0 (to - 1) t
	end

fun getSleepPeriods ma actions =
    let
	fun helper [] = []
	  | helper (action :: restOfActions) =
	    case action of
		Begins _ => helper restOfActions
	      | FallsAsleep tsFallsAsleep => let
		  val (wakes :: rest) = restOfActions
		  val from = #minute tsFallsAsleep
		  val to = case wakes of
				 WakesUp tsWakesUp => #minute tsWakesUp
			       | _ => raise InvalidSequence
	      in
		  (from, to) :: helper (rest)
	      end
	      | WakesUp _ => raise InvalidSequence
    in
	helper actions
    end

fun getSleepMinutes ma actions =
    List.foldl (fn ((from, to), night) => addSleepingMinutes from to night)
		 getMidnightHour (getSleepPeriods ma actions)

fun getSleepMinutesForAll ma =
    intMap.map (getSleepMinutes ma) ma

fun getTotalSleepForAll ma =
    intMap.map (List.foldl op+ 0) ma

fun getSleepestMinute ma guard =
    getIdxOfMaxElement op> 0 (valOf (intMap.find (ma, guard)))

fun getLongestSleeper ma =
    let
	val sleepMinutesForAll = getSleepMinutesForAll ma
	val totalSleepForAll = intMap.listItemsi (getTotalSleepForAll sleepMinutesForAll)
	val longest = List.foldl (fn ((id1, t1), (id2, t2)) =>
				     if t1 > t2 then (id1, t1) else (id2, t2)) (0, 0) totalSleepForAll
	val sleepestMinute = getSleepestMinute sleepMinutesForAll (#1 longest)
    in
	(longest, sleepestMinute)
    end

fun getFrequentSleepMinute ma =
    intMap.map (getMax op> 0) (getSleepMinutesForAll ma)

fun star1 ma =
    let
	val ((id, _), min) = getLongestSleeper ma
    in
	id * min
    end

fun star2 ma =
    let
	val freqMinuteForAll = getFrequentSleepMinute ma
	fun foldFun (k, (mi, m), (ak, vmi, vm)) =
	    if vm > m
	    then (ak, vmi, vm)
	    else (k, mi, m)
	val (idx, min, _) = intMap.foldli foldFun (0, 0, 0) freqMinuteForAll
    in
	idx * min
    end

fun starsFile file =
    let
	val ma = groupByGuard (orderChronologicaly (getInput file))
    in
	(star1 ma, star2 ma)
    end

fun stars () =
    starsFile "input.txt"

fun starsSmall () =
    starsFile "small_input.txt"
