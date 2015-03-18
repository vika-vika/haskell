import Data.List

maxTone = 12
maxPosition = 7

notes = [("C",0,0), ("Cb",0,11),("C#",0,1),("D",1,2),("Db",1,1),("D#",1,3),("E",2,4),("Eb",2,3),("E#",2,5),("F",3,5), ("Fb",3,4), ("F#",3,6), ("G",4,7),("Gb",4,6),("G#",4,8), ("A",5,9),("Ab",5,8),("A#",5,10),("B",6,11),("Bb",6,10),("B#",6,12)]
dur_intervals  = [2,2,1,2,2,2,1]
moll_intervals = [2,1,2,2,1,2,2]


-- ********* Harmonization *********
-- Music Theory: harmonization of major/minor scales with Triads, Seventh chords etc
-- http://en.wikipedia.org/wiki/Harmonization 

-- Алгоритм построения аккорда в тональности от тоники: 
-- 1. определяем ступень тоники: 0 для С, 3 для F etc
-- 2. строим n терций от тоники: ступени [0,2,4] для С, n = 3; ступени [3,5,0,2] для F, n = 4
-- 3. Выбираем из нот в тональности соответствующие ступени:
--		Тональность F-moll состоит из ["F"<3>,"G"<4>,"Ab<5>","Bb<6>","C<0>","Db<1>","Eb<2>"]
--      для тоники G, n = 3 ступени равны [4,6,1]. В тональности F-moll на этих ступенях стоят [G,Bb,Db]
       

findNoteByLevelTonality :: [([Char], Integer, Integer)] -> Integer -> [([Char], Integer, Integer)]
findNoteByLevelTonality tonality level  = [n| n <- tonality, (snd3 n) == level] -- make me lambda, sweety

buildAccordLevels :: [Char] -> Integer -> [Integer]
buildAccordLevels noteName count = 
	let start = (snd3 (searchNoteByName noteName))
	in map transposePosition [start, start + 2.. start + 2 * count - 1]
	
generateAccord ::[Char] -> [Char] -> Integer -> [[([Char], Integer, Integer)]] 
generateAccord note tonic count =
	let tonality = buildTonality tonic True
	in map (findNoteByLevelTonality tonality) (buildAccordLevels note count)
 
-- ********* Tonality functions *********

--  Sample :
--  Main> buildTonalityShort "A" True
--   ["A","B","C#","D","E","F#","G#"]
--  Main> buildTonalityShort "Afdg" True
--   ["Bad input"]

buildTonalityShort :: [Char] -> Bool -> [[Char]]
buildTonalityShort noteName isDur = map frst (buildTonality noteName isDur)

--  Sample:
--  Main> buildTonality "C" True
--   [("C",0,0),("D",1,2),("E",2,4),("F",3,5),("G",4,7),("A",5,9),("B",6,11)]

buildTonality :: [Char] -> Bool -> [([Char], Integer, Integer)]
buildTonality noteName isDur 
    | null [(note) | note <- notes, frst note == noteName] = [("Bad input", 0, 0)]
	| otherwise = map searchNoteByCharacteristic (buildRowNotesDataSeq (searchNoteByName noteName) isDur)
		
		
buildRowNotesDataSeq ::  ([Char], Integer, Integer)  -> Bool -> [(Integer, Integer)] -- 
buildRowNotesDataSeq note isDur =  
	let intervals  
		| isDur = dur_intervals
		| otherwise = moll_intervals
	in  zip (getLevelValues (snd3 note)) (getToneValues (last3 note) intervals)

-- ********* generate tones and levels lists for Tonality *********

getToneValues :: Integer -> [Integer] -> [Integer]
getToneValues num list = map transposeTone (summ num list)

getLevelValues :: Integer -> [Integer]
getLevelValues num = map transposePosition (getLevelValuesRaw num) -- out ex: [5,6,0,1,2,3,4]

getLevelValuesRaw :: Integer -> [Integer]   
getLevelValuesRaw num =  [num, num + 1.. num + maxPosition - 1]    -- out ex: [5,6,7,8,9,10,11]

-- ********* TRANPOSING TONES and LEVELS *********
-- tone 14 ~ 2, level 8 ~ 1

transposePosition :: Integer -> Integer
transposePosition position = transpose' position maxPosition
	
transposeTone :: Integer -> Integer
transposeTone tone = transpose' tone maxTone
	
transpose' :: Integer -> Integer -> Integer
transpose' x value
	| x < 0 = x + value
	| x < value = x
	| otherwise = x `mod` value

-- ********* NOTES LIST SEARCH *********
searchNoteByName :: [Char] -> ([Char], Integer, Integer)
searchNoteByName name = [a | a <- notes, (frst a == name)] !! 0

searchNoteByCharacteristic :: (Integer, Integer) -> ([Char], Integer, Integer)
searchNoteByCharacteristic chars = [note | note <- notes, (fst chars) == (snd3 note), (snd chars) == (last3 note)] !! 0

-- ********* LIST UTILS *********
	
summ :: Integer -> [Integer] -> [Integer]
summ num [] = [num]
summ num (x:xs) = num : summ (num + x) xs
	
frst :: (a, b, c) -> a
frst (a,_,_) = a

snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b

last3 :: (a, b, c) -> c
last3 (_,_,c) = c

-- *********
-- key function 1. Generates list of Enharmonic notes C# ~ Db 
-- TODO: remove duplicates, fixme

findEnharmonicNotes:: [([Char], Integer, Integer)] -> [([Char], [Char])]
findEnharmonicNotes list = [(frst a, frst b) | a <- list, b <-list, last3 a == last3 b, frst a /= frst b]