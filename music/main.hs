import Data.List

maxTone = 12
maxPosition = 7
err = -1;

notes = [("C",0,0), ("Cb",0,11),("C#",0,1), ("Cbb",0,10), ("C##",0,2)
        ,("D",1,2), ("Db",1,1), ("D#",1,3), ("Dbb",1,0) , ("D##",1,4)
		,("E",2,4), ("Eb",2,3), ("E#",2,5), ("Ebb",2,2) , ("E##",2,6)
		,("F",3,5), ("Fb",3,4), ("F#",3,6), ("Fbb",3,3) , ("F##",3,7)
		,("G",4,7), ("Gb",4,6), ("G#",4,8), ("Gbb",4,5) , ("G##",4,9)
		,("A",5,9), ("Ab",5,8), ("A#",5,10),("Abb",5,7) , ("A##",5,11)
		,("B",6,11),("Bb",6,10),("B#",6,12),("Bbb",6,9) , ("B##",6,1)
		]
		
dur_intervals  = [2,2,1,2,2,2,1]
moll_intervals = [2,1,2,2,1,2,2]


-- ********* Harmonization *********
-- Music Theory: harmonization of major/minor scales with Triads, Seventh chords etc
-- http://en.wikipedia.org/wiki/Harmonization 

-- Алгоритм построения аккорда в тональности от тоники: 
-- 0. Проверяем принадлежит ли тоника заданной тональности, если нет, то halt
-- 1. определяем ступень тоники: 0 для С, 3 для F etc
-- 2. строим n-1 терций от тоники: ступени [0,2,4] для С, n = 3; ступени [3,5,0,2] для F, n = 4
-- 3. Выбираем из нот в тональности соответствующие ступени:
--		Тональность F-moll состоит из ["F"<3>,"G"<4>,"Ab<5>","Bb<6>","C<0>","Db<1>","Eb<2>"]
--      для тоники G, n = 3 ступени равны [4,6,1]. В тональности F-moll на этих ступенях стоят [G,Bb,Db]

harmonizeTonality :: [Char] -> Bool -> Integer -> [[([Char])]]
harmonizeTonality tonalityName isDur count = 
    map (map frst) (harmonizeTonality' tonalityName isDur count)
	
--  with note level and tone  
harmonizeTonality' :: [Char] -> Bool -> Integer -> [[([Char], Integer, Integer)]]
harmonizeTonality' tonalityName isDur count = 
	map (\tonic -> buildAccord tonic tonalityName isDur count) (buildTonality tonalityName isDur)

buildAccord ::[Char] -> [Char]-> Bool -> Integer -> [([Char], Integer, Integer)]
buildAccord tonicName tonalityName isDur count 
	| null [(note) | note <- notes, frst note == tonicName] = [("Bad input, Unrecognised note", err, -2)]
	| length tonalityName > 2 = [("No Doubles allowed, Use simple enharmonic note for tonality", err, -10)]
	| not (tonicName `elem` (buildTonality tonalityName isDur)) = [("Accord's Tonic not in Tonality", err, -1)]
	| otherwise = map (findNoteByLevelInTonality tonality) levelsList
        where tonality = buildTonality' tonalityName isDur
              tonicNote = searchNoteByName tonicName
              tonicLevel = snd3 tonicNote
              levelsList = map transposePosition [tonicLevel, tonicLevel + 2.. tonicLevel + 2 * count - 1]
			  
			  
findNoteByLevelInTonality :: [([Char], Integer, Integer)] -> Integer -> ([Char], Integer, Integer)
findNoteByLevelInTonality tonality level  = [n| n <- tonality, (snd3 n) == level] !! 0

-- ********* Tonality functions *********

--  Sample :
--  Main> buildTonality "A" True
--   ["A","B","C#","D","E","F#","G#"]
--  Main> buildTonality"Afdg" True
--   ["Bad input"]

buildTonality :: [Char] -> Bool -> [[Char]]
buildTonality noteName isDur = map frst (buildTonality' noteName isDur)

--  Sample:
--  Main> buildTonality' "C" True
--   [("C",0,0),("D",1,2),("E",2,4),("F",3,5),("G",4,7),("A",5,9),("B",6,11)]

--  with note level and tone 
buildTonality' :: [Char] -> Bool -> [([Char], Integer, Integer)]
buildTonality' noteName isDur 
    | null [(note) | note <- notes, frst note == noteName] = [("Bad input", err, -2)]
	| length noteName > 2 = [("No Doubles allowed, Use simple enharmonic note for tonality", err, -10)]
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

-- **************************************************************************
-- key function 1. Generates list of Enharmonic notes C# ~ Db 
-- TODO: remove duplicates, fixme
       
findEnharmonicNotes:: [([Char], Integer, Integer)] -> [([Char], [Char])]
findEnharmonicNotes list = [(frst a, frst b) | a <- list, b <-list, last3 a == last3 b, frst a /= frst b]
