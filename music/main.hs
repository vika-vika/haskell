import Data.List

maxTone = 12
maxPosition = 7

notes = [("C",0,0), ("Cb",0,11),("C#",0,1),("D",1,2),("Db",1,1),("D#",1,3),("E",2,4),("Eb",2,3),("E#",2,5),("F",3,5), ("Fb",3,4), ("F#",3,6), ("G",4,7),("Gb",4,6),("G#",4,8), ("A",5,9),("Ab",5,8),("A#",5,10),("B",6,11),("Bb",6,10),("B#",6,12)]
dur_intervals  = [2,2,1,2,2,2,1]
moll_intervals = [2,1,2,2,1,2,2]
 
-- key function 1. Generates list of Enharmonic notes C# ~ Db 
-- TODO: remove duplicates

findEnharmonicNotes:: [([Char], Integer, Integer)] -> [([Char], [Char])]
findEnharmonicNotes list = [(frst a, frst b) | a <- list, b <-list, last3 a == last3 b, frst a /= frst b]

-- get Tonality functions

getMollTonality :: [Char] -> [([Char], Integer, Integer)]
getMollTonality noteName = map searchNoteByCharacteristic (getMollRowValues (searchNoteByName noteName))

getDurTonality :: [Char] -> [([Char], Integer, Integer)]
getDurTonality noteName = map searchNoteByCharacteristic (getDurRowValues (searchNoteByName noteName))

getDurRowValues :: ([Char], Integer, Integer) -> [(Integer, Integer)]
getDurRowValues note =  zip  (getLevelValuesTransposed (snd3 note)) (getToneValues (last3 note) dur_intervals)

getMollRowValues :: ([Char], Integer, Integer) -> [(Integer, Integer)]
getMollRowValues note =  zip  (getLevelValuesTransposed (snd3 note)) (getToneValues (last3 note) moll_intervals)

-- generate tones and levels lists for Tonality

getToneValues :: Integer -> [Integer] -> [Integer]
getToneValues num list = summ num list

getLevelValues :: Integer -> [Integer]
getLevelValues num =  [num, num + 1.. num + maxPosition - 1]

getLevelValuesTransposed :: Integer -> [Integer]
getLevelValuesTransposed num = map transposePosition (getLevelValues num)

-- TRANPOSING TONES and LEVELS
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

-- LIST SEARCH
searchNoteByName :: [Char] -> ([Char], Integer, Integer)
searchNoteByName name = [a | a <- notes, (frst a == name)] !! 0

searchNoteByCharacteristic :: (Integer, Integer) -> ([Char], Integer, Integer)
searchNoteByCharacteristic chars = [note | note <- notes, (fst chars) == (snd3 note), (snd chars) == (last3 note)] !! 0

-- LIST UTILS
	
summ :: Integer -> [Integer] -> [Integer]
summ num [] = [num]
summ num (x:xs) = num : summ (transposeTone (num + x)) xs
	
frst :: (a, b, c) -> a
frst (a,_,_) = a

snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b

last3 :: (a, b, c) -> c
last3 (_,_,c) = c