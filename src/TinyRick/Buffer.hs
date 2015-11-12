{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TinyRick.Buffer where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Monoid
import Data.String
import Data.Foldable
-- import Data.Maybe
import Data.List (findIndex)
-- import Debug.Trace
-- import Text.Printf

instance IsString (Seq Char) where
  fromString = Seq.fromList

seqReplace :: (Int, Int) -> Seq a -> Seq a -> Seq a
seqReplace (start, end) xs original = left <> xs <> right
  where
    left  = Seq.take start original
    right = Seq.drop end original

seqRange :: (Int, Int) -> Seq a -> Seq a
seqRange (start, end) = Seq.drop start . Seq.take end

data Buffer = Buffer 
  { bufSelection :: !(Int, Int)
  , bufText      :: !(Seq Char)
  , bufPath      :: !FilePath
  } deriving Show

newBuffer :: Buffer
newBuffer = Buffer 
  { bufSelection = (0,0)
  , bufText = ""
  , bufPath = ""
  }

bufferFromString :: FilePath -> String -> Buffer
bufferFromString filePath string = Buffer
  { bufSelection = (0,0)
  , bufText = fromString string
  , bufPath = filePath
  }

stringFromBuffer :: Buffer -> String
stringFromBuffer = toList . bufText

selectionFromBuffer :: Buffer -> String
selectionFromBuffer (Buffer selection text _) = toList (seqRange selection text)

insertBuffer :: Seq Char -> Buffer -> Buffer
insertBuffer chars buffer@(Buffer (start, end) text _) = 
  buffer { bufSelection = (newCursor, newCursor), bufText = newText }
  where 
    newText = seqReplace (start, end) chars text
    newCursor = (start + Seq.length chars)

insertChar :: Char -> Buffer -> Buffer
insertChar char = insertString [char]

insertString :: String -> Buffer -> Buffer
insertString string = insert (fromString string)

insert :: Seq Char -> Buffer -> Buffer
insert chars = insertBuffer chars

moveLeft :: Buffer -> Buffer
moveLeft = go
  where
    go buffer@(Buffer (0, 0) _ _) = buffer
    go buffer@(Buffer (start, end) _ _) 
      | start == end = buffer { bufSelection = (start - 1, start - 1) }
    go buffer@(Buffer (start, _) _ _) = buffer { bufSelection = (start, start) }

selectLeft :: Buffer -> Buffer
selectLeft = go
  where
    go buffer@(Buffer (0,       _) _ _) = buffer
    go buffer@(Buffer (start, end) _ _) = buffer { bufSelection = (start - 1, end) }

moveRight :: Buffer -> Buffer
moveRight = go
  where
    go buffer@(Buffer (start, end) text _) 
      | end == Seq.length text = buffer
      | start == end = buffer { bufSelection = (end + 1, end + 1) }
    go buffer@(Buffer (_, end) _ _) = buffer { bufSelection = (end, end) }


selectRight :: Buffer -> Buffer
selectRight = go
  where
    go buffer@(Buffer (_, end) text _)
      | end == Seq.length text = buffer
    go buffer@(Buffer (start, end) _ _) = buffer { bufSelection = (start, end + 1) }

backspace :: Buffer -> Buffer
backspace buffer = 
  let (start, end) = bufSelection buffer
  in insert "" (if start == end then selectLeft buffer else buffer)

moveToEnd :: Buffer -> Buffer
moveToEnd buffer =
  let end = Seq.length (bufText buffer)
  in buffer { bufSelection = (end, end) }

moveToBeginning :: Buffer -> Buffer
moveToBeginning buffer = 
  buffer { bufSelection = (0, 0) }

moveDown :: Buffer -> Buffer
moveDown buffer = 
  let (cursorLocation, _)     = bufSelection buffer
      -- Add an artificial "newline" at -1 to represent the beginning of the document
      lineLocations           = (-1):Seq.elemIndicesL '\n' (bufText buffer)
  -- If there's no newline beyond the cursor, do nothing
  in case findIndex (>= cursorLocation) lineLocations of
      Nothing -> buffer
      Just nextLineIndex -> 
        -- Thanks to the (-1) fake newline, we can always count on nextLineIndex begin at least 1
        let currentLineLocation     = lineLocations !! (nextLineIndex - 1)
            nextLineLocation        = lineLocations !! nextLineIndex
            nextNextLineLocation    = lineLocations !! (min (length lineLocations - 1) $ nextLineIndex + 1)
            currentDistanceFromLeft = cursorLocation - currentLineLocation
            -- Don't jump futher than the next newline location
            newCursor               = min nextNextLineLocation (nextLineLocation + currentDistanceFromLeft)
        in buffer { bufSelection = (newCursor, newCursor) }


moveUp :: Buffer -> Buffer
moveUp buffer =
  let (cursorLocation, _)     = bufSelection buffer
      lineLocations           = (-1):(-1):Seq.elemIndicesL '\n' (bufText buffer)
  -- If there's no newline beyond the cursor, do nothing
  in case findIndex (>= cursorLocation) lineLocations of
      Nothing -> buffer
      Just nextLineIndex -> 
        let currentLineLocation     = lineLocations !! (nextLineIndex - 1)
            prevLineLocation        = lineLocations !! (nextLineIndex - 2)
            currentDistanceFromLeft = cursorLocation - currentLineLocation
            -- Don't jump futher than the prev newline location
            newCursor               = max 0 $ min currentLineLocation (prevLineLocation + currentDistanceFromLeft)
        in buffer { bufSelection = (newCursor, newCursor) }

-- main = do
--   flip runStateT newBuffer $ do
--     insert "hello"
--     insert " there"
--     insert "\n"
--     moveLeft
--     moveLeft
--     insert "mr."
--     insert "magpie"
--     selectLeft
--     selectLeft
--     insert ""
--     liftIO . print =<< get
