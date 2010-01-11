-- A port of the SFont library for use with HSSDL
-- Liam O'Connor-Davis (liamoc@cse.unsw.edu.au)
-- Released under the BSD3 license

module Graphics.UI.SDL.SFont 
   ( SFont
   , initFont
   , write
   , writeChar
   , textWidth
   , textHeight
   ) where

import Graphics.UI.SDL
import Graphics.UI.SDL.Sprig
import Data.Array
import Data.List (groupBy)
import Data.Char (ord)

data SFont = SFont { surface :: Surface 
                   , charPos :: Array Int Int
                   } 

-- | Load a font from an SDL surface
initFont :: Surface -> IO SFont
initFont surface = do pinks <- let nums = [0..surfaceGetWidth surface] 
                               in sequence (map isPink nums) >>= return . zip nums 
                      let pos = ( map fst $ 
                                  map head $ 
                                  groupBy (\a b -> snd a == snd b) $ 
                                  pinks ) ++ [surfaceGetWidth surface]
                       in return $ SFont surface $ listArray (0,length pos) pos
  where                   
    isPink x = do pink <- mapRGB (surfaceGetPixelFormat surface) 255 0 255
                  color <- getPixel surface x 0
                  return $ pink == color

-- | Write the specified text at the given coordinates on the surface
write :: Surface -> SFont -> (Int, Int) -> String -> IO ()
write surf sfont (x, y) str = let xpos = map (+x) $ scanl (+) 0 $ map (charWidth sfont) str 
                                  ypos = repeat y
                                  crds = zip xpos ypos
                                  done = zip crds str
                              in sequence (map (uncurry $ writeChar surf sfont) done) >> return ()

writeChar :: Surface -> SFont -> (Int,Int) -> Char -> IO ()
writeChar dest font@(SFont src charPos) (x,y) c = let offset = (ord c - 33) * 2 + 1                       
                                                  in if all (inRange $ bounds charPos) [offset, offset+1] 
                                                     then let cWidth = charWidth font c
                                                              cHeight = textHeight font
                                                              srcRect = Rect (charPos ! offset) 0 cWidth cHeight
                                                              dstRect = Rect x y cWidth cHeight
                                                           in blit src (Just srcRect) dest (Just dstRect) >> return ()
                                                     else return ()
charWidth :: SFont -> Char -> Int
charWidth (SFont _ charPos) c = let offset = (ord c - 33) * 2 + 1
                                in if all (inRange $ bounds charPos) [offset, offset+1]
                                   then (charPos ! (offset + 1)) - (charPos ! offset)
                                   else (charPos ! 1) - (charPos ! 0)

-- | Returns the width in pixels of a given string rendered in the given font
textWidth :: SFont -> String -> Int
textWidth sfont = sum . map (charWidth sfont) 

-- | Returns the height of the font.
textHeight :: SFont -> Int
textHeight (SFont surf _ ) = surfaceGetHeight surf - 1

