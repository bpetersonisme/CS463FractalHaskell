{-# LANGUAGE InstanceSigs #-}
-- allows us to put signatures inside our /instances/ (for clarity).
-- P3, completed by Bradley Peterson G00982821 CS 463
module Fractals where


-- any modules you import should go here, e.g. Data.List. 
import Data.List

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Required definitions (provided)

type MaxIter = Int
type Row     = Int
type Col     = Int

data Complex = C Double Double  deriving (Show, Eq)


-- you can create your own instances for Show/Eq if you prefer.
data Mandelbrot = M MaxIter Row Col Complex Complex          deriving (Show, Eq)
data Julia      = J MaxIter Row Col Complex Complex Complex  deriving (Show, Eq)


class (Show f) => Fractal f where
  escapeCount   :: f -> Complex -> MaxIter
  escapes       :: f -> [[MaxIter]]
  escapesString :: f -> String
  toFile        :: f -> FilePath -> IO ()



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Required instances

instance Ord Complex where
  compare :: Complex -> Complex -> Ordering
  compare (C a b) (C c d) 
    | comLength(C a b) > comLength(C c d) = GT 
    | comLength(C a b) < comLength(C c d) = LT 
    | otherwise                           = EQ 

instance Num Complex where 
  (+) :: Complex -> Complex -> Complex  
  (+) (C ra ia) (C rb ib) = C (ra + rb) (ia + ib)
 
  (*) :: Complex -> Complex -> Complex 
  (*) (C ra ia) (C rb ib) = C ((ra * rb) + ((ia * ib) * (-1))) ((ra * ib) + (rb * ia))
 
  abs :: Complex -> Complex
  abs (C r i) = (C (comLength (C r i)) 0)

  signum :: Complex -> Complex
  signum _ = error "no signum for complex numbers!" 

  fromInteger :: Integer -> Complex
  fromInteger r = C (fromIntegral r) 0 

  negate :: Complex -> Complex
  negate (C r i) = C (r * (-1)) (i * (-1))

instance Fractal Mandelbrot where
  escapeCount::Mandelbrot->Complex->MaxIter 
  escapeCount (M max rows cols (C rLow iLow) (C rHigh iHigh)) (C rCoord iCoord) = universalEscapeCount (max) (C 0 0) (C rCoord iCoord) 
  
  escapes::Mandelbrot->[[Int]] 
  escapes (M max rows cols (C rLow iLow) (C rHigh iHigh)) = 
    mandelbrotEscapes 
        (M max rows cols (C rLow iLow) (C rHigh iHigh)) 
        (buildGrid 
            rows cols 
            (C rLow iLow) (C rHigh iHigh)) 
        []

  escapesString :: Mandelbrot->String 
  escapesString curFrac = (labelGridMandelbrot curFrac (showGrid (escapes curFrac)))
  
  toFile :: Mandelbrot -> FilePath -> IO ()
  toFile curFractal target = writeFile target (escapesString curFractal)
    

instance Fractal Julia where
  escapeCount::Julia->Complex->MaxIter 
  escapeCount (J max rows cols (C rLow iLow) (C rHigh iHigh) (C rConst iConst)) (C rCoord iCoord) = 
    universalEscapeCount 
        max 
        (C rCoord iCoord)
        (C rConst iConst) 
  
  escapes::Julia->[[Int]] 
  escapes (J max rows cols (C rLow iLow) (C rHigh iHigh) (C rConst iConst)) = 
    juliaEscapes 
    (J max rows cols (C rLow iLow) (C rHigh iHigh) (C rConst iConst)) 
    (buildGrid 
        rows cols 
        (C rLow iLow) (C rHigh iHigh)) 
    []

  escapesString :: Julia->String 
  escapesString curFrac = (labelGridJulia curFrac (showGrid (escapes curFrac)))
 
  toFile :: Julia->FilePath-> IO ()
  toFile curFractal target = writeFile target (escapesString curFractal)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- required functions

buildGrid :: Row -> Col -> Complex -> Complex -> [[Complex]]
buildGrid nrows ncols (C rLow iLow) (C rHigh iHigh) = 
    buildGridHelper 
        nrows ncols 
        (C rLow iHigh) 
        0 
        (makeDelta rHigh rLow ncols) (makeDelta iHigh iLow nrows) 
        []

showGrid :: [[MaxIter]] -> String
showGrid grid = 
    escapeStringBuilder 
        grid 
        "" 
        (length grid) (length (head grid))
        0 
        (calcMaxSpaces (getHighest grid))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Functions that aren't required, except to hold onto my sanity :)

labelGridMandelbrot :: Mandelbrot -> String -> String 
labelGridMandelbrot (M max rows cols (C rLow iLow) (C rHigh iHigh)) escapeGrid = 
    "mandelbrot " ++ 
    (show max)    ++ " " ++ 
    (show rows)   ++ " " ++ (show cols)  ++ " " ++
    (show rLow)   ++ " " ++ (show iLow)  ++ " " ++
    (show rHigh)  ++ " " ++ (show iHigh) ++ " " ++
    (show 0.0)      ++ " " ++ (show 0.0)     ++ 
    "\n\n" ++ escapeGrid
    
labelGridJulia :: Julia -> String -> String 
labelGridJulia (J max rows cols (C rLow iLow) (C rHigh iHigh) (C rConst iConst)) escapeGrid = 
    "julia "      ++ 
    (show max)    ++ " " ++ 
    (show rows)   ++ " " ++ (show cols)    ++ " " ++
    (show rLow)   ++ " " ++ (show iLow)    ++ " " ++
    (show rHigh)  ++ " " ++ (show iHigh)   ++ " " ++
    (show rConst) ++ " " ++ (show iConst)  ++  
    "\n\n" ++ escapeGrid
    


buildGridHelper :: Row -> Col -> Complex -> Int -> Double -> Double -> [[Complex]] -> [[Complex]] 
buildGridHelper nrows ncols (C rCur iCur) rowIt deltaR deltaI result   
    | rowIt >= nrows = result 
    | otherwise      = 
        buildGridHelper 
            nrows ncols 
            (C rCur (iCur - deltaI))
            (rowIt + 1) 
            deltaR deltaI 
            (result ++ [buildRowHelper ncols (C rCur iCur) 0 deltaR []])


buildRowHelper :: Col -> Complex -> Int -> Double -> [Complex] -> [Complex] 
buildRowHelper ncols (C rCur iCur) colIt deltaR curRow  
    | colIt >= ncols = curRow 
    | otherwise      = 
        buildRowHelper 
            ncols 
            (C (rCur + deltaR) iCur) 
            (colIt + 1) 
            deltaR 
            (curRow ++ [(C rCur iCur)])




-- Given two doubles, returns their "distance" 
comLength :: Complex -> Double 
comLength (C r i) = (((r)**2) + ((i)**2))**(1/2)


--Given Z0 and a Complex constant, returns the number of escapes needed 
universalEscapeCount :: Int->Complex->Complex->Int 
universalEscapeCount max (C r0 i0) (C rc ic) = 
    uECHelper 
        max 
        ((+) ((*) (C r0 i0) (C r0 i0)) (C rc ic)) --Loop starts at Z1  
        (C rc ic) 
        0  


--This gives "looping" functionality with some nice recursion 
uECHelper :: Int->Complex->Complex->Int->Int 
uECHelper max (C rn iN) (C rc ic) numIters 
    | numIters >= max           = max 
    | (abs (C rn iN)) > (C 2 0) = numIters 
    | otherwise                 = uECHelper 
        max 
        ((+) ((*) (C rn iN) (C rn iN)) (C rc ic)) 
        (C rc ic) 
        (numIters + 1) 
        


--These are used to help build the escape grid     
mandelbrotEscapes :: Mandelbrot -> [[Complex]] -> [[Int]] -> [[Int]] 
mandelbrotEscapes curFractal startGrid result 
    | startGrid == [] = result 
    | otherwise       = mandelbrotEscapes 
        curFractal 
        (tail startGrid) 
        (result ++ [mERows curFractal (head startGrid) []]) 

mERows :: Mandelbrot -> [Complex] -> [Int] -> [Int] 
mERows curFractal curRow result 
    | curRow == [] = result 
    | otherwise    = mERows 
        curFractal 
        (tail curRow) 
        (result ++ [(escapeCount curFractal (head curRow))])
    
    
    
juliaEscapes :: Julia -> [[Complex]] -> [[Int]] -> [[Int]] 
juliaEscapes curFractal startGrid result 
    | startGrid == [] = result 
    | otherwise = juliaEscapes 
        curFractal 
        (tail startGrid) 
        (result ++ [jERows curFractal (head startGrid) []]) 

jERows :: Julia -> [Complex] -> [Int] -> [Int] 
jERows curFrac curRow result 
    | curRow == [] = result 
    | otherwise    = jERows 
        curFrac 
        (tail curRow) 
        (result ++ [(escapeCount curFrac (head curRow))])
   
 
--Builds the final string by going row by row 
escapeStringBuilder :: [[Int]] -> String -> Row -> Col -> Int -> Int -> String 
escapeStringBuilder escapeGrid result nrows ncols rowIt maxSpaces 
    
    |  rowIt >= nrows = result 
    | otherwise       = escapeStringBuilder 
        escapeGrid 
        (result ++ (esbRows (escapeGrid !! rowIt) "" ncols 0 maxSpaces)) 
        nrows ncols 
        (rowIt + 1) 
        maxSpaces
    
-- Builds each row string in the escape grid by going through a given row 
esbRows :: [Int] -> String -> Col -> Int -> Int -> String 
esbRows escapeRow result ncols colIt maxSpaces
    | colIt >= ncols = result ++ "\n"
    | otherwise      = esbRows 
        escapeRow 
        (result ++ (makeSpaces maxSpaces (escapeRow !! colIt)) ++ (show (escapeRow !! colIt)) ++ " ") 
        ncols 
        (colIt + 1)
        maxSpaces

--Works out the number of spaces a given number will need behind it to maintain alignment  

makeDelta :: Double -> Double -> Int -> Double 
makeDelta highVal lowVal numSets = (abs (highVal - lowVal)) / (fromIntegral (numSets - 1))
 
-- Given an integer, returns the number of spaces needed for good kerning 
calcMaxSpaces :: Int -> Int 
calcMaxSpaces max = 
    (if (max >= 10) then 
        (1 + (calcMaxSpaces (div max 10))) 
    else  
        (1)
    )
 
--Creates a string equal to the number of spaces indicated 
makeSpaces :: Int -> Int -> String 
makeSpaces colWidth num = spaceLoop (colWidth - (length (show num))) ""

spaceLoop :: Int -> String -> String 
spaceLoop numSp result 
    |numSp == 0 = result 
    |otherwise  = spaceLoop (numSp - 1) (result ++ " ")
    
    
    
--Given a 2D list, returns the highest value in that list 
getHighest :: [[Int]] -> Int 
getHighest grid = highestHelper grid 0

highestHelper :: [[Int]] -> Int -> Int 
highestHelper grid num 
    | grid == [] = num 
    | otherwise  = highestHelper (tail grid) (highestRow (head grid) num)

highestRow :: [Int] -> Int -> Int 
highestRow [] num  = num 
highestRow row num = highestRow (tail row) 
    (if (num > (head row)) 
        then (num) 
        else (head row))



