{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive hiding ((.))
import ITMOPrelude.List
import ITMOPrelude.Categories

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat }

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State getNat' 
	where getNat' world = (world', value)
		where world' = RealWorld (tail . stdIn $ world) (stdOut world) (exitCode world)
		      value = head . stdIn $ world

putNat :: Nat -> IO ()
putNat n = State putNat' 
	where putNat' world = (world', ())
		where world' = RealWorld (stdIn world) (Cons n $ stdOut world) (exitCode world)

setExitCode :: Nat -> IO ()
setExitCode n = State setExitCode' 
	where setExitCode' world = (world', ())
		where world' = RealWorld (stdIn world) (stdOut world) n 
