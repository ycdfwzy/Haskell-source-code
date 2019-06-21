module SimpleREPL where

import AST
import SimpleParser
import qualified EvalType as EType
import qualified EvalValue as EValue
import qualified Data.Map.Strict as Map
import System.IO
import Control.Monad.State
import Control.Monad

data Global = Global {
    ctxT :: Map.Map String Type,
    ctxV :: Map.Map String EValue.Value
                    }
    deriving (Show, Eq)

type GlobalContext a = State Global a

myRead :: IO String
myRead = putStr "REPL> "
    >> hFlush stdout
    >> getLine

myWrite :: String -> IO ()
myWrite str = if (null str) then putStr str else putStrLn str

myEval :: String -> GlobalContext String
myEval input = do
    global <- get
    case runParser parseBind input of
        ([], Just (s, e)) -> case EType.evalType2 (ctxT global) (Program [] e) of
                                Just t -> case EValue.evalProgram2 (ctxV global) (Program [] e) of
                                            Just v -> do
                                                        put $ Global { ctxT = Map.insert s t (ctxT global), ctxV = Map.insert s v (ctxV global) }
                                                        return []
                                            _      -> return "Failed! Please check your grammar!"
                                _      -> return "Failed! Please check your grammar!"
        (s, Just _)       -> return $ "Unknown token \"" ++ s ++ "\"!"
        _                 ->  case runParser parseGetType input of
                                ([], Just p) -> case EType.evalType2 (ctxT global) p of
                                                    Just x -> return $ show x
                                                    _      -> return $ "Failed! Please check your grammar!"
                                (s, Just _)  -> return $ "Unknown token \"" ++ s ++ "\"!"
                                _            -> case runParser parseProgram input of
                                                    ([], Just p) -> return (show $ EValue.evalValue2 (ctxV global) p)
                                                    (s, Just _)  -> return $ "Unknown token \"" ++ s ++ "\"!"
                                                    _            -> return $ input ++ "\nFailed! Please check your grammar!"

loop :: Global -> IO()
loop global = do
    input <- myRead
    unless (input == ":q") $
        let res = myEval input
            newGlobal = execState res global
            output = evalState res global
        in myWrite output >> loop newGlobal

repl :: IO()
repl = loop $ Global { ctxT=Map.empty, ctxV=Map.empty }