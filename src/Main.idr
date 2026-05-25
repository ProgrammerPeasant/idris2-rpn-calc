module Main

import Stack
import Env
import Parser
import Data.String
import System

record ReplState where
  constructor MkReplState
  stack : (n : Nat ** Stack n)
  env   : Env

initState : ReplState
initState = MkReplState (0 ** Empty) []

showNum : Double -> String
showNum x =
  let i = the Integer (cast x)
  in if the Double (cast i) == x then show i else show x

showStack : (n : Nat ** Stack n) -> String
showStack (0 ** Empty) = "(пусто)"
showStack (_ ** s)     =
  let items = map showNum (reverse (toList s))
  in joinBy "  " items ++ "  ←"


needsN : Nat -> String
needsN 1 = "нужен минимум 1 элемент на стеке"
needsN n = "нужно минимум " ++ show n ++ " элемента на стеке"

applyBin : (Double -> Double -> Double)
         -> ReplState -> Either String ReplState
applyBin f (MkReplState (_ ** s) env) =
  case s of
    Push y (Push x rest) =>
      Right (MkReplState (_ ** Push (f x y) rest) env)
    _ => Left (needsN 2)

applyCommand : Command -> ReplState -> Either String ReplState
applyCommand (CNum x) (MkReplState (n ** s) env) =
  Right (MkReplState (S n ** Push x s) env)

applyCommand CAdd st = applyBin (+) st
applyCommand CSub st = applyBin (-) st
applyCommand CMul st = applyBin (*) st
applyCommand CDiv st = applyBin (/) st

applyCommand CNeg (MkReplState (_ ** s) env) =
  case s of
    Push x rest => Right (MkReplState (_ ** Push (negate x) rest) env)
    _           => Left (needsN 1)

applyCommand CDup (MkReplState (_ ** s) env) =
  case s of
    Push x rest => Right (MkReplState (_ ** Push x (Push x rest)) env)
    _           => Left (needsN 1)

applyCommand CDrop (MkReplState (_ ** s) env) =
  case s of
    Push _ rest => Right (MkReplState (_ ** rest) env)
    _           => Left (needsN 1)

applyCommand CSwap (MkReplState (_ ** s) env) =
  case s of
    Push x (Push y rest) => Right (MkReplState (_ ** Push y (Push x rest)) env)
    _                    => Left (needsN 2)

applyCommand (CLet name) (MkReplState ds env) =
  case ds of
    (_ ** Push x _) => Right (MkReplState ds (envInsert name x env))
    _               => Left (needsN 1)

applyCommand (CVar name) (MkReplState (n ** s) env) =
  case envLookup name env of
    Just x  => Right (MkReplState (S n ** Push x s) env)
    Nothing => Left ("переменная не найдена: " ++ name)

applyCommand CClear _ = Right initState

applyCommand CDump  st = Right st
applyCommand CVars  st = Right st
applyCommand CHelp  st = Right st
applyCommand CQuit  st = Right st


printHelp : IO ()
printHelp = putStrLn """
  Команды:
    <число>        - положить на стек (напр. 3.14)
    +  -  *  /     - бинарная арифметика (RPN: 3 4 - = -1)
    neg            - сменить знак вершины
    dup            - дублировать вершину
    drop           - удалить вершину
    swap           - поменять местами два верхних
    let <имя>      - сохранить вершину в переменную
    $<имя>         - загрузить переменную на стек
    dump           - показать стек
    vars           - показать переменные
    clear          - очистить стек и переменные
    quit / exit    - выход
"""

printVars : Env -> IO ()
printVars [] = putStrLn "  (нет переменных)"
printVars vs = traverse_ (\(n, v) => putStrLn ("  " ++ n ++ " = " ++ showNum v)) vs


replLoop : ReplState -> IO ()
replLoop state = do
  putStr "rpn> "
  line <- getLine
  let trimmed = trim line
  if trimmed == ""
    then replLoop state
    else case parseCommand trimmed of
      Nothing  => do
        putStrLn "  неизвестная команда (введите help)"
        replLoop state
      Just CQuit => putStrLn "  пока!"
      Just CHelp => do printHelp; replLoop state
      Just CDump => do
        putStrLn ("  стек: " ++ showStack state.stack)
        replLoop state
      Just CVars => do
        printVars state.env
        replLoop state
      Just cmd   =>
        case applyCommand cmd state of
          Left err     => do
            putStrLn ("  ошибка: " ++ err)
            replLoop state
          Right state' => do
            putStrLn ("  стек: " ++ showStack state'.stack)
            replLoop state'

main : IO ()
main = do
  putStrLn "rpn-calc  (RPN-калькулятор с типизированным стеком)"
  putStrLn "  help - справка,  quit - выход\n"
  replLoop initState
