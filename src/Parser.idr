module Parser

import Data.String

public export
data Command
  = CNum Double
  | CAdd | CSub | CMul | CDiv
  | CNeg | CDup | CDrop | CSwap
  | CLet String
  | CVar String
  | CDump | CVars | CClear | CHelp | CQuit

public export
parseCommand : String -> Maybe Command
parseCommand s =
  case words (trim s) of
    ["+"]      => Just CAdd
    ["-"]      => Just CSub
    ["*"]      => Just CMul
    ["/"]      => Just CDiv
    ["neg"]    => Just CNeg
    ["dup"]    => Just CDup
    ["drop"]   => Just CDrop
    ["swap"]   => Just CSwap
    ["dump"]   => Just CDump
    ["vars"]   => Just CVars
    ["clear"]  => Just CClear
    ["help"]   => Just CHelp
    ["quit"]   => Just CQuit
    ["exit"]   => Just CQuit
    ["let", n] => if n /= "" then Just (CLet n) else Nothing
    [w]        =>
      case unpack w of
        ('$' :: rest) => let name = pack rest
                         in if name /= "" then Just (CVar name) else Nothing
        _             => map CNum (Data.String.parseDouble w)
    _          => Nothing
