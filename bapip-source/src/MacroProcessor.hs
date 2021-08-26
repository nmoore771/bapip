{-# OPTIONS_GHC -fno-warn-tabs #-}

module MacroProcessor where 

import System.Environment
import System.IO
import System.Directory
import Data.List
import Data.Char
import Debug.Trace

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Perm
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Prim as Q

import LexerTypes 

runBSVMacros :: FileContents -> [BSVMacro] -> FileContents
runBSVMacros = foldl runBSVMacro

runBSVMacro :: FileContents -> BSVMacro -> FileContents
runBSVMacro [] _ = []
runBSVMacro (f:fs) (SimpleMacro fnd repl) = if (f == '`' ) && (fnd `isPrefixOf` (fs)) 
      then repl ++ runBSVMacro (drop (length fnd) (fs)) (SimpleMacro fnd repl)
      else f : (runBSVMacro fs (SimpleMacro fnd repl))
runBSVMacro (f:fs) (CompoundMacro fnd args repl) = if (f == '`' ) && (fnd `isPrefixOf` (fs)) 
      then repl' ++ runBSVMacro restOfFile (CompoundMacro fnd args repl)
      else f : (runBSVMacro fs (CompoundMacro fnd args repl))
  where 
    argReplacements = deintercalateWithTerminator startingAtArgs ',' ')' 
    startingAtArgs = tail $ drop (length fnd) fs
    restOfFile = tail $ dropWhile (\ z -> not (z == ')')) fs 
    replacements = zip args argReplacements
    repl' = applyReplacements repl replacements 

-- File Contents -> intercalation character -> terminator -> The stuff between
deintercalateWithTerminator :: String -> Char -> Char -> [String]
deintercalateWithTerminator (xs) inter termin  = y : ys
  where
    breaker = (break (\ z -> (z == inter) || (z == termin)) xs)
    y = fst breaker
    ys = if (head (snd breaker) == termin) 
     then []
     else deintercalateWithTerminator (tail (snd breaker)) inter termin

applyReplacements :: String -> [(String, String)] -> String
applyReplacements = foldl applyReplacement

applyReplacement :: String -> (String, String) -> String
applyReplacement [] _ = []
applyReplacement (x:xs) (fnd, repl) = if (fnd `isPrefixOf` (x:xs)) 
	   then repl ++ applyReplacement (drop (length fnd) (x:xs)) (fnd, repl)
	   else applyReplacement (xs) (fnd, repl)
     
removeIncludes :: FileContents -> FileContents
removeIncludes [] = []
removeIncludes (x:xs) = if ("`include" `isPrefixOf` (x:xs) )
      then removeIncludes (dropWhile (\ z -> not (z == '\n')) (x:xs))
      else x : (removeIncludes xs)

removeDefines :: FileContents -> FileContents
removeDefines [] = []
removeDefines (x:xs) = if ("`define" `isPrefixOf` (x:xs) )
      then removeDefines (dropWhile (\ z -> not (z == '\n')) (x:xs))
      else x : (removeDefines xs)      
      
findIncludes :: FileContents -> [String]
findIncludes [] = [] 
findIncludes (x:xs) = if ("`include" `isPrefixOf` (x:xs) ) 
  then (cleaned) : (findIncludes xs)
  else findIncludes xs 
  where 
    raw = drop (length "`include") (takeWhile (\ z -> z /= '\n') (x:xs))
    cleaned = [ x | x <- raw, not (x `elem` " \"")]
    
findLocalDefines :: FileContents -> [BSVMacro]
findLocalDefines [] = [] 
findLocalDefines (x:xs) = if ("`define" `isPrefixOf` (x:xs) ) 
  then (cleaned) : (findLocalDefines xs)
  else findLocalDefines xs 
  where
    raw = drop (length "`define") (takeWhile (\ z -> z /= '\n') (x:xs))
    raw' = dropAllComments raw
    raw'' = words raw'
    cleaned = (SimpleMacro (head raw'') (last raw''))

    
dropAllComments :: String -> String
dropAllComments [] = []
dropAllComments ('/':'/':xs) = inComment xs 
dropAllComments ('/':'*':xs) = inMultiComment xs
dropAllComments (x:xs) = x : dropAllComments xs    

inComment :: String -> String
inComment ('\n':xs) = '\n' : (dropAllComments xs)
inComment (_:xs) = inComment xs
inComment [] = []

inMultiComment :: String -> String
inMultiComment ('\n':xs) = '\n' : (inMultiComment xs)
inMultiComment ('*':'/':xs) = dropAllComments xs
inMultiComment (_:xs) = inMultiComment xs
inMultiComment [] = []    
    
    
-- dropAllComments :: String -> String 
-- dropAllComments xs = unlines $ map dropComment $ lines $ dropMultilineComments xs
-- 
-- dropMultilineComments :: String -> String 
-- dropMultilineComments [] = []
-- dropMultilineComments xs = if ("/*" `isPrefixOf` xs)
--                               then trace ("[T] - found /* ") $ dropWhileNotPrefix "*/" xs
--                               else (head xs) : (dropMultilineComments (tail xs))
-- 
-- dropWhileNotPrefix :: String -> String -> String 
-- dropWhileNotPrefix _ [] = []
-- dropWhileNotPrefix test xs 
--  | test `isPrefixOf` xs = drop (length test) xs
--  | otherwise            = dropWhileNotPrefix test $ tail xs 
--                               
-- dropComment :: String -> String 
-- dropComment [] = []
-- dropComment (x:[]) = (x:[])
-- dropComment (x1:x2:xs) = if (x1 == '/' && x2 == '/') 
--                             then [] 
--                             else x1 : (dropComment (x2:xs))

  
-------------------------------------------------------------DEFINITION FILE PARSER--------------------------------------------------------------------------

lexer = P.makeTokenParser bsvDef
bsvDef :: LanguageDef st
bsvDef = LanguageDef 
  { commentStart = "/*"
  , commentEnd = "*/"
  , commentLine = "//"
  , identStart = oneOf [x | x <- ['$' .. 'z'], isAlphaNum x || x `elem` ['_','$'] ]
  , identLetter = oneOf [x | x <- ['$' .. 'z'], isAlphaNum x || x `elem` ['_','$'] ]
  , reservedNames = [
{- Reserved BSV keywords (78) -}
  "Action", "ActionValue", "BVI", "C", "CF", "E", "SB", "SBR", "action", "endaction", "actionvalue", "endactionvalue", "ancestor", "begin", "bit", "case", "endcase", "clocked_by", "default", "default_clock", "default_reset", "dependencies", "deriving", "determines", "e", "else", "enable", "end", "enum", "export", "for", "function", "endfunction", "if", "ifc_inout", "import", "inout", "input_clock", "input_reset", "instance", "endinstance", "interface", "endinterface", "let", "match", "matches", "method", "endmethod", "module", "endmodule", "numeric", "output_clock", "output_reset", "package", "endpackage", "parameter", "path", "port", "provisos", "reset_by", "return", "rule", "endrule", "rules", "endrules", "same_family", "schedule", "struct", "tagged", "type", "typeclass", "endtypeclass", "typedef", "union", "valueOf", "valueof", "void", "while", 
{- Reserved attribute identifiers -}
	{- Module attributes-} "synthesize", "always_ready", "noinline", "always_enabled", "descending_urgency", "execution_order", "mutually_exclusive", "conflict_free", "preempts", "clock_prefix", "gate_prefix", "reset_prefix", "gate_input_clocks", "gate_all_clocks", "default_clock_osc", "default_clock_gate", "default_gate_inhigh", "default_gate_unused", "default_reset", "no_default_reset", "clock_family", "clock_ancestors", "doc",
	{- Interface attributes-} -- no unique
	{- Argument attributes-} "ready", "enable", "result", "prefix", "port",
	{- Method Declaration attributes-} -- no unique
	{- Action Declaration attributes-} -- no unique
	{- Rule Declaration attributes-} "fire_when_enalbed", "no_implicit_conditions",
	{- Method Body Declaration attributes-} -- no unique
	{- Statement attributes-} "split", "nosplit",
{- Reserved SystemVerilog keywords (disallowed identifiers, excluding BSV keywords) (183) -}
  "alias", "always", "always_comb", "always_ff", "always_latch", "and", "assert", "assert_strobe", "assign", "assume", "automatic", "before", "bind", "bins", "binsof", "break", "buf", "bufif0", "bufif1", "byte", "casex", "casez", "cell", "chandle", "class", "endclass", "clocking", "endclocking", "cmos", "config", "endconfig", "const", "constraint", "context", "continue", "cover", "covergroup", "endgroup", "coverpoint", "cross", "deassign", "defparam", "design", "disable", "dist", "do", "edge", "event", "expect", "extends", "extern", "final", "first_match", "for", "force", "foreach", "forever", "fork", "forkjoin", "generate", "endgenerate", "genvar", "highz0", "highz1", "iff", "ifnone", "ignore_bins", "illegalbins", "incdir", "include", "initial", "input", "inside", "instance", "int", "integer", "intersect", "join", "join_any", "join_none", "large", "liblist", "library", "local", "localparam", "logic", "longint", "macromodule", "matches", "medium", "modport", "nand", "negedge", "new", "nmos", "nor", "noshowcancelled", "not", "notif0", "notif1", "null", "or", "output", "packed", "pmos", "posedge", "primitive", "endprimitive", "priority", "program", "endprogram", "property", "endproperty", "protected", "pull0", "pull1", "pulldown", "pullup", "pulsestyle_onevent", "pulsestyle_ondetect", "pure", "rand", "randc", "randcase", "randsequence", "rcmos", "real", "realtime", "ref", "reg", "release", "repeat", "rnmos", "rpmos", "rtran", "rtranif0", "rtranif1", "scalared", "sequence", "endsequence", "shortint", "shortreal", "showcancelled", "signed", "small", "solve", "specify", "endspecify", "specparam", "static", "string", "strong0", "strong1", "struct", "super", "supply0", "supply1", "table", "endtable", "task", "endtask", "this", "throughout", "time", "timeprecision", "timeunit", "tran", "tranif0", "tranif1", "tri", "tri0", "tri1", "triand", "trior", "trireg", "union", "unique", "unsigned", "use", "var", "vectored", "virtual", "wait", "wait_order", "wand", "weak0", "weak1", "wildcard", "wire", "with", "within", "wor", "xnor", "xor"
  ]
  , reservedOpNames = ["&&", "||", "!", "&", "|", "^", "<<", ">>", "+", "-", "*", "/", "%", "==", "!=", ">=", "<=", ">", "<"] 
  , caseSensitive = True
  , nestedComments = True
  , opStart = oneOf [x | x <- ['$' .. 'z'], (\y -> not (isAlphaNum y)) x ]
  , opLetter = oneOf [x | x <- ['$' .. 'z']]
  }

whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer
symbol = P.symbol lexer
natural = P.natural lexer
parens = P.parens lexer
semi = P.semi lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer 


-- | Run the lexer and extract only the declared interface information.  This is required for certain pre-processing steps.  
-- parseDefines :: Parser [BSVMacro] -> String -> [BSVMacro]
-- parseDefines p input 
-- 	= case (parse p "" input) of
-- 		Left err -> error $ show err
-- 		Right x -> x


parseMacros' :: String -> [BSVMacro]
parseMacros' xs = parseMacros'' wrds
  where 
    lino = lines xs
    wrds = map words lino

parseMacros'' :: [[String]] -> [BSVMacro]    
parseMacros'' [] = []
parseMacros'' (x:xs) = unMaybe parsed
  where 
    parsed = parseMacros''' x
    unMaybe (Just x) = x : (parseMacros'' xs)
    unMaybe (Nothing)= parseMacros'' xs

parseMacros''' :: [String] -> Maybe BSVMacro
parseMacros''' [] = Nothing
parseMacros''' xs = if (head xs == "`define") 
		       then Just (SimpleMacro (xs !! 1) (xs !! 2) )
		       else Nothing

-- parseMacros' :: String -> [BSVMacro]
-- parseMacros' [] = []
-- parseMacros' ('`':xs) = macro : (parseMacros' theRest)
--   where 
--     (macro,theRest) = parseMacro xs
-- parseMacros' (x:xs) = parseMacros' xs
-- 
-- parseMacro :: String -> (BSVMacro, String)
-- parseMacro xs = (macro, cut5)
--   where 
--     cut1 = if ("define " `isPrefixOf` xs) then tail $ dropWhile (/= ' ') xs else xs
--     nom = takeWhile (\ x -> not (x `elem` " \t")) cut1
--     cut2 = tail $ dropWhile (\ x -> not (x `elem` " \t")) cut1
--     cut3 = dropWhile (\ x -> x `elem` " \t") cut2
--     rep = takeWhile (\ x -> not (x `elem` " \t")) cut3
--     cut4 = tail $ dropWhile (\ x -> not (x `elem` " \t")) cut3
--     cut5 = tail $ dropWhile (\ x -> not (x == '\n')) cut4
--     macro = (SimpleMacro nom rep)
    

-- parseMacros :: Parser [BSVMacro] 
-- parseMacros = do { whiteSpace
-- 		 ; ms <- many macroDec 
-- 		 ; return ms
-- 		 } <?> "Macro Definition File"
		 
macroDec :: Parser BSVMacro 
macroDec = try ( do { string "`define" ; whiteSpace 
	            ; x <- identifier ; whiteSpace
	            ; y <- many (noneOf "\n\\ ")
	            ; return (SimpleMacro x y)
	            } )
	     <|> do { string "`define" ; whiteSpace 
	            ; x <- identifier
	            ; args <- vanillaArgsList ; whiteSpace 
	            ; y <- many (noneOf "\n\\ ")
	            ; return (CompoundMacro x args y)
	            } 
	            
vanillaArgsList :: Parser [String]
vanillaArgsList = do 	{ char '(' 
			; args <- many ( try ( do { x <- identifier
					     	  ; wchar ',' 
					     	  ; return x 
					     	  } ) 
					<|> do 	  { x <- identifier
						  ; return x
						  } )
			; char ')'
			; return args
			}	            

wchar :: Char -> Parser Char
wchar c = do { x <- char c
	     ; whiteSpace
	     ; return x
	     }			
		    
		    
-----------------------------------------------------------------------GENERATOR--------------------------------------------------------------------

genDefineFile :: FileName -> [BSVMacro] -> File
genDefineFile nom macros = (nom, (intercalate "\n" (map showBSVMacro macros)))

showBSVMacro :: BSVMacro -> String 
showBSVMacro (SimpleMacro ident streng)        = "`define " ++ ident ++ " " ++ streng
showBSVMacro (CompoundMacro ident args streng) = "`define " ++ ident ++ "(" ++ (intercalate "," args) ++ ") " ++ streng
