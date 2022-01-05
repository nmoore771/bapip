{-# OPTIONS_GHC -fno-warn-tabs #-}

module TSPLexer where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Perm
--import Text.Show.Pretty
import Data.Char
import Data.List (intercalate)
import Data.Functor.Identity
import qualified Control.Applicative as A
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Prim as Q

--import BSV2PVS
--import PVS2BSV
--import BSVGenerator
--import PVSGenerator
import LiteralLexer
--import FileGenerators
import LexerTypes

lexer = P.makeTokenParser pvsDef
pvsDef :: LanguageDef st
pvsDef = LanguageDef 
{ commentStart = ""
, commentEnd = ""
, commentLine = "%"
, identStart = oneOf [x | x <- ['$' .. 'z'], ((isLower x) || (isUpper x)) ]
, identLetter = oneOf [x | x <- ['$' .. 'z'], isAlphaNum x || x `elem` ['_','?'] ]
, reservedNames = [
{- Reserved keywords (80) -}
"and", "andthen", "array", "assuming", "assumption", "auto_rewrite", "auto_rewrite+", "auto_rewrite-", "axiom", "begin", "but", "by", "cases", "challenge", "claim", "closure", "cond", "conjecture", "containing", "conversion", "conversion+", "conversion-", "corollary", "datatype", "else", "elseif", "end", "endassuming", "endcases", "endcond", "endif", "endtable", "exists", "exporting", "fact", "false", "forall", "formula", "from", "function", "has_type", "if", "iff", "implies", "importing", "in", "inductive", "judgement", "lambda", "law", "lemma", "let", "library", "macro", "measure", "nonempty_type", "not", "o", "obligation", "of", "or", "orelse", "postulate", "proposition", "recursive", "sublemma", "subtypes", "subtypes_of", "table", "then", "theorem", "theory", "true", "type", "type+", "var", "when", "where", "with", "xor" 
]
, reservedOpNames = ["#", "##", "#)", "#]", "%", "&", "&&", "(", "(#", "(:", "(|", "(||)", ")", "*", "**", "+", "++", ",", "-", "->", ".", "/", "//", "/=", "/\\", ":", ":)", "::", ":=", ";", "<", "<<", "<<=", "<=", "<=>", "<>", "<|", "=", "==", "=>", ">", ">=", ">>", ">>=", "@", "@@", "[", "[#", "[]", "[|", "[||]", "\\", "\\/", "]", "]|", "^", "^^", "`", "{", "{|", "{||}", "|", "|)", "|-", "|->", "|=", "|>", "|[", "|]", "||", "|}", "}", "~"] -- ^ "," and "`" have been excluded from this list because I think they would break the parser.
, caseSensitive = False
, nestedComments = False
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
-------- to do list ----------
-- PVS State Declaration parser
------------------------------

runTSPparser :: (Eq a) => Parser a -> String -> a
runTSPparser p input 
	= case (parse p "" input) of
		Left err -> error $ show err
		Right x -> x

parseTSP :: Parser TSPpackage
parseTSP = do { nom <- tspPreamble
		; whiteSpace
			; importsTSP
			; whiteSpace
			--; xs <- many tspPermParser
			; xs <- permIterator
			; let ys = sortPerm xs ([],[],[], [], [], [])
			; return $ assembleTSPpackage nom ((\(x,_,_,_,_,_) -> x)  ys) ((\(_,x,_,_,_,_) -> x) ys) ((\(_,_,x,_,_,_) -> x) ys) ((\(_,_,_,x,_,_) -> x) ys) ((\(_,_,_,_,x,_) -> x) ys) ((\(_,_,_,_,_,x) -> x) ys)
			}

permIterator :: Parser [Tperm]
permIterator = try (do { terminator 
				; return []
				} )
	<|> try ( do { whiteSpace
		; x <- typedec
		; whiteSpace
		; xs <- permIterator
		; return ((Nothing, Nothing, Just x, Nothing, Nothing, Nothing) : xs ) 
		} )
	<|> try ( do { whiteSpace
		; x <- tspDefInst
		--; error $ show $ x
		; whiteSpace
		; xs <- permIterator
		; return ((Nothing, Nothing, Nothing, Just x, Nothing, Nothing) : xs ) 
		} )
	<|> try ( do { whiteSpace
		; x <- tspFuncDec
		--; error $ show $ x
		; whiteSpace
		; xs <- permIterator
		; return ((Nothing, Nothing, Nothing, Nothing, Just x, Nothing) : xs ) 
		} )
	<|> try ( do { whiteSpace
		; x <- tspMacro
		--; error $ show $ x
		; whiteSpace
		; xs <- permIterator
		; return ((Nothing, Nothing, Nothing, Nothing, Nothing, Just x) : xs ) 
		} )
	<|> try ( do { whiteSpace
		; x <- varDec
		; whiteSpace
		; xs <- permIterator
		; return ((Just x, Nothing, Nothing, Nothing, Nothing, Nothing) : xs ) 
		} )
	<|> do { whiteSpace
		; y <- tspTable 
		; whiteSpace
		; ys <- permIterator
		; return ((Nothing, Just y, Nothing, Nothing, Nothing, Nothing) : ys )
			} <?> "TSP Permutant"
			
tspDefInst :: Parser PVSInstDef
tspDefInst = do { inst <- pvsStateInstantiator 
		; return inst
		} <?> "Structure Instantiation Definition"

pvsStateInstantiator :: Parser PVSInstDef
pvsStateInstantiator = do { q <- varDec
			--; error $ show $ q
					; whiteSpace
					; z <- identifier ; wchar '('  ; nom <- terminatedString "_var" ; wstring ") : bool" ; whiteSpace ; wchar '=' 
					; whiteSpace
					--; error "!" 
					; sts <- xintercalatedBy "AND" True pvsStateInst
					; whiteSpace
					; return (nom, sts) 
					} <?> "State Instantiator Declaration"

pvsStateInst :: Parser (String, Literal)
pvsStateInst =      do { namo <- terminatedString "_var`" -- head register
					; nom <- identifier ; whiteSpace ; wchar '=' ; whiteSpace
					; lit <- literalParser
					; whiteSpace
					; return (nom, lit)
					} <?> "state instantiation declaration"

			
tspPermParser :: Parser Tperm 
tspPermParser = try (do { whiteSpace
			; x <- typedec 
			; whiteSpace
					; return (Nothing, Nothing, Just x, Nothing, Nothing, Nothing) 
				})
	<|> try ( do { whiteSpace
			; x <- tspDefInst 
			; return (Nothing, Nothing, Nothing, Just x, Nothing, Nothing) 
			})
	<|> try ( do { whiteSpace
			; x <- tspFuncDec
			; return (Nothing, Nothing, Nothing, Nothing, Just x, Nothing) 
			})
	<|> try ( do { whiteSpace
			; x <- tspMacro
			; return (Nothing, Nothing, Nothing, Nothing, Nothing, Just x) 
			})
	<|> try ( do { whiteSpace
			; x <- varDec
			; return (Just x, Nothing, Nothing, Nothing, Nothing, Nothing)
			})
			<|> do { whiteSpace
				; y <- tspTable 
				; whiteSpace
					; return (Nothing, Just y, Nothing, Nothing, Nothing, Nothing)
					} <?> "Tabspec Permutation Parse"

typedec :: Parser PVSTypeDef
typedec = try ( do { nom <- (name <|> identifier) ; wstring ": type ="-- structures
		; wstring "[#"
		; xs <- fields  
		; string "#]"
		--; error $ show $ xs
		; return (PVS_Struct (checkCapitalization nom) xs)
		} )
	<|> try ( do { nom <- (name <|> identifier) ; wstring ": type ="-- enumerations
		; wchar '{'
		; xs <- intercalatedBy "," True "}"
		; return (PVS_Enumeration (checkCapitalization nom) (map checkCapitalization xs))
		} )
		<|> do { nom <- (name <|> identifier) ; wstring ": type =" -- synonyms
		; typ <- pvsType
		; return (PVS_Synonym nom typ)
		} <?> "PVS Type Definition"

tspMacro :: Parser PVSMacro 
tspMacro = do { nom <- (name <|> identifier) 
		; (wstring ": MACRO" <|> wstring ": macro")
		; typ <- pvsType 
		; wchar '=' 
		; lit <- literalParser 
		; return (nom, typ, lit)
		} <?> "PVS Macro Declaration"
		
tspFuncDec :: Parser PVSFunction
tspFuncDec = do { nom <- (name <|> identifier)
		; wchar '(' ; args <- fields ; wstring ") :"
		; typ <- pvsType
		; wchar '=' 
		; exp <- pvsExpression 
		; return (nom, args, typ, exp);
		}
		
checkCapitalization :: String -> String 
checkCapitalization (x:xs) = if (isUpper x) then (x:xs) else error "Error! Please ensure structure and enumerat names are capitalized, so they represent valid types in BSV!"

fields :: Parser [PVS_Field]
fields = try(do{ x <- field 
		; wchar ',' 
		; xs <- fields 
		; return (x:xs)
		} ) 
	<|> do { x <- field
		; return (x:[])
		} <?> "Listing of Fields"
		
		
field :: Parser PVS_Field
field = do { nom <- identifier
	; wchar ':'
	--; error "blotto"
	; typ <- pvsType
	; return (nom, typ)
	} <?> "Field"
	
					
sortPerm :: [Tperm] -> ([TVarDec] , [TSPTable], [PVSTypeDef], [PVSInstDef], [PVSFunction], [PVSMacro]) -> ([TVarDec] , [TSPTable], [PVSTypeDef], [PVSInstDef], [PVSFunction], [PVSMacro])
sortPerm [] x = x
sortPerm ((Just x, _, _, _, _, _):xs) (ys, zs, as, bs, cs, ds) = sortPerm xs ((x:ys), zs, as, bs, cs, ds)
sortPerm ((_, Just x, _, _, _, _):xs) (ys, zs, as, bs, cs, ds) = sortPerm xs (ys, (x:zs), as, bs, cs, ds)  
sortPerm ((_, _, Just x, _, _, _):xs) (ys, zs, as, bs, cs, ds) = sortPerm xs (ys, zs, (x:as), bs, cs, ds)  
sortPerm ((_, _, _, Just x, _, _):xs) (ys, zs, as, bs, cs, ds) = sortPerm xs (ys, zs, as, (x:bs), cs, ds)  
sortPerm ((_, _, _, _, Just x, _):xs) (ys, zs, as, bs, cs, ds) = sortPerm xs (ys, zs, as, bs, (x:cs), ds)
sortPerm ((_, _, _, _, _, Just x):xs) (ys, zs, as, bs, cs, ds) = sortPerm xs (ys, zs, as, bs, cs, (x:ds))  

			
assembleTSPpackage :: String -> [TVarDec] -> [TSPTable] -> [PVSTypeDef] -> [PVSInstDef] -> [PVSFunction] -> [PVSMacro] -> TSPpackage
assembleTSPpackage nom var tsp tds insts funcs macs = TSPpackage
{ tName = nom
, varDecs = var
, tsps = tsp
, typedefs = tds
, defInsts = insts
, tsp_funcs = funcs 
, macros = macs
}

tspPreamble :: Parser String
tspPreamble = do { nom <- name 
		; wstring "[(IMPORTING Time) delta_t:posreal]: THEORY" <|> wstring " : THEORY"
		; wstring "BEGIN" <|> wstring "begin"
		; return nom
				} <?> "Preamble"
	
	
	
importsTSP :: Parser [String]
importsTSP = try ( do { reserved "IMPORTING" 
				; x <- name
				; xs <- importsTSP
				; return (x:xs)
		} ) 
	<|> try ( do { reserved "IMPORTING" 
				; x <- name
				; wchar '[' 
				; terminatedString "]"
				; xs <- importsTSP
				; return (x:xs)
		} ) 
	<|> try ( do { reserved "IMPORTING" 
				; x <- name
				; wchar '[' 
				; terminatedString "]"
				; return (x:[])
		} ) 
		<|> do { reserved "IMPORTING" 
			; x <- name
				; return (x:[])
			} <?> "Import Statements"

-- varDecs :: Parser [TVarDec]
-- varDecs = try ( do { x <- varDec
--                    ; xs <- varDec
-- 		   ; return (x:xs)
-- 		   }
-- 	    <|> do { x <- varDec
-- 		   ; return (x:[])
-- 		   } <?> "Variable Declarations"

varDec :: Parser TVarDec
varDec = do { vars <- intercalatedBy "," True []
			; whiteSpace
			; do { try ( do { wstring ": var" } ) <|> do { wstring ": VAR" } }
			; whiteSpace
			; typ <- pvsType 
			; return (vars, typ)
			} <?> "Variable Declaration"

tspTable :: Parser TSPTable
tspTable = try( do { nom <- name ; wchar '('
	; vars <- intercalatedBy "," True []
		; wstring ")(t): bool ="
		; helper
		; var <- timeRefID
		; wchar '='
		; wstring "IF init(t) THEN" ; exp <- pvsExpression ; whiteSpace
		; helper2 ; swapo <- swappers ; helper3
		; wstring "TABLE"
		; tspLines <- many1 tspLine
		; wstring "ENDTABLE"
		; wstring "ENDIF"
		; return (nom, var, exp, swapo, vars, tspLines)
		} )
<|> try (do { nom <- name ; wchar '('
	; vars <- intercalatedBy "," True []
		; wstring ")(t): bool ="
		; helper
		; var <- timeRefID 
		; wchar '='
		; wstring "IF init(t) THEN" ; exp <- pvsExpression ; whiteSpace ; wstring "ELSE"
		; wstring "TABLE"
		--; error "boink.1"
		; tspLines <- many1 tspLine
		; wstring "ENDTABLE"
		; wstring "ENDIF"
		; return (nom, var, exp, [], vars, tspLines)
	} )
<|> try (do { nom <- name ; wchar '('
	; vars <- intercalatedBy "," True []
		; wstring ")(t): bool ="
		; helper
		; var <- timeRefID 
		; wchar '='
		; wstring "TABLE"
		; tspLines <- many1 tspLine
		; wstring "ENDTABLE"
		; return (nom, var, (Literal (LitVoid)), [], vars, tspLines)
	} )
	<|> do { nom <- name ; wchar '('
	; vars <- intercalatedBy "," True []
		; wstring ")(t): bool ="
		; helper
		; var <- timeRefID 
		; wchar '='
		; wstring "TABLE"
		; tspLines <- many1 tspLine
		; wstring "ENDTABLE"
		; return (nom, var, (Literal LitVoid), [], vars, tspLines)
	} <?> "Tabular Specification"
	
			
timelineReference :: Parser Int	
timelineReference = try(do{ wstring "pre("
			; n <- timelineReference 
			; wchar ')'
			; return (n - 1)
			} )
		<|> try(do{ wchar 't'
			; return 0;
			} )
		<|>     do{ wstring "next("
			; n <- timelineReference
			; wchar ')'
			; return (n+1)
			} <?> "Temporal Reference"

timeRefID :: Parser (ID_Path, Temporal)
timeRefID =  try(do{ x <- identifier
		; wchar '('
		; n <- timelineReference 
		; wstring ")`"
		; y <- identifier
			; return ((ID_Submod_Struct x (ID y)), (N_Time n))
		} )
		<|> do{ x <- identifier
		; wchar '('
		; n <- timelineReference 
		; wchar ')'
		; return ((ID x), (N_Time n))
		} <?> "Time referenced identifier"
			
helper :: Parser String
helper = try ( do { wstring "FORALL t:" ; return [] } ) 
<|> do { whiteSpace ; return [] } <?> "Optional FORALL t:"

swappers :: Parser [Replacement]
swappers = try (do { x <- swapper ; wchar ',' 
			; xs <- swappers
			; return (x:xs) 
			} )
	<|> do { x <- swapper 
			; return (x:[])
		} <?> "List of Synonym Declarations"
		
swapper :: Parser Replacement
swapper = do { x <- pvsExpression 
		; return $ sepEquality x
		} <?> "synonym declaration"

sepEquality :: Expression -> (Expression, Expression)
sepEquality (Equals x y) = (x, y)
sepEquality _ = error "Error! Top level operation of expression is not an equality!" 
		
helper2 :: Parser String
helper2 = try ( do { wstring "ELSE LET" ; return [] } ) 
<|> do { wstring "ELSE let" ; return [] } <?> "\"ELSE LET\"" 

helper3 :: Parser String
helper3 = try ( do { wstring "IN" ; return [] } ) 
<|> do { wstring "in" ; return [] } <?> "\"IN\""


tspLine :: Parser TSPLine 
tspLine = do { wchar '|' 
		; exp <- pvsExpression
		; wchar '|'
		; res <- pvsExpression ; whiteSpace
		; wstring "||"
		; return (exp, res)
			} <?> "Tabular Specification Line"
		
terminator :: Parser String
terminator = do { reserved "END"
				; nom <- name
				; return nom
		}
		

------------------------------------------------------------------------------------------

pvsType :: Parser PVSType
pvsType = try(do { wstring "bool" 
		; return (PVS_Bool)
		} )
	<|>try(do{ wstring "Bit("
		; x <- natural
		; wchar ')'
		; return (PVS_Bit x)
		} )
	<|>try(do{ wstring "tick"
		; return (PVS_Custom "tick")
		} )		 
	<|>try(do{ wstring "Int("
		; x <- natural
		; wchar ')'
		; return (PVS_Int x)
		} )
	<|>try(do{ wstring "UInt("
		; x <- natural
		; wchar ')'
		; return (PVS_UInt x)
		} )
	<|>try(do{ wstring "real" 
		; return (PVS_Real)
		} )
	<|>try(do{ wstring "pred[tick]"
		; return (PVS_Bool)
		})
	<|>try(do{ wstring "[tick ->"
			; x <- pvsType
		; wchar ']'
		; return x
		})
	<|> do { x <- identifier
			; whiteSpace
			; return (PVS_Custom x)
			} <?> "Description Name"

-- | parses a name, which may have a capitalized first character, as opposed to an identifier, which may not.
name :: Parser String
name = do { x <- upper
	; xs <- many1 (upper <|> lower <|> digit <|> oneOf "_$")
	; whiteSpace
	; return (x:xs)
	} <?> "Description Name"

-- | parses a character with terminating whiteSpace.
wchar :: Char -> Parser Char
wchar c = do { x <- char c
		; whiteSpace
		; return x
		}

-- | parses a string with terminating whiteSpace.
wstring :: String -> Parser String
wstring s = do { x <- string s
		; whiteSpace
		; return s
		}

intercalatedBy :: String -> Bool -> String -> Parser [String]
intercalatedBy s whitespace terminator = try ( do { x <- (identifier <|> name)
					; if (whitespace) then wstring s else string s
					; xs <- intercalatedBy s whitespace terminator
					; return (x : xs)
					} )
					<|> do { 
					; x <- if (not (null terminator))
					then terminatedString terminator 
					else (identifier <|> name)
					; return (x : [])
					} 

xintercalatedBy :: String -> Bool -> Parser a -> Parser [a]
xintercalatedBy s whitespace parser = try ( do { whiteSpace ; x  <- parser
					--; error "boingo1"
					; if (whitespace) then wstring s else string s
					; xs <- xintercalatedBy s whitespace parser
					; return (x : xs)
					} )
					<|> do { whiteSpace ; x <- parser
					--; error "boingo2"
					; return (x : [])
					}                                                 
					
pvsExpression :: Parser Expression
pvsExpression = buildExpressionParser table subExpr
		<?> "expression"


-- | expression parsing table.  Specifies valid operators, declares precedence, associativity, and gives the transformed PVS syntax for the corresponding operation.  In general, the correspondance is very close, with noteable exceptions in == (=), != (/=), && (AND), || (OR), / (div(,)), and % (mod(,)). 
table = [ [ prefix " " '@' id
	, postfix " " '@' id 
	, prefix "\t" '@' id
	, postfix "\t" '@' id
	]
	, [ prefix "-" '@' (\ x -> (Negative x)) -- prefixes
	, prefix "+" '@' id
	, prefix "NOT" '@' (\ x -> (Not x)) 
	]  
	, [ binary "=" '@' (\ x y -> (Equals x y)) AssocLeft
		, binary "/=" '@' (\ x y -> (NotEquals x y)) AssocLeft
	, binary ">=" '@' (\ x y -> (GreaterEquals x y)) AssocLeft
	, binary "<=" '@' (\ x y -> (LessEquals x y)) AssocLeft
	, binary ">" '=' (\ x y -> (Greater x y)) AssocLeft
		, binary "<" '=' (\ x y -> (Less x y)) AssocLeft -- comparators
	]
	, [ binary "AND" '@' (\ x y -> (And x y)) AssocLeft
	, binary "&" '@' (\ x y -> (And x y)) AssocLeft -- boolean
	, binary "OR" '@' (\ x y -> (Or x y)) AssocLeft 
	]
-- bitwise ops handled as subexpressions 
{-        , [ binary "&" '@' (\ x y -> (BitwiseAND x y)) AssocLeft -- bitwise
		, binary "|" '@' (\ x y -> (BitwiseOR x y)) AssocLeft
		, binary "^" '@' (\ x y -> (BitwiseXOR x y)) AssocLeft
		] -}
	, [ binary "*" '@' (\ x y -> (Multiply x y)) AssocLeft ] -- arithmetic distributive
--	  , binary "/" '@' (\ x y -> (Divide x y)) AssocLeft  -- div and mod handled as subexpressions.
--	  , binary "%" '@' (\ x y -> (Modulo x y)) AssocLeft
--	  ] 
	, [ binary "+" '@' (\ x y -> (Add x y)) AssocLeft  -- arithmetic nondistributive
	, binary "-" '@' (\ x y -> (Subtract x y)) AssocLeft
	]
	]

-- | declares an operation as binary
binary s nfb fun assoc 	= Infix (try (do { string s
					; notFollowedBy (char nfb)
					; return fun 
					} ) 
				<|>  do { char '@'
					; return fun
					}
					) assoc
-- | declares an operation as a prefix operation
prefix s nfb fun 	= Prefix (do {string s; notFollowedBy (char nfb); return fun } ) 
-- | declares an operation as a postfix operation
postfix s nfb fun 	= Postfix (do {string s; notFollowedBy (char nfb); return fun } ) 

{-opChar :: String
opChar = "+-/%*=!<>|&^~"

binary s fun assoc = Infix ( reservedOp s >> return fun ) assoc
prefix s fun 	= Prefix ( reservedOp s >> return fun ) 
postfix s fun 	= Postfix ( reservedOp s >> return fun ) -}


-- | parses a sub-expression, interacting very closely with the expression parser. Parses subexpressions of variable redundancy, such as redundant brackets, prefixed "!" indicating boolean "not", prefixed "- x" abbreviating "0 - x", redundant prefixed "+", value method calls, register identifiers, and literals.  
subExpr :: Parser Expression
subExpr = try(do{ whiteSpace -- redundant brackets
		; wchar '('
		; whiteSpace
			; x <- pvsExpression
		; whiteSpace
			; wchar ')' 
		; return x 
		}) 
	<|>try(do{ whiteSpace -- prefixed "!" with parentheses
		; wstring "NOT ("
		; whiteSpace
			; x <- pvsExpression
		; whiteSpace
			; wchar ')' 
		; return ( Not x )
		})
	<|>try(do{ whiteSpace -- prefixed "-"
		; wstring "-("
		; whiteSpace
			; x <- pvsExpression
		; whiteSpace
			; wchar ')' 
		; return ( Negative x )
		})
	<|>try(do{ whiteSpace -- prefixed "+"
		; wstring "+("
		; whiteSpace
			; x <- pvsExpression
		; whiteSpace
			; wchar ')' 
		; return x
		})
	<|>try(do{ whiteSpace -- prefixed "!" without parentheses
		; wstring "NOT"
		; whiteSpace
			; x <- pvsExpression
		; whiteSpace
		; return ( Not x )
		})
	<|>try(do{ whiteSpace -- Bitwise AND
-- "( bv2nat ( ( nat2bv (" ++ (showPVSExpression x z ) ++")) AND ( nat2bv ("++ (showPVSExpression y z ) ++ "))))"
		; wstring "( bv2nat ( ( nat2bv ("
		; whiteSpace ; x <- pvsExpression ; whiteSpace
				; wstring ")) AND ( nat2bv ("
		; whiteSpace ; y <- pvsExpression ; whiteSpace
				; wstring "))))"
		; return (BitwiseAND x y)
		})
	<|>try(do{ whiteSpace -- Bitwise OR
		; wstring "( bv2nat ( ( nat2bv ("
		; whiteSpace ; x <- pvsExpression ; whiteSpace
				; wstring ")) OR ( nat2bv ("
		; whiteSpace ; y <- pvsExpression ; whiteSpace
				; wstring "))))"
		; return (BitwiseOR x y)
		})
	<|>try(do{ whiteSpace -- Bitwise XOR
		; wstring "( bv2nat ( ( nat2bv ("
		; whiteSpace ; x <- pvsExpression ; whiteSpace
				; wstring ")) XOR ( nat2bv ("
		; whiteSpace ; y <- pvsExpression ; whiteSpace
				; wstring "))))"
		; return (BitwiseXOR x y)
		})
	<|>try(do{ whiteSpace -- Left Shift
-- "( ( (" ++ (showPVSExpression x z) ++") * ( 2 ^ "++ (showPVSExpression y z) ++ " ) )" 
		; wstring "( ( ("
		; whiteSpace ; x <- pvsExpression ; whiteSpace
				; wstring ") * ( 2 ^ "
		; whiteSpace ; y <- pvsExpression ; whiteSpace
				; wstring " ) )"
		; return (LShift x y)
		})
	<|>try(do{ whiteSpace -- Right Shift
-- "( div((" ++ (showPVSExpression x z) ++ "), (2 ^ " ++ (showPVSExpression y z) ++ ") ) )" -- ^ uses the equivalency: 
		; wstring "( div(("
		; whiteSpace ; x <- pvsExpression ; whiteSpace
				; wstring "), (2 ^ "
		; whiteSpace ; y <- pvsExpression ; whiteSpace
				; wstring ") ) )"
		; return (RShift x y)
		})
	<|>try(do{ whiteSpace -- div
-- "( div((" ++ (showPVSExpression x z ) ++" ), ("++ (showPVSExpression y z ) ++ ")) )"
		; wstring "( div(("
		; whiteSpace ; x <- pvsExpression ; whiteSpace
				; wstring " ), ("
		; whiteSpace ; y <- pvsExpression ; whiteSpace
				; wstring ")) )"
		; return (Divide x y)
		})
	<|>try(do{ whiteSpace -- mod
		; wstring "( mod(("
		; whiteSpace ; x <- pvsExpression ; whiteSpace
				; wstring " ), ("
		; whiteSpace ; y <- pvsExpression ; whiteSpace
				; wstring ")) )"
		; return (Modulo x y)
		})
	<|>try(do{ f <- identifier -- function call
		; args <- methodArgs
		; return (Exp_FunctionCall f args) -- $ "@" ++ i ++ "`" ++ m ++ (if null args then "" else "(" ++ (intercalate "," args) ++ ")")
		})
--        <|>try(do{ s <- identifier
-- 		; char '`'
-- 		; f <- identifier 
-- 		; return (Struct_Call s f)
-- 		})
	<|>try( do { x <- idPath -- register reads
		; return (Identifier x)
		})
	<|> literalParser -- literals (see LiteralLexer.hs)

	<?> "simple expression"

idPath :: Parser ID_Path 
idPath = try(do{ i <- identifier
			; char '`'
			; q <- idPath 
			; return (ID_Submod_Struct i q)
			} )      
	<|>try(do{ i <- identifier 
			; wchar '[' 
			; index <- pvsExpression
			; wchar ']'
			; return $ ID_Vect i index 
			})
	<|>    do{ i <- identifier 
			; return $ ID i 
			} <?> "ID Path"

methodArgs :: Parser [Expression]
methodArgs =   try ( do { wchar '(' -- w/ args
			; xs <- many ( try ( do { x <- pvsExpression
					; wchar ',' 
					; return x 
					} ) 
					<|> do 	  { x <- pvsExpression
					; return x
					} ) 
			; wchar ')'
			; return (xs)
			} )
	<|> 	try( do { wchar '(' 
			; x <- pvsExpression
			; wchar ')'
			; return ((x:[]))
			} )
		<|> do { wstring "()" -- empty parens 
			; return []
			} <?> "Method Args List"

terminatedString :: String -> Parser String
terminatedString s = try ( do { string s
					; return []
					} )
					<|> do { c <- oneOf [x | x <- ['$' .. 'z'], ((isAlphaNum x) || x `elem` ['_', '$'])]
					; tailo <- terminatedString s
-- ; error $ show $ (c :tailo) 
					; return (c : tailo)
					} <?> ("String terminated with: " ++ s)

xterminatedString :: String -> Parser String
xterminatedString s = try ( do { string s
					; return []
					} )
					<|> do { c <- oneOf [x | x <- ['$' .. 'z'], ((isAlphaNum x) || x `elem` ['_', '$'])]
					; tailo <- terminatedString s
-- ; error $ show $ (c :tailo) 
					; return (c : tailo)
					} <?> ("String terminated with: " ++ s)
					

