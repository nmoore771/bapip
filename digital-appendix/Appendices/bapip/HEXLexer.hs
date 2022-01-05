{-# OPTIONS_GHC -fno-warn-tabs #-}

module HEXLexer where

import Data.Char
import LexerTypes
import BSVLexer (runParser)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Perm
import qualified Control.Applicative as A
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Prim as Q


-- lexer definitions --------------------
-- | Lexer definition for PVS for the Parsec parser-combinator library.
lexer = P.makeTokenParser pvsDef
pvsDef :: LanguageDef st
pvsDef = LanguageDef 
{ commentStart = "/*"
, commentEnd = "*/"
, commentLine = "//"
, identStart = oneOf [x | x <- ['$' .. 'z'], isLower x ]
, identLetter = oneOf [x | x <- ['$' .. 'z'], isAlphaNum x || x `elem` ['_','?'] ]
, reservedNames = []
, reservedOpNames = []
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

-- runParser :: (Eq a) => Parser a -> String -> a
-- runParser p input 
-- 	= case (parse p "" input) of
-- 		Left err -> error $ show err
-- 		Right x -> x

hexParser :: String -> Parser HexFile 
hexParser nom = do { xs <- many tkn
		; return (nom , xs)
		}

		
tkn :: Parser Literal
tkn = do { xs <- many1 hexDigit
	; sc
	; return $ Literal $ LitInt $ hexTodec xs 
	}

-- hexDigit :: Parser Char
-- hexDigit = oneOf "0123456789abcdefABCDEF"

hexTodec :: String -> Integer
hexTodec [] = 0
hexTodec (('_'):xs) = hexTodec xs
hexTodec xxs
| x == '_' 		= hexTodec xs
| x `elem` ['0'..'9'] = ( ( read [x] ) :: Integer ) + 16 * (hexTodec xs) 
| x `elem` ['a'..'f'] = ( (asciiVal x) - 87 ) + 16 * (hexTodec xs) 
| x `elem` ['A'..'F'] = ( (asciiVal x) - 55 ) + 16 * (hexTodec xs)
where 
	x = last xxs
	xs = init xxs

sc :: Parser String 
sc = do { x <- many1 wsc
	; return x
	}

wsc :: Parser Char	
wsc = char ' ' <|> char '_' <|> char '\t' <|> char '\n'	

asciiVal :: Char -> Integer
asciiVal 'a' = 97
asciiVal 'b' = 98
asciiVal 'c' = 99
asciiVal 'd' = 100
asciiVal 'e' = 101
asciiVal 'f' = 102
asciiVal 'A' = 65
asciiVal 'B' = 66
asciiVal 'C' = 67
asciiVal 'D' = 68
asciiVal 'E' = 69
asciiVal 'F' = 70
asciiVal x = 0
