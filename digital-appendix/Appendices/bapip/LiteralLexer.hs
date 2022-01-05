{-# OPTIONS_GHC -fno-warn-tabs #-}

module LiteralLexer where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.Char
import Numeric
import LexerTypes

-- Parses literal values
-- | parses a literal, returning a string corresponding to the same literal in BSV syntax.  Numeric literals written in bases other than decimal are converted to decimal literals.
-- | literals may be of type Real, Integer, String, Boolean, or Enumerated.
literalParser :: Parser Expression
literalParser =  try( do { char '(' 
					; lit <- literalParser 
					; char ')'
					; return lit 
}) <|> try(intLiteral) <|> try(stringLiteral) <|> try(realLiteral) <|> try(boolLiteral) <|> try(enumLiteral) <|> otherLiteral <?> "literal"

otherLiteral :: Parser Expression 
otherLiteral = try( do { string "defaultValue"
		; return (Literal (LitStructConstructor));
		}) 
	<|>  do { string "default"
		; return (Literal (LitStructConstructor));
	}
		

-- | parses an integer literal.  Integer literals may be sized or unsized. "'0" and "'1" are also acceptable.
intLiteral :: Parser Expression
intLiteral = try( do { char '\'' 
			; which <- oneOf "01" 
			; return (Literal (LitSizedInt 1 (read (which:[]) :: Integer))) } )
		<|> try ( sizedIntLiteral ) 
		<|> unsizedIntLiteral 
		<?> "integer literal"		
		
-- | parses a sized integer literal.  Such literals are preceeded by a bit width, followed by a based literal.  Unlike unsized integer literals, the base of sized integer literals must be specified. 
sizedIntLiteral :: Parser Expression 
sizedIntLiteral = do 	{ n <- bitWidth
			; value <- baseLiteral'
			; return (Literal (LitSizedInt n value))
			} <?> "sized integer literal"

-- | parses the bit width component of a sized integer literal.  
bitWidth :: Parser Integer
bitWidth = do { x <- many1 digit
			; return (read x :: Integer)
			}

-- | parses the based literal of a sized integer literal.  The base of such a literal may be specified as decimal, hexadecimal, octal, or binary. Each base has a different set of allowable digits, and each base is converted to a (decimal) PVS format, for inclusion in PVS expressions.
-- baseLiteral :: Parser Integer 
-- baseLiteral = do { -- char '\''
--         -- ; error "!"
-- 		 --; 
-- 		 result <- baseLiteral'
-- 		 ; return (result)
-- 		 }
		
baseLiteral' :: Parser Integer 
baseLiteral' = try ( do { char '\'' ; oneOf "dD"
				; value <- many1 decDigit'
				; return ( decTodec value )
				}) 
		<|> try (do { char '\'' ; oneOf "hH"
				; value <- many1 hexDigit'
				; return ( hexTodec value ) 
				} )
		<|> try (do { char '\'' ; oneOf "oO"
				; value <- many1 octDigit'
				; return ( octTodec value ) 
				})
		<|> do { char '\'' ; oneOf "bB"
				
				; value <- many1 binDigit'
				; return ( binTodec value ) 
				} <?> "base literal"

-- | Parses a permissable decimal digit. Underscores are permissible "blank" characters in BSV integer literals, an mainly serve to visually divide large numbers (especially binary numbers) into smaller chunks to increase readability.
decDigit' :: Parser Char 
decDigit' = oneOf "0123456789_"

-- | Parses a permissible hexadecimal digit. Underscores are permissible "blank" characters in BSV integer literals, an mainly serve to visually divide large numbers (especially binary numbers) into smaller chunks to increase readability.
hexDigit' :: Parser Char
hexDigit' = oneOf "0123456789abcdefABCDEF_"

-- | Parses a permissible octal digit. Underscores are permissible "blank" characters in BSV integer literals, an mainly serve to visually divide large numbers (especially binary numbers) into smaller chunks to increase readability.
octDigit' :: Parser Char
octDigit' = oneOf "01234567_"

-- | Parses a permissible binary digit. Underscores are permissible "blank" characters in BSV integer literals, an mainly serve to visually divide large numbers (especially binary numbers) into smaller chunks to increase readability.
binDigit' :: Parser Char
binDigit' = oneOf "01_"

-- | Converts a BSV decimal literal to its corresponding PVS representation.
decTodec :: String -> Integer
decTodec [] = 0
decTodec (('_'):xs) = decTodec xs
decTodec xxs = if (x == '_' ) then decTodec xs else ( (read [x]) :: Integer ) + 10 * (decTodec xs) 
where 
	x = last xxs
	xs = init xxs

-- | Converts a BSV hexadecimal literal to its corresponding PVS representation.
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

-- | Converts a BSV Octal literal to its corresponding PVS representation.
octTodec :: String -> Integer
octTodec [] = 0
octTodec (('_'):xs) = octTodec xs
octTodec xxs = if (x == '_' ) then octTodec xs else ( ( read [x] ) :: Integer ) + 8 * (octTodec xs)
where 
	x = last xxs
	xs = init xxs

-- | Converts a BSV binary literal to its corresponding PVS representation.
binTodec :: String -> Integer
binTodec [] = 0
binTodec (('_'):xs) = binTodec xs 
binTodec xxs = if (x == '_' ) then binTodec xs else ( ( read [x] ) :: Integer ) + 2 * (binTodec xs)
where 
	x = last xxs
	xs = init xxs

-- | A look-up table for converting hexadecimal digits greater than 9 to their corresponding ascii character codes.
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

-- | parses an unsized integer literal.  Unsized integer literals may also be based literals, but if not, the default base is decimal.  
unsizedIntLiteral :: Parser Expression
unsizedIntLiteral = try ( do { x <- baseLiteral'
			; return (Literal (LitInt x))
			})
--         <|> try ( do { 
--                      })
		<|> do 	{ val <- many1 decDigit'
			; return (Literal (LitInt (decTodec val)))
			} <?> "Unsized Integer Literal"	

-- | parses a real literal in scientific notation, as well as real literals without an exponent specified. Regardless of format, real literals are converted to regular notation for use in PVS (i.e., the power of ten is applied to the literal).
realLiteral :: Parser Expression
realLiteral = do 	{ front <- many1 decDigit'
			; dot <- try ( do { char '.' 
					; return (Just ".")
					} )
				<|> (return (Nothing))
			; back <- try (do { x <- many1 decDigit'
					; return (Just x) 
					} )
				<|> (return (Nothing))
			; expo <- try (do { x <- char 'e' <|> char 'E'
					; return (Just (x:[])) 
					} )
				<|> (return (Nothing))
			; sign <- try (do { x <- char '-' 
					; return (Just (x:[])) 
					} )
				<|> (return (Nothing))
			; size <- try (do { x <- unsizedIntLiteral
					; return $ (\ (Literal (LitInt y)) -> (Just (show y))) x
					} )
				<|> (return (Nothing))
			; return (Literal (LitReal (read (concatAndUnMaybe [(Just front), dot, back, expo, sign, size]) :: Float))) 
			} <?> "Real Literal"

concatAndUnMaybe :: [(Maybe [a])] -> [a]
concatAndUnMaybe [] = []
concatAndUnMaybe ((Just x):xs) = x ++ (concatAndUnMaybe xs)
concatAndUnMaybe ((Nothing):xs) = concatAndUnMaybe xs

			

-- | Parses a string literal.  String literalls may contain any character, but must be enclosed by quotation marks. 
stringLiteral :: Parser Expression
stringLiteral = try (do { string "\"\""
			; return (Literal (LitString ("")))
			}
			) <|>
		do	{ char '\"'
			; val <- many (noneOf "\"")
			; char '\"'
			; return (Literal (LitString (val)))
			} <?> "String Literal"

-- | parses a boolean literal, regardless of capitalization (or lack thereof).
boolLiteral :: Parser Expression
boolLiteral = try(do{ string "true"
					; return (Literal (LitBool True))
					}) 
	<|> try(do{ string "True"
				; return (Literal (LitBool True))
				}) 
	<|> try(do{ string "TRUE"
				; return (Literal (LitBool True))
				})
	<|> try(do{ string "false"
				; return (Literal (LitBool False))
				})
	<|> try(do{ string "False"
				; return (Literal (LitBool False))
				})
	<|> do { string "FALSE" 
			; return (Literal (LitBool False))
			}
	<?> "boolean literal" 

-- | parses an enumeration literal, distinguished from registers by having a capitalized first character.  registers use identifiers, which must start with a lower-case character.  
enumLiteral :: Parser Expression
enumLiteral = do { x <- upper
		; xs <- many1 (upper <|> lower <|> digit <|> oneOf "_$")
		; return (Literal (LitEnum (x:xs)))
		} <?> "Enumeration Literal"


