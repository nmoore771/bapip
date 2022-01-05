{-# OPTIONS_GHC -fno-warn-tabs #-}

module BSVLexer where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Perm
import Data.Char
import Debug.Trace
import Data.List (intercalate)
import Data.Functor.Identity
import qualified Control.Applicative as A
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Prim as Q

import TSPLexer (intercalatedBy, xintercalatedBy)

import LiteralLexer
import LexerTypes
import MacroProcessor (dropAllComments)
-- lexer definitions --------------------
-- | Lexer definition for BSV for the Parsec parser-combinator library.
lexer = P.makeTokenParser bsvDef
bsvDef :: LanguageDef st
bsvDef = LanguageDef 
{ commentStart = "/*"
, commentEnd = "*/"
, commentLine = "//"
, identStart = oneOf [x | x <- ['$' .. 'z'], isLower x || x `elem` ['_'] ]
, identLetter = oneOf [x | x <- ['$' .. 'z'], isAlphaNum x || x `elem` ['_','$'] ]
, reservedNames = [
{- Reserved BSV keywords (78) -}
"Action", "ActionValue", "BVI", "C", "CF", "E", "SB", "SBR", "action", "endaction", "actionvalue", "endactionvalue", "ancestor", "begin", "bit", "case", "endcase", "clocked_by", "default", "default_clock", "default_reset", "dependencies", "deriving", "determines", "e", "else", "enable", "end", "enum", "export", "for", "function", "endfunction", "if", "ifc_inout", "import", "inout", "input_clock", "input_reset", "instance", "endinstance", "interface", "endinterface", "let", "match", "matches", "method", "endmethod", "module", "endmodule", "numeric", "output_clock", "output_reset", "package", "endpackage", "parameter", "path", "port", "provisos", "reset_by", "return", "rule", "endrule", "rules", "endrules", "same_family", "schedule", "struct", "tagged", "type", "typeclass", "endtypeclass", "typedef", "union", "valueOf", "valueof", "void", "while", 
{- Reserved attribute identifier's -}
	{- Module attributes-} "synthesize", "always_ready", "noinline", "always_enabled", "descending_urgency", "execution_order", "mutually_exclusive", "conflict_free", "preempts", "clock_prefix", "gate_prefix", "reset_prefix", "gate_input_clocks", "gate_all_clocks", "default_clock_osc", "default_clock_gate", "default_gate_inhigh", "default_gate_unused", "default_reset", "no_default_reset", "clock_family", "clock_ancestors", "doc",
	{- Interface attributes-} -- no unique
	{- Argument attributes-} "ready", "enable", "result", "prefix", "port",
	{- Method Declaration attributes-} -- no unique
	{- Action Declaration attributes-} -- no unique
	{- Rule Declaration attributes-} "fire_when_enalbed", "no_implicit_conditions",
	{- Method Body Declaration attributes-} -- no unique
	{- Statement attributes-} "split", "nosplit",
{- Reserved SystemVerilog keywords (disallowed identifier's, excluding BSV keywords) (183) -}
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


runParser :: (Eq a) => String -> Parser a -> String -> a
runParser f p input'
	= case (parse p "" input) of
		Left err -> error $ "Error!  Parsing failed on file : " ++ f ++ "\n\n" ++ (show err)
		Right x -> x
	where 
		input = dropAllComments input'

-- | Parser for extracting declared interface declarations.  Virtually identical to other top-level parser variants, differing only in the particular data which is returned.
getInterfaces :: Parser [InterfaceDec]
getInterfaces = do  
-- package <name> ;
			{ whiteSpace
			; reserved "package" 
			; pkgName <- name
			; semi
-- permuted declarations  
			; decl' <- many1 permutedDeclarations 
			; let decl = recordify (fold1_9TupleList (Just pkgName) Nothing decl' ([], [], [], [], [], [], [], [], [], []))
-- modules
			; many moduleDecl
-- endpackage
			; endPackage
			; return $ interfaces decl
			} <?> "BSV file"

-- | Parser for extracting declared file imports. Virtually identical to other top-level parser variants, differing only in the particular data which is returned. 
getImports :: Parser ([PackageName], [String])
getImports = do  
-- package <name> ;
			{ whiteSpace
			; reserved "package" 
			; pkgName <- name
			; semi
-- permuted declarations 	
			; decl' <- many1 permutedDeclarations 
			; let decl = recordify (fold1_9TupleList (Just pkgName) Nothing decl' ([], [], [], [], [], [], [], [], [], []))
-- endpackage
			; 
			; endPackage
			; return $ ((imports decl), (getRegFiles decl)) 
			} <?> "BSV file"

getRegFiles :: BSVPackage -> [String]
getRegFiles pkg = concat $ map extract states
where
	mods = bsv_modules pkg
	states = concat $ map state mods
	extract (BSV_RegFile _ _ _ x) = (extract' x):[]
	extract x = []
	extract' (RegFileLoad x _ _) = x


-- | Parser for extracting declared file imports. Virtually identical to other top-level parser variants, differing only in the particular data which is returned. 
getIncludes :: Parser [PackageName]
getIncludes = do        { whiteSpace
			; reserved "package" 
			; pkgName <- name
			; semi
-- permuted declarations 	
			; decl' <- many1 permutedDeclarations 
			; let decl = recordify (fold1_9TupleList (Just pkgName) Nothing decl' ([], [], [], [], [], [], [], [], [], []))
-- endpackage
			; endPackage
			; return $ including decl
			} <?> "BSV file"			
			
-- | Parses a file, returning a record containing all information found in the file (the so-called "omnibus").  For particular information on what data is extracted from a file, please see the documentation on the "BSVPackage" record type. 

-- | A BSV file is enclosed by a package declaration, and the elements of a bsv file may occur in any order, though each one must be continuous.  A permutation parser is employed to ensure that such language elements as import statements, type declarations, interface declarations and module declarations may occur in any order and still be accepted by the Parser as being syntactically correct.
getOmnia :: [HexFile] -> Parser BSVPackage
getOmnia hex = do  
-- package <name> ;
			{ whiteSpace
			; reserved "package" 
			; pkgName <- name
			; semi
-- permuted declarations
			; decl' <- many1 permutedDeclarations 
			; let decl = recordify (fold1_9TupleList (Just pkgName) (Just hex) decl' ([], [], [], [], [], [], [], [], [], []))
-- endpackage
			; endPackage
			; return (decl)
			} <?> "BSV file"

endPackage :: Parser String
endPackage = try ( do { reserved "endpackage"
					; x <- name
					; return x
					}) 
			<|> do    { reserved "endpackage" 
					; return "Bugger all"
					}
					
			
-- | handles the parsing of selecting which of the six possible language elements the next language element in a bsv file will parse as.  This is an inclusive list, and a bsv file must contain at least one of these elements.  This algorithm acts as a permutation parser because any of the six possible language elements may be parsed and returned to the calling function, after which they are marshalled into a record type.  
-- | The permutation parsers returns a six-tuple, with one position for the result of each type of parser.  Depending on which language element was parsed, the corresponding slot in the tuple will be filled with the result.  Elsewhere in the program, the list of all these language elements is collapsed, and empty elements removed, changing from a list of tuples to a tuple of lists, which is then further converted to a record.  
permutedDeclarations :: Parser (Maybe String, Maybe InterfaceDec, Maybe String, Maybe BSVConstantDec, Maybe BSVTypeDef, Maybe BSVModuleDec, Maybe BSVInstDef, Maybe BSVFunction)
permutedDeclarations = try (do { whiteSpace
				; x <- importDecl
				; whiteSpace
				; return (Just x, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
				} )
		<|> try (do { whiteSpace
				; x <- interfaceDecl
				; whiteSpace
				; return (Nothing, Just x,  Nothing,Nothing, Nothing, Nothing, Nothing, Nothing)
				} )
		<|> try (do { whiteSpace
				; x <- bsvConstantDeclaration
				; whiteSpace
				; return (Nothing, Nothing, Nothing, Just x, Nothing, Nothing, Nothing, Nothing)
				} )
		<|> try (do { whiteSpace
				; x <- bsvTypedefDecl
				; whiteSpace
				; return (Nothing, Nothing, Nothing, Nothing, Just x, Nothing, Nothing, Nothing)
				} )
		<|> try (do { whiteSpace
				; x <- moduleDecl 
				; whiteSpace
				; return (Nothing, Nothing, Nothing, Nothing, Nothing, Just x, Nothing, Nothing)
				} )
			<|> try (do { whiteSpace
				; x <- functionDecl
				; whiteSpace
				; return (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just x)
				} )
			<|> try (do { whiteSpace
				; x <- includeDecl
				; whiteSpace
				; return (Nothing, Nothing, Just x, Nothing, Nothing, Nothing, Nothing, Nothing)
				} )
		<|> do { whiteSpace
				; x <- defaultInstance
				; whiteSpace
				; return (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just x, Nothing)
		} <?> "Permuted BSV Package"
				

-- | recieves a list of tuples from the permutation parser and converts it to a tuple of lists of the omnibus information.  This function takes advantage of the knowledge that the permutation parser returns sparse tuples (i.e., only one element per tuple will be non-empty). As the name implies, this is a folding algorithm, where the second argument is the argument that the algorithm is folding into.  As such, it is recommended that it is passed a "blank" tuple, insofar as blankness exists within the context of the relevent type definitions.  
fold1_9TupleList :: Maybe a -> Maybe [j] -> [(Maybe b, Maybe c, Maybe d, Maybe e, Maybe f, Maybe g, Maybe h, Maybe i)]   
-> (a, [b], [c], [d], [e], [f], [g], [h], [i], [j]) -> (a, [b], [c], [d], [e], [f], [g], [h], [i], [j])
fold1_9TupleList (Nothing)_  [] xt = xt
fold1_9TupleList (Just a) j xs (as,bs,cs,ds,es,fs,gs,hs,is,js) = fold1_9TupleList (Nothing) j xs (a, bs, cs, ds, es, fs, gs, hs, is, js)
fold1_9TupleList (Nothing) (Just j) xs (as,bs,cs,ds,es,fs,gs,hs,is,js) = fold1_9TupleList (Nothing) (Nothing) xs (as, bs, cs, ds, es, fs, gs, hs, is, j)
fold1_9TupleList (Nothing) _ ((Just b, _, _, _, _, _, _, _):xs) (as, bs, cs, ds, es, fs, gs, hs, is, js) = fold1_9TupleList (Nothing) (Nothing) xs (as, (b:bs), cs, ds, es, fs, gs, hs, is, js) 
fold1_9TupleList (Nothing) _ ((_, Just c, _, _, _, _, _, _):xs) (as, bs, cs, ds, es, fs, gs, hs, is, js) = fold1_9TupleList (Nothing) (Nothing) xs (as, bs, (c:cs), ds, es, fs, gs, hs, is, js) 
fold1_9TupleList (Nothing) _ ((_, _, Just d, _, _, _, _, _):xs) (as, bs, cs, ds, es, fs, gs, hs, is, js) = fold1_9TupleList (Nothing) (Nothing) xs (as, bs, cs, (d:ds), es, fs, gs, hs, is, js) 
fold1_9TupleList (Nothing) _ ((_, _, _, Just e, _, _, _, _):xs) (as, bs, cs, ds, es, fs, gs, hs, is, js) = fold1_9TupleList (Nothing) (Nothing) xs (as, bs, cs, ds, (e:es), fs, gs, hs, is, js) 
fold1_9TupleList (Nothing) _ ((_, _, _, _, Just f, _, _, _):xs) (as, bs, cs, ds, es, fs, gs, hs, is, js) = fold1_9TupleList (Nothing) (Nothing) xs (as, bs, cs, ds, es, (f:fs), gs, hs, is, js) 
fold1_9TupleList (Nothing) _ ((_, _, _, _, _, Just g, _, _):xs) (as, bs, cs, ds, es, fs, gs, hs, is, js) = fold1_9TupleList (Nothing) (Nothing) xs (as, bs, cs, ds, es, fs, (g:gs), hs, is, js) 
fold1_9TupleList (Nothing) _ ((_, _, _, _, _, _, Just h, _):xs) (as, bs, cs, ds, es, fs, gs, hs, is, js) = fold1_9TupleList (Nothing) (Nothing) xs (as, bs, cs, ds, es, fs, gs, (h:hs), is, js) 
fold1_9TupleList (Nothing) _ ((_, _, _, _, _, _, _, Just i):xs) (as, bs, cs, ds, es, fs, gs, hs, is, js) = fold1_9TupleList (Nothing) (Nothing) xs (as, bs, cs, ds, es, fs, gs, hs, (i:is), js) 
fold1_9TupleList (Nothing) _ ((Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing):xs) ys = fold1_9TupleList (Nothing) (Nothing) xs ys

-- | Converts a list of tuples containing the omnibus information to a permutated declaration record containing the same information.
recordify :: (PackageName, [PackageName], [InterfaceDec], [String], [BSVConstantDec], [BSVTypeDef], [BSVModuleDec], [BSVInstDef], [BSVFunction], [HexFile]) -> BSVPackage
recordify (nom, imp, inter, incl, str, typ, mods, insts, funcs, hex) = BSVPackage 
				{ bsv_packageName = nom
				, imports = imp
				, including = incl
				, interfaces = inter
				, bsv_constants = str
				, bsv_typedefs = typ
				, bsv_modules = mods
				, bsv_instDefs = insts
				, bsv_functions = funcs
				, bsv_macros = []
				, hexFiles = hex
--				, requestedTransitions = []
				}

-- | returns an interface declaration corresponding to the one invoked by the passed module declaration.
chooseInterface :: BSVModuleDec -> [InterfaceDec] -> InterfaceDec
chooseInterface _ [] = error "No Interface matching module."
chooseInterface mod (z:zs) = if (interfaceName mod) == ( (\ (x , y, _, _) -> x ) z ) then z else chooseInterface mod zs

-- | Parser which extracts, gathers, and returns all information contained by a module.  Since module elements (such as state declarations, submodule declarations, rules and methods) may occur in any order, similarly to a package, the module parser uses the same permutation parsing scheme as the package parser to both extract coherent data, and make all possible permutations syntactically valid.  
moduleDecl :: Parser BSVModuleDec
moduleDecl = do { atts <- mall -- ; error "!"
-- module mk<name> (<name>);
			; reserved "module"
			; wstring "mk"
			; id <- name
			; wchar '(' ; inter <- name ;  wchar ')' ; semi
			; mod' <- many1 permutedModuleBlocks
			; let mod = recordify' (fold4TupleList mod' ([],[],[],[],[]))

			; reserved "endmodule"
			; try ( do { wchar ':' ; identifier' })
-- debug		; error (ppShow (directivesM mod))

			; return BSVModuleDec 	{ mName = id
					, instanceName = ""
					, instances = []
					, interfaceName = inter
					, interfaceDecs = intersM mod
					, state = stateM mod
					, actions = map killActionVoids (actionsM mod)
					, attributes = concat atts
					, rules = map killRuleVoids (rulesM mod)
					, methods = map killMethodVoids (methodsM mod)
					}
			} <?> "Module Declaration"

-- | handles the parsing of selecting which of the five possible language elements the next language element in a module will parse as.  This is an inclusive list, and a module must contain at least one of these elements.  This algorithm acts as a permutation parser because any of the five possible language elements may be parsed and returned to the calling function, after which they are marshalled into a record type.  
-- | The permutation parsers returns a five-tuple, with one position for the result of each type of parser.  Depending on which language element was parsed, the corresponding slot in the tuple will be filled with the result.  Elsewhere in the program, the list of all these language elements is collapsed, and empty elements removed, changing from a list of tuples to a tuple of lists, which is then further converted to a record.  
permutedModuleBlocks :: Parser (Maybe BSVstateDec, Maybe ActionDec, Maybe RuleDec, Maybe MethodBody, Maybe MidModInterfaceDec)
permutedModuleBlocks = try (do { x <- actionDecl
				; return (Nothing, Just x, Nothing, Nothing, Nothing)
				}  
			)
		<|> try (do { x <- ruleDecl
				; return (Nothing, Nothing, Just x, Nothing, Nothing)
				}  
			)
		<|> try (do { x <- midmodInterfaceDecl
				; return (Nothing, Nothing, Nothing, Nothing, Just x)
				}  
			)
		<|> try (do { x <- methodBlockDecl
				; return (Nothing, Nothing, Nothing, Just x, Nothing)
				}  
			)
		<|>  do { x <- stateDecl 
				;      return (Just x, Nothing, Nothing, Nothing, Nothing)
				}  

-- | takes a tuple of lists of the syntactic elements of a module and produces a Permutated Method record, an intermediate representation that is eventually used to construct a full module declaration, as used by the file generation back-end.
recordify' :: ([BSVstateDec], [ActionDec], [RuleDec], [MethodBody], [MidModInterfaceDec]) -> PermMod
recordify' (sta, act, rul, met, int) = PermMod
				{ stateM = sta
				, actionsM = act
				, rulesM = rul
				, methodsM = met
				, intersM = int
				}

-- | An implementation of the fold1_8TupleList function defined for permuted declarations, retyped for module declarations.  
-- | recieves a list of tuples from the permutation parser and converts it to a tuple of lists of the module information.  This function takes advantage of the knowledge that the permutation parser returns sparse tuples (i.e., only one element per tuple will be non-empty). As the name implies, this is a folding algorithm, where the second argument is the argument that the algorithm is folding into.  As such, it is recommended that it is passed a "blank" tuple, insofar as blankness exists within the context of the relevent type definitions.  
fold4TupleList :: [(Maybe a, Maybe b, Maybe c, Maybe d, Maybe e)] -> ([a], [b], [c], [d], [e]) -> ([a], [b], [c], [d], [e])
fold4TupleList [] xt = xt
fold4TupleList ((Just a, Nothing, Nothing, Nothing, Nothing):xs) (as,bs,cs,ds,es) = fold4TupleList xs ((a:as), bs, cs, ds, es)
fold4TupleList ((Nothing, Just b, Nothing, Nothing, Nothing):xs) (as,bs,cs,ds,es) = fold4TupleList xs (as, (b:bs), cs, ds, es)
fold4TupleList ((Nothing, Nothing, Just c, Nothing, Nothing):xs) (as,bs,cs,ds,es) = fold4TupleList xs (as, bs, (c:cs), ds, es)
fold4TupleList ((Nothing, Nothing, Nothing, Just d, Nothing):xs) (as,bs,cs,ds,es) = fold4TupleList xs (as, bs, cs, (d:ds), es)
fold4TupleList ((Nothing, Nothing, Nothing, Nothing, Just e):xs) (as,bs,cs,ds,es) = fold4TupleList xs (as, bs, cs, ds, (e:es))
fold4TupleList ((Nothing, Nothing, Nothing, Nothing, Nothing):xs) ys = fold4TupleList xs ys

-- | parses an import statement, and returns the name of the imported package.
importDecl :: Parser String
importDecl = do { reserved "import"
		; x <- name
		; wstring "::" ; wchar '*'
		; semi 
		; return (x)
		} <?> "import statement"

includeDecl :: Parser String
includeDecl = do { string "`include" ; whiteSpace
		; wchar '\"' 
		; x <- many (noneOf "\"")
		; wchar '\"' 
		; return (x)
		} <?> "import statement"
-- | parses an interface delcaration.  In BSV, each module must be declared with an interface, which itself declares the full typing information of all methods accessible to parent modules.  BSV is strictly hierarchical, so these methods are not public in an object oriented sense, but similar to other module interface declarations in other HDLS such as SystemVerilog.  
-- | An interface declaration consists of a wrapper (from which the interface name is derived) and a series of method declarations.  
interfaceDecl :: Parser InterfaceDec
interfaceDecl =	try (do { atts <- many interfaceAttributeList
					; reserved "interface"
			; x <- name
			; semi
			; decs <- many1 iDecPerm
			; reserved "endinterface" 
			; wchar ':' 
			; name
			; let folded = fold2TupleList decs ([],[]) 
			; return (x, fst folded, snd folded, (concat atts)) 
			}) 
	<|>	do 	{ atts <- many interfaceAttributeList
					; reserved "interface"
			; x <- name
			; semi
			; decs <- many1 iDecPerm
			; reserved "endinterface" 
			; let folded = fold2TupleList decs ([],[]) 
			; return (x, fst folded, snd folded, (concat atts)) 
			} <?> "interface declaration"

midmodInterfaceDecl :: Parser MidModInterfaceDec
midmodInterfaceDecl = try (do { reserved "interface"
					; whiteSpace
					; n <- name
					; whiteSpace
					; n2 <- name
					; semi 
					; ms <- many1 methodBlockDecl
					; reserved "endinterface" 
					; wchar ':' 
					; name
					; return (n, n2, ms)
					} )
				<|> do { reserved "interface"
					; whiteSpace
					; n <- name
					; whiteSpace
					; n2 <- name
					; semi 
					; ms <- many1 methodBlockDecl
					; reserved "endinterface" 
					; return (n, n2, ms)
					} 
			
fold2TupleList :: [(Maybe a, Maybe b)] -> ([a],[b]) -> ([a],[b])
fold2TupleList [] ts = ts
fold2TupleList ((Just a, Nothing):xs) (as,bs) = fold2TupleList xs ((a:as),bs)
fold2TupleList ((Nothing, Just b):xs) (as,bs) = fold2TupleList xs (as,(b:bs))

			
-- | parses a method declaration, as a component of the interface declaration parser.  Method declarations take the general form of the keyword "method," followed by the method's return type, the method's name, and finally a list of arguments for which typing information has been provided.
iDecPerm :: Parser (Maybe MethodDec, Maybe InterfaceRef)
iDecPerm =  try(do	
-- method <returnType> <methodName> <arguments>;
			{ atts <- many methodDecAttributeList
					; reserved "method"
			; return' <- returnTypeDecl
			; methodName <- identifier'
			; args <- argList
			; semi 
			; return (Just (methodName, return', args, (concat atts)), Nothing)
			} )
		<|>  do { atts <- many methodDecAttributeList
			; reserved "interface"
			; iName <- name
			; lName <- name
			; semi
			; return (Nothing, Just (iName, lName))
			}
			<?> "method declaration"

-- | parses the return type of a method declaration.  Return types may be "Action," "Value," or "ActionValue."
-- | "Action" types create methods which are permitted to have side effects on the state of the containing module, but do not return anything when called.  In this sense, they are comparable to "void" methods in object-oriented languages.
-- | "Value" types create methods which are not permitted to have side effects on the state of the containing module, but do return data to calling module.  These methods are created not through use of a "Value" keyword, but by specifying the return type of the method in terms of the regular BSV types (bool Int#() etc.) 
-- | "ActionValue" types are methods which both have side effects and return values.  The return type of an ActionValue method must still be specified. 
returnTypeDecl :: Parser ReturnType
returnTypeDecl = do 	{ reserved "Action"
			; return Action
			} 
		<|> do 	{ reserved "ActionValue"
			; wstring "#("
			; x <- bsvType 
			; wchar ')'
			; return (ActionValue x)
			}
		<|> do 	{ x <- bsvType 
			; whiteSpace
			; return (Value x)
			} <?> "Return Type"

-- | parses a list of arguments enclosed by parentheses and delinieated by commas. The arguments here consist of a BSV type followed by an argument identifier'.
argList :: Parser [Argument]
argList =try(do { wchar '(' ; wchar ')' ; return [] } )
<|>   do 	{ wchar '(' 
		; xs <- argList'
		; wchar ')'
		; return xs
		} <?> "Argument List"

-- | parses an argument, which consists of a BSV type followed by an argument identifier', which may or may not include a terminal comma.
argList' :: Parser [Argument]
argList' =try(do{ atts <- many argumentAttributeList
				; x <- bsvType
		; whiteSpace
		; y <- identifier'
		; wchar ','
		; whiteSpace
		; xs <- argList'
		; return ((y, x, (concat atts)):xs)
		} )
	<|> do  { atts <- many argumentAttributeList
				; x <- bsvType
		; whiteSpace
		; y <- identifier'
		; return ((y, x, (concat atts)):[])
		} <?> "Argument"

functionDecl :: Parser BSVFunction 
functionDecl = do { reserved "function"
		; ret <- bsvType 
		; x <- identifier'
		; args <- argList
		; semi
		; stmts <- many1 statement
		; reserved "endfunction"
		; return (x, args, ret, stmts)
		} <?> "BSV Function Declaration" 
		
-- | parses the declaration of a string constant.  String constants are sometimes used as debug messages, and are included in this translator to ensure the parsability of the example BSV descriptions used by Richards and Lester for their prototype embedding.  
-- | A string constant declaration begins with the "String" keyword, followed by an identifier', "=", the string literal, and a semicolon.  This parser directly returns the correct PVS syntax for the equivalent string constant declaration.  
bsvConstantDeclaration :: Parser BSVConstantDec
bsvConstantDeclaration = do	
-- String <strName> = <strVal>;
			{ x <- bsvType
			; whiteSpace
			; nom <- identifier'
			; wchar '='
			; lit <- literalParser
			; semi
			; return (nom, x, lit)
			} <?> "constant declaration"

-- | parses a type declaration, either a direct type synonym (second case) or a type enumeration (first case).  
-- | A type synonym declaration consists of the keyword "typedef", followed by the BSV type that will be synonymized, the name (as distinct from an identifier') which will become synonymous with the leading BSV type, and a semicolon.
-- | A type enumeration is declared with the keywords "typedef enum", followed by a list of enumerats enclosed by curly braces and a semicolon. A terminal "deriving" phrase may be present, in the haskell style.
-- | Both cases directly return the correct PVS syntax for the equivalent type definition in PVS.  
bsvTypedefDecl ::  Parser BSVTypeDef
bsvTypedefDecl = try ( do  { whiteSpace ; reserved "typedef" ; whiteSpace-- enum {"--reserved "typedef" ; whiteSpace
			; reserved "enum"
			; wchar '{'
			; enumerats <- intercalatedBy "," True "}" 
			; whiteSpace
			; typeName <- name
			; try  ( do 	{ reserved "deriving"
					; wchar '('
					; many (noneOf ")")
					; wchar ')'
					; return []
					} ) <|> do { return [] }
			; semi
			; return (BSV_Enumeration typeName enumerats)
			} )
		<|>try(do{ reserved "typedef"
			; x <- bsvType 
			; n <- name 
			; semi
			; return (BSV_Synonym n x)
			} )
		<|>try(do{ reserved "typedef" ; reserved "struct" ; wchar '{' 
			; wchar '}' ; nom <- name 
			; try  ( do 	{ reserved "deriving"
					; wchar '('
					; many (noneOf ")")
					; wchar ')'
					; return []
					} ) <|> do { return [] }
			; semi
			; return (BSV_Struct nom [])
			}) 
		<|>  do {  reserved "typedef" ; reserved "struct" ; wchar '{' 
			; fs <- many1 field 
			; wchar '}' ; nom <- name 
			; try  ( do 	{ reserved "deriving"
					; wchar '('
					; many (noneOf ")")
					; wchar ')'
					; return []
					} ) <|> do { return [] }
			; semi
			; return (BSV_Struct nom fs)
			} <?> "Type Definition" 
			
		
field :: Parser BSV_Field
field = do { typ <- bsvType
	; whiteSpace
	; nom <- identifier'
	; semi
	; return (nom, typ)
	} <?> "Structure Field"
		
		
defaultInstance :: Parser BSVInstDef 
defaultInstance = do { reserved "instance"
			; wstring "DefaultValue#("
			; n <- name 
			; wstring ");"
			; wstring "defaultValue" ; wchar '='
			; name ; wchar '{'
			; fs <- defaultDeclarationList
			; wstring "};"
			; reserved "endinstance"
			; return (n, fs)
			} <?> "Instance of structure default value delcaration"
			
defaultDeclarationList :: Parser [(String, Literal)]
defaultDeclarationList = try(do { n <- identifier' 
				; wchar ':' 
				; l <- expression
				; wchar ',' 
				; xs <- defaultDeclarationList 
				; return ((n, l):xs)
				} )
			<|> do { n <- identifier'
					; wchar ':'
					; l <- expression
					; return ((n, l):[])
					} <?> "List of Default Structure value declarations"



-- extracts list of memory names, corresponding datatypes, whether they are registers or fifos and the instantiation value.   
-- | parses a state declaration in PVS, and returns a "StateDec" value. 
-- | Submodules are instantiated by first invoking the name of the module that's being instantiated.  The next identifier' is the instance name of the module, which will be the name used to reference it in statements and other expressions.  Finally, the instantiation terminates with the phrase "<- mk[Name];", where <Name> is identical to the name of the module.  
-- | Registers are declared with the "Reg#([T])", where [T] is the BSV type of the register.  Next, the register's name occurs, followed by "<- mkReg([L]);", where [L] is the initialization value of the register, as a literal of type [T].
-- | FIFOs are declared using the "FIFO#([T])" keyword, which operates the same as with registers.  At present, the software parses only fifos of size 1, again for compatability with Richards and Lester's examples.  Generalizing this for fifos of any size is a planned addition to the translator.  
stateDecl :: Parser BSVstateDec
stateDecl = try(do{ interfaceName <- name
		; whiteSpace
		; instName <- identifier' 
		; wstring "<-"
		; modName <- identifier'
		; semi
		; return $ (BSV_SubModuleDec interfaceName modName instName)
		} )
		<|>
	try(do{ interfaceName <- name
		; whiteSpace
		; instName <- identifier' 
		; wstring "<-"
		; modName <- identifier'
		; wstring "()"
		; semi
		; return $ (BSV_SubModuleDec interfaceName modName instName)
		} )
		<|>
	try (do { wstring "RegFile#(" 
				; whiteSpace 
		; a_type <- bsvType 
		; wchar ','
		; m_type <- bsvType
				; whiteSpace
		; wchar ')'
		; m_name <- identifier'
		; wstring "<-"
		; ld <- regFileLoader 
		; semi
		; return $ BSV_RegFile (ID m_name) a_type m_type ld
		})
		<|>
		do 	{ wstring "Reg#("
				; whiteSpace
		; m_type <- bsvType
				; whiteSpace
		; wchar ')'
		; m_name <- identifier'
		; wstring "<-"
		; wstring "mkReg" ; wstring "("
		; whiteSpace
		; m_init <- expression
				; whiteSpace
		; wstring ")"
		; semi
		; return $ BSV_Reg (ID m_name) m_type m_init
		}
		<|> try (
		do 	{ wstring "Wire#("
				; whiteSpace
		; m_type <- bsvType
				; whiteSpace
		; wchar ')'
		; m_name <- identifier'
		; wstring "<-"
		; wstring "mkUnsafeDWire"; wstring "("
		; whiteSpace
		; m_init <- expression
				; whiteSpace
		; wstring ")"
		; semi
		; return $ DWire (ID m_name) m_type m_init
		})
		<|> 
		do 	{ wstring "Wire#("
				; whiteSpace
		; m_type <- bsvType
				; whiteSpace
		; wchar ')'
		; m_name <- identifier'
		; wstring "<-"
		; wstring "mkDWire"; wstring "("
		; whiteSpace
		; m_init <- expression
				; whiteSpace
		; wstring ")"
		; semi
		; return $ DWire (ID m_name) m_type m_init
		}
		<|> try (
		do 	{ wstring "FIFO#("
				; whiteSpace
		; m_type <- bsvType
				; whiteSpace
		; wchar ')'
		; m_name <- identifier'
		; wstring "<-"
		; f_type <- fifoType 
		; semi
		; return $ BSV_Fifo f_type (ID m_name) m_type
		} )
		<|> try (
		do 	{ wstring "FIFOF#("
				; whiteSpace
		; m_type <- bsvType
				; whiteSpace
		; wchar ')'
		; m_name <- identifier'
		; wstring "<-"
		; f_type <- fifoType 
		; semi
		; return $ BSV_Fifo f_type (ID m_name) m_type
		} )
		<|>
		do 	{ wstring "Vector#("
				; whiteSpace
		; size <- literalParser
				; whiteSpace
		; wchar ',' 
		; m_type <- bsvType
		; wchar ')'
		; m_name <- identifier'
				; wstring "="
				; vi <- vectorInit 
		; semi
		; return $ BSV_Vector (ID m_name) m_type ((\ (Literal (LitInt x)) -> x) size) vi
		}
		<?> "Register or FIFO declaration"

-- fixedInit :: BSVType -> Expression -> Expression
-- fixedInit (BSV_Maybe t) (Tagged x exp) = (Tagged x exp)
-- fixedInit (BSV_Maybe t) exp = (Tagged Nothing (Valid exp))
-- fixedInit t exp = exp	    
		
fifoType :: Parser FifoType 
fifoType = try (do  { wstring "mkSizedBypassFIFOF" ; wchar '(' ; n <- literalParser ; wchar ')'
					; return (SizedBypassFIFOF n)
					})
		<|> try( do { wstring "mkSizedFIFO" ; wchar '(' ; n <- literalParser ; wchar ')'
					; return (SizedFIFO n)
					} )   
		<|> try( do { wstring "mkSizedFIFOF" ; wchar '(' ; n <- literalParser ; wchar ')'
					; return (SizedFIFOF n)
					} )   
		<|> try( do { wstring "mkFIFOF"
					; return (FIFOF)
					} )                  
			<|> do { wstring "mkFIFO" 
					; return (FIFO)
					} 
				
		

regFileLoader :: Parser RegFileLoader 
regFileLoader = do { wstring "mkRegFileLoad" ; wchar '(' ; wchar '\"'  
		; fn <- many1 (noneOf "\"")
		; char '\"'
		; wchar ','
		; min <- literalParser 
		; wchar ','
		; max <- literalParser
		; wchar ')'
		; return $ RegFileLoad fn min max
		}

vectorInit :: Parser VectorInit
vectorInit = do { wstring "replicate("
				; init <- literalParser
				; wchar ')'
				; return (Replicate init)
				} <?> "Vector Initialization"

constructor :: Parser Expression 
constructor = do { wstring "mkReg("
				; exp <- expression
				; wchar ')'
				; return exp
				} <?> "Constructor"

-- | Parses an action declaration.  Action declarations consist of a collection of statements, and an identifier'.  When invoked by another statement-carrying declaration, the list of statements represented by the action are inserted into the calling entity.  As such, all action invokations are flattened by the translator's file generation pre-processor, as the statements must be clear and present for the re-organization from rule-centric to state-centric paradigms.  
actionDecl :: Parser ActionDec
actionDecl = do { atts <- many actionAttributeList 
				; reserved "Action"
		; a_name <- identifier'
		; wchar '=' ; wchar '('
		; reserved "action"
		; statementList <- many1 statement
		; reserved "endaction"
		; wchar ')'
		; semi
		; return $ (a_name, statementList, (concat atts))
		} <?> "Action Declaration"

-- | parses a statement, which may be found in rules, methods, and actions.  
-- | Two different syntaxes for register writes are parseable, and equivalent in semantic reconstruction.  They both consist of the register being written to being identified, followed by the expression to be written to it, once evaluated.
-- | Action-typed Method calls are valid statements, which are referred to by the instance name of the submodule from which the method is being called, and the name of the method being called.  The method may or may not be passed a series of expressions, which are applied as agruments.
-- | Action invokations contain only an action name, and the statement(s) contained by the invoked action are inserted where this statement occurs.  Action invokations are flattened as a file generation pre-processing step.
-- | Certain valid statements, such as messages for the debug buffer, are not needed by PVS, and are discarded.  Such statements are considered void, and void statements are removed from statement lists as a file generation pre-processing step.
-- | return statements are used only by methods, and operate fairly conventionally.  They are also flattened as a file generation pre-processing step.  

statement :: Parser Statement 
statement =try(do{ atts <- many statementAttributeList
				; name <- idPath   -- Register write 1
		; wstring "<="
		; exp <- expression 
		; semi
		; return $ Write name exp (concat atts)
		})
--         <|>try(do{ atts <- many statementAttributeList
--                 ; name <- identifier'    -- Vector write
--                 ; wchar '['           
--                 ; index <- expression 
--                 ; wchar ']'
-- 		; wstring "<="
-- 		; exp <- expression 
-- 		; semi
-- 		; return $ VectW (ID name) index exp (concat atts)
-- 		} )
	<|>try(do{ reserved "begin"
		; ss <- many1 statement
		; reserved "end"
		; return $ StatementBlock ss
			}) 
	<|>try(do{ atts <- many statementAttributeList
		; i <- idPath 
		; wstring "<="
		; n <- name 
		; wchar '{'
		; stmts <- recordStatements
		; wchar '}' ; semi
		; return $ StatementBlock $ map (\ x -> makeStatement i x (concat atts)) stmts
		})
	<|>try(do{ atts <- many statementAttributeList
				; name <- identifier'   -- Register write 2
		; wstring "._write"
		; wchar '('
		; exp <- expression  
		; wchar ')'
		; semi
		; return $ Write (ID name) exp (concat atts)
		})
	<|>try(do{ atts <- many statementAttributeList
			; name <- idPath 
		; exp <- many1 (do{ wchar '(' <|> wchar ',' 
				; ex <- expression -- ### 
				; return ex
				})
		; wchar ')'
		; semi
		; return $ MethodCall (allButLastID name) (getLastID name) exp (concat atts)
		})
	<|>try(do{ atts <- many statementAttributeList 
		; name <- idPath
		; wchar '('
		; wchar ')'
		; semi
		; return (MethodCall (allButLastID name) (getLastID name) [] (concat atts))
		})
	<|>try(do{ atts <- many statementAttributeList 
		; name <- idPath  
		; semi
		; return (MethodCall (allButLastID name) (getLastID name) [] (concat atts))
		})
	<|>try(do{ atts <- many statementAttributeList
				; name <- identifier'
		; semi
		; return $ ActionCall name (concat atts)
		})
	<|>try(do{ char '$' 
		; many (noneOf ";")
		; semi
		; return $ Void
		}) 
	<|>try(do{ atts <- many statementAttributeList
				; reserved "return"
				; whiteSpace
		; exp <- expression
		; semi
		; return $ Return exp (concat atts)
		})
	<|>try(do{ atts <- many statementAttributeList
				; reserved "return"
				; whiteSpace
				; typ <- bsvType
				; whiteSpace
				; wchar '{'
		; fields <- xintercalatedBy "," True recordStatement 
		; wchar '}'
		; semi
		--; error "!"
		; return $ StructReturn typ fields (concat atts)
		})
		<|>try(do{ atts <- many statementAttributeList
				; reserved "if"    
				; wchar '('
				; guard <- expression 
				; wchar ')' 
				; thn <- statement
				; whiteSpace 
				; recElse <- recursiveElses 
				; return $ If guard thn recElse (concat atts)
				})
		<|>try(do{ atts <- many statementAttributeList
				; reserved "if"    
				; wchar '('
				; guard <- expression 
				; wchar ')' 
				; thn <- statement
				; whiteSpace 
				; wstring "else" 
				; els <- statement 
				; return $ If guard thn els (concat atts)
				})
	<|>try(do{ atts <- many statementAttributeList
				; reserved "if"    
				; wchar '('
				; matching <- idPath
				; whiteSpace
				; reserved "matches"
				; whiteSpace
				; reserved "tagged"
				; wstring "Valid"
				; whiteSpace
				; wchar '.'
				; lambda <- idPath
				; wchar ')' 
				; thn <- statement
				; recElse <- recursiveElses
				; return $ PMatchIf matching lambda thn recElse (concat atts)
				})
	<|>try(do{ atts <- many statementAttributeList
				; reserved "if"    
				; wchar '('
				; matching <- idPath
				; whiteSpace
				; reserved "matches"
				; whiteSpace
				; reserved "tagged"
				; wstring "Valid"
				; whiteSpace
				; wchar '.'
				; lambda <- idPath
				; wchar ')' 
				; thn <- statement
				; return $ PMatchIf matching lambda thn (Void) (concat atts)
				})
		<|>try(do{ atts <- many statementAttributeList
				; reserved "if"
				; wchar '('
				; guard <- expression 
				; wchar ')' 
				; thn <- statement
				; return $ If guard thn (Void) (concat atts)
				})
		<|>try(do{ atts <- many statementAttributeList
				; reserved "for"
				; wchar '('
				; inst <- many1 uStatement
				; semi
				; guard <- expression
				; inc <- many1 uStatement
				; wchar ')'
				; stmt <- statement
				; return $ ForLoop inst guard inc stmt (concat atts)
				})
		<|>try(do{ atts <- many statementAttributeList
				; reserved "case"
				; wchar '(' 
				; guard <- expression 
				; wchar ')' 
				; reserved "matches"
				; cases <- many bsvCase 
				; reserved "endcase"
				; return $ Switch guard (concat cases) (concat atts)
				})
		<|>try(do{ atts <- many statementAttributeList
				; reserved "case"
				; wchar '(' 
				; guard <- expression 
				; wchar ')' 
				; cases <- many bsvCase 
				; reserved "endcase"
				; return $ Switch guard (concat cases) (concat atts)
				})
		<|>try(do{ atts <- many statementAttributeList
				; typ <- bsvType 
				; whiteSpace
				; i <- idPath
				; wchar '='
				; exp <- expression
				; semi
				; return $ LocalDec ((i, (Left (Just typ)), exp):[]) Void (concat atts)
				})
	<|>try(do{ atts <- many statementAttributeList
				; reserved "let" 
				; whiteSpace
				; i <- idPath
				; wchar '='
				; exp <- expression
				; semi
				; return $ LocalDec ((i, (Left Nothing), exp):[]) (Void) (concat atts)
				})
		<|>try(do{ atts <- many statementAttributeList
				; whiteSpace
				; i <- idPath
				; wchar '='
				; exp <- expression
				; semi
				; return $ LocalDec ((i, (Left Nothing), exp):[]) (Void) (concat atts)
				})
		<|>  do { atts <- many statementAttributeList
				; reserved "let"
				; name <- identifier'
				; wstring "<-"
				; exp <- expression
				; semi
				; return $ LocalDec (((ID name), (Left Nothing), exp):[]) Void (concat atts)
				}
	<?> "statement"	

getLastID :: ID_Path -> String
getLastID (ID x) = x
getLastID (ID_Submod_Struct x y) = getLastID y

allButLastID :: ID_Path -> ID_Path
allButLastID (ID_Submod_Struct x (ID_Submod_Struct y z)) = (ID_Submod_Struct x (allButLastID (ID_Submod_Struct y z)))
allButLastID (ID_Submod_Struct x (ID y)) = (ID x)
	
recursiveElses :: Parser Statement
recursiveElses = try ( do { atts <- many statementAttributeList
					; reserved "else" ; whiteSpace
					; reserved "if"
					; wchar '('
					; guard <- expression 
					; wchar ')' 
					; thn <- statement
					; whiteSpace 
					; recElse <- recursiveElses 
					; return $ If guard thn recElse (concat atts)
					})
	<|>try(do{ atts <- many statementAttributeList
				; reserved "else" ; whiteSpace ; reserved "if"    
				; wchar '('
				; matching <- idPath
				; whiteSpace
				; reserved "matches"
				; whiteSpace
				; reserved "tagged"
				; wstring "Valid"
				; whiteSpace
				; wchar '.'
				; lambda <- idPath
				; wchar ')' 
				; thn <- statement
				; whiteSpace 
				; recElse <- recursiveElses
				; return $ PMatchIf matching lambda thn recElse (concat atts)
				})
			<|>  do { reserved "else" 
					; els <- statement 
					; return els
					}
	
recordStatements :: Parser [(String, Expression)]
recordStatements = try(do { x <- recordStatement ; whiteSpace
			; wchar ','
			; xs <- recordStatements
			; return (x:xs)
					}) 
				<|> do { x <- recordStatement ; whiteSpace
					; return (x:[])
					} <?> "List of Record Statements"
	

	
recordStatement :: Parser (String, Expression)
recordStatement = do { x <- identifier' 
			; char ':'
			; whiteSpace 
			; exp <- expression
			; return (x, exp)
			} <?> "Record Statement"

makeStatement :: ID_Path -> (String, Expression) -> [StatementAttribute] -> Statement
makeStatement i (f, exp) atts = (Write (insertIdAtEnd i f) exp atts)

insertIdAtEnd :: ID_Path -> String -> ID_Path
insertIdAtEnd (ID_Submod_Struct m p) i = (ID_Submod_Struct m (insertIdAtEnd p i))

insertIdAtEnd (ID x) i = (ID_Submod_Struct x (ID i))
			
uStatements :: Parser [UStatement]
uStatements = try ( do { x <- uStatement 
					; wchar ',' 
					; xs <- uStatements
					; return $ x : xs
					})
				<|> do { x <- uStatement
					; semi
					; return (x : [])
					}
				<?> "Uninterpreted Statement List"

uStatement :: Parser UStatement
uStatement = try ( do { x <- uType
					; nom <- identifier'
					; wchar '='
					; lit <- literalParser
					; return $ DeclAssign x nom lit
					} ) 
			<|>  do { nom <- identifier'
					; wchar '='
					; lit <- literalParser
					; return $ UAssign nom lit
					} 
			<?> "Uninterpreted Statement"

uType :: Parser UType 
uType = try ( do { wstring "int"
				; return U_Int
				})
		<|>  do { wstring "string"
				; return U_String
				}

bsvCase :: Parser [Case]
bsvCase = do { lits <- litList 
			--; error $ show $ lits
			; stmt <- statement
			; return $ map (\ x -> (x, stmt) ) lits
			}

litList :: Parser [Literal]
litList = try ( do { x <- literalParser
				; wchar ','
				; xs <- litList
				; return (x:xs)
				})
			<|> do { x <- literalParser
		; whiteSpace
				; wchar ':'
				; whiteSpace
				; return (x:[])
				}
			<?> "List of case selection literals in case statement block"

-- | uses Parsec's built-in expression parsing tools to build an expression parser for BSV.
expression :: Parser Expression
expression = buildExpressionParser table subExpr
		<?> "expression"


-- | expression parsing table.  Specifies valid operators, declares precedence, associativity, and gives the transformed PVS syntax for the corresponding operation.  In general, the correspondance is very close, with noteable exceptions in == (=), != (/=), && (AND), || (OR), / (div(,)), and % (mod(,)). 
table = [ [ prefix " " '@' id
	, postfix " " '@' id 
	, prefix "\t" '@' id
	, postfix "\t" '@' id
	]
	, [ prefix "-" '@' (\ x -> (Negative x)) -- prefixes
	, prefix "+" '@' id
	, prefix "!" '@' (\ x -> (Not x)) 
	]  
	, [ binary "==" '@' (\ x y -> (Equals x y)) AssocLeft
		, binary "!=" '@' (\ x y -> (NotEquals x y)) AssocLeft
	, binary ">=" '@' (\ x y -> (GreaterEquals x y)) AssocLeft
	, binary "<=" '@' (\ x y -> (LessEquals x y)) AssocLeft
	, binary ">" '=' (\ x y -> (Greater x y)) AssocLeft
		, binary "<" '=' (\ x y -> (Less x y)) AssocLeft -- comparators
	]
	, [ binary "&&" '@' (\ x y -> (And x y)) AssocLeft -- boolean
	, binary "||" '@' (\ x y -> (Or x y)) AssocLeft 
	]
		, [ binary "&" '@' (\ x y -> (BitwiseAND x y)) AssocLeft -- bitwise
		, binary "|" '@' (\ x y -> (BitwiseOR x y)) AssocLeft
		, binary "^" '@' (\ x y -> (BitwiseXOR x y)) AssocLeft
		]
	, [ binary "*" '@' (\ x y -> (Multiply x y)) AssocLeft -- arithmetic distributive
	, binary "/" '@' (\ x y -> (Divide x y)) AssocLeft
	, binary "%" '@' (\ x y -> (Modulo x y)) AssocLeft
	] 
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


-- | parses a sub-expression, interacting very closely with the expression parser. Parses subexpressions of variable redundancy, such as redundant brackets, prefixed "!" indicating boolean "not", prefixed "- x" abbreviating "0 - x", redundant prefixed "+", value method calls, register identifier's, and literals.  
subExpr :: Parser Expression
subExpr = try(do{ wchar '(' ; g <- expression ; wchar ')'-- Turnary Expression
				; wchar '?'
		; t <- expression
		; wchar ':'
		; e <- expression
		; return (Exp_If g t e) 
		}) 
	<|>try(do{ x <- idPath -- Ranged Bit Selection Operator 1 (Binary operation)
			; wchar '['
			; y <- expression
			; wchar ':'
			; z <- expression
			; wchar ']'
		; whiteSpace
		; return (BitSelectRange (Identifier x) y z)
		})
	<|>try(do{ x <- idPath -- Bit Selection Operator 1 (Binary operation)
			; wchar '['
			--; error $ (show x)
			; y <- expression
			--; error "Boink"
			; wchar ']'
			--; error $ (show x) ++"  "++ (show y) 
		; return (BitSelect (Identifier x) (y))
		})
	<|>try(do{ reserved "matches" -- PMatch ID 
		; reserved "tagged"
		; x <- maybeID
		; return ( PMatch x )
		}) 
	<|>try(do{ wchar '{' -- Bit Concatenation Operator (n-ary operation)
		; concatenands <- xintercalatedBy "," True expression
		; wchar '}'
		; return ( BitConcat concatenands)
		})
	<|>try(do{ whiteSpace -- redundant brackets
		; wchar '('
		; whiteSpace
			; x <- expression
		; whiteSpace
			; wchar ')' 
		; return x 
		}) 
	<|>try(do{ whiteSpace -- prefixed "!" with parentheses
		; wstring "!("
		; whiteSpace
			; x <- expression
		; whiteSpace
			; wchar ')' 
		; return ( Not x )
		})
	<|>try(do{ whiteSpace -- prefixed "-"
		; wstring "-("
		; whiteSpace
			; x <- expression
		; whiteSpace
			; wchar ')' 
		; return ( Negative x )
		})
	<|>try(do{ whiteSpace -- prefixed "+"
		; wstring "+("
		; whiteSpace
			; x <- expression
		; whiteSpace
			; wchar ')' 
		; return x
		})
	<|>try(do{ whiteSpace -- prefixed "!" without parentheses
		; wstring "!"
		; whiteSpace
			; x <- expression
		; whiteSpace
		; return ( Not x )
		})
	<|>try(do{ whiteSpace -- prefixed "!" without parentheses
		; wstring "defaultValue"
		; return (Literal LitStructConstructor)
		})
	<|>try(do{ whiteSpace -- "tagged"
		; reserved "tagged" 
		; whiteSpace
			; x <- maybee
		; whiteSpace
		; return (Tagged Nothing x)
		})
	<|>try(do{ whiteSpace -- Struct Cluster
		; typ <- bsvType
		; wchar '{'
		; fs <- defaultDeclarationList
		; wchar '}'
		; return (StructCluster (Left typ) fs)
		})
	<|>try(do{ wstring "fromMaybe"
		; wchar '('
		; x <- expression
		; wchar ','
		; i <- idPath
		; wchar ')'
		; return $ FromMaybe i x
			})
	<|>try(do{ i <- identifier' -- Value Method call
				; char '.'
		; m <- identifier'
		; args <- methodArgs
		; return (Exp_MethodCall (ID i) m args Nothing) -- $ "@" ++ i ++ "`" ++ m ++ (if null args then "" else "(" ++ (intercalate "," args) ++ ")")
		})
	<|>try(do{ i <- identifier' 
		; char '.'
		; i' <- identifier'
		; char '.'
		; m <- identifier'
		; args <- methodArgs
		; return (Exp_MethodCall (ID_Submod_Struct i (ID i')) m args Nothing)
	})
	<|>try(do{ f <- identifier' -- Function call
				; args <- methodArgs
		; return (Exp_FunctionCall f args) -- $ "@" ++ i ++ "`" ++ m ++ (if null args then "" else "(" ++ (intercalate "," args) ++ ")")
		})
	<|>try(do{ x <- idPath -- register reads
		; return (Identifier x)
		})
	<|> literalParser -- literals (see LiteralLexer.hs)

	<?> "simple expression"

maybee :: Parser MaybeTag
maybee = try (do { wstring "Valid" 
		; whiteSpace      
		; exp <- expression
		; return (Valid exp)
		}) 
	<|> do { wstring "Invalid"
		; return (Invalid)
			} <?> "Maybe Tagging"
			
maybeID :: Parser MaybeIDTag
maybeID = try (do { wstring "Valid" 
		; whiteSpace    
		; char '.'
		; i <- idPath
		; return (ValidID i)
		}) 
	<|> do { wstring "Invalid"
		; return (InvalidID)
			} <?> "Maybe ID Tagging"            
	
idPath :: Parser ID_Path 
idPath = try(do{ i <- identifier'
			; char '.'
			; q <- idPath 
			; return (ID_Submod_Struct (removePrependedUnderscores i) q)
			} )
	<|>try(do{ i <- identifier' 
			; wchar '[' 
			; index <- expression 
			; wchar ']' 
			; return $ ID_Vect (removePrependedUnderscores i) index 
			})
	<|>    do{ i <- identifier' 
			; return $ ID (removePrependedUnderscores i)
			}

removePrependedUnderscores :: String -> String 
removePrependedUnderscores ('_':xs) = removePrependedUnderscores xs
removePrependedUnderscores xs = xs

-- | parses the arguments of a method call.  Enclosed by parentheses and deliniated by commas, these arguments are expressions in the most recursive sense, and may even inclue other method calls (and any other generally valid expression).  In this particular implementation, sets of arguments are parsed separately, depending on whether they contain many, one, or no expressions.
methodArgs :: Parser [Expression]
methodArgs =   try ( do { wchar '(' -- w/ args
			; xs <- many ( try ( do { x <- expression
					; wchar ',' 
					; return x 
					} ) 
					<|> do 	  { x <- expression
					; return x
					} ) 
			; wchar ')'
			; return (xs)
			} )
	<|> 	try( do { wchar '(' 
			; x <- expression
			; wchar ')'
			; return ((x:[]))
			} )
		<|> do { wstring "()" -- empty parens 
			; return []
			} <?> "Method Args List"

-- | parses a rule declaration.  Rules consist of a name, a guard, and a block of concurrently executed statements.  A rule's guard governs execution (a rule only being executable in clock cycles for which its guard evaluates as true), but is syntactically optional.  Rules that have no guard expression specified are assumed universally permissive, and have permission to execute every clock cycle.  Whether or not a rule actually does execute every clock cycle is more complicated, as other, higher priority rules may block its execution.  
ruleDecl :: Parser RuleDec
ruleDecl =try(do{ atts <- many ruleAttributeList
		; reserved "rule"
		; name <- identifier' 
		; wchar '('
		; expr1 <- expression
		; wstring "matches"
		; wstring "tagged"
		; wstring "Valid"
		; wchar '.'
		; idf <- idPath
		; wchar ')'
		; semi
		; statementList <- many1 statement
		; reserved "endrule"
		; return (name, (IsValid (MaybeContainer expr1)), ((LocalDec ((idf, (Left Nothing), (MaybeValue expr1)):[]) (StatementBlock statementList) []):[]), (concat atts))
		})
	<|>try(do{ atts <- many ruleAttributeList
				; reserved "rule"
		; name <- identifier' 
		; semi
		; statementList <- many1 statement
		; whiteSpace
--         ; error $ show statementList
		; wstring "endrule"-- reserved "endrule"
		; whiteSpace
		; let tracy = "[ruleDecl] rule = " ++ (show name) ++ "\nstatements = " ++ (show statementList)
		; return ( name, (Literal (LitBool True)), statementList, (concat atts) )
		} )
	<|> do  { atts <- many ruleAttributeList
				; reserved "rule"
		; name <- identifier' 
		; wchar '('
		; guard <- expression
		; wchar ')'
		; semi
		; statementList <- many1 statement
		; reserved "endrule"
		; return ( name, guard, statementList, (concat atts) )
		} <?> "rule"

-- | parses the declaration of a method body, as opposed to a simple method declaration, which occurs in a module's associated interface declaration.  Method bodies may be declared with or without a guard, where the abscence of a guard is assumed as universal permissivity.  The method body consists of the identification of the method being given a body, a list of untyped arguments (which become tokens in the following statements for the expressions passed to a method in a call of the method by the containing module's supermodule.) A list of one or more statements then follows.  
methodBlockDecl :: Parser MethodBody
methodBlockDecl = try(do{ atts <- many methodBodyAttributeList
					; reserved "method"  --with if
			; r <- returnTypeDecl
			; name <- identifier'
			; args <- try(do { argos <- argList 
					; return (dropTyping argos)
					})
					<|> do { argos <- vanillaArgsList
					; return argos 
					}
			; whiteSpace
			; reserved "if"
			; guard <- expression
			; semi
			; statementList <- many1 statement
			; reserved "endmethod"
			; return ( name, r, args, guard, statementList, (concat atts))
			})
		<|> try(do{ atts <- many methodBodyAttributeList
					; reserved "method"  --with if
			; r <- returnTypeDecl
			; name <- identifier'
			; args <- try(do { argos <- argList 
					; return (dropTyping argos)
					})
					<|> do { argos <- vanillaArgsList
					; return argos 
					}
			; whiteSpace
			; reserved "if"
			; wchar '('
			; expr1 <- expression
			; wstring "matches"
			; wstring "tagged"
			; wstring "Valid"
			; wchar '.'
			; idf <- idPath
			; wchar ')'
			; semi
			; statementList <- many1 statement
			; reserved "endmethod"
			; return ( name, r, args, (IsValid (MaybeContainer expr1)), ((LocalDec ((idf, (Left Nothing), (MaybeValue expr1)):[]) (StatementBlock statementList) []):[]), (concat atts))
			})
		<|> do 	{ atts <- many methodBodyAttributeList
					; reserved "method" -- no args
			; r <- returnTypeDecl
			; name <- identifier'
					; args <- try (do { argos <- argList 
					; return (dropTyping argos)
					})
					<|> do { argos <- vanillaArgsList
					; return argos 
					}
			; semi
			; statementList <- many1 statement
			; reserved "endmethod"
			; return ( name, r, args, (Literal (LitBool True)), statementList, (concat atts))
			}
		<?> "Method Declaration"

dropTyping :: [Argument] -> UTArgs 
dropTyping [] = [] 
dropTyping ((nom, t, _):xs) = (nom, Just t) : (dropTyping xs)

-- parses a list of arguments enclosed by parentheses that do not include typing information. such argument lists are found in method body declarations, where the arguments are identifier's that tokenize expressions passed to the method during the method's call in the supermodule.  
vanillaArgsList :: Parser UTArgs
vanillaArgsList = do 	{ char '(' 
			; args <- many ( try ( do { x <- argo
					; wchar ',' 
					; return x 
					} ) 
					<|> do 	  { x <- argo
					; return x
					} )
			; char ')'
			; return args
			}
					
argo :: Parser (String, Maybe BSVType)
argo = try (do{ t <- bsvType
			; whiteSpace
			; i <- identifier'
			; return (i, Just t)
			})
		<|> do{ i <- identifier' 
			; return (i, Nothing)
			}
			
					
-- | parses a name, which may have a capitalized first character, as opposed to an identifier', which may not.
name :: Parser String
name = do { x <- (upper <|> oneOf "_")
	; xs <- many1 (upper <|> lower <|> digit <|> oneOf "_$")
	; whiteSpace
	; return (removePrependedUnderscores (x:xs))
	} <?> "Description Name"

-- | Parses the declaration of a BSV data type.  
-- | Boolean variables are parsable and self-explanatory
-- | Bit, Int and UInt represent bit vectors, integers, and unsigned integers respectively.  Each one is passed a natural number to indicate the bit width of the type where ever it is used.  One reason type synonyms are used is to hide information about the bit width of variables from the programmer.
-- | Reals are self explanatory.  BSV uses a 64 bit, IEEE 754 compliant format.
-- | The last option is a general name, which must be a declared type synonym or enumeration.  Because files being translated are assumed to have been type checked, this is in general a safe assumption. 
bsvType :: Parser BSVType
bsvType = try(do { string "Bool" -- bool
		; return (BSV_Bool)
		} )
	<|>try(do{ string "Bit#("
		; x <- literalParser
		; wchar ')'
		; return (BSV_Bit (lit2Int x))
		} )
	<|>try(do{ string "Int#("
		; x <- literalParser
		; wchar ')'
		; return (BSV_Int (lit2Int x))
		} )
	<|>try(do{ string "UInt#("
		; x <- literalParser
		; wchar ')'
		; return (BSV_UInt (lit2Int x))
		} )
	<|>try(do{ string "Maybe#("
		; x <- bsvType
		; wchar ')'
		; return (BSV_Maybe x)
		} )
	<|>try(do{ string "Float" 
		; return (BSV_Real)
		} )
	<|> do { x <- upper -- enumerations and custom types
			; xs <- many1 (upper <|> lower <|> digit <|> oneOf "_$")
			; whiteSpace
			; return (BSV_Custom (x:xs))
			} <?> "BSV Type"

lit2Int :: Literal -> Integer
lit2Int (Literal (LitInt n)) = n
lit2Int (Literal (LitSizedInt _ n)) = n
lit2Int x = error "Error! Literal is not numeric!"
	
mall :: Parser [[ModuleAttribute]] 	
mall = try ( do { x <- modAttributeList
				; whiteSpace
				; xs <- mall 
				; return (x:xs)
				} )
		<|> do { return []
				}
				
				
	
modAttributeList :: Parser [ModuleAttribute]
modAttributeList    = do { wstring "(*" 
			; args <- many(try(do{ arg <- moduleAttribute
					; wchar ','
					; return arg
					} )
				<|> ( do { arg <- moduleAttribute
					; return arg
					} ) )
			; wstring "*)" 
			; return args
			} <?> "Module Attribute List" 

stringList :: Parser String
stringList = try (do{ arg <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
			; wchar ','
			; return arg
			} )
	<|> ( do { arg <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
			; return arg
			} ) <?> "List of strings"

-- | Module Attributes
moduleAttribute :: Parser ModuleAttribute
moduleAttribute = try ( do { reserved "synthesize"
			; return (Synthesize)
			} ) 
		<|>try(do{ reserved "always_ready"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Mod_Always_Ready xs)
			})
<|>try(do{ reserved "always_ready"
			; return (Mod_Always_Ready [])
			})
		<|>try(do{ reserved "always_enabled"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Mod_Always_Enabled xs)
			})
<|>try(do{ reserved "always_enabled"
			; return (Mod_Always_Enabled [])
			})
		<|>try(do{ reserved "descending_urgency"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Descending_Urgency xs)
			})
		<|>try(do{ reserved "execution_order"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Mod_Execution_Order xs)
			})
		<|>try(do{ reserved "mutually_exclusive"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Mod_Mutually_Exclusive xs)
			})
		<|>try(do{ reserved "conflict_free"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Mod_Conflict_Free xs)
			})
		<|>try(do{ reserved "preempts"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Mod_Preempts xs)
			})
		<|>try(do{ reserved "clock_prefix"
			; wchar '='
			; wchar '\"'
			; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
			; wchar '\"'
			; return (Clock_Prefix x)
			})
		<|>try(do{ reserved "gate_prefix"
			; wchar '='
			; wchar '\"'
			; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
			; wchar '\"'
			; return (Gate_Prefix x)
			})
		<|>try(do{ reserved "reset_prefix"
			; wchar '='
			; wchar '\"'
			; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
			; wchar '\"'
			; return (Reset_Prefix x)
			})
		<|>try(do{ reserved "gate_input_clocks"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Gate_Input_Clocks xs)
			})
		<|>try(do{ reserved "gate_all_clocks"
			; return (Gate_All_Clocks)
			})
		<|>try(do{ reserved "default_clock_osc"
			; wchar '='
			; wchar '\"'
			; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
			; wchar '\"'
			; return (Default_Clock_OSC x)
			})
		<|>try(do{ reserved "default_clock_gate"
			; wchar '='
			; wchar '\"'
			; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
			; wchar '\"'
			; return (Default_Clock_Gate x)
			})
		<|>try(do{ reserved "default_gate_inhigh"
			; return (Default_Gate_Inhigh)
			})
		<|>try(do{ reserved "default_gate_unused"
			; return (Default_Gate_Unused)
			})
		<|>try(do{ reserved "default_reset" 
			; wchar '='
			; wchar '\"'
			; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
			; wchar '\"'
			; return (Default_Reset x)
			})
		<|>try(do{ reserved "no_default_reset"
			; return (No_Default_Reset)
			})
		<|>try(do{ reserved "clock_family"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Clock_Family xs)
			})
		<|>try(do{ reserved "clock_ancestors"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'			
			; return (Clock_Ancestors xs)
			})
		<|> ( do{ reserved "doc"
			; wchar '='
			; wchar '\"'
			; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_, " )
			; wchar '\"'
			; return (Mod_Doc x)
			}) <?> "Module Attribute"

interfaceAttributeList :: Parser [InterfaceAttribute]
interfaceAttributeList    = do { wstring "(*" 
			; args <- many(try(do{ arg <- interfaceAttribute
					; wchar ','
					; return arg
					} )
				<|> ( do { arg <- interfaceAttribute
					; return arg
					} ) )
			; wstring "*)" 
			; return args
			} <?> "Interface Attribute List" 

interfaceAttribute :: Parser InterfaceAttribute
interfaceAttribute   = try ( do { reserved "always_ready"
				; return (Int_Always_Ready)
				} ) 
			<|>try(do{ reserved "always_enabled"
				; return (Int_Always_Enabled)
				})
			<|> do { reserved "doc"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_, " )
				; wchar '\"'			
				; return (Int_Doc x)
				} <?> "Interface Attribute"

argumentAttributeList :: Parser [ArgumentAttribute]
argumentAttributeList    = do { wstring "(*" 
			; args <- many(try(do{ arg <- argumentAttribute
					; wchar ','
					; return arg
					} )
				<|> ( do { arg <- argumentAttribute
					; return arg
					} ) )
			; wstring "*)" 
			; return args
			} <?> "Argument Attribute List" 

argumentAttribute :: Parser ArgumentAttribute
argumentAttribute    = try ( do { reserved "ready"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
				; wchar '\"'
				; return (Arg_Ready x)
				} ) 
			<|>try(do{ reserved "enable"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
				; wchar '\"'
				; return (Arg_Enable x)
				} )
			<|>try(do{ reserved "result"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
				; wchar '\"'
				; return (Arg_Result x)
				} )
			<|>try(do{ reserved "prefix"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
				; wchar '\"'
				; return (Arg_Prefix x)
				} )
			<|>try(do{ reserved "port"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
				; wchar '\"'
				; return (Arg_Port x)
				} )
			<|>try(do{ reserved "always_ready"
				; return (Arg_Always_Ready)
				} )
			<|>try(do{ reserved "always_enabled"
				; return (Arg_Always_Enabled)
				} )
			<|> do { reserved "doc"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_, " )
				; wchar '\"'			
				; return (Arg_Doc x )
				} <?> "Argument Attribute"

methodDecAttributeList :: Parser [MethodDecAttribute]
methodDecAttributeList    = do { wstring "(*" 
			; args <- many(try(do{ arg <- methodDecAttribute
					; wchar ','
					; return arg
					} )
				<|> ( do { arg <- methodDecAttribute
					; return arg
					} ) )
			; wstring "*)" 
			; return args
			} <?> "Module Attribute List" 

methodDecAttribute :: Parser MethodDecAttribute
methodDecAttribute   = try ( do { reserved "ready"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
				; wchar '\"'
				; return (MDc_Ready x)
				} ) 
			<|>try(do{ reserved "enable"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
				; wchar '\"'
				; return (MDc_Enable x)
				} )
			<|>try(do{ reserved "result"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
				; wchar '\"'
				; return (MDc_Result x)
				} )
			<|>try(do{ reserved "prefix"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
				; wchar '\"'
				; return (MDc_Prefix x)
				} )
			<|>try(do{ reserved "port"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_ " )
				; wchar '\"'
				; return (MDc_Port x)
				} )
			<|>try(do{ reserved "always_ready"
				; return (MDc_Always_Ready)
				} )
			<|>try(do{ reserved "always_enabled"
				; return (MDc_Always_Enabled)
				} )
			<|> do { reserved "doc"
				; wchar '='
				; wchar '\"'
				; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_, " )
				; wchar '\"'			
				; return (MDc_Doc x)
				} <?> "Method Declaration Attribute"

actionAttributeList :: Parser [ActionAttribute]
actionAttributeList    = do { wstring "(*" 
			; args <- many(try(do{ arg <- actionAttribute
					; wchar ','
					; return arg
					} )
				<|> ( do { arg <- actionAttribute
					; return arg
					} ) )
			; wstring "*)" 
			; return args
			} <?> "Action Attribute List" 

actionAttribute :: Parser ActionAttribute
actionAttribute  = do { reserved "doc"
			; wchar '='
			; wchar '\"'
			; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_, " )
			; wchar '\"'			
			; return (Act_Doc x)
			} <?> "Action Attribute"

ruleAttributeList :: Parser [RuleAttribute]
ruleAttributeList  = do { wstring "(*" 
			; args <- many(try(do{ arg <- ruleAttribute
					; wchar ','
					; return arg
					} )
				<|> ( do { arg <- ruleAttribute
					; return arg
					} ) )
			; wstring "*)" 
			; return args
			} <?> "Rule Attribute List" 

ruleAttribute :: Parser RuleAttribute
ruleAttribute = try ( do{ reserved "fire_when_enabled"
			; return (Fire_When_Enabled)
			} ) 
		<|>try(do{ reserved "no_implicit_conditions"
			; return (No_Implicit_Conditions)
			} )
		<|>try(do{ reserved "descending_urgency"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Rul_Descending_Urgency xs)
			} )
		<|>try(do{ reserved "execution_order"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Rul_Execution_Order xs)
			} )
		<|>try(do{ reserved "mutually_exclusive"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Rul_Mutually_Exclusive xs)
			} )
		<|>try(do{ reserved "conflict_free"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Rul_Conflict_Free xs)
			} )
		<|>try(do{ return "preempts"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Rul_Preempts xs)
			} )
		<|> do  { reserved "doc"
			; wchar '='
			; wchar '\"'
			; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_, " )
			; wchar '\"'			
			; return (Rul_Doc x)
			}

methodBodyAttributeList :: Parser [MethodBodyAttribute]
methodBodyAttributeList    = do { wstring "(*" 
			; args <- many(try(do{ arg <- methodBodyAttribute
					; wchar ','
					; return arg
					} )
				<|> ( do { arg <- methodBodyAttribute
					; return arg
					} ) )
			; wstring "*)" 
			; return args
			} <?> "MethodBody Attribute List" 


methodBodyAttribute :: Parser MethodBodyAttribute
methodBodyAttribute = do{ reserved "doc"
			; wchar '='
			; wchar '\"'
			; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_, " )
			; wchar '\"'			
			; return (Met_Doc x)
					}

statementAttributeList :: Parser [StatementAttribute]
statementAttributeList    = do { wstring "(*" 
			; args <- many(try(do{ arg <- statementAttribute
					; wchar ','
					; return arg
					} )
				<|> ( do { arg <- statementAttribute
					; return arg
					} ) )
			; wstring "*)" 
			; return args
			} <?> "Statement Attribute List" 

statementAttribute :: Parser StatementAttribute
statementAttribute = try ( do{ reserved "split"
			; return (Split)
			} ) 
		<|>try(do{ reserved "nosplit"
			; return (NoSplit)
			} )
		<|>try(do{ reserved "descending_urgency"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Sta_Descending_Urgency xs)
			} )
		<|>try(do{ reserved "execution_order"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Sta_Execution_Order xs)
			} )
		<|>try(do{ reserved "mutually_exclusive"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Sta_Mutually_Exclusive xs)
			} )
		<|>try(do{ reserved "conflict_free"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Sta_Conflict_Free xs)
			} )
		<|>try(do{ return "preempts"
			; wchar '='
			; wchar '\"'
			; xs <- many stringList
			; wchar '\"'
			; return (Sta_Preempts xs)
			} )
		<|> do  { reserved "doc"
			; wchar '='
			; wchar '\"'
			; x <- many1 ( upper <|> lower <|> digit <|> oneOf "$_, " )
			; wchar '\"'			
			; return (Sta_Doc x)
			}

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

-- Char is the terminal character
listWithCommas :: Parser [String]
listWithCommas = do { args <- many (try ( do { arg <- name
				; wchar ','
				; return arg
				} )
			<|>   do { arg <- name
				; return arg
				} )
			; return args
			}

killActionVoids :: ActionDec -> ActionDec
killActionVoids (name, statements, attributes) = (name, (killVoids statements), attributes )

killRuleVoids :: RuleDec -> RuleDec
killRuleVoids (name, guard, statements, attributes) = (name, guard, (killVoids statements), attributes )

killMethodVoids :: MethodBody -> MethodBody
killMethodVoids (name, typ, args, guard, statements, attributes) = (name, typ, args, guard, (killVoids statements), attributes)

killVoids :: [Statement] -> [Statement]
killVoids [] = []
killVoids ((Void):xs) = killVoids xs
killVoids (x:xs) = x : (killVoids xs)

identifier' :: Parser String
identifier' = do { x <- identifier
				; return (removePrependedUnderscores x)
				}

