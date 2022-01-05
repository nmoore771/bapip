{-# OPTIONS_GHC -fno-warn-tabs #-}

module TSP2BSV where

import LexerTypes
import Data.Bits
import Data.List (nub, intersect, subsequences, delete)
import BSV2PVS (applyReplacementExp, rmRF) 
import PVSGenerator (showPVSExpression)

tsp2bsv :: TSPpackage -> BSVPackage
tsp2bsv tsp = BSVPackage 
{ bsv_packageName = nom
, imports = "DefaultValue":[]
, including = if (not ( null (macros tsp))) then (nom ++ ".defines"):[] else []
, interfaces = (genInterface tds nom vars teaspoons):[]
, bsv_constants = []
, bsv_typedefs = (map p2bTypeDef tds)
, bsv_modules = (genModule tds nom vars teaspoons):[]
, bsv_instDefs = map id $ defInsts tsp
, bsv_functions = map p2bFunc funcs
, bsv_macros = map p2bMacro $ macros tsp
, hexFiles = []
}
where
	nom = tName tsp
	vars = cleanseVars (varDecs tsp) teaspoons
	teaspoons = tsps tsp
	tds = typedefs tsp
	funcs = tsp_funcs tsp 

p2bMacro :: PVSMacro -> BSVMacro 
p2bMacro (n, _, lit) = (SimpleMacro n (showPVSExpression emptyPackage [] Nothing Nothing lit Nothing ""))

emptyPackage :: PVSPackage 
emptyPackage = PVSPackage  	{ pvs_packageName = "null"
				, pvs_constants = []
				, pvs_typedefs = []
				, transitions = []
				, pvs_state = [] 
				, pvs_instantiations = []
				, pvs_functions = []
				}

p2bFunc :: PVSFunction -> BSVFunction     
p2bFunc (nom, args, typ, exp) = ( nom
, (map p2bArg args)
, (p2bType typ)
, ((constructReturnStatement exp):[])
)

constructReturnStatement :: Expression -> Statement
constructReturnStatement (Exp_If x y z) = (If x (constructReturnStatement y) (constructReturnStatement z) [])
constructReturnStatement x = (Return x [])

p2bArg :: PVSArgument -> Argument 
p2bArg (nom, typ) = (nom, p2bType typ, [])

p2bTypeDef :: PVSTypeDef -> BSVTypeDef
p2bTypeDef (PVS_Synonym nom typ) = (BSV_Synonym nom (p2bType typ))
p2bTypeDef (PVS_Enumeration nom enums) = (BSV_Enumeration nom enums)
p2bTypeDef (PVS_Struct nom fields) = (BSV_Struct nom (map (\ (x,y)-> (x, (p2bType y))) fields))
	
cleanseVars :: [TVarDec] -> [TSPTable] -> [TVarDec]
cleanseVars [] _ = []
cleanseVars ((ns, typ):xs) tab = if (tCheck) 
			then cleanseVars xs tab
			else if (null (new ns)) 
			then cleanseVars xs tab
			else (((new ns), typ) : cleanseVars xs tab)
where                  
	tCheck = typ == (PVS_Custom "tick")
	new x = filter (isIO) x 
	isIO x = x `elem` (ioVarsList tab) 
	
ioVarsList :: [TSPTable] -> [String]
ioVarsList [] = []
ioVarsList ((_, y, _, _, ys, _):xs) = ( (showIDPath (fst y)) : ys) ++ (ioVarsList xs)
	
genInterfaces :: [PVSTypeDef] -> String -> [TVarDec] -> [TSPTable] -> [InterfaceDec]
genInterfaces p nom var tab = (x : [])
where
	x = genInterface p nom var tab
	
genInterface :: [PVSTypeDef] -> String -> [TVarDec] -> [TSPTable] -> InterfaceDec
genInterface p nom var tab = (nom, (genMethDecs p var tab), [], [])

genMethDecs :: [PVSTypeDef] -> [TVarDec] -> [TSPTable] -> [MethodDec]
genMethDecs p var tab = (inputMeth : outputMeths)
where 
	inputMeth = genInputMeth p var tab
	outputMeths = genOutputMeths p var tab

genInputMeth :: [PVSTypeDef] -> [TVarDec] -> [TSPTable] -> MethodDec
genInputMeth p var tab = ("set_Inputs", Action, args, [])  
where
	args = genArgs p var (gatherInputs' tab)

genOutputMeths :: [PVSTypeDef] -> [TVarDec] -> [TSPTable] -> [MethodDec]
genOutputMeths _ _ [] = []
genOutputMeths p var (x:xs) = (genOutputMeth p var x) : (genOutputMeths p var xs)

genOutputMeth :: [PVSTypeDef] -> [TVarDec] -> TSPTable -> MethodDec
genOutputMeth p var (_,x,_,_,_,_) = (nom, (Value typ), [], [])
where 
	nom = "get_" ++ (showIDPath (fst x))
	typ = p2bType $ findType p var (fst x)

gatherInputs' :: [TSPTable] -> [String]
gatherInputs' xs = crush (map showIDPath outputVars) $ nub $ concat $ gatherInputs xs 
where 
	outputVars = nub $ gatherOutputs xs

crush :: (Eq a) => [a] -> [a] -> [a]
crush xs ys = if (crushed == ys) then ys else crush xs crushed 
where 
	crushed = foldl (\ y x -> delete x y) ys xs  
	
gatherInputs :: [TSPTable] -> [[String]]
gatherInputs [] = []
gatherInputs (x:xs) = ( ( (\ (_,(y, _),_,_,ys,_) -> filter (inputNEQ y) ys) x ) : (gatherInputs xs))

inputNEQ :: ID_Path -> String -> Bool
inputNEQ x y = not (inputEQ x y)

inputEQ :: ID_Path -> String -> Bool
inputEQ (ID x) y = x == y
inputEQ (ID_Vect x n) y = x == y
inputEQ (ID_Submod_Struct m x) y = m == y

gatherOutputs :: [TSPTable] -> [ID_Path]
gatherOutputs xs = map (\ (_,y,_,_,_,_) -> fst y) xs

genArgs :: [PVSTypeDef] -> [TVarDec] -> [String] -> [Argument]
genArgs _ _ [] = []
genArgs p var (x:xs) = (x, (p2bType (findType p var (ID x))), []) : (genArgs p var xs)

findType :: [PVSTypeDef] -> [TVarDec] -> ID_Path -> PVSType
findType _ [] nom = error $ "Error! variable \"" ++ (showIDPath nom) ++ "\" has no corresponding variable declaration!"
findType d (x:xs) (ID nom) = if (nom `elem` (fst x)) then snd x else findType d xs (ID nom)
findType d (x:xs) (ID_Vect nom n) = if (nom `elem` (fst x)) then snd x else findType d xs (ID_Vect nom n)
findType d (x:xs) (ID_Submod_Struct nom next) =  if (nom `elem` (fst x)) -- if found, dig out the record type, and search records for the correct subtype.
then findTypeInRecord d d (snd x) next
else findType d xs (ID_Submod_Struct nom next) 

findTypeInRecord :: [PVSTypeDef] -> [PVSTypeDef] -> PVSType -> ID_Path -> PVSType
findTypeInRecord p ((PVS_Synonym _ _):xs) y z = findTypeInRecord p xs y z
findTypeInRecord p ((PVS_Enumeration _ _):xs) y z = findTypeInRecord p xs y z
findTypeInRecord p ((PVS_Struct nom felds):xs) (PVS_Custom y) z = if (nom == y) 
then findTypeInFields p felds z 
else findTypeInRecord p xs (PVS_Custom y) z
findTypeInRecord p ((PVS_Struct nom felds):xs) _ z = error $ "Error! Trying to match a non-custom data type with record profiles!"

findTypeInFields :: [PVSTypeDef] -> [PVS_Field] -> ID_Path -> PVSType
findTypeInFields p f (ID nom) = deMaybe $ lookup nom f
findTypeInFields p f (ID_Vect nom n) = deMaybe $ lookup nom f
findTypeInFields p f (ID_Submod_Struct nom path) = findTypeInRecord p p (deMaybe $ lookup nom f) path 
	
deMaybe :: Maybe a -> a
deMaybe (Nothing) = error "Nothing found in structure lookup"
deMaybe (Just x) = x 

genModule :: [PVSTypeDef] -> String -> [TVarDec] -> [TSPTable] -> BSVModuleDec
genModule p nom var tab = BSVModuleDec
{ mName = "mk" ++ nom
, instanceName = "root"
, instances = []
, interfaceName = nom
, interfaceDecs = []
, attributes = []
, state = genStates' p var tab
, actions = []
, rules = genRules' var tab
, methods = genMethods' p var tab
}


-----------------------------VERSION 1-------------------------------------------------------------------------------------------------
{- There are two ways I can currently think of to map teaspoons into modules. 

Approach 1: 

Both inputs and outputs are registered.  The input method writes to all the input regs, and rules determine the values of the output regs.  
This means you need 2 clock cycles to calculate data (1 to register the inputs and 1 to register the outputs).

Approach 2: 

Only outputs are registered.  The input method calculates the output regs directly and pushes the data.  You save a clock cycle this way, 
but readability goes way down.  

-}

----------------------------Approach 1---------------------------------
-- genStates :: [TVarDec] -> [TSPTable] -> [BSVstateDec]
-- genStates [] _ = []
-- genStates (x:xs) tab = (genStatesFromVar x tab) ++ (genStates xs tab)
-- 
-- genStatesFromVar :: TVarDec -> [TSPTable] -> [BSVstateDec]
-- genStatesFromVar ( [] , _ ) _ = []
-- genStatesFromVar ((x:xs), typ) tab = (BSV_Reg x (p2bType typ) ini) : (genStatesFromVar (xs, typ) tab)
--   where 
--     ini = if (x `elem` (map (\ (_,x,_,_,_,_) -> x) tab) )
-- 	      then fetchInit x tab
-- 	      else kludgeInit typ
-- 
fetchInit :: [PVSTypeDef] -> PVSType -> String -> [TSPTable] -> Expression
fetchInit [] t x [] = error $ "Error! This error should be impossible!" 
fetchInit [] t x ((_,y,z,_,_,_):ys) = if (x == showIDPath' (fst y)) 
			then z
else fetchInit [] t x ys 
fetchInit ((PVS_Synonym _ _):ts) t x tabs = fetchInit ts t x tabs 
fetchInit ((PVS_Enumeration _ _):ts) t x tabs = fetchInit ts t x tabs 
fetchInit ((PVS_Struct y _):ts) (PVS_Custom t) x tabs =  if (t == y) then (Literal LitStructConstructor) else fetchInit ts (PVS_Custom t) x tabs
fetchInit ((PVS_Struct y _):ts) t x tabs =  fetchInit ts t x tabs 
			
kludgeInit :: PVSType -> Literal
kludgeInit (PVS_Bool) = (Literal (LitBool False))
kludgeInit (PVS_Bit n) = (Literal (LitSizedInt n 0))
kludgeInit (PVS_Int n) = (Literal (LitSizedInt n 0))
kludgeInit (PVS_UInt n) = (Literal (LitSizedInt n 0))
kludgeInit (PVS_Real) = (Literal (LitReal 0))
kludgeInit (PVS_Custom n) = (Literal LitVoid)
-- 	     
-- genRules :: [TVarDec] -> [TSPTable] -> [RuleDec]
-- genRules _ [] = []
-- genRules var (x:xs) = (assertMutex (genRuleSet var 1 x)) ++ (genRules var xs)
-- 
-- genRuleSet :: [TVarDec] -> Int -> TSPTable -> [RuleDec]
-- genRuleSet _ _ (_,_,_,_,_, []) = []
-- genRuleSet var num (nom, vario, p, swaps, q, (x:xs)) = (genRule var num nom vario (swapStuff swaps x)) : (genRuleSet var (num + 1) (nom, vario, p, swaps, q, xs))
-- 
-- swapStuff :: [Replacement] -> TSPLine -> TSPLine
-- swapStuff [] x = x
-- swapStuff (x:xs) (y1,y2) = swapStuff xs (z1, z2)
--   where 
--     z1 = applyReplacementExp x y1
--     z2 = applyReplacementExp x y2
-- 
-- genRule :: [TVarDec] -> Int -> TName -> String -> TSPLine -> RuleDec
-- genRule vars num nom var (exp, result) = (namo, guard, stmts, [])
--   where
--     namo = nom ++ "_" ++ (show num)
--     guard = exp
--     stmts = (Write var result []) : []
-- 
-- assertMutex :: [RuleDec] -> [RuleDec]
-- assertMutex [] = []
-- assertMutex ((n, g, sts, atts):xs) = ((n,g,sts,((Rul_Mutually_Exclusive noms):atts)):xs)
--   where 
--     noms = n : ( map (\ (z,_,_,_) -> z) xs ) 
--     
-- genMethods :: [TVarDec] -> [TSPTable] -> [MethodBody]
-- genMethods var tab = input : (outputs ++ secondaryOuputs)
--   where 
--     outVars = gatherOutputs tab
--     input = genInputMethod var (filter (\x -> not (isSecondaryTable x outVars)) tab)
--     outputs = genOutputMethods var (filter (\x -> not (isSecondaryTable x outVars)) tab)
--     secondaryOuputs = genSecondaryOutputMethods var (filter (\x -> isSecondaryTable x outVars) tab) 
--     
genInputMethod :: [TVarDec] -> [TSPTable] -> MethodBody
genInputMethod vars tabs = ("set_Inputs", Action, (map (\ x -> (x ++ "_in", Nothing)) (inputs)), (Literal (LitBool True)), stmts, [])
where 
	inputs = gatherInputs' tabs
	stmts = map (\ x -> (Write (ID x) (Identifier (ID (x ++ "_in")))) []) (inputs)

genOutputMethods :: [PVSTypeDef] -> [TVarDec] -> [TSPTable] -> [MethodBody]
genOutputMethods _ _ [] = [] 
genOutputMethods p vars ((_, var, _, _, _, _):xs) = (("get_" ++ (showIDPath (fst var))), (Value typ), [], (Literal (LitBool True)), (stmt:[]), []) : (genOutputMethods p vars xs)
where
	stmt = (Return (Identifier (fst var)) [])
	typ = p2bType $ findType p vars (fst var)

----------------------------Approach 2---------------------------------

genStates' :: [PVSTypeDef] -> [TVarDec] -> [TSPTable] -> [BSVstateDec]
genStates' tds vars tabs = (genStates'' tds vars tabs') -- ((genStates'' vars tabs') ++ (genStates''' vars tabs))
where 
	outVars = gatherOutputs tabs
	tabs' = filter (\ x -> not (isSecondaryTable x outVars)) tabs

genStates'' :: [PVSTypeDef] -> [TVarDec] -> [TSPTable] -> [BSVstateDec]
genStates'' tds xs tabs = concat $ map (\ x -> genStatesFromVar' tds xs x tabs) xs

genStatesFromVar' :: [PVSTypeDef] -> [TVarDec] -> TVarDec -> [TSPTable] -> [BSVstateDec]
genStatesFromVar' tds vds ( [] , _ ) _ = []
genStatesFromVar' tds vds ((x:xs), typ) tab = if not $ null $ filter (eqStateID x) (map (\ (_,x,_,_,_,_) -> fst x) tab) -- ((ID x) `elem` (map (\ (_,x,_,_,_,_) -> fst x) tab) ) ---- <<<< 
		then (BSV_Reg (ID x) (p2bType typ) ini') : (genStatesFromVar' tds vds (xs, typ) tab)
	else (genStatesFromVar' tds vds (xs, typ) tab)
where 
	typ = fetchType x vds 
	ini = fetchInit tds typ x tab 
	ini' = if (ini == (Literal LitVoid) ) then (kludgeInit typ) else ini
	
eqStateID :: String -> ID_Path -> Bool
eqStateID x (ID y) = x == y 
eqStateID x (ID_Vect y n) = x == y
eqStateID x (ID_Submod_Struct y z) = x == y
	
genStates''' :: [TVarDec] -> [TSPTable] -> [BSVstateDec]
genStates''' vars ts = concat $ map (\ x -> gatherHistory vars x) ts

gatherHistory :: [TVarDec] -> TSPTable -> [BSVstateDec]
gatherHistory vars (_, _, _, reps, _, tspLines) = convertToRegister (extractHistoryReferences insts) vars
where 
	newTspLines = concat $ (map (\ x -> map (applyReplacementExp x) (map (fst) tspLines)) reps) ++ (map (\ x -> map (applyReplacementExp x) (map (snd) tspLines)) reps)
	insts = nub $ concat $ map (findExpressionsContaining (Exp_FunctionCall "pre" ((Identifier (ID "t")):[]))) newTspLines

extractHistoryReferences :: [Expression] -> [String]
extractHistoryReferences [] = []
extractHistoryReferences ((Exp_FunctionCall x ys):xs) = (x) : (extractHistoryReferences xs)    
extractHistoryReferences (_:xs) = (extractHistoryReferences xs)

convertToRegister :: [String] -> [TVarDec]-> [BSVstateDec]
convertToRegister [] _ = []
convertToRegister (x:xs) vars = (BSV_Reg (ID (x ++ "_prev")) (p2bType typ) ini) : (convertToRegister xs vars)
where 
	typ = fetchType x vars
	ini = kludgeInit typ

fetchType :: String -> [TVarDec] -> PVSType 
fetchType x [] = error "Error! This error should be impossible!" 
fetchType x ((zs, typ):ys) = if (x `elem` zs) then typ else fetchType x ys

genRules' :: [TVarDec] -> [TSPTable] -> [RuleDec]
genRules' _ _ = []

genMethods' :: [PVSTypeDef] -> [TVarDec] -> [TSPTable] -> [MethodBody]
genMethods' p var tab = input : (outputs ++ secondaryOuputs)
where 
	outVars = gatherOutputs tab
	input = genInputMethod' var (filter (\x -> not (isSecondaryTable x outVars)) tab)
	outputs = genOutputMethods p var (filter (\x -> not (isSecondaryTable x outVars)) tab)
	secondaryOuputs = genSecondaryOutputMethods p var (filter (\x -> isSecondaryTable x outVars) tab)  
	
genInputMethod' :: [TVarDec] -> [TSPTable] -> MethodBody
genInputMethod' vars tabs =  ("set_Inputs", Action, (map (\ x -> (x ++ "_in", Nothing)) (inputs)), (Literal (LitBool True)), stmts, [])-- error $ show $ (\(_,_,_,x,_,_) -> x) $ head tabs'''--
where 
	inputs = gatherInputs' tabs
	tabs' = map (\x -> addTimeRefReplacements x False (gatherOutputs tabs')) tabs
	tabs'' =  map (\x -> addRegRefsReps (gatherOutputs tabs') x ) tabs' -- error $ show $ map (\(_,_,_,x,_,_) -> x) $
	tabs''' = map (\x -> addInputRefs x) tabs''
	stmts = (map generateTableStatement tabs''') ++ (historyWrites vars tabs (gatherOutputs tabs') (inputs)) -- map (\ x -> (Write x (Identifier (ID (x ++ "_in")))) []) (inputs)
	
addInputRefs :: TSPTable -> TSPTable 
addInputRefs (a,b,c,reps,inputs,f) = (a,b,c,reps',inputs,f)
where
	reps' = reps ++ (map genRep inputs)
	genRep x = ((Identifier (ID x)) , (Identifier (addIn (ID x))))
	
generateTableStatement :: TSPTable -> Statement
generateTableStatement (_, reggie, _, reps, _, ((guard, result):[])) = (Write reg resultio [])
where 
	reg = fst reggie
	resultio = foldl ( \ p q -> applyReplacementExp q p ) result reps  
generateTableStatement (a, reggie, b, reps, d, ((guard, result):xs)) = (If guardia theno elso [])
where
	reg = fst reggie
	guardia = foldl ( \ p q -> applyReplacementExp q p ) guard reps 
	theno = (Write reg resultio [])
	elso = generateTableStatement (a, reggie, b, reps, d, xs)
	resultio = foldl ( \ p q -> applyReplacementExp q p ) result reps  
	
historyWrites :: [TVarDec] -> [TSPTable] -> [ID_Path] -> [String] -> [Statement]
historyWrites vars tabs outputs inputs = map (\ x -> (Write x (outExp x) [])) prevs  
where 
	deprev (ID x) = (ID (reverse $ tail $ dropWhile (/= '_') $ reverse x ))
	prevs = map (\ (BSV_Reg x _ _ ) -> x ) $ concat $ map (gatherHistory vars) tabs
	outExp x = if ((showIDPath (deprev x)) `elem` ((map showIDPath outputs) ++ inputs))
		then (Identifier ((\ (ID y) -> (ID (y ++ "_in") ) ) x))
		else (Identifier (deprev x))

addRegRefsReps :: [ID_Path] -> TSPTable -> TSPTable    
addRegRefsReps outputs (a, reg, b, reps, d, tspLines) = (a, reg, b, ((map ( \ x -> applyRepsToReps x newReps) reps) ++ newReps{-reps ++ newReps-}), d, tspLines)
where 
	newReps = map (\ x -> ((Identifier (addIn x)) , (Identifier x)) ) outputs

applyRepsToReps :: Replacement -> [Replacement] -> Replacement    
applyRepsToReps reps [] = reps
applyRepsToReps (exp1, exp2) reps = (exp1, (foldl (\ x y -> applyReplacementExp y x) exp2 reps))
	
addTimeRefReplacements :: TSPTable -> Bool -> [ID_Path] -> TSPTable 
addTimeRefReplacements (a, reg, b, reps, d, tspLines) mode outputs = (a, reg, b, (atrr reps mode outputs), d, (atrr tspLines mode outputs))

atrr :: [(Expression, Expression)] -> Bool -> [ID_Path] -> [(Expression, Expression)]
atrr x mode output = zip (foldl ( \ p q -> map rmRF ((map (applyReplacementExp q) p ))) (map fst x) xReps) (foldl ( \ p q -> map rmRF ((map (applyReplacementExp q) p ))) (map snd x) xReps)
where 
	xList = (map fst x) ++ (map snd x) 
	xRepList1 = nub $ concat $ map (findExpressionsContaining (Exp_FunctionCall "pre" ((Identifier (ID "t")):[]))) xList
	xRepList2 = nub $ concat $ map (findExpressionsContaining (Identifier (ID "t"))) xList
	xRepList3 = map (\ x -> ((Identifier (addIn x)) , (Identifier x)) ) output
	xReps = (map (\x -> convertExpressionToReplacement x mode) (xRepList1 ++ xRepList2)) ++ xRepList3
	
addIn :: ID_Path -> ID_Path
addIn (ID x) = (ID (x ++ "_in"))
addIn (ID_Vect x n) = (ID_Vect (x ++ "_in") n)
addIn (ID_Submod_Struct x y) = (ID_Submod_Struct x (addIn y))

convertExpressionToReplacement :: Expression -> Bool -> (Expression, Expression)
convertExpressionToReplacement (Exp_FunctionCall x ys) mode = ((Exp_FunctionCall x ys), (Identifier (ID x')))
where 
	y = head ys
	x' = if (funcOrId y) 
		then x ++ "_prev"
		else (if (mode) then x else x ++ "_in")
convertExpressionToReplacement _ _ = error $ "Error! Reaching this statement shouldn't be possible!"
-- 
-- True for function, false for ID
funcOrId :: Expression -> Bool
funcOrId (Exp_FunctionCall _ _) = True
funcOrId (Identifier _) = False

genSecondaryOutputMethods :: [PVSTypeDef] -> [TVarDec] -> [TSPTable] -> [MethodBody]
genSecondaryOutputMethods _ _ [] = []
genSecondaryOutputMethods p vars ((a,outVar,b,c,d,lins):xs) = (("get_" ++ (showIDPath var)), (Value typ), [], (Literal (LitBool True)), stmts, []) : (genSecondaryOutputMethods p vars xs)
where 
	var = fst outVar
	tab = (a,outVar,b,c,d,lins)
	typ = p2bType $ findType p vars var
	tab' = addTimeRefReplacements tab True (gatherOutputs (tab:xs))
	stmts = (generateTableStatement' tab') : []

showIDPath :: ID_Path -> String
showIDPath (ID_Submod_Struct m p) = m ++ "_" ++ (showIDPath p)
showIDPath (ID x) = x
showIDPath (ID_Vect x n) = x ++ "[" ++ (show n) ++ "]"

showIDPath' :: ID_Path -> String
showIDPath' (ID_Submod_Struct m p) = m 
showIDPath' (ID x) = x
showIDPath' (ID_Vect x n) = x ++ "[" ++ (show n) ++ "]"
	
isSecondaryTable :: TSPTable -> [ID_Path] -> Bool 
isSecondaryTable (_,_,_,_,vars,_) outVars = and $ map (`elem` (map showIDPath outVars)) vars

generateTableStatement' :: TSPTable -> Statement
generateTableStatement' (_, reg, _, reps, _, ((guard, result):[])) = (Return resultio [])
where 
	reps' = removeIn reps
	resultio = foldl ( \ p q -> applyReplacementExp q p ) result reps'
generateTableStatement' (a, reg, b, reps, d, ((guard, result):xs)) = (If guardia theno elso [])
where
	reps' = removeIn reps
	guardia = foldl ( \ p q -> applyReplacementExp q p ) guard reps'
	theno = (Return resultio [])
	elso = generateTableStatement' (a, reg, b, reps', d, xs)
	resultio = foldl ( \ p q -> applyReplacementExp q p ) result reps'

removeIn :: [Replacement] -> [Replacement]
removeIn [] = []
removeIn ((x,(Identifier (ID y))):xs) = (if (reverse (take 3 (reverse y)) == "_in") then (x, (Identifier(ID(reverse (drop 3 (reverse y)))))) else (x,(Identifier (ID y)))) : (removeIn xs)
removeIn ((x,y):xs) = (x,y) : (removeIn xs)



--------------------------------------------------------Here Be Dragons ----------------------------------------------------------------



p2bType :: PVSType -> BSVType
p2bType (PVS_Bool) = (BSV_Bool)
p2bType (PVS_Bit n) = (BSV_Bit n)
p2bType (PVS_Int n) = (BSV_Int n)
p2bType (PVS_UInt n) = (BSV_UInt n)
p2bType (PVS_Real) = (BSV_Real)
p2bType (PVS_Custom n) = (BSV_Custom n)

findExpressionsContaining :: Expression -> Expression -> [Expression]
findExpressionsContaining exp (Negative x)	= if (exp == x) then [(Negative x)] else (findExpressionsContaining exp x)
findExpressionsContaining exp (Not x) 		= if (exp == x) then [(Not x)] else (findExpressionsContaining exp x)
findExpressionsContaining exp (Equals x y) 	= if (exp == x) then [(Equals x y)] else (findExpressionsContaining exp x) 
++ if (exp == y) then [(Equals x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (NotEquals x y) 	= if (exp == x) then [(NotEquals x y)] else (findExpressionsContaining exp x)
++ if (exp == y) then [(NotEquals x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (GreaterEquals x y)=if (exp == x) then [(GreaterEquals x y)] else (findExpressionsContaining exp x)
++ if (exp == y) then [(GreaterEquals x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (LessEquals x y) 	= if (exp == x) then [(LessEquals x y)] else (findExpressionsContaining exp x) 
++ if (exp == y) then [(LessEquals x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (Greater x y) 	= if (exp == x) then [(Greater x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(Greater x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (Less x y) 	= if (exp == x) then [(Less x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(Less x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (And x y) 	= if (exp == x) then [(And x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(And x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (Or x y) 		= if (exp == x) then [(Or x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(Or x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (BitwiseAND x y) 	= if (exp == x) then [(BitwiseAND x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(BitwiseAND x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (BitwiseOR x y) 	= if (exp == x) then [(BitwiseOR x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(BitwiseOR x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (BitwiseXOR x y) 	= if (exp == x) then [(BitwiseXOR x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(BitwiseXOR x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (LShift x y) 	= if (exp == x) then [(LShift x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(LShift x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (RShift x y) 	= if (exp == x) then [(RShift x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(RShift x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (Multiply x y) 	= if (exp == x) then [(Multiply x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(Multiply x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (Divide x y) 	= if (exp == x) then [(Divide x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(Divide x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (Modulo x y)      = if (exp == x) then [(Modulo x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(Modulo x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (Add x y) 	= if (exp == x) then [(Add x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(Add x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (Subtract x y) 	= if (exp == x) then [(Subtract x y)] else (findExpressionsContaining exp x)  
++ if (exp == y) then [(Subtract x y)] else (findExpressionsContaining exp y)
findExpressionsContaining exp (Literal x) 	= []
findExpressionsContaining exp (Identifier x)	= []
findExpressionsContaining exp (Exp_MethodCall inst meth zs q) = (if (exp `elem` zs) then [(Exp_MethodCall inst meth zs q)] else []) ++ (concat (map (findExpressionsContaining exp) zs))
findExpressionsContaining exp (Skip)            = []
findExpressionsContaining exp (RPFlag x)        = if (exp == x) then [(RPFlag x)] else (findExpressionsContaining exp x)
findExpressionsContaining exp (Exp_FunctionCall x ys) = (if (exp `elem` ys) then [(Exp_FunctionCall x ys)] else []) ++ (concat (map (findExpressionsContaining exp) ys))

