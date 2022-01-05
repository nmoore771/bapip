{-# OPTIONS_GHC -fno-warn-tabs #-}

module ConflictSolver where

import LexerTypes
import BSV2PVS (omniFlatten, migrateAttributes, instantiateSubmods, letPreproc, rf2Vec, genRuleSchedules, propagateMethodCalls, findMethod, convertMethodToRule, findMod, splitStatementPreprocPackage, scrubValueMethods, substituteActionMethods)
import Data.List
import Data.SBV
import Data.SBV.Dynamic 
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import qualified Data.Map as M
import Debug.Trace

type Env = M.Map ID_Path SVal


getSolvedSchedule :: [BSVPackage] -> Maybe String -> IO BSVPackage
getSolvedSchedule pkgs (Just topMod) = do
putStrLn "><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><"
putStrLn "> Now executing SMT solving of rule conflicts using SBV and Yices2"
let pkg = rf2Vec $ letPreproc $ splitStatementPreprocPackage $ substituteActionMethods $ instantiateSubmods topMod $ migrateAttributes $ omniFlatten (Just topMod) pkgs 
let mod = maybe (error "Module Not Found!") (id) $ findMod pkg topMod -- scrubValueMethods $ findMod pkg topMod
let sched = exposeSchedule pkg topMod
putStrLn "> Generating list of conflicts..."
let nConfs = (getNumberOfConflicts sched) `quot` 2
putStrLn $ "> Found " ++ (show nConfs) ++ " Potential Conflicts!"
if (nConfs > 0) 
	then putStrLn $ "> Checking satisfiability of guard conjunctions"
	else putStrLn $ "> No need to continue! "
mutexes <- solveSchedule' (bsv_typedefs pkg) (state mod) sched (fromIntegral nConfs) 0 False
putStrLn "><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><"
return $ distributeMutexes pkg mod mutexes 
getSolvedSchedule pkgs Nothing = do
putStrLn "><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><"
putStrLn "> No schedules to solve!"
putStrLn "> Skipping SMT solving... "
putStrLn "><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><"
let pkg = rf2Vec $ letPreproc $ splitStatementPreprocPackage $ migrateAttributes $ omniFlatten Nothing pkgs 
return pkg

distributeMutexes :: BSVPackage -> BSVModuleDec -> [ModuleAttribute] -> BSVPackage
distributeMutexes pkg mod mutexes = update' pkg
where
	update x = x { attributes = (attributes x) ++ (mutexes) }
	update' x = x { bsv_modules = (update mod):(filter ((\ x y -> (mName x) /= (mName y)) mod) (bsv_modules pkg)) }

{-solveSchedule :: [BSVstateDec] -> [RuleSchedule] -> Integer -> Integer -> Integer  -> IO [ModuleAttribute]
solveSchedule _ [] _ _ _ = do
putStrLn ">>> Solver Checking Complete! <<<"
return []
solveSchedule st (x:xs) total nSchedules done = do
putStrLn $ "> Processing Module Schedule.  " ++ (show (length xs)) ++ " module schedules remaining."
result <- solveSchedule' st x total done 
zs <- solveSchedule xs total (nSchedules + 1) (done + (fromIntegral (length x)))
return (result ++ zs) -} 

solveSchedule' :: [BSVTypeDef] -> [BSVstateDec] -> [RuleSchedule] -> Integer -> Integer -> Bool -> IO [ModuleAttribute]
solveSchedule' _ _ [] _ _ sat = do 
	if (sat) 
	then do 
		putStrLn ">> Schedule Checking Complete, Unresolved conflicts found! <<"
		putStrLn "><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><"
		error "Error!  In order to continue execution of BAPIP, all unresolved scheduling ambiguities must be resolved!  Please consider introducing a rule precedence scheme using pragmas." 
	else do 
		putStrLn ">> Schedule Checking Complete, All Conflicts Resolved! <<"
		return []
solveSchedule' tds st (x:xs) total done sat = do 
let confs = unMaybeList $ map (findSchedule xs) $ unaddressedConflicts x
results <- mapM (solveConflict tds st x) confs      
let r = procResults results
rs <- solveSchedule' tds st xs total (done + (fromIntegral (length confs))) (or (sat:(map (\ (_,_,q) -> q) results)))
return (r ++ rs)

unaddressedConflicts :: RuleSchedule -> [ActionPath]
unaddressedConflicts rule = [ x | x <- conflicts, not (x `elem` resolvedConflicts)]
where
	conflicts = conflictsWith rule
	resolvedConflicts = (preempts rule) ++ (isPreemptedBy rule) ++ (executesAfter rule) ++ (executesBefore rule)


procResults :: [(ActionPath, ActionPath, Bool)] -> [ModuleAttribute]
procResults [] = []
procResults ((_, _, True):xs)        = procResults xs
procResults ((x, y, False):xs) = (Mod_Mutually_Exclusive ((showAP x):(showAP y):[])) : (procResults xs)

solveConflict :: [BSVTypeDef] -> [BSVstateDec] -> RuleSchedule -> RuleSchedule -> IO (ActionPath, ActionPath, Bool)
solveConflict tds st conf tst = do 
q <- invokeSBV tds st ((rName tst), (rGuard tst)) ((rName conf), (rGuard conf))
return ((rName tst),(rName conf), q)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- invokeSBV :: [BSVTypeDef] -> [BSVstateDec] -> Expression -> Expression -> IO Bool
-- invokeSBV tds st ex1 ex2 = do
--   let vs = (extractSymbols ex1) ++ (extractSymbols ex2)
--   predicate <- prd vs ex1 ex2
--   result <- Data.SBV.Dynamic.satWith yices $ fmap svAnd c1 <*> c2
--   return $ modelExists result

invokeSBV :: [BSVTypeDef] -> [BSVstateDec] -> (ActionPath, Expression) -> (ActionPath, Expression) -> IO Bool
invokeSBV tds st (n1, exp1) (n2, exp2) = do
--   let c1symbList = extractSymbols exp1 
--   let c2symbList = extractSymbols exp2
--   let env = M.fromList (\ x -> (x , (findType st x))) $ nub (c1symbList ++ c2symbList)
--   let c1 = exp2Cnst tds st exp1 
--       c2 = exp2Cnst tds st exp2
let exp1' = sizeThemLiterals tds st Nothing exp1
let exp2' = sizeThemLiterals tds st Nothing exp2
--putStrLn $ "Guard Expression 1: " ++ (show exp1') ++ "\n"
--putStrLn $ "Guard Expression 2: " ++ (show exp2') ++ "\n"
result <- Data.SBV.Dynamic.satWith yices $ (makePredicate tds st exp1' exp2')
putStrLn $ "> " ++ (showAP n1) ++ " vs " ++ (showAP n2) ++ "...\n>   " ++ (show result)
return $ modelExists result
--     where
--       modify x = x {verbose = True}


makePredicate :: [BSVTypeDef] -> [BSVstateDec] -> Expression -> Expression -> Symbolic SVal
makePredicate tds st exp1 exp2 = do
let c1symbList = extractSymbols exp1 
let c2symbList = extractSymbols exp2
let vs = nub (c1symbList ++ c2symbList)
let vts = map (\ x -> (x , (findType st x))) $ nub (c1symbList ++ c2symbList)
syms <- mapM (mkVS tds) vts
let env = M.fromList (zip vs syms) 
let interp = interpret tds env 
e1 <- interp exp1 
e2 <- interp exp2
return $ svAnd e1 e2

interpret :: [BSVTypeDef] -> Env -> Expression -> Symbolic SVal
interpret tds env exp = do 
let interp = interpret tds env
let mabes = maybe (error "Error! Bit extraction given non-integer term as argument!") id
case exp of 
	Negative x 		-> fmap svUNeg (interp x)
	Not x 			-> fmap svNot (interp x)
	Equals x y       		-> fmap svEqual (interp x) <*> (interp y)
	NotEquals x y     	-> fmap svNotEqual (interp x) <*> (interp y)
	GreaterEquals x y 	-> fmap svGreaterEq (interp x) <*> (interp y)
	LessEquals x y   		-> fmap svLessEq (interp x) <*> (interp y)
	Greater x y      		-> fmap svGreaterThan (interp x) <*> (interp y)
	Less x y  		-> fmap svLessThan (interp x) <*> (interp y)
	And x y  			-> fmap svAnd (interp x) <*> (interp y)
	Or x y	        	-> fmap svOr (interp x) <*> (interp y)
	BitwiseAND x y   		-> fmap svAnd (interp x) <*> (interp y)
	BitwiseOR x y    		-> fmap svOr (interp x) <*> (interp y)
	BitwiseXOR x y    	-> fmap svXOr (interp x) <*> (interp y)
	BitConcat (x:xs)		-> foldl (\ x y -> fmap svJoin x <*> y) (interp x) (map (interp) xs)
	BitSelect x y		-> fmap svExtract i1 <*> i1 <*> (interp x)
		where 
		i1 = fmap fromIntegral $ fmap mabes $ fmap svAsInteger $ interp y
	BitSelectRange x y z -> fmap svExtract i1 <*> i2 <*> (interp x)
		where 
		i1 = fmap fromIntegral $ fmap mabes $ fmap svAsInteger $ interp y
		i2 = fmap fromIntegral $ fmap mabes $ fmap svAsInteger $ interp z
	LShift x y       		-> fmap svShl (interp x) <*> i1
		where 
		i1 = fmap fromIntegral $ fmap mabes $ fmap svAsInteger $ interp y
	RShift x y        	-> fmap svShr (interp x) <*> i1
		where 
		i1 = fmap fromIntegral $ fmap mabes $ fmap svAsInteger $ interp y
	Multiply x y     		-> fmap svTimes (interp x) <*> (interp y)
	Divide x y      		-> fmap svQuot (interp x) <*> (interp y)
	Modulo x y       		-> fmap svRem (interp x) <*> (interp y)
	Add x y  			-> fmap svPlus (interp x) <*> (interp y)
	Subtract x y      	-> fmap svMinus (interp x) <*> (interp y)
	Literal x  		-> lit2SV tds x
	Identifier x     		-> return $ envLookup x env
	Exp_If x y z 		-> fmap svIte (interp x) <*> (interp y) <*> (interp z)
	RPFlag x 			-> (interp x)
	Tagged w (Valid x)  	-> (interp x)

envLookup :: ID_Path -> Env -> SVal
envLookup v e = maybe (error $ "Var not found: " ++ show v) id
					(M.lookup v e)


extractSymbols :: Expression -> [ID_Path]
extractSymbols (Negative x) 	        = (extractSymbols x)
extractSymbols (Not x)                  = (extractSymbols x)
extractSymbols (Equals x y)       	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (NotEquals x y)      	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (GreaterEquals x y)   	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (LessEquals x y)   	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (Greater x y)      	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (Less x y)  		= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (And x y)  		= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (Or x y)	        	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (BitwiseAND x y)   	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (BitwiseOR x y)    	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (BitwiseXOR x y)    	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (BitConcat xs)		= concat $ map extractSymbols xs
extractSymbols (BitSelect x y)		= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (BitSelectRange x y z)	= (extractSymbols x) ++ (extractSymbols y) ++ (extractSymbols z)
extractSymbols (LShift x y)       	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (RShift x y)       	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (Multiply x y)     	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (Divide x y)       	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (Modulo x y)       	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (Add x y)  		= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (Subtract x y)      	= (extractSymbols x) ++ (extractSymbols y)
extractSymbols (Literal x)  		= []
extractSymbols (Identifier x)     	= x:[]
extractSymbols (ValueMethodCall w x y z) = []
extractSymbols (Exp_If x y z) 		 = (extractSymbols x) ++ (extractSymbols y) ++ (extractSymbols z)
extractSymbols (RPFlag x) 		  = (extractSymbols x)
extractSymbols (Tagged _ (Valid x))         = (extractSymbols x)
extractSymbols (Tagged _ Invalid)           = []
extractSymbols (MaybeIf i j x y)          = (extractSymbols x) ++ (extractSymbols y)
extractSymbols (Exp_MethodCall x y z _)     = concat $ map extractSymbols z
extractSymbols (Binding x y) 		  = extractSymbols y

mkVS :: [BSVTypeDef] -> (ID_Path, BSVType) -> Symbolic SVal
mkVS tds (n', t) = do 
case t of 
	BSV_Bool 	-> ask >>= liftIO . svMkSymVar Nothing (KBool) (Just (showIDPath n'))-- sBool (showIDPath n')
	BSV_Bit n	-> sWordN (fromIntegral n) (showIDPath n')
	BSV_Int n	-> sIntN (fromIntegral n) (showIDPath n')
	BSV_UInt n 	-> sWordN (fromIntegral n) (showIDPath n') 
	BSV_Custom nom	-> getCustomTypeSymbol tds tds n' nom 
	BSV_Maybe t	-> error "Maybe maybes?"

-- convToSVars :: [BSVTypeDef] -> (ID_Path, BSVType) -> Symbolic SVal
-- convToSVars tds (n', (BSV_Bool)) = ask >>= liftIO . svMkSymVar Nothing (KBool) (Just (showIDPath n'))-- sBool (showIDPath n')
-- convToSVars tds (n', (BSV_Bit n)) = sWordN (fromIntegral n) (showIDPath n')
-- convToSVars tds (n', (BSV_Int n)) = sIntN (fromIntegral n) (showIDPath n')
-- convToSVars tds (n', (BSV_UInt n)) = sWordN (fromIntegral n) (showIDPath n') 
-- convToSVars tds (n', (BSV_Custom nom)) = getCustomTypeSymbol tds tds n' nom 
-- convToSVars tds (n', (BSV_Maybe t)) = error "Maybe maybes?"

getCustomTypeSymbol :: [BSVTypeDef] -> [BSVTypeDef] -> ID_Path -> String -> Symbolic SVal
getCustomTypeSymbol tds [] _ _ = error "Error! type not found!"
getCustomTypeSymbol tds ((BSV_Synonym n t):xs) nom tNom 
| tNom == n = mkVS tds (nom, t) 
| otherwise = getCustomTypeSymbol tds xs nom tNom 
getCustomTypeSymbol tds ((BSV_Enumeration n enums):xs) nom tNom
| tNom == n = ask >>= liftIO . svMkSymVar Nothing (mkEnumKind (BSV_Enumeration n enums)) (Just (showIDPath nom))
| otherwise = getCustomTypeSymbol tds xs nom tNom 
getCustomTypeSymbol tds ((BSV_Struct n fs):xs) (ID_Submod_Struct x y) tNom
| tNom == n = findField tds fs y
| otherwise = getCustomTypeSymbol tds xs (ID_Submod_Struct x y) tNom 
getCustomTypeSymbol tds ((BSV_Struct n fs):xs) nom tNom = getCustomTypeSymbol tds xs nom tNom 

findField :: [BSVTypeDef] -> [BSV_Field] -> ID_Path -> Symbolic SVal
findField _ [] _ = error "Error! Field not found!"
findFieldType tds ((n, t):fs) n' 
| (showIDPath n') == n = mkVS tds (n', t)
| otherwise 		 = findFieldType tds fs n'

--BSV_Synonym Name BSVType | BSV_Enumeration Name [Enumerat] | BSV_Struct Name [BSV_Field] deriving (Eq, Show)
mkEnumKind :: BSVTypeDef -> Kind
--mkEnumKind (BSV_Enumeration n enums) = KUserSort n (Right enums)
mkEnumKind (BSV_Enumeration n enums) = KBounded True $ fromIntegral $ bitLimit $ fromIntegral $ length enums

-- exp2Cnst :: [BSVTypeDef] -> [BSVstateDec] -> Expression -> Symbolic SVal 
-- exp2Cnst tds tt (Negative x) 	= fmap svUNeg (exp2Cnst tds tt x)
-- exp2Cnst tds tt (Not x)             = fmap svNot (exp2Cnst tds tt x)
-- exp2Cnst tds tt (Equals x y)       	= fmap svEqual (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (NotEquals x y)     = fmap svNotEqual (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (GreaterEquals x y) = fmap svGreaterEq (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (LessEquals x y)   	= fmap svLessEq (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (Greater x y)      	= fmap svGreaterThan (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (Less x y)  	= fmap svLessThan (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (And x y)  		= fmap svAnd (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (Or x y)	        = fmap svOr (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (BitwiseAND x y)   	= fmap svAnd (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (BitwiseOR x y)    	= fmap svOr (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (BitwiseXOR x y)    = fmap svXOr (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (BitConcat (x:xs))	= foldl (\ x y -> fmap svJoin x <*> y) (exp2Cnst tds tt x) (map (exp2Cnst tds tt) xs)
-- exp2Cnst tds tt (BitSelect x y)	= fmap svExtract i1 <*> i1 <*> (exp2Cnst tds tt x)
--   where 
--     i1 = fmap fromIntegral $ fmap unMaybe $ fmap svAsInteger $ exp2Cnst tds tt y
--     unMaybe Nothing = error "Error! Bit extraction given non-integer term as argument!"
--     unMaybe (Just x) = x
-- exp2Cnst tds tt (BitSelectRange x y z)= fmap svExtract i1 <*> i2 <*> (exp2Cnst tds tt x)
--   where 
--     i1 = fmap fromIntegral $ fmap unMaybe $ fmap svAsInteger $ exp2Cnst tds tt y
--     i2 = fmap fromIntegral $ fmap unMaybe $ fmap svAsInteger $ exp2Cnst tds tt z
--     unMaybe Nothing = error "Error! Bit extraction given non-integer term as argument!"
--     unMaybe (Just x) = x
-- exp2Cnst tds tt (LShift x y)       	= fmap svShl (exp2Cnst tds tt x) <*> i1
--   where 
--     i1 = fmap fromIntegral $ fmap unMaybe $ fmap svAsInteger $ exp2Cnst tds tt y
--     unMaybe Nothing = error "Error! Bit extraction given non-integer term as argument!"
--     unMaybe (Just x) = x
-- exp2Cnst tds tt (RShift x y)        = fmap svShr (exp2Cnst tds tt x) <*> i1
--   where 
--     i1 = fmap fromIntegral $ fmap unMaybe $ fmap svAsInteger $ exp2Cnst tds tt y
--     unMaybe Nothing = error "Error! Bit extraction given non-integer term as argument!"
--     unMaybe (Just x) = x
-- exp2Cnst tds tt (Multiply x y)     	= fmap svTimes (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (Divide x y)       	= fmap svQuot (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (Modulo x y)       	= fmap svRem (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (Add x y)  		= fmap svPlus (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (Subtract x y)      	= fmap svMinus (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y)
-- exp2Cnst tds tt (Literal x)  		= lit2SV tds x
-- exp2Cnst tds tt (Identifier x)     	= convToSVars tds (x, (findType tt x))
--   where 
--     unMaybe (Just x) = x
--     unMaybe Nothing = error "What the heck, man?!"
-- exp2Cnst tds tt (Exp_If x y z) 	= fmap svIte (exp2Cnst tds tt x) <*> (exp2Cnst tds tt y) <*> (exp2Cnst tds tt z)
-- exp2Cnst tds tt (RPFlag x) 		= (exp2Cnst tds tt x)
-- exp2Cnst tds tt (Tagged (Valid x))  = (exp2Cnst tds tt x)



lit2SV :: [BSVTypeDef] -> Lit -> Symbolic SVal
lit2SV tds (LitEnum x) 		= return $ svInteger (getEnumKind tds x) (enumToNum tds x)
lit2SV _ (LitInt x) 		= return $ svInteger (KBounded True (fromInteger (bitLimit x))) x
lit2SV _ (LitBool x) 		= return $ svBool x
lit2SV _ (LitReal x) 		= return $ svFloat x
lit2SV _ (LitSizedInt n x) 	= return $ svInteger (KBounded True (fromInteger n)) x
lit2SV _ (LitStructConstructor) = error "All the nope!"
lit2SV _ (LitVoid) = error "That don't make no sense!" 

enumToNum :: [BSVTypeDef] -> String -> Integer 
enumToNum [] e = error $ "Error!  Could not find enumerat \"" ++ e ++ "\""
enumToNum ((BSV_Enumeration n es):xs) e  
| e `elem` es 	= maybe (error "This is a weird error that shouldn't happen!") fromIntegral (e `elemIndex` es)
| otherwise 		= enumToNum xs e
enumToNum (x:xs) e = enumToNum xs e

bitLimit :: Integer -> Integer 
bitLimit n 
| n > 0     = 1 + bitLimit (floor ((fromInteger n) / 2))
| otherwise = 1
	
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	
getEnumKind :: [BSVTypeDef] -> String -> Kind
getEnumKind [] e = error $ "Error!  Could not find enumerat \"" ++ e ++ "\""
getEnumKind ((BSV_Enumeration n es):xs) e 
| e `elem` es = mkEnumKind (BSV_Enumeration n es)
| otherwise   = getEnumKind xs e
getEnumKind (x:xs) e = getEnumKind xs e
	
findSchedule :: [RuleSchedule] -> ActionPath -> Maybe RuleSchedule
findSchedule [] _ = Nothing
findSchedule (x:xs) y = if (rName x) == y then Just x else findSchedule xs y  

exposeSchedule :: BSVPackage -> String -> [RuleSchedule]
exposeSchedule universe topMod = genSchedule universe $ newMod mod
where 
	mod = scrubValueMethods $ maybe (error "Module Not Found!") (id) $ findMod universe topMod
	meths = methods mod 
		-- trace ("[T] exposeSchedule - " ++ (show mod) ) $ 
	quasiRules = unMaybeList $ map convertMethodToRule meths 
	newMod x = x {rules = (rules mod) ++ quasiRules}
	
genSchedule :: BSVPackage -> BSVModuleDec -> [RuleSchedule]
genSchedule universe mod = schedule
where 
	tracy = "[genSchedule] - module = " ++ (mName mod) ++"\nMethods = " ++ (intercalate "\n\t" methList)
	methList = map (\ (x,_,_,_,_,_) -> x) (methods mod)
	meths = map (maybe (error "Error! Method Not Found!") (id) ) $ map (findMethod (methods mod)) methList
	ruleList = (rules mod) ++ (unMaybeList (map convertMethodToRule meths))
	update x = x {rules = ruleList, methods = []}
	newMod = propagateMethodCalls universe mod (map (\ x -> (ID x)) methList) []
	schedule = genRuleSchedules universe newMod [] methList

getNumberOfConflicts :: [RuleSchedule] -> Int
getNumberOfConflicts xs = sum $ map length confs
where
	confs = map unaddressedConflicts xs
	
unMaybeList :: (Eq a) => [Maybe a] -> [a]
unMaybeList [] = []
unMaybeList ((Just x):xs) = x : (unMaybeList xs)
unMaybeList ((Nothing):xs) = unMaybeList xs
	
findType :: [BSVstateDec] -> ID_Path -> BSVType
findType [] _ = error "Error! Type not found!"
findType ((BSV_Reg i' t _):ss) i	= if shallowEQ i' i then t else findType ss i
findType ((BSV_Fifo f i' t):ss) i 	= if shallowEQ i' i then t else findType ss i
findType ((BSV_Vector i' t _ _):ss) i 	= if shallowEQ i' i then t else findType ss i
findType ((BSV_RegFile i' _ t _):ss) i 	= if shallowEQ i' i then t else findType ss i
findType ((BSV_SubModuleDec _ _ _):ss) i = findType ss i
findType ((DWire i' t _ ):ss) i 	= if shallowEQ i' i then t else findType ss i
	
shallowEQ :: ID_Path -> ID_Path -> Bool
shallowEQ (ID x) (ID y) 				= x == y
shallowEQ (ID_Vect x _) (ID y) 				= x == y
shallowEQ (ID_Submod_Struct x _) (ID y) 		= x == y
shallowEQ (ID x) (ID_Vect y _) 				= x == y
shallowEQ (ID_Vect x _) (ID_Vect y _) 			= x == y
shallowEQ (ID_Submod_Struct x _) (ID_Vect y _) 		= x == y
shallowEQ (ID x) (ID_Submod_Struct y _) 		= x == y
shallowEQ (ID_Vect x _) (ID_Submod_Struct y _) 		= x == y
shallowEQ (ID_Submod_Struct x _) (ID_Submod_Struct y _) = x == y

showIDPath :: ID_Path -> String
showIDPath (ID_Submod_Struct m p) = m ++ "_" ++ (showIDPath p)
showIDPath (ID x) = x
showIDPath (ID_Vect x n) = x ++ "[" ++ (show n) ++ "]"

showAP :: ActionPath -> String 
showAP (ActionNameAP x) = x
showAP (RuleNameAP x) = x
showAP (MethodNameAP x) = x
showAP (SubmoduleNameAP x y) = x ++ "." ++ (showAP y)

--data ActionPath = ActionNameAP String | RuleNameAP String | MethodNameAP String | SubmoduleNameAP String ActionPath deriving (Eq, Show)

sizeThemLiterals :: [BSVTypeDef] -> [BSVstateDec] -> Maybe BSVType -> Expression -> Expression
sizeThemLiterals tds st shouldBe  (Negative x) 		= Negative 	(sizeThemLiterals tds st shouldBe x)
sizeThemLiterals tds st shouldBe  (Not x)             	= Not 		(sizeThemLiterals tds st shouldBe x)
sizeThemLiterals tds st Nothing   (Equals x y)       	= Equals   	(sizeThemLiterals tds st (getType tds st y) x) (sizeThemLiterals tds st (getType tds st x) y)
sizeThemLiterals tds st Nothing   (NotEquals x y)     	= NotEquals 	(sizeThemLiterals tds st (getType tds st y) x) (sizeThemLiterals tds st (getType tds st x) y)
sizeThemLiterals tds st shouldBe  (GreaterEquals x y) 	= GreaterEquals	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (LessEquals x y)   	= LessEquals	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (Greater x y)      	= Greater 	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (Less x y)  		= Less		(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (And x y)  		= And		(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (Or x y)	        = Or		(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (BitwiseAND x y)   	= BitwiseAND	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (BitwiseOR x y)    	= BitwiseOR	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (BitwiseXOR x y)    	= BitwiseXOR 	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (BitConcat xs)	= BitConcat 	(map (\ x -> sizeThemLiterals tds st shouldBe x) xs)
sizeThemLiterals tds st shouldBe  (BitSelect x y)	= BitSelect	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (BitSelectRange x y z)= BitSelectRange (sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y) (sizeThemLiterals tds st shouldBe z)
sizeThemLiterals tds st shouldBe  (LShift x y)       	= LShift 	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (RShift x y)       	= RShift	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (Multiply x y)     	= Multiply 	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (Divide x y)       	= Divide	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (Modulo x y)       	= Modulo	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (Add x y)  		= Add		(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (Subtract x y)      	= Subtract 	(sizeThemLiterals tds st shouldBe x) (sizeThemLiterals tds st shouldBe y)
sizeThemLiterals tds st shouldBe  (Literal x)  		= Literal $ applyTypeToLit tds shouldBe x
sizeThemLiterals tds st shouldBe  (Identifier x)     	= (Identifier x)

applyTypeToLit :: [BSVTypeDef] -> Maybe BSVType -> Lit -> Lit
applyTypeToLit _ (Just (BSV_Int n)) (LitInt x) 	= LitSizedInt n x
applyTypeToLit _ (Just (BSV_UInt n)) (LitInt x) 	= LitSizedInt n x
applyTypeToLit _ (Just (BSV_Bit n)) (LitInt x) 	= LitSizedInt n x
applyTypeToLit tds (Just (BSV_Custom nom)) (LitInt x) = applyTypeToLit tds (Just (getSynonymType tds tds nom)) (LitInt x)
applyTypeToLit _ (Just t) (LitInt x) 		= LitInt x
applyTypeToLit _ (Nothing) (LitInt x) 		= LitInt x
applyTypeToLit _ _ x  				= x

getSynonymType :: [BSVTypeDef] -> [BSVTypeDef] -> String -> BSVType
getSynonymType _ [] _ = error "Error! Custom type not found!"
getSynonymType tds ((BSV_Synonym n t):xs) n' 
| n == n' = case t of 
		(BSV_Custom x) -> getSynonymType tds tds x 
		x 		  -> x
| otherwise = getSynonymType tds xs n'
getSynonymType tds (x:xs) n' = getSynonymType tds xs n' 


getType :: [BSVTypeDef] -> [BSVstateDec] -> Expression -> Maybe BSVType
getType tds st (Negative x) 		= getType tds st x
getType tds st (Not x)             	= getType tds st x
getType tds st (Equals x y)       	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (NotEquals x y)     	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (GreaterEquals x y) 	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (LessEquals x y)   	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (Greater x y)      	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (Less x y)  		= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (And x y)  		= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (Or x y)	        	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (BitwiseAND x y)   	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (BitwiseOR x y)    	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (BitwiseXOR x y)    	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (BitConcat xs)		= concatMerge (map (\ x -> getType tds st x) xs)
getType tds st (BitSelect x y)		= Just $ BSV_Bit 1
getType tds st (BitSelectRange x y z)	= Just $ BSV_Bit $ (compute y) - (compute z)
getType tds st (LShift x y)       	= (getType tds st x) 
getType tds st (RShift x y)       	= (getType tds st x) 
getType tds st (Multiply x y)     	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (Divide x y)       	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (Modulo x y)       	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (Add x y)  		= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (Subtract x y)      	= mergeTypes (getType tds st x) (getType tds st y)
getType tds st (Literal x)  		= findLitType x 
getType tds st (Identifier x)     	= Just $ findType st x

concatMerge :: [Maybe BSVType] -> Maybe BSVType
concatMerge xs = Just $ BSV_Bit $ sum $ map ( maybe (0) (typeSize))  xs

compute :: Expression -> Integer
compute (Negative x) 		= 0 - (compute x)
-- compute (Not x)             	= not x
-- compute (Equals x y)       	= (==) (compute x) (compute y)
-- compute (NotEquals x y)     	= (/=) (compute x) (compute y)
-- compute (GreaterEquals x y) 	= (>=) (compute x) (compute y)
-- compute (LessEquals x y)   	= (<=) (compute x) (compute y)
-- compute (Greater x y)      	= (>) (compute x) (compute y)
-- compute (Less x y)  		= (<) (compute x) (compute y)
-- compute (And x y)  		= (&&) (compute x) (compute y)
-- compute (Or x y)	        = (||) (compute x) (compute y)
-- compute (BitwiseAND x y)   	= 
-- compute (BitwiseOR x y)    	= 
-- compute (BitwiseXOR x y)    	= 
-- compute (LShift x y)       	= (*2) (compute x) 
-- compute (RShift x y)       	= (\ z -> floor (z/2)) (compute x) 
compute (Multiply x y)     	= (*) (compute x) (compute y)
-- compute (Divide x y)       	= (\ a b -> floor (a / b)) (compute x) (compute y)
--compute (Modulo x y)       	= (%) (compute x) (compute y)
compute (Add x y)  		= (+) (compute x) (compute y)
compute (Subtract x y)      	= (-) (compute x) (compute y)
compute (Literal x)  		= extractLit x
compute (Identifier x)     	= error "Error! Bit selection must be concrete literals!"

extractLit :: Lit -> Integer
extractLit (LitInt x) 			= x
extractLit (LitSizedInt n x) 		= x

findLitType :: Lit -> Maybe BSVType
findLitType (LitBool x) 	= Just BSV_Bool
findLitType (LitSizedInt n x) 	= Just $ BSV_Bit n
findLitType x 			= Nothing

mergeTypes :: Maybe BSVType -> Maybe BSVType -> Maybe BSVType
mergeTypes Nothing Nothing 	= Nothing
mergeTypes (Just x) Nothing 		= Just x
mergeTypes Nothing (Just x) 		= Just x
mergeTypes (Just x) (Just y)
| typeSize x == typeSize y	= Just $ BSV_Bit (typeSize x)
| otherwise 			= Nothing 

typeSize :: BSVType -> Integer 
typeSize (BSV_Bool) 	  = 1
typeSize (BSV_Bit n)	  = n
typeSize (BSV_Int n)	  = n 
typeSize (BSV_UInt n) 	  = n 
typeSize (BSV_Maybe t)	  = typeSize t

----------------------------------------imported!-----------------------------------------
-- | Create an N-bit symbolic unsigned named variable
sWordN :: Int -> String -> Symbolic SVal
sWordN w nm = ask >>= liftIO . svMkSymVar Nothing (KBounded False w) (Just nm)

-- | Create an N-bit symbolic signed named variable
sIntN :: Int -> String -> Symbolic SVal
sIntN w nm = ask >>= liftIO . svMkSymVar Nothing (KBounded True w) (Just nm)

