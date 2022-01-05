{-# OPTIONS_GHC -fno-warn-tabs #-}

module BSV2PVS where

--  (\ x -> twace ("[T] - " ++ (show x) ) x) $

--import ActionScheduler
import LexerTypes
import Data.Maybe
import Data.Bits
import Data.List (nub, intersect, subsequences, stripPrefix, delete, intercalate, isSuffixOf, (\\))
import Data.List.Extra
import Debug.Trace

bsv2pvs :: BSVPackage -> Maybe String -> [[ID_Path]] -> PVSPackage
--bsv2pvs u t _ = error $ show $ u 
bsv2pvs universe (Just topMod) uSched = recordifyPVS ( topMod
					, consts
					, typs 
					, trans
					, states
					, instDefs
					, funcs
					)
where 
	tracy = "[bsv2pvs] uSched - " ++ (show uSched)
	--universe = error $ show $ map (\ x -> mNames (bsv_modules x)) $ hexFiles $ letPreproc $ instantiateSubmods topMod $ migrateAttributes $ omniFlatten topMod multiverse
	--universe = rf2Vec $ letPreproc $ instantiateSubmods topMod $ migrateAttributes $ omniFlatten topMod multiverse
	-- universe = splitStatementPreprocPackage universe'
	consts = map b2pConstDecl (bsv_constants universe)
	typs = map b2pTypeDef $ bsv_typedefs universe 
	states = b2pStateDecs universe -- twace ("[T] bsv2pvs : " ++ (show ( universe))) $ b2pStateDecs universe
	instDefs = map (b2pInstDef universe) (bsv_instDefs universe)
	funcs = map b2pFuncs (bsv_functions universe)    
	trans = map (genTransitions universe topMod) methCombinations'
	where
	mod = maybe (error "Module Not Found!") (id) $ findMod universe topMod
	meths = onlyInputMethods $ methods mod 
	methCombinations = nub $  ([] : (uSched))
	methCombinations' = zip [0..(fromIntegral (length methCombinations))] methCombinations
bsv2pvs universe Nothing uSched = recordifyPVS ( bsv_packageName universe
					, consts
					, typs 
					, trans
					, states
					, instDefs
					, funcs
					)
where 
	-- universe = splitStatementPreprocPackage universe'
	consts = map b2pConstDecl (bsv_constants universe)
	typs = map b2pTypeDef $ bsv_typedefs universe 
	states = b2pStateDecs universe -- twace ("[T] bsv2pvs : " ++ (show (b2pStateDecs universe))) $ b2pStateDecs universe
	instDefs = map (b2pInstDef universe) (bsv_instDefs universe)
	funcs = map b2pFuncs (bsv_functions universe)    
	trans = [] 


-- | converts a PVS package to a BSV package for the purposes of the pvs to bsv compilation process.
-- bsv2pvs' :: [BSVPackage] -> String -> PVSPackage
-- bsv2pvs' multiverse topMod = recordifyPVS ( topMod
--                                          , consts
--                                          , typs 
--                                          , trans
--                                          , states
--                                          , instDefs
--                                          , funcs
--                                          )
--   where 
--     --universe = error $ show $ splitStatementPreprocPackage $ rf2Vec $ letPreproc $ instantiateSubmods topMod $ migrateAttributes $ omniFlatten topMod multiverse
--     universe = splitStatementPreprocPackage $ rf2Vec $ letPreproc $ instantiateSubmods topMod $ migrateAttributes $ omniFlatten topMod multiverse
--     consts = map b2pConstDecl (bsv_constants universe)
--     typs = map b2pTypeDef $ bsv_typedefs universe 
--     states = b2pStateDecs universe
--     instDefs = map (b2pInstDef universe) (bsv_instDefs universe)
--     funcs = map b2pFuncs (bsv_functions universe)    
--     trans = map (genTransitions universe topMod) methCombinations'
--       where
-- 	mod = findMod universe topMod
-- 	meths = onlyInputMethods $ methods mod 
-- 	methCombinations = (requestedTransitions universe) ++ (map (\ (x,_,_,_,_,_) -> [x]) meths)
-- 	methCombinations' = zip [0..(fromIntegral (length methCombinations))] methCombinations

substituteActionMethods :: BSVPackage -> BSVPackage 
substituteActionMethods universe = BSVPackage{ bsv_packageName = bsv_packageName universe
				, imports = imports universe
				, including = including universe
				, interfaces = interfaces universe
				, bsv_constants = bsv_constants universe
				, bsv_typedefs = bsv_typedefs universe
				, bsv_modules = map samMod $ bsv_modules universe
				, bsv_instDefs = bsv_instDefs universe
				, bsv_functions = bsv_functions universe
				, bsv_macros = bsv_macros universe
				, hexFiles = hexFiles universe 
				}


samMod :: BSVModuleDec -> BSVModuleDec 
samMod mod = BSVModuleDec { mName = mName mod
			, instanceName = instanceName mod
			, instances = map samMod $ instances mod
			, interfaceName = interfaceName mod
			, interfaceDecs = map (samInt mod) $ interfaceDecs mod
			, state = state mod
			, actions = map (samAct mod) $ actions mod
			, attributes = attributes mod
			, rules = map (samRul mod) $ rules mod
			, methods = map (samMet mod) $ methods mod
			}

samInt :: BSVModuleDec -> MidModInterfaceDec -> MidModInterfaceDec
samInt mod (x,y,ms) = (x, y, (map (samMet mod) ms))

samAct :: BSVModuleDec -> ActionDec -> ActionDec 
samAct mod (x, y, z) = (x, (concat (map (samStmt mod) y)), z)			
			
samRul :: BSVModuleDec -> RuleDec -> RuleDec     
samRul mod (w,x,y,z) = (w,x,result,z)
where
	tracy = "\n\n[samRul] rule name = " ++ (show w) ++ "\nStatements before = " ++ (intercalate "\n" (map show y)) ++ "\nStatements after = " ++ (intercalate "\n" (map show result))
	result = (concat (map (samStmt mod) y))

samMet :: BSVModuleDec -> MethodBody -> MethodBody
samMet mod (u,v,w,x,y,z) = (u,v,w,x,(concat (map (samStmt mod) y)),z)

samStmt :: BSVModuleDec -> Statement -> [Statement]
samStmt mod (MethodCall path "deq" args atts) = (MethodCall path "deq" args atts):[]
samStmt mod (MethodCall path "enq" args atts) = (MethodCall path "enq" args atts):[]
samStmt mod (MethodCall path "clear" args atts) = (MethodCall path "clear" args atts):[]
samStmt mod (MethodCall path "first" args atts) = (MethodCall path "first" args atts):[]
samStmt mod (MethodCall path meth args atts) = concat $ map (samStmt mod') ificized 
	where
		tracy = "\n[samStmt] - MethodCall\npath = " ++ (show path) ++ "\nmeth = " ++ (show meth) ++ "\nreplacements = " ++ (intercalate "\n\t" (map show reps)) ++ "\nraw stmts = " ++ (show (snd pair)) ++ "\nstmts = " ++ (show stmts) ++ "\nstmts' = " ++ (show stmts')
		pair = maybe (error errorMsg) (id) $ findSubModbyInst' (instances mod) path meth
		errorMsg = "Error! Corresponding submodule not found!\n\npath = " ++ (show path) ++ "\nmeth = " ++ (show meth) ++ "\ninstances = " ++ (intercalate "\n" (map instanceName (instances mod)))
		mod' = fst pair
		methBody' = samMet mod' (snd pair)
		methBody = addPathToMethBody methBody' (rmInstanceDef path mod)  -- maybe (error "Error! Corresponding method not found!") (id) $ findMethod (methods mod') meth
		gd = (\ (_,_,_,x,_,_) -> x ) methBody
		utargs = (\ (_,_,x,_,_,_) -> x ) methBody
		stmts = (\ (_,_,_,_,x,_) -> x) methBody
		utargs' = map (\ (x,_) -> (Identifier (ID x))) utargs
		reps = zip utargs' args
		stmts' = map (\x -> foldl (\ p q -> applyReplacementStmt q p) x reps) stmts
		ificized = if (gd == (Literal (LitBool True)))
			then stmts'
			else map (\ x -> (If gd x (Void) (atts))) stmts'
samStmt mod (If w x y z) = (If w x'' y'' z):[]
	where
		x' = samStmt mod x 
		x'' = if (length x') > 1 then (StatementBlock x') else (head x')
		y' = samStmt mod y 
		y'' = if (length y') > 1 then (StatementBlock y') else (head y')
samStmt mod (PMatchIf i j x y z) = (PMatchIf i j x'' y'' z):[]
	where
		x' = samStmt mod x 
		x'' = if (length x') > 1 then (StatementBlock x') else (head x')
		y' = samStmt mod y 
		y'' = if (length y') > 1 then (StatementBlock y') else (head y')
samStmt mod (ForLoop w x y stmt z) = (ForLoop w x y stmt z):[]
	where
		stmt' = samStmt mod stmt 
		stmt'' = if (length stmt') > 1 then (StatementBlock stmt') else (head stmt')
samStmt mod (Switch gd cases z)  = (Switch gd cases' z):[]
where
	cases' = map helper cases
	helper (x,y) = (x, y'')
		where
		y' = samStmt mod y 
		y'' = if (length y') > 1 then (StatementBlock y') else (head y')
samStmt mod (LocalDec lv stmt z) = (LocalDec lv stmt'' z):[]
	where
		stmt' = samStmt mod stmt 
		stmt'' = if (length stmt') > 1 then (StatementBlock stmt') else (head stmt')
samStmt mod (StatementBlock xs)  = (concat (map (samStmt mod) xs))
samStmt mod x = x:[]

addPathToMethBody :: MethodBody -> ID_Path -> MethodBody
addPathToMethBody (u,v,w,x,y,z) p = (u,v,w,x',y',z)
where
	x' = applyRootPrefix (map fst w) (idpath2strings p) x
	y' = applyRootPrefixStmts (map fst w) (idpath2strings p) y
	
applyRootPrefixStmts :: [MethodArg] -> [String] -> [Statement] -> [Statement]
applyRootPrefixStmts args prefix = map (applyRootPrefixStmt args prefix)

applyRootPrefixStmt :: [MethodArg] -> [String] -> Statement -> Statement 
applyRootPrefixStmt m p (Write i exp z) = (Write (applyRootPrefix' i p) (applyRootPrefix m p exp) z)
applyRootPrefixStmt m p (MethodCall i meth exps z) = (MethodCall i meth (map (applyRootPrefix m p) exps) z)
applyRootPrefixStmt m p (ActionCall x y) = (ActionCall x y)
applyRootPrefixStmt m p (Return exp z) = (Return (applyRootPrefix m p exp) z)
applyRootPrefixStmt m p (StructReturn t cases z) = (StructReturn t cases' z)
where
	cases' = map (\ (x, y) -> (x, (applyRootPrefix m p y))) cases
applyRootPrefixStmt m p (If gd thn els z) = (If (applyRootPrefix m p gd) (applyRootPrefixStmt m p thn) (applyRootPrefixStmt m p els) z)
applyRootPrefixStmt m p (PMatchIf i1 i2 thn els z) = (PMatchIf (applyRootPrefix' i1 p) i2 (applyRootPrefixStmt m' p thn) (applyRootPrefixStmt m' p els) z)
where
	m' = (idpath2string i2) : m
applyRootPrefixStmt m p (ForLoop a b c d e) = (ForLoop a b c d e)
applyRootPrefixStmt m p (Switch gd cases z) = (Switch (applyRootPrefix m p gd) cases' z)
where
	cases' = map (\ (x, y) -> (x, (applyRootPrefixStmt m p y))) cases
applyRootPrefixStmt m p (LocalDec lvars stmt z) = (LocalDec lvars' stmt' z)
where
	lvars' = map (\ (x,y,z) -> (x,y,(applyRootPrefix m p z))) lvars
	stmt' = applyRootPrefixStmt (m ++ (map (\(x,_,_) -> (idpath2string x)) lvars)) p stmt
applyRootPrefixStmt m p (StatementBlock xs) = (StatementBlock (map (applyRootPrefixStmt m p) xs))
applyRootPrefixStmt _ _ (Void) = (Void)

rmInstanceDef :: ID_Path -> BSVModuleDec -> ID_Path
rmInstanceDef (ID x) _ = (ID x)
rmInstanceDef (ID_Submod_Struct x (ID y)) mod = if (midmod /= Nothing) then (ID x) else (ID_Submod_Struct x (rmInstanceDef (ID y) inst))
where
	inst = maybe (error ("Error! no instance of " ++ (show x))) (id) $ getModByInst (instances mod) x
	midmod = interfaceLookup 2 y (interfaceDecs inst)
rmInstanceDef (ID_Submod_Struct x y) mod = (ID_Submod_Struct x (rmInstanceDef y inst))
	where
		inst = maybe (error ("Error! no instance of " ++ (show x))) (id) $ getModByInst (instances mod) x

getModByInst :: [BSVModuleDec] -> String -> Maybe BSVModuleDec
getModByInst [] x = Nothing
	where
	tracy = "\n[getModByInst]\n - x = " ++ (show x)
getModByInst (x:xs) y = if ((instanceName x) == y) then Just x else getModByInst xs y
where
	tracy = "\n[getModByInst]\n - instanceName = " ++ (show (instanceName x)) ++ "\ny = " ++ (show y)
		
rf2Vec :: BSVPackage -> BSVPackage 
rf2Vec universe = BSVPackage{ bsv_packageName = bsv_packageName universe
				, imports = imports universe
				, including = including universe
				, interfaces = interfaces universe
				, bsv_constants = bsv_constants universe
				, bsv_typedefs = bsv_typedefs universe
				, bsv_modules = map (rfMod (bsv_typedefs universe) (hexFiles universe) [] crunchedSubInterfaces) $ bsv_modules universe
				, bsv_instDefs = bsv_instDefs universe
				, bsv_functions = bsv_functions universe
				, bsv_macros = []
				, hexFiles = []
--				, requestedTransitions = []
				}
	where
		crunchedSubInterfaces = getSubinterfaceNames universe
			-- (\ x -> trace ("[rf2Vec] - interfaceDecs " ++ (show (interfaces universe)) ++ "\n\ncrunchedSubInterfaces = " ++ (show x)) x) $ getSubinterfaceNames universe
		
getSubinterfaceNames :: BSVPackage -> [InterfaceRef]
getSubinterfaceNames uni = concat $ map (\ (_,_,x,_) -> x ) $ interfaces uni
			

rfMod :: [BSVTypeDef] -> [HexFile] -> [String] -> [InterfaceRef] -> BSVModuleDec -> BSVModuleDec 
rfMod td hex p irefs mod = BSVModuleDec { mName = mName mod
			, instanceName = instanceName mod
			, instances = map (rfMod td hex p' irefs) $ instances mod
			, interfaceName = interfaceName mod
			, interfaceDecs = rfInterfaces td mod st' p' irefs $ interfaceDecs mod
			, state =  map (rfState hex irefs) st -- map fixMaybeDefaults $ map (rfState hex) st
			, actions = map (rfAct td mod st' p' irefs) $ actions mod
			, attributes = attributes mod
			, rules = map (rfRul td mod st' p' irefs) $ rules mod
			, methods = map (rfMet td mod st' p' irefs) $ methods mod
			}
where 
	tracy = "[rfMod] instance name = " ++ (instanceName mod) ++ "\np = " ++ (show p) ++ "\np' = " ++ (show p')
	p' = if (instanceName mod) /= "root" 
			then p `snoc` (instanceName mod)
			else p
	st = state mod
	st' = getRegFiles st

fixMaybeDefaults :: BSVstateDec -> BSVstateDec 
fixMaybeDefaults (BSV_Reg i t init) = BSV_Reg i t $ fixedInit t init
fixMaybeDefaults (DWire i t init) = DWire i t $ fixedInit t init
fixMaybeDefaults x = x

fixedInit :: BSVType -> Expression -> Expression
fixedInit (BSV_Maybe t) (Tagged x exp) = Tagged x exp
fixedInit (BSV_Maybe t) exp = Tagged Nothing $ Valid exp
fixedInit t exp = exp
	
getRegFiles :: [BSVstateDec] -> [String]
getRegFiles [] = []
getRegFiles ((BSV_RegFile i _ _ _):xs) = (showIDPath i) : (getRegFiles xs)
getRegFiles (x:xs) = getRegFiles xs

rfState :: [HexFile] -> [InterfaceRef] -> BSVstateDec -> BSVstateDec
rfState hex iref (BSV_RegFile i w t l) = unMaybe result
where 
	conv (RegFileLoad n x y) = n
	result = lookupBy isSuffixOf (conv l) hex 
	unMaybe Nothing = error $ "BSV2PVS Error! Couldn't find hex file with which to instantiate regfile! \n\n  hex files : " ++ (show hex) ++ "\n\n  Searching For : " ++ (conv l)
	unMaybe (Just z) = BSV_Vector i t (2^(getTypeBitSize w)) $ Explicit z
rfState _ _ x = x

lookupBy :: (a -> b -> Bool) -> a -> [(b,c)] -> Maybe c
lookupBy _ _ []    = Nothing
lookupBy pred x ((y,z):ys) = if (pred x y) then Just z else lookupBy pred x ys

getTypeBitSize :: BSVType -> Integer 
getTypeBitSize BSV_Bool = 1
getTypeBitSize (BSV_Bit n) = n
getTypeBitSize (BSV_Int n) = n
getTypeBitSize (BSV_UInt n) = n    
	
rfInterfaces :: [BSVTypeDef] -> BSVModuleDec -> [String] -> [String] -> [InterfaceRef] -> [MidModInterfaceDec] -> [MidModInterfaceDec]    
rfInterfaces _ _ _ _ _ [] = []
rfInterfaces td mod st p iref ((x,y,ms):xs) = ((x, y, (map (rfMet td mod st p iref) ms)):(rfInterfaces td mod st p iref xs))


rfAct :: [BSVTypeDef] -> BSVModuleDec -> [String] -> [String] -> [InterfaceRef] -> ActionDec -> ActionDec 
rfAct td mod st p iref (x, y, z) = (x, (map (rfStmt td mod st p Nothing iref) y), z)

rfRul :: [BSVTypeDef] -> BSVModuleDec -> [String] -> [String] -> [InterfaceRef] -> RuleDec -> RuleDec 
rfRul td mod st p iref (w, x, y, z) = (w, (rfExp td mod st p Nothing iref x), (map (rfStmt td mod st p Nothing iref) y), z)

rfMet :: [BSVTypeDef] -> BSVModuleDec -> [String] -> [String] -> [InterfaceRef] -> MethodBody -> MethodBody 
rfMet td mod st p iref (u, v, w, x, y, z) = (u, v, w, (rfExp td mod st p Nothing iref x), (map (rfStmt td mod st p (returnTypeToBSVType v) iref) y), z)
where
	tracy = "[rfMeth] instance name "++ (show (instanceName mod)) ++"\n\tu, v = " ++ (show u) ++ " , " ++ (show v) ++ "\np = " ++ (show p)

returnTypeToBSVType :: ReturnType -> Maybe BSVType
returnTypeToBSVType (Action) = Nothing
returnTypeToBSVType (ActionValue t) = Just t 
returnTypeToBSVType (Value t) = Just t

rfStmt :: [BSVTypeDef] -> BSVModuleDec -> [String] -> [String] -> Maybe BSVType -> [InterfaceRef] -> Statement -> Statement
rfStmt td mod st p btyp iref (Write x y z) 		= (Write x (rfExp td mod st p (Just typ) iref y) z)
where
	tracy = "[rfStmt] id path - " ++ (show x) ++ "\npath = " ++ (show p)
	-- tracy = "[rfStmt] id path - " ++ (show x) ++ "\ngiven BType = " ++ (show btyp) ++ "\nfound Type = " ++ (show typ)
	-- typo = maybe (typ) (id) $ tryForStructType 
	typ = getStateType mod (state mod) x
rfStmt td mod st p btyp iref (ActionCall x y) 	= (ActionCall x y) 					
rfStmt td mod st p btyp iref (Return x y) 		= (Return (rfExp td mod st p btyp iref x) y)	
rfStmt td mod st p btyp iref (If w x y z) 		= (If (rfExp td mod st p Nothing iref w) (rfStmt td mod st p btyp iref x) (rfStmt td mod st p btyp iref y) z) 
rfStmt td mod st p btyp iref (PMatchIf v w x y z)  = (PMatchIf v w (rfStmt td mod st p btyp iref x) (rfStmt td mod st p btyp iref y) z) 	
rfStmt td mod st p btyp iref (ForLoop v w x y z) 	= (ForLoop v (rfExp td mod st p Nothing iref w) x (rfStmt td mod st p btyp iref y) z)	
rfStmt td mod st p btyp iref (Switch x y z) 	= (Switch (rfExp td mod st p Nothing iref x) (map (recRfCase td mod st p btyp iref) y) z) 	
--rfStmt mod st btyp (Let x y z) 		= (Let x (rfExp st btyp y) z) 		
rfStmt td mod st p btyp iref (LocalDec x y z) 	= (LocalDec (map (\ a -> rfLocal td mod st p a iref) x) (rfStmt td mod st p btyp iref y) z)	
rfStmt td mod st p btyp iref (StatementBlock x) 	= (StatementBlock (map (rfStmt td mod st p Nothing iref) x) )	
rfStmt td mod st p btyp iref (Void) 		= (Void)	
rfStmt td mod st p btyp iref (StructReturn x y z) = (StructReturn x (map (\ (a, b) -> (a, rfExp td mod st p btyp iref b)) y) z)
rfStmt td mod st p btyp iref (MethodCall w x y z) 	= if ((showIDPath w) `elem` st) && (x == "upd") 
then (Write newID newExp z)
else (MethodCall w x (map (rfExp td mod st p btyp iref) y) z)	
where
	newID  = (ID_Vect (showIDPath w) (y !! 0))
	newExp = rfExp td mod st p btyp iref $ y !! 1

recRfCase :: [BSVTypeDef] -> BSVModuleDec -> [String] -> [String] -> Maybe BSVType -> [InterfaceRef] -> Case -> Case 
recRfCase td mod st p btyp iref (x, y) = (x, (rfStmt td mod st p btyp iref y))    
	
rfLocal :: [BSVTypeDef] -> BSVModuleDec -> [String] -> [String] -> LocalVar -> [InterfaceRef] -> LocalVar
rfLocal td m st p (x,(Right y),z) iref = (x, (Right y),(rfExp td m st p (fmap p2bType y) iref z))
rfLocal td m st p (x,(Left y),z) iref = (x, (Left y),(rfExp td m st p y iref z))

-- also gets types of tagged expressions. Also gets reads for Method calls
rfExp :: [BSVTypeDef] -> BSVModuleDec -> [String] -> [String] -> Maybe BSVType -> [InterfaceRef] -> Expression -> Expression
rfExp td m st path btyp iref (Negative x)		= (Negative (rfExp td m st path btyp iref x))
rfExp td m st path btyp iref (Not x) 		= (Not (rfExp td m st path btyp iref x))
rfExp td m st path btyp iref (Equals x y) 		= (Equals (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (NotEquals x y)	= (NotEquals (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (GreaterEquals x y) 	= (GreaterEquals (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (LessEquals x y) 	= (LessEquals (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (Greater x y) 		= (Greater (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (Less x y) 		= (Less (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (And x y) 		= (And (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (Or x y) 		= (Or (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (BitwiseAND x y) 	= (BitwiseAND (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (BitwiseOR x y) 	= (BitwiseOR (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (BitwiseXOR x y) 	= (BitwiseXOR (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (LShift x y) 		= (LShift (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (RShift x y) 		= (RShift (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (BitSelect x y)     	= (BitSelect (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (BitSelectRange x y z) = (BitSelectRange (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y) (rfExp td m st path btyp iref z))
rfExp td m st path btyp iref (BitConcat xs) 	= (BitConcat (map (rfExp td m st path btyp iref) xs))
rfExp td m st path btyp iref (Multiply x y) 	= (Multiply (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (Divide x y) 		= (Divide (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (Modulo x y)  		= (Modulo (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (Add x y) 		= (Add (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (Subtract x y) 	= (Subtract (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (Literal x) 		= (Literal x)
rfExp td m st path btyp iref (Identifier x) 	= (Identifier (removeIRefs x iref))
rfExp td m st path btyp iref (Exp_If x y z) 	= (Exp_If (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y) (rfExp td m st path btyp iref z))
rfExp td m st path btyp iref (Skip) 		= (Skip)
rfExp td m st path btyp iref (RPFlag x) 		= rfExp td m st path btyp iref x
rfExp td m st path (Nothing) iref (Tagged Nothing (Valid x)) 	= Tagged Nothing $ Valid $ rfExp td m st path Nothing iref x
rfExp td m st path (Just bt) iref (Tagged Nothing (Valid x)) 	= Tagged (Just (b2pType bt)) $ Valid $ rfExp td m st path (Just bt) iref x
rfExp td m st path (Nothing) iref (Tagged Nothing Invalid) 	= Tagged (Nothing) Invalid
rfExp td m st path (Just bt) iref (Tagged Nothing Invalid) 	= Tagged (Just (b2pType bt)) Invalid
rfExp td m st path btyp iref (MaybeIf i j x y) 	= (MaybeIf i j (rfExp td m st path btyp iref x) (rfExp td m st path btyp iref y))    
rfExp td m st path btyp iref (MaybeValue x) 	= (MaybeValue (rfExp td m st path btyp iref x) )
rfExp td m st path btyp iref (Exp_FunctionCall x ys) = (Exp_FunctionCall x (map (rfExp td m st path Nothing iref) ys))
rfExp td m st path btyp iref (Exp_MethodCall w x y Nothing)  = if ((showIDPath w) `elem` st) && (x == "sub") 
then (Identifier (ID_Vect (showIDPath w) (y !! 0)))
else exp'
where
	path' = strangeMerge path w iref
	exp' = (Exp_MethodCall path' x y (Just reados))
	tracy = "[rfExp] exp = " ++ (show (Exp_MethodCall w x y Nothing)) ++ "\n\tmodule = " ++ (show (instanceName m)) ++ "\n\tpath = " ++ (show path) ++ "\n\tpath' = " ++ (show path') ++ "\nexp' = " ++ (show exp')
	reados = getReadsBy m (\ x -> True) [] (Exp_MethodCall w x y Nothing)
rfExp td m st path btyp iref (FromMaybe x y) 	= (FromMaybe x (rfExp td m st path btyp iref y))
rfExp td m st path btyp iref (StructCluster x y) = StructCluster x (map (rfStructCase td m st path x' iref) y)
	where
		x' = either (id) (p2bType) $ x
		tracy = ""
rfExp td m st path btyp iref (IsValid x) = (IsValid (boink x))
where
	boink (Valid exp) = (Valid (rfExp td m st path btyp iref exp))
	boink (Invalid) = (Invalid)
	boink (MaybeContainer exp) = (MaybeContainer (rfExp td m st path btyp iref exp))
rfExp td m st path btyp iref (FieldAccess exp i) = (FieldAccess (rfExp td m st path btyp iref exp) (removeIRefs i iref))

strangeMerge :: [String] -> ID_Path -> [InterfaceRef] -> ID_Path
strangeMerge [] i' iref = result
where
	result = i
	tracy = "[strangeMerge 1] i = " ++ (show i') ++ "\nnew i = " ++ (show i) ++ "\nirefs = " ++ (show iref) ++ "\nresult = " ++ (show result)
	i = applyInterfaceReferences iref i'
strangeMerge (x:[]) i' iref = result
	where
	result = if (idTopLevel i) == x then i else mergeIDPaths (strings2idpath (x:[])) (i)
	tracy = "[strangeMerge 2] i = " ++ (show i') ++ "\nnew i = " ++ (show i) ++ "\nirefs = " ++ (show iref) ++ "\nresult = " ++ (show result)  
	i = applyInterfaceReferences iref i'
strangeMerge xs i' iref = result
where
	result = if (idTopLevel i) == (last xs) then mergeIDPaths (strings2idpath (init xs)) (i) else mergeIDPaths (strings2idpath xs) (i)
	tracy = "[strangeMerge 3] i = " ++ (show i') ++ "\nnew i = " ++ (show i) ++ "\nirefs = " ++ (show iref) ++ "\nresult = " ++ (show result)
	i = applyInterfaceReferences iref i'
	

applyInterfaceReferences :: [InterfaceRef] -> ID_Path -> ID_Path
applyInterfaceReferences irefs x = if (lastID x) `elem` (map snd irefs)
					then idInit x
					else x
where
	tracy = "[applyInterfaceReferences] irefs = " ++ (show irefs) ++ "\nx = " ++ (show x)

idInit :: ID_Path -> ID_Path
idInit (ID x) = (ID x)
idInit x = strings2idpath $ init $ idpath2strings x
	
idTopLevel :: ID_Path -> String 
idTopLevel (ID x) = x
idTopLevel (ID_Submod_Struct x y) = x
idTopLevel (ID_Vect x n) = x
	
rfStructCase :: [BSVTypeDef] -> BSVModuleDec -> [String] -> [String] -> BSVType -> [InterfaceRef] -> (String,Expression) -> (String,Expression)
rfStructCase tds mod st path (BSV_Custom btyp) iref (nom, exp) = (nom, (rfExp tds mod st path (Just btyp') iref exp))
where
	btyp' = searchFieldsOf (getTypeDef tds btyp) nom
	

	
removeIRefs :: ID_Path -> [InterfaceRef] -> ID_Path
removeIRefs (ID x) irefs = (ID x)
removeIRefs (ID_Submod_Struct x (ID y)) irefs = if (y `elem` (map snd irefs)) 
					then (ID x)
					else if (x `elem` (map snd irefs)) 
					then (ID y)
					else (ID_Submod_Struct x (ID y)) 
removeIRefs (ID_Submod_Struct x y) irefs = if (x `elem` (map snd irefs)) 
					then (removeIRefs y irefs)
					else (ID_Submod_Struct x (removeIRefs y irefs))
	where
		showy = "[removeIRefs] - " ++ (show (map snd irefs))
removeIRefs (ID_Vect x n) irefs = (ID_Vect x n)
	
letPreproc :: BSVPackage -> BSVPackage
letPreproc universe = BSVPackage{ bsv_packageName = bsv_packageName universe
				, imports = imports universe
				, including = including universe
				, interfaces = interfaces universe
				, bsv_constants = bsv_constants universe
				, bsv_typedefs = bsv_typedefs universe
				, bsv_modules = map convertVectors $ map lpMod $ bsv_modules universe
				, bsv_instDefs = bsv_instDefs universe
				, bsv_functions = map dinvFunc $ map lpFun $ bsv_functions universe
				, bsv_macros = []
				, hexFiles = hexFiles universe 
--				, requestedTransitions = requestedTransitions universe
				}

convertVectors :: BSVModuleDec -> BSVModuleDec
convertVectors mod = BSVModuleDec { mName = mName mod
			, instanceName = instanceName mod
			, instances = map convertVectors $ map lpMod $ instances mod
			, interfaceName = interfaceName mod
			, interfaceDecs = map (cvInter st) $ interfaceDecs mod
			, state = state mod
			, actions = map (cvAct st) $ actions mod
			, attributes = attributes mod
			, rules = map (cvRul st) $ rules mod
			, methods = map (cvMet st) $ methods mod
			}
where 
	st = state mod

cvInter :: [BSVstateDec] -> MidModInterfaceDec -> MidModInterfaceDec
cvInter st (x, y, ms) = (x, y, (map (cvMet st) ms))
	
cvAct :: [BSVstateDec] -> ActionDec -> ActionDec 
cvAct st (x, y, z) = (x, (map (cvStmt st) y), z)

cvRul :: [BSVstateDec] -> RuleDec -> RuleDec 
cvRul st (w, x, y, z) = (w, (cvExp st x), (map (cvStmt st) y), z)

cvMet :: [BSVstateDec] -> MethodBody -> MethodBody 
cvMet st (u, v, w, x, y, z) = (u, v, w, (cvExp st x), (map (cvStmt st) y), z)

cvStmt :: [BSVstateDec] -> Statement -> Statement
cvStmt st (Write x y z) 		= (Write x (cvExp st y) z)
cvStmt st (MethodCall w x y z) 	= (MethodCall w x (map (cvExp st) y) z)	
cvStmt st (ActionCall x y) 	= (ActionCall x y) 					
cvStmt st (Return x y) 		= (Return (cvExp st x) y)	
cvStmt st (If w x y z) 		= (If (cvExp st w) (cvStmt st x) (cvStmt st y) z) 		
cvStmt st (PMatchIf v w x y z)  = (PMatchIf v w (cvStmt st x) (cvStmt st y) z) 	
cvStmt st (ForLoop v w x y z) 	= (ForLoop v (cvExp st w) x (cvStmt st y) z)			
cvStmt st (Switch x y z) 	= (Switch (cvExp st x) (map (recCvCase st) y) z)			
--cvStmt st (Let x y z) 		= (Let x (cvExp st y) z) 		
cvStmt st (LocalDec x y z) 	= (LocalDec (map (rcCvLocal st) x) (cvStmt st y) z)	
cvStmt st (StatementBlock x) 	= (StatementBlock (map (cvStmt st) x) )				
cvStmt st (Void) 		= (Void)	
cvStmt st (StructReturn x y z) = (StructReturn x (map (\ (a, b) -> (a, cvExp st b)) y) z)

recCvCase :: [BSVstateDec] -> Case -> Case 
recCvCase st (x, y) = (x, (cvStmt st y))

rcCvLocal :: [BSVstateDec] -> LocalVar -> LocalVar
rcCvLocal st (x, (Left y), z) = (x, (Right (fmap b2pType y)), (cvExp st z))
rcCvLocal st (x, (Right y), z) = (x, (Right y), (cvExp st z))


cvExp :: [BSVstateDec] -> Expression -> Expression
cvExp st (Negative x)		= (Negative (cvExp st x))
cvExp st (Not x) 		= (Not (cvExp st x))
cvExp st (Equals x y) 		= (Equals (cvExp st x) (cvExp st y))
cvExp st (NotEquals x y)	= (NotEquals (cvExp st x) (cvExp st y))
cvExp st (GreaterEquals x y) 	= (GreaterEquals (cvExp st x) (cvExp st y))
cvExp st (LessEquals x y) 	= (LessEquals (cvExp st x) (cvExp st y))
cvExp st (Greater x y) 		= (Greater (cvExp st x) (cvExp st y))
cvExp st (Less x y) 		= (Less (cvExp st x) (cvExp st y))
cvExp st (And x y) 		= (And (cvExp st x) (cvExp st y))
cvExp st (Or x y) 		= (Or (cvExp st x) (cvExp st y))
cvExp st (BitwiseAND x y) 	= (BitwiseAND (cvExp st x) (cvExp st y))
cvExp st (BitwiseOR x y) 	= (BitwiseOR (cvExp st x) (cvExp st y))
cvExp st (BitwiseXOR x y) 	= (BitwiseXOR (cvExp st x) (cvExp st y))
cvExp st (LShift x y) 		= (LShift (cvExp st x) (cvExp st y))
cvExp st (RShift x y) 		= (RShift (cvExp st x) (cvExp st y))
cvExp st (BitSelect x y)     	= (BitSelect (cvExp st x) (cvExp st y))
cvExp st (BitSelectRange x y z) = (BitSelectRange (cvExp st x) (cvExp st y) (cvExp st z))
cvExp st (BitConcat xs) 	= (BitConcat (map (cvExp st) xs))
cvExp st (Multiply x y) 	= (Multiply (cvExp st x) (cvExp st y))
cvExp st (Divide x y) 		= (Divide (cvExp st x) (cvExp st y))
cvExp st (Modulo x y)  		= (Modulo (cvExp st x) (cvExp st y))
cvExp st (Add x y) 		= (Add (cvExp st x) (cvExp st y))
cvExp st (Subtract x y) 	= (Subtract (cvExp st x) (cvExp st y))
cvExp st (Literal x) 		= (Literal x)
cvExp st (Identifier x) = oboe wige
	where
		wige = cvID st x []
		oboe (Left q) = q
		oboe (Right r) = (Identifier r)
cvExp st (ValueMethodCall w x y z) 		   = (ValueMethodCall w x y z)
cvExp st (Exp_MethodCall inst meth zs q) 		   = (Exp_MethodCall inst meth (map (cvExp st) zs) q)
cvExp st (Exp_If x y z) 			   = (Exp_If (cvExp st x) (cvExp st y) (cvExp st z))
cvExp st (Skip) 		= (Skip)
cvExp st (RPFlag x) 		= cvExp st x
cvExp st (Tagged t (Valid x)) 	= Tagged t $ Valid $ cvExp st x
cvExp st (Tagged t Invalid) 	= Tagged t Invalid
cvExp st (MaybeIf i j x y) 	= (MaybeIf i j (cvExp st x) (cvExp st y))
cvExp st (Exp_FunctionCall x ys) = (Exp_FunctionCall x (map (cvExp st) ys))
cvExp st (FromMaybe x y) 	= (FromMaybe x (cvExp st y))
cvExp st (MaybeValue x) = (MaybeValue (cvExp st x))
cvExp st (IsValid x) = (IsValid (boink x))
where
	boink (Valid exp) = (Valid (cvExp st exp))
	boink (Invalid) = (Invalid)
	boink (MaybeContainer exp) = (MaybeContainer (cvExp st exp))
cvExp st (StructCluster x y) = StructCluster x (map (\ (y1,y2) -> (y1,(cvExp st y2))) y)
cvExp st (FieldAccess x p) = oboe wige
	where 
		wige = cvID st p []
		oboe (Left (BitSelect y z)) = (BitSelect (cvExp st y) z) 
		oboe (Right z) = FieldAccess (cvExp st x) (z)
cvExp _ x = error $ show x

cvID :: [BSVstateDec] -> ID_Path -> [String] -> Either Expression ID_Path
cvID st (ID_Submod_Struct inst path) i = (cvID st path (i ++ [inst]))
cvID st (ID str) i = Right (string2IDPath (intercalate "`" (i ++ [str])))
cvID st (ID_Vect str i) i' = huh whatsItGonnaBe 
	where
		whatsItGonnaBe = vectorDecision st str i
		huh (Left x) = Left x
		huh (Right x) = Right $ mergeIDPaths (string2IDPath (intercalate "`" i')) (ID_Vect str i)

vectorDecision :: [BSVstateDec] -> String -> Literal -> Either Expression ID_Path
vectorDecision state nom lit = if nom `elem` (vectorNames state) 
then (Right (ID_Vect nom lit))
else Left (BitSelect (Identifier (ID nom)) lit)
	
vectorNames :: [BSVstateDec] -> [String]
vectorNames [] = []
vectorNames ((BSV_Vector (ID nom) _ _ _):xs) = nom : (vectorNames xs)
vectorNames (x:xs)  = (vectorNames xs)

dinvFunc :: BSVFunction -> BSVFunction 
dinvFunc (n, args, typ, stmts) = (n, args, typ, (map (dinvStmt args) stmts))

dinvStmt :: [Argument] -> Statement -> Statement
dinvStmt st (Write x y z) 		= (Write x (dinvExp st y) z)
dinvStmt st (MethodCall w x y z) 	= (MethodCall w x (map (dinvExp st) y) z)	
dinvStmt st (ActionCall x y) 	= (ActionCall x y) 					
dinvStmt st (Return x y) 		= (Return (dinvExp st x) y)	
dinvStmt st (If w x y z) 		= (If (dinvExp st w) (dinvStmt st x) (dinvStmt st y) z) 		
dinvStmt st (PMatchIf v w x y z)  = (PMatchIf v w (dinvStmt st x) (dinvStmt st y) z) 	
dinvStmt st (ForLoop v w x y z) 	= (ForLoop v (dinvExp st w) x (dinvStmt st y) z)			
dinvStmt st (Switch x y z) 	= (Switch (dinvExp st x) (map (recDinvCase st) y) z)			
--dinvStmt st (Let x y z) 		= (Let x (dinvExp st y) z) 		
dinvStmt st (LocalDec x y z) 	= (LocalDec (map (rcDinvLocal st) x) (dinvStmt st y) z)	
dinvStmt st (StatementBlock x) 	= (StatementBlock (map (dinvStmt st) x) )				
dinvStmt st (Void) 		= (Void)	
dinvStmt st (StructReturn x y z) = (StructReturn x (map (\ (a, b) -> (a, dinvExp st b)) y) z)

recDinvCase :: [Argument] -> Case -> Case 
recDinvCase st (x, y) = (x, (dinvStmt st y))

rcDinvLocal :: [Argument] -> LocalVar -> LocalVar
rcDinvLocal st (x, (Left y), z) = (x, (Right (fmap b2pType y)), (dinvExp st z))
rcDinvLocal st (x, (Right y), z) = (x, (Right y), (dinvExp st z))
					
dinvExp :: [Argument] -> Expression -> Expression
dinvExp arg (Negative x)	= (Negative (dinvExp arg x))
dinvExp arg (Not x) 		= (Not (dinvExp arg x))
dinvExp arg (Equals x y) 	= (Equals (dinvExp arg x) (dinvExp arg y))
dinvExp arg (NotEquals x y)	= (NotEquals (dinvExp arg x) (dinvExp arg y))
dinvExp arg (GreaterEquals x y) = (GreaterEquals (dinvExp arg x) (dinvExp arg y))
dinvExp arg (LessEquals x y)= (LessEquals (dinvExp arg x) (dinvExp arg y))
dinvExp arg (Greater x y) 	= (Greater (dinvExp arg x) (dinvExp arg y))
dinvExp arg (Less x y) 		= (Less (dinvExp arg x) (dinvExp arg y))
dinvExp arg (And x y) 		= (And (dinvExp arg x) (dinvExp arg y))
dinvExp arg (Or x y) 		= (Or (dinvExp arg x) (dinvExp arg y))
dinvExp arg (BitwiseAND x y)= (BitwiseAND (dinvExp arg x) (dinvExp arg y))
dinvExp arg (BitwiseOR x y) = (BitwiseOR (dinvExp arg x) (dinvExp arg y))
dinvExp arg (BitwiseXOR x y)= (BitwiseXOR (dinvExp arg x) (dinvExp arg y))
dinvExp arg (LShift x y) 	= (LShift (dinvExp arg x) (dinvExp arg y))
dinvExp arg (RShift x y) 	= (RShift (dinvExp arg x) (dinvExp arg y))
dinvExp arg (BitSelect x y) = (BitSelect (dinvExp arg x) (dinvExp arg y))
dinvExp arg (BitSelectRange x y z) = (BitSelectRange (dinvExp arg x) (dinvExp arg y) (dinvExp arg z))
dinvExp arg (BitConcat xs) 	= (BitConcat (map (dinvExp arg) xs))
dinvExp arg (Multiply x y) 	= (Multiply (dinvExp arg x) (dinvExp arg y))
dinvExp arg (Divide x y) 	= (Divide (dinvExp arg x) (dinvExp arg y))
dinvExp arg (Modulo x y)  	= (Modulo (dinvExp arg x) (dinvExp arg y))
dinvExp arg (Add x y) 		= (Add (dinvExp arg x) (dinvExp arg y))
dinvExp arg (Subtract x y) 	= (Subtract (dinvExp arg x) (dinvExp arg y))
dinvExp arg (Literal x) 	= (Literal x)
dinvExp arg (Exp_If x y z) 	= (Exp_If (dinvExp arg x) (dinvExp arg y) (dinvExp arg z))
dinvExp arg (Skip) 		    = (Skip)
dinvExp arg (RPFlag x) 		= dinvExp arg x
dinvExp arg (Tagged t (Valid x)) 	= Tagged t $ Valid $ dinvExp arg x
dinvExp arg (Tagged t Invalid) 	= Tagged t Invalid
dinvExp arg (MaybeIf i j x y) 	= (MaybeIf i j (dinvExp arg x) (dinvExp arg y))    
dinvExp arg (Exp_FunctionCall x ys) = (Exp_FunctionCall x (map (dinvExp arg) ys))
dinvExp arg (Exp_MethodCall w x ys q)  = (Exp_MethodCall w x (map (dinvExp arg) ys) q)
dinvExp arg (FromMaybe x y) 	= (FromMaybe x (dinvExp arg y))
dinvExp arg (StructCluster x y) = StructCluster x (map (\ (y1,y2) -> (y1,(dinvExp arg y2))) y)
dinvExp arg (Identifier x) 	= vectorProcess arg (Identifier x)
					
vectorProcess :: [Argument] -> Expression -> Expression
vectorProcess arg (Identifier (ID_Vect n i)) = if (n `elem` (vectorNames' arg)) then (Identifier (ID_Vect n i)) else (BitSelect (Identifier (ID n)) i)
vectorProcess _ x = x

-- kind of a moot point until vectors are supported as function arguments...
vectorNames' :: [Argument] -> [String]
vectorNames' _ = []
--vectorNames' ((n, ()):xs) = nom : (vectorNames xs)
--vectorNames' (x:xs)  = (vectorNames xs)

lpMod :: BSVModuleDec -> BSVModuleDec				
lpMod mod = BSVModuleDec{ mName = mName mod
			, instanceName = instanceName mod
			, instances = map lpMod $ instances mod
			, interfaceName = interfaceName mod
			, interfaceDecs = map lpInter $ interfaceDecs mod
			, state = state mod
			, actions = map lpAct $ actions mod
			, attributes = attributes mod
			, rules = map lpRul $ rules mod
			, methods = map lpMet $ methods mod
			}

lpInter :: MidModInterfaceDec -> MidModInterfaceDec
lpInter (x, y, ms) = (x, y, (map lpMet ms))
			
lpAct :: ActionDec -> ActionDec
lpAct (x, y, z) = (x, (lpStmts y), z)

lpRul :: RuleDec -> RuleDec
lpRul (w, x, y, z) = (w, x, (lpStmts y), z)
	-- twace ("[T] Calling lpRul\n\t" ++ (intercalate "\n\t" (map show y)) ++ " \n\t\t VVVVVVVVVVVVVVVVV\n\t"  ++ (intercalate "\n\t" (map show (lpStmts y)))) $ (w, x, (lpStmts y), z)
	--twace ("[T] Calling lpRul " ++ (show w) {- ++ "\n\t" ++ (intercalate "\n\t" (map show (lpStmts y))) -} ) (w, x, (lpStmts y), z)

lpMet :: MethodBody -> MethodBody 
lpMet (u, v, w, x, y, z) = (u, v, w, x, (lpStmts y), z)
			
lpFun :: BSVFunction -> BSVFunction
lpFun (n, a, t, stmts) = (n,a,t,(lpStmts stmts))

lpStmts :: [Statement] -> [Statement]
lpStmts stmts = if (null lets) 
	-- (\ x -> twace ("[lpStmts] BEFORE - " ++ (show stmts) ++ "\n\nAFTER - " ++ (show x)) x) $ if (null lets) 
		then map lpStmt stmts
	else (amalgamateLets lets (map lpStmt notLets)):[]
where 		  
	lets = getLets stmts
	notLets = filter (\ x -> not (x `elem` lets)) stmts
	
lpStmt :: Statement -> Statement
lpStmt (Write x y z) 		= (Write x y z) 						
lpStmt (MethodCall w x y z) 	= (MethodCall w x y z)					
lpStmt (ActionCall x y) 	= (ActionCall x y) 					
lpStmt (Return x y) 		= (Return x y)
lpStmt (StructReturn x y z) 	= (StructReturn x y z)
lpStmt (If w x y z) 		= (If w (lpStmt x) (lpStmt y) z) 		
lpStmt (PMatchIf v w x y z)    	= (PMatchIf v w (lpStmt x) (lpStmt y) z) 	
lpStmt (ForLoop v w x y z) 	= (ForLoop v w x (lpStmt y) z)			
lpStmt (Switch x y z) 		= (Switch x (map reclpCase y) z)							
lpStmt (StatementBlock x) 	= (StatementBlock (lpStmts x) )				
lpStmt (Void) 			= (Void)	
lpStmt (LocalDec x y z) = (LocalDec x (lpStmt y) z)
-- lpStmt x = error $ show x
--lpStmt (LocalDec x y z) 	= (LocalDec x (lpStmt y) z)				

-- | Applies let preprocessor to case statments
reclpCase :: Case -> Case
reclpCase (x, y) = (x, (lpStmt y))

-- | Returns all local variable assignments in a list of statements  
getLets :: [Statement] -> [Statement]
getLets [] = []
getLets ((LocalDec x y z):xs) = (LocalDec x y z):(getLets xs)
getLets (x:xs) = (getLets xs)

-- | Merges all individual let statements into one let block.
amalgamateLets :: [Statement] -> [Statement] -> Statement
amalgamateLets lets' notLets' = ( LocalDec allDecs (if (length notLets == 1) then head notLets else StatementBlock (notLets ++ allStmts)) allAtts)
where
	lets = fst $ mergeDeeperLets (lets', notLets') notLets'
	notLets = snd $ mergeDeeperLets (lets', notLets') notLets'
	allDecs = concat $ map ( \ (LocalDec x _ _ ) -> x) lets
	allAtts = concat $ map ( \ (LocalDec _ _ x ) -> x) lets
	allStmts = map ( \ (LocalDec _ x _ ) -> x) lets
	
-- | Finds all statements not containing lets at the top level but containing them deeper on, and merges the let assignments into an if tree 
-- | and modifies the corresponding local variable declaration.  Performed as a fold over the non-let statements
mergeDeeperLets :: ([Statement], [Statement]) -> [Statement] -> ([Statement], [Statement])
mergeDeeperLets x [] = x
mergeDeeperLets (lets, notLets) (x:xs) = mergeDeeperLets (lets', notLets') xs
where
	x' = bubbleUpLets lets x
	lets'    = if (containsLVarDec x)
				then lets `replaceLetWith` x'
				else lets
	notLets' = if (containsLVarDec x )
				then delete x notLets
				else notLets 

replaceLetWith :: [Statement] -> Statement -> [Statement]       
replaceLetWith [] _ = []
replaceLetWith ((LocalDec ((n1, a, b):[]) c d):xs) (LocalDec ((n2, w, x):[]) y z) 
| n1 == n2  = (LocalDec ((n2, w, x):[]) y z) : xs 
| otherwise = (LocalDec ((n1, a, b):[]) c d) : (xs `replaceLetWith` (LocalDec ((n2, w, x):[]) y z))
				
-- | recursively raises local variable declarations until they are the top level of the statement
bubbleUpLets :: [Statement] -> Statement -> Statement
bubbleUpLets lets (Write x y z) = (Write x y z)
bubbleUpLets lets (Return x y) = (Return x y)
bubbleUpLets lets (LocalDec x y z) = (LocalDec x y z)
bubbleUpLets lets (If x y z atts) = (LocalDec (var:[]) stmt atts)
	where
		bubbledThen = bubbleUpLets lets y
		bubbledElse = bubbleUpLets lets z
		tVars = getLetVars bubbledThen
		eVars = getLetVars bubbledElse
		ifExp = genIfExp x tVars eVars
		matchingLV = lets `findLVarsWithName` (getLVarName tVars eVars)
		var = mergeLocalVars ((\ (LocalDec (x:[]) _ _) -> x) matchingLV) ifExp
		stmt = (\ (LocalDec _ x _) -> x) matchingLV
		atts = (\ (LocalDec _ _ x) -> x) matchingLV

mergeLocalVars :: LocalVar -> (Expression -> Expression) -> LocalVar
mergeLocalVars (nom, typ, exp) fExp = (nom, typ, (fExp exp))
		
findLVarsWithName :: [Statement] -> ID_Path -> Statement
findLVarsWithName ((LocalDec ((n, w, x):[]) y z):xs) n' 
| n == n'   = (LocalDec ((n, w, x):[]) y z)
| otherwise = findLVarsWithName xs n'
findLVarsWithName (x:xs) n' = findLVarsWithName xs n'
		
getLVarName :: Maybe LocalVar -> Maybe LocalVar -> ID_Path 
getLVarName (Just (n, _, _)) _ = n
getLVarName (Nothing) (Just (n, _, _)) = n
-- getLVarName gd Nothing Nothing = Nothing -- case should not happen
		
genIfExp :: Expression -> Maybe LocalVar -> Maybe LocalVar -> (Expression -> Expression)        
genIfExp gd (Just (_, _, tExp)) (Just (_, _, eExp)) = (\ q -> (Exp_If gd tExp eExp))
genIfExp gd (Nothing) (Just (_, _, eExp)) = (\ q -> (Exp_If gd q eExp))
genIfExp gd (Just (_, _, tExp)) (Nothing) = (\ q -> (Exp_If gd tExp q ))
-- genIfExp gd Nothing Nothing = id -- case should not happen
		
getLetVars :: Statement -> Maybe LocalVar
getLetVars (LocalDec (x:[]) Void _) = Just x
getLetVars (Void) = Nothing
getLetVars (Write _ _ _) = Nothing
	
-- | Recursively tests Statement to see if it contains local variable declarations.  Assumes statement blocks have been split.     
containsLVarDec :: Statement -> Bool
containsLVarDec (Write _ _ _) = False
containsLVarDec (If _ x y _) = containsLVarDec x || containsLVarDec y
containsLVarDec (PMatchIf _ _ x y _) = containsLVarDec x ||  containsLVarDec y
containsLVarDec (LocalDec _ _ _) = True
containsLVarDec (Void) = False
containsLVarDec (Return _ _) = False
containsLVarDec (StructReturn _ _ _) = False
containsLVarDec (MethodCall _ _ _ _) = False 
containsLVarDec (ActionCall _ _) = False
containsLVarDec (StatementBlock xs) = or (map containsLVarDec xs)
containsLVarDec (Switch _ cs _) = or (map checkCase cs)
where
	checkCase (l, stmt) = containsLVarDec stmt 
containsLVarDec x = error $ show x

	
splitStatementPreprocPackage :: BSVPackage -> BSVPackage
splitStatementPreprocPackage universe = BSVPackage 
				{ bsv_packageName = bsv_packageName universe
				, imports = imports universe
				, including = including universe
				, interfaces = interfaces universe
				, bsv_constants = bsv_constants universe
				, bsv_typedefs = bsv_typedefs universe
				, bsv_modules = q -- error $ show $ map (instanceName) $ bsv_modules universe
				, bsv_instDefs = bsv_instDefs universe
				, bsv_functions = map (splitFunctions (head q)) $ bsv_functions universe
				, bsv_macros = []
				, hexFiles = hexFiles universe
--				, requestedTransitions = requestedTransitions universe
				}
where 
	q =  map splitStatementPreprocModule (bsv_modules universe)
	
splitStatementPreprocModule :: BSVModuleDec -> BSVModuleDec 
splitStatementPreprocModule mod = BSVModuleDec 	{ mName = mName mod
					, instanceName = instanceName mod
					, instances = map splitStatementPreprocModule $ instances mod -- error $ show $ map (interfaceName) (instances mod) -- map splitStatementPreprocModule $ instances mod
					, interfaceName = interfaceName mod
					, state = state mod
					, actions = map (splitPreprocAction mod) (actions mod)
					, attributes = attributes mod
					, rules = map (splitPreprocRule mod) (rules mod)
					-- (\ x -> trace ("[T] splitStatementPreprocModule - " ++ (show x)) x) $ map splitPreprocRule (rules mod)
					, methods = map (splitPreprocMethod mod) (methods mod)
					, interfaceDecs = map (splitPreprocInterface mod) (interfaceDecs mod)
					}

splitFunctions :: BSVModuleDec -> BSVFunction -> BSVFunction						
splitFunctions m (w, x, y, z) = (w, x, y, step3:[])
	-- trace ("[T] splitFunctions - function = " ++ w ++ " \nstatement = " ++ (show z) ++ "\nstatement' = " ++ (show step3)) (w, x, y, step3:[])
	where 
		step1 = splitStatementBlocks m z -- (\ x -> twace ("[T] splitFunctions step 1 - " ++ (show x)) x) $ splitStatementBlocks z
		step2 = map eliminateSingletonSBs step1 --(\ x -> twace ("[T] splitFunctions step 2 - " ++ (show x)) x) $ resolveMultipleStatements step1
		step3 = resolveMultipleStatements step2 --(\ x -> twace ("[T] splitFunctions step 3 - " ++ (show x)) x) $ eliminateSingletonSBs step2

splitPreprocInterface :: BSVModuleDec -> MidModInterfaceDec -> MidModInterfaceDec 
splitPreprocInterface m (x, y, ms) = (x, y, (map (splitPreprocMethod m) ms))
		
splitPreprocAction :: BSVModuleDec -> ActionDec -> ActionDec
splitPreprocAction m (x, y, z) = (x, (splitStatementBlocks m y), z)

splitPreprocRule :: BSVModuleDec -> RuleDec -> RuleDec
splitPreprocRule m (w, x, y, z) = (w, x, (splitStatementBlocks m y), z)
	--trace ("[T] calling splitPreprocRule -" ++ (show w) ++ "\n" ++ (show y) ++ "\nVVVVVVVVVVVVV\n" ++ (show (splitStatementBlocks y) )) $ (w, x, (splitStatementBlocks y), z)

splitPreprocMethod :: BSVModuleDec -> MethodBody -> MethodBody
splitPreprocMethod m (u, v, w, x, y, z) = (u, v, w, x, (splitStatementBlocks m y), z)
					
	
b2pFuncs :: BSVFunction -> PVSFunction
b2pFuncs (nom, args, typ, stmts) = (nom
, (map b2pArgument args)
, (b2pType typ)
, (extractExpression [] (ID "") [] (resolveMultipleStatements stmts))
)

resolveMultipleStatements :: [Statement] -> Statement
resolveMultipleStatements (x:[]) = x
resolveMultipleStatements ((Return exp atts):_) = Return exp atts
resolveMultipleStatements ((StructReturn x y z):_) = (StructReturn x y z)
resolveMultipleStatements (x:xs) = resolveMultipleStatements xs 

b2pArgument :: Argument -> PVSArgument
b2pArgument (nom, typ, _) = (nom, (b2pType typ))
	
b2pInstDef :: BSVPackage -> BSVInstDef -> PVSInstDef
b2pInstDef pkg (x, ys) = (x, (map (b2pInstDef' pkg x) ys))

b2pInstDef' :: BSVPackage -> Name -> (Name, Expression) -> (Name, Expression)
b2pInstDef' pkg nom (x, (Identifier (ID "defaultValue"))) = (x, properConstructor)
where 
	tds = bsv_typedefs pkg
	td = getTypeDef tds nom
	typ = searchFieldsOf td x
	properConstructor = extractConstructor typ
b2pInstDef' _ _ x = x

extractConstructor :: BSVType -> Expression
extractConstructor (BSV_Custom x) = Identifier $ ID $ "mk" ++ x

searchFieldsOf :: BSVTypeDef -> Name -> BSVType
searchFieldsOf (BSV_Struct x ys) n = maybe (error "Error! Did not find field!") (id) $ lookup n ys
searchFieldsOf x n = error "What?! Who did this?! WHYYYYYYYYYYYYYYY!!!"

getTypeDef :: [BSVTypeDef] -> Name -> BSVTypeDef
getTypeDef ((BSV_Struct x ys):xs) n = if x == n then (BSV_Struct x ys) else getTypeDef xs n
getTypeDef (x:xs) n = getTypeDef xs n

instantiateSubmods :: String -> BSVPackage -> BSVPackage 
instantiateSubmods topMod universe = update universe 
where 
	mod = maybe (error "Module Not Found!") (id) $ findMod universe topMod
	newTopMod = (\ x -> x {instanceName = "root"}) $ instantiateSubmods' universe mod 
	newMods = newTopMod : (filter (\ x -> not ((mName x) == topMod) ) (bsv_modules universe) )
	update x = x {bsv_modules = newMods}

instantiateSubmods' :: BSVPackage -> BSVModuleDec -> BSVModuleDec 
instantiateSubmods' universe mod = update mod
where 
	newInstances = collateMods $ map (instantiateSubmod universe mod) (state mod)
	newInstancesRecursive = map (instantiateSubmods' universe) newInstances
	update x = x {instances = newInstancesRecursive}

collateMods :: [Maybe BSVModuleDec] -> [BSVModuleDec]
collateMods [] = []
collateMods ((Nothing):xs) = collateMods xs
collateMods ((Just x):xs) = x : (collateMods xs)

instantiateSubmod :: BSVPackage -> BSVModuleDec -> BSVstateDec -> Maybe BSVModuleDec 
instantiateSubmod universe mod (BSV_SubModuleDec inter nom inst) = (Just (update newMod))
where 
	newMod = maybe (error "Module Not Found!") (id) $ findMod universe inter
	update x = x {instanceName = inst} 
instantiateSubmod universe mod x = Nothing

migrateAttributes :: BSVPackage -> BSVPackage
migrateAttributes universe = update universe 
where 
	mods = bsv_modules universe 
	newMods = map migrateModAttributes mods
	update x = x {bsv_modules = newMods}

migrateModAttributes :: BSVModuleDec -> BSVModuleDec 
migrateModAttributes mod = update mod
where 
	oldRules = rules mod 
	newRules = map migrateRuleAttributes oldRules 
	newAtts = (attributes mod) ++ (getModAttsRM newRules)
	newRules2 = map crushModAttsR newRules
	update x = x {attributes = newAtts, rules = newRules2}

migrateRuleAttributes :: RuleDec -> RuleDec
migrateRuleAttributes (nom, guard, stmts, atts) = (nom, guard, newStmts, newAtts)
where 
	newStmts = map crushModAttsS stmts 
	newAtts  = atts ++ (getModAttsSR stmts)

getModAttsRM :: [RuleDec] -> [ModuleAttribute]
getModAttsRM [] = []
getModAttsRM ((_, _, _, atts):xs) = (convertModAttsR atts) ++ (getModAttsRM xs)

getModAttsSR :: [Statement] -> [RuleAttribute]
getModAttsSR [] = []
getModAttsSR (x:xs) = (convertModAttsS (getModAttsS x)) ++ (getModAttsSR xs)

getModAttsS :: Statement -> [StatementAttribute]
getModAttsS (Write x y atts) = atts
getModAttsS (MethodCall x y z atts) = atts 
getModAttsS (ActionCall x atts) = atts
getModAttsS (Return x atts) = atts
getModAttsS (If x y z atts) = atts ++ (getModAttsS y) ++ (getModAttsS z) 
getModAttsS (ForLoop w x y z atts) = atts
getModAttsS (Switch x y atts) = atts
--getModAttsS (Let x y atts) = atts
getModAttsS (LocalDec x y atts) = atts ++ (getModAttsS y)
getModAttsS (Void) = []
getModAttsS (StatementBlock xs) = concat $ map getModAttsS xs
getModAttsS (PMatchIf w x y z atts) = atts ++ (getModAttsS y) ++ (getModAttsS z) 

convertModAttsR :: [RuleAttribute] -> [ModuleAttribute]
convertModAttsR [] = [] 
convertModAttsR ((Fire_When_Enabled):ys) = convertModAttsR ys
convertModAttsR ((No_Implicit_Conditions):ys) = convertModAttsR ys
convertModAttsR ((Rul_Descending_Urgency xs):ys) = (Descending_Urgency xs) : (convertModAttsR ys)
convertModAttsR ((Rul_Execution_Order xs):ys) = (Mod_Execution_Order xs) : (convertModAttsR ys)
convertModAttsR ((Rul_Mutually_Exclusive xs):ys) = (Mod_Mutually_Exclusive xs) : (convertModAttsR ys)
convertModAttsR ((Rul_Conflict_Free xs):ys) = (Mod_Conflict_Free xs) : (convertModAttsR ys)
convertModAttsR ((Rul_Preempts xs):ys) = (Mod_Preempts xs) : (convertModAttsR ys)
convertModAttsR ((Rul_Doc x):ys) = convertModAttsR ys

convertModAttsS :: [StatementAttribute] -> [RuleAttribute]
convertModAttsS [] = [] 
convertModAttsS ((Split):ys) = convertModAttsS ys
convertModAttsS ((NoSplit):ys) = convertModAttsS ys
convertModAttsS ((Sta_Descending_Urgency xs):ys) = (Rul_Descending_Urgency xs) : (convertModAttsS ys)
convertModAttsS ((Sta_Execution_Order xs):ys) = (Rul_Execution_Order xs) : (convertModAttsS ys)
convertModAttsS ((Sta_Mutually_Exclusive xs):ys) = (Rul_Mutually_Exclusive xs) : (convertModAttsS ys)
convertModAttsS ((Sta_Conflict_Free xs):ys) = (Rul_Conflict_Free xs) : (convertModAttsS ys)
convertModAttsS ((Sta_Preempts xs):ys) = (Rul_Preempts xs) : (convertModAttsS ys)
convertModAttsS ((Sta_Doc x):ys) = convertModAttsS ys

crushModAttsR :: RuleDec -> RuleDec 
crushModAttsR (x, y, z, atts) = (x, y, z, (crushModAttsRA atts))

crushModAttsRA :: [RuleAttribute] -> [RuleAttribute]
crushModAttsRA [] = []
crushModAttsRA ((Fire_When_Enabled):ys) = (Fire_When_Enabled):(crushModAttsRA ys)
crushModAttsRA ((No_Implicit_Conditions):ys) = (No_Implicit_Conditions):(crushModAttsRA ys)
crushModAttsRA ((Rul_Descending_Urgency xs):ys) = crushModAttsRA ys
crushModAttsRA ((Rul_Execution_Order xs):ys) = crushModAttsRA ys
crushModAttsRA ((Rul_Mutually_Exclusive xs):ys) = crushModAttsRA ys
crushModAttsRA ((Rul_Conflict_Free xs):ys) = crushModAttsRA ys
crushModAttsRA ((Rul_Preempts xs):ys) = crushModAttsRA ys
crushModAttsRA ((Rul_Doc x):ys) = (Rul_Doc x) : (crushModAttsRA ys)

crushModAttsS :: Statement -> Statement
crushModAttsS (Write x y atts) = (Write x y (crushModAttsSA atts))
crushModAttsS (MethodCall x y z atts) = (MethodCall x y z (crushModAttsSA atts))
crushModAttsS (ActionCall x atts) = (ActionCall x (crushModAttsSA atts))
crushModAttsS (Return x atts) = (Return x (crushModAttsSA atts))
crushModAttsS (If x y z atts) = (If x (crushModAttsS y) (crushModAttsS z) (crushModAttsSA atts))
crushModAttsS (ForLoop w x y z atts) = (ForLoop w x y (crushModAttsS z) (crushModAttsSA atts))
crushModAttsS (Switch x y atts) = (Switch x (map crushModAttsCases y) (crushModAttsSA atts))
--crushModAttsS (Let x y atts) = (Let x y (crushModAttsSA atts))
crushModAttsS (LocalDec x y atts) = (LocalDec x (crushModAttsS y) (crushModAttsSA atts))
crushModAttsS (StatementBlock xs) = (StatementBlock (map crushModAttsS xs))
crushModAttsS (PMatchIf w x y z atts) = (PMatchIf w x (crushModAttsS y) (crushModAttsS z) (crushModAttsSA atts))
crushModAttsS (Void) = (Void)

crushModAttsCases :: Case -> Case
crushModAttsCases (x, y) = (x, (crushModAttsS y))

crushModAttsSA :: [StatementAttribute] -> [StatementAttribute]
crushModAttsSA [] = [] 
crushModAttsSA ((Split):ys) = ((Split):(crushModAttsSA ys))
crushModAttsSA ((NoSplit):ys) = ((NoSplit):(crushModAttsSA ys))
crushModAttsSA ((Sta_Descending_Urgency xs):ys) = crushModAttsSA ys
crushModAttsSA ((Sta_Execution_Order xs):ys) = crushModAttsSA ys
crushModAttsSA ((Sta_Mutually_Exclusive xs):ys) = crushModAttsSA ys
crushModAttsSA ((Sta_Conflict_Free xs):ys) = crushModAttsSA ys
crushModAttsSA ((Sta_Preempts xs):ys) = crushModAttsSA ys
crushModAttsSA ((Sta_Doc x):ys) = ((Sta_Doc x):(crushModAttsSA ys))

-- | Generate a single PVS transition, containing an index number, argument list, list of value methods, and a list of transition tables.  
genTransitions :: BSVPackage -> String -> (Integer, [ID_Path]) -> PVStransition
genTransitions uni topMod (num, reqMeths') = (num, revisedArgsList, valMeths, transTables)
where 
	mod = maybe (error "Module Not found! ") (id) $ findMod uni topMod
	reqMeths = map lastID reqMeths'
	newMod = propagateMethodCalls uni mod reqMeths' []
	schedule = genRuleSchedules uni newMod [] reqMeths
	valMeths = orderValueMethods uni mod transTables $ genValueMethods uni transTables mod topMod ("pre":[])
		-- old version concat $ map (genValueMethods uni transTables) (reverse (bsv_modules uni))
	--table = genTransition uni newMod schedule reqMeths
	methArgs = map (\ (n,_,x,_,_,_) -> zip x (typos uni mod n)) (filter (\ (x,_,_,_,_,_) -> (x `elem` reqMeths)) (methods mod))
	methArgs' = map (map (\ ((a,b),c) -> (a,c))) methArgs
	revisedArgsList = applyNumbersToArgsCD ((length methArgs') - 1) (zip reqMeths methArgs')
	methArgs'' = map fst $ concat $ map snd revisedArgsList
	transTables = genTransTable uni mod methArgs'' (("pre"):[]) schedule (state mod) 
	showy = "[genTranstitions] schedule - " ++ (show (num, reqMeths))

applyNumbersToArgsCD :: Int -> [(String, [(String, PVSType)])] -> [(String, [(String, PVSType)])]
applyNumbersToArgsCD _ [] = []
applyNumbersToArgsCD n ((x, ys):zs) = (x, (map (applyNumbersToArgs' n) ys)) : (applyNumbersToArgsCD (n - 1) zs)
	
	
applyNumbersToArgs :: Int -> [(String, [(String, PVSType)])] -> [(String, [(String, PVSType)])]
applyNumbersToArgs _ [] = []
applyNumbersToArgs n ((x, ys):zs) = (x, (map (applyNumbersToArgs' n) ys)) : (applyNumbersToArgs (n + 1) zs)

applyNumbersToArgs' :: Int -> (String, PVSType) -> (String, PVSType)
applyNumbersToArgs' n (x, y) = ((x ++ "_" ++ (show n)) , y) 

orderValueMethods ::  BSVPackage -> BSVModuleDec -> [TransitionTable] -> [ValueMethod] -> [ValueMethod] 
orderValueMethods _ _ _ [] = []
orderValueMethods uni mod tables vs = supremum : (orderValueMethods uni mod tables theRest)
where
	tracy = "[orderValueMethods] supremum = " ++ (justTheName supremum) ++ "\n\ndepsList = " ++ (intercalate "\n\t" (map (\ (x,y) -> (justTheName y) ++ " => " ++(show x) ) deps))
	justTheName (x,y,_,_,_,_,_) = y ++ "`" ++ x
	supremum = maybe (error errormsg) (id) $ lookup [] deps
	errormsg = "[orderValueMethods] Circular Value Method Reference Error! - dependencies : \n\t"++ (intercalate "\n\t" (map (\ (a,b) -> (justTheName b) ++ " => " ++ (show a) ) deps))
	refs = map stringize vs
	theRest = delete supremum vs
	deps = map (killNonreferencedMethods refs) $ map (getMethodDependencies uni mod tables) vs
	stringize (x, _, _, y,_,_,_ ) = (intercalate "`" (tail y)) ++ "`" ++ x
	
getMethodDependencies :: BSVPackage -> BSVModuleDec -> [TransitionTable] -> ValueMethod -> ([String], ValueMethod)
getMethodDependencies uni mod tables (t,u,v,path,x,exp,z) = (mCalls, (t,u,v,path,x,exp,z))
where
	tracy = "[getMethodDependencies] method = " ++ (show t) ++ "\npath = " ++ (show path) ++ "\nexp = " ++ (show exps) ++ "\ndeps " ++ (show mCalls)
	exps = exp : (getWireMCalls uni mod tables path exp)
	mCalls = map stringize $ concat $ map (getMethCallsExp uni mod True) exps  
	stringize (Exp_MethodCall x y _ _) = (lastID x) ++ "`" ++ y

getWireMCalls :: BSVPackage -> BSVModuleDec -> [TransitionTable] -> [String] -> Expression -> [Expression]
getWireMCalls uni mod tables path exp = concat $ map (getMCallsOverTree uni mod) trees
	where
		tracy = "[getWireMCalls] wires = " ++ (show wires) ++ "\ntrees = " ++ (show trees) ++ "\nMCalls = " ++ (show (map (getMCallsOverTree uni mod) trees))
		reads = getReadsBy mod (\ x -> True) [] exp
		wires = map strings2idpath $ map (\ y -> path ++ y) $ map idpath2strings reads
		trees = map treeize $ catMaybes $ map (lookupWireTable tables) wires 
		treeize (TransDWire _ t _) = t

getMCallsOverTree :: BSVPackage -> BSVModuleDec -> SpecificTree -> [Expression]        
getMCallsOverTree uni mod (SpecStem gd (Right ttree) ftree) = (getMethCallsExp uni mod True gd) ++ (getMCallsOverTree uni mod ttree) ++ (getMCallsOverTree uni mod ftree)
getMCallsOverTree uni mod (SpecStem gd (Left texp) ftree) = (getMethCallsExp uni mod True gd) ++ (getMethCallsExp uni mod True texp) ++ (getMCallsOverTree uni mod ftree)
getMCallsOverTree uni mod (SpecLeaf gd texp fexp) = (getMethCallsExp uni mod True gd) ++ (getMethCallsExp uni mod True texp) ++ (getMethCallsExp uni mod True fexp)
getMCallsOverTree uni mod (SpecEx exp) = getMethCallsExp uni mod True exp


killSidewaysReferences :: BSVPackage -> ([String], ValueMethod) -> ([String], ValueMethod)
killSidewaysReferences uni (mCalls, meth) = (mCalls', meth)
where
	tracy = "[killSidewaysReferences] path = " ++ (show (stringize meth)) ++ "\nmCalls = " ++ (intercalate "\n\t" mCalls)
	stringize (x, _, _, y,_,_,_ ) = y
	noBueno x = True
	mCalls' = (filter noBueno mCalls)



getTheTopMod :: BSVPackage -> BSVModuleDec
getTheTopMod u = topMod' mods
	where
		mods = bsv_modules u
		topMod' [] = error "No module labelled as root!"
		topMod' (m:ms) = if (instanceName m == "root") then m else topMod' ms

killNonreferencedMethods :: [String] -> ([String], ValueMethod) -> ([String], ValueMethod)
killNonreferencedMethods refs (mCalls, meth) = ((filter (\ x -> x `elem` refs') mCalls), meth)
where
	refs' = delete (stringize meth) refs
	stringize (x, _, _, y,_,_,_ ) = (intercalate "`" (tail y)) ++ "`" ++ x
	tracy = "[killNonreferencedMethods] mCalls = " ++ (show mCalls) ++ "\nrefs = " ++ (show refs)
	
-- genTransition :: BSVPackage -> BSVModuleDec -> [RuleSchedule] -> [String] -> PVStransition
-- genTransition universe mod sched meths = (methNoms, methArgs, transTables)
--   where 
--     
--     
--     
--     methNoms = meths	
	
	
-- genTransitions :: BSVPackage -> String -> [[String]] -> PVStransition
-- genTransitions universe topMod reqMeths = (vars, valMeths, tables)
--   where 
--     guards = genTransitionGuards' universe --(rulizeMethods (findMod universe topMod))
--     vars = [] -- genTransitionVars universe (findMod universe topMod)
--     valMeths = concat $ map (genValueMethods universe) (reverse (bsv_modules universe))
--     --wireFunctions = genWireFunctions (findMod universe topMod)
--     tables = genTransitions' universe (findMod universe topMod) reqMeths

scrubValueMethods :: BSVModuleDec -> BSVModuleDec 
scrubValueMethods mod = update mod
	where 
		methList = (methods mod) \\ (gatherValueMethods (methods mod))
		update x = x {methods = methList}

genValueMethods :: BSVPackage -> [TransitionTable] -> BSVModuleDec -> String -> [String] -> [ValueMethod]
genValueMethods universe tables mod namo path = (concat (map (\ x -> genValueMethods universe tables x namo (path `snoc` (instanceName x))) (instances mod))) ++ cMeths
where 
	tracy = "[genValueMethods] for " ++ namo ++ "\nmeths = " ++ (intercalate "\n\t\t" (map (\ (x,y,_,_,_,_) -> show (x, y)) meths) )
	meths =  gatherValueMethods $ methods mod -- gatherValueMethods $ methods mod
	cMeths = map (convertToFunction universe tables mod namo path) meths    
	
--error $ show $ map (\ (x,_,_,_,_,_) -> x ) $
gatherValueMethods :: [MethodBody] -> [MethodBody]
gatherValueMethods [] = []
gatherValueMethods ((x,(Action),y,z,a,b):xs) = gatherValueMethods xs
gatherValueMethods ((x,(ActionValue typ),y,z,a,b):xs) = gatherValueMethods xs
gatherValueMethods ((x,(Value typ),y,z,a,b):xs) = (x,(Value typ),y,z,a,b) : (gatherValueMethods xs)

convertToFunction :: BSVPackage -> [TransitionTable] -> BSVModuleDec -> String -> [String] -> MethodBody -> ValueMethod
convertToFunction universe tables mod namo path (nom, (Value typ), _, guard, stmts, atts) = 
	( nom
	, (mName mod)
	, namo 
	, path
	, (b2pType typ)
	, expFinal
	, wires
	)
where 
	tracy = "[convertToFunction] nom = " ++ (nom) ++ "\nexp = " ++ (show expFinal)
	exp' = maybe (error "Error! Did not find a return expression!") (id) $ getReturnExp stmts 
	exp = exp' -- addModuleInfo universe mod (tail path) $ exp'
	exp'' = id exp -- addWireLets mod wires tables exp -- error $ show $ map getTransitionName tables -- 
	st = state mod
	wires = (getReadsBy mod (\ q -> True) [] exp) ++ (getReadsBy mod (\ q -> True) [] guard)
	allWires = getWireNames st
	expFinal = (applyRootPrefix allWires path exp'')

getTransitionName :: TransitionTable -> ID_Path
getTransitionName (TransReg x _) = x
getTransitionName (TransVect x _ _) = x
getTransitionName (TransStruct x _) = (ID x)
getTransitionName (TransDWire x _ _) = x
	
addWireLets :: BSVModuleDec -> [ID_Path] -> [TransitionTable] -> Expression -> Expression
addWireLets mod wires tables exp = if (null lvs) then exp else (Binding lvs exp) -- error $ show $ wires -- 
	where
		wireTables = catMaybes $ map (lookupWireTable tables) wires
		lvs = map (mkLocalVars mod) wireTables 
		
mkLocalVars :: BSVModuleDec -> TransitionTable -> LocalVar
mkLocalVars mod (TransDWire i tree dv) = (i, (Left (Just typ)), exp)
where
	st = state mod
	typ = getStateType mod st i
	exp = expressionizeSpecificTree tree
	
expressionizeSpecificTree :: SpecificTree -> Expression
expressionizeSpecificTree (SpecStem gd (Left texp) ftree) = (Exp_If gd texp (expressionizeSpecificTree ftree))
expressionizeSpecificTree (SpecStem gd (Right ttree) ftree) = (Exp_If gd (expressionizeSpecificTree ttree) (expressionizeSpecificTree ftree))
expressionizeSpecificTree (SpecLeaf gd texp fexp) = (Exp_If gd texp fexp)
expressionizeSpecificTree (SpecEx exp) = exp 
	
getStateType :: BSVModuleDec -> [BSVstateDec] -> ID_Path -> BSVType
getStateType m ((BSV_Reg i t _ ):xs) i' = if i == i' then t else getStateType m xs i'
getStateType m ((BSV_Fifo f i t ):xs) i'= if i == i' then t else getStateType m xs i'
getStateType m ((BSV_Vector i t _ _):xs) i'= if i == i' then t else getStateType m xs i'
getStateType m ((BSV_RegFile i _ t _):xs) i'= if i == i' then t else getStateType m xs i'
getStateType m ((BSV_SubModuleDec  _ _ _ ):xs) i'= getStateType m xs i'
getStateType m ((DWire i t _):xs) i' = if i == i' then t else getStateType m xs i'
getStateType m [] (ID_Submod_Struct x y) = getStateType mod (state mod) y
where
	mod = maybe (error msg) (id) $ getModByInst (instances m) x
	msg = "[getStateType] id = " ++ (show (ID_Submod_Struct x y))

	
lookupWireTable :: [TransitionTable] -> ID_Path -> Maybe TransitionTable 
lookupWireTable [] x = Nothing
lookupWireTable q (ID_Submod_Struct "pre" y) = lookupWireTable q y
lookupWireTable ((TransDWire (ID i) tree dv):xs) (ID i') = if (i == i') then Just (TransDWire (ID i) tree dv) else lookupWireTable xs (ID i')
lookupWireTable ((TransMod x ts):xs) (ID_Submod_Struct x' y) = if (x == x') then lookupWireTable ts y else lookupWireTable xs (ID_Submod_Struct x' y)
lookupWireTable (x:xs) i =  lookupWireTable xs i
	
isWire :: [BSVstateDec] -> ID_Path -> Bool
isWire st i = if (showIDPath i) `elem` wires then True else False
where
	wires = getWireNames st
	
getTotalReturnExp :: BSVPackage -> BSVModuleDec -> Expression -> Expression 
getTotalReturnExp universe mod exp = if null mCalls then exp else (getTotalReturnExp universe mod (rmRF (foldl (\ x y -> applyReplacementExp y x) exp returnExpReplacements)))
where 
	mCalls = getMethCallsExp universe mod False exp  
	mods = catMaybes $ map (findModbyInst (state mod)) (map (\(Exp_MethodCall i m args _) -> i) mCalls)
	methNames = (map (\(Exp_MethodCall i m args _) -> m) mCalls) 
	meths = map (maybe (error "Error! Method Not Found!") (id)) $ map (\ (x,y) -> findMethod (methods y) x) (zip methNames (map (\ x -> maybe (error "Error, Module not found!") (id) (findMod universe x)) mods)) 
	returnExpReplacements = zip mCalls $ map (\ x -> maybe (error "Error! Return Expression Not Found!") (id) x) $ map getReturnExp $ map (\(_,_,_,_,x,_) -> x) meths
	
getReturnExp :: [Statement] -> Maybe Expression
getReturnExp [] = Nothing -- (Identifier (ID "Marscapone")) -- error $ "BSV2PVS Error! Value method does not contain return statement!"
getReturnExp ((Return exp atts):xs) = Just exp 
getReturnExp ((StructReturn t ss _):xs) = Just (StructCluster (Left t) ss)
getReturnExp ((If grd thn els _):xs) = if (thnRet /= Nothing) 
					then Just (Exp_If grd (unMaybe thnRet) (unMaybe elsRet))
					else getReturnExp xs 
	where 
		thnRet = getReturnExp (thn:[])
		elsRet = getReturnExp (els:[])
		unMaybe (Just x) = x
		unMaybe (Nothing) = (Skip)
getReturnExp ((PMatchIf id1 id2 thn els _):xs) = if (thnRet /= Nothing) 
					then Just (MaybeIf id1 id2 (unMaybe thnRet) (unMaybe elsRet))
					else getReturnExp xs 
	where 
		thnRet = getReturnExp (thn:[])
		elsRet = getReturnExp (els:[])
		unMaybe (Just x) = x
		unMaybe (Nothing) = (Skip)
getReturnExp ((LocalDec lvs stmt x):xs) = if (stmtRet /= Nothing) 
					then Just (Binding lvs (unMaybe stmtRet))
					else getReturnExp xs 
	where 
		stmtRet = getReturnExp (stmt:[])
		unMaybe (Just x) = x
		unMaybe (Nothing) = (Skip)
getReturnExp ((StatementBlock xs):ss) = getReturnExp (xs ++ ss)
getReturnExp ((Switch gd cases atts):xs) = getReturnExp ((iffd):(xs))
	where
		iffd = switchToIf gd cases atts
getReturnExp (x:xs) = error $ show $ x 



eliminateSingletonSBs :: Statement -> Statement
eliminateSingletonSBs (StatementBlock (x:[])) = eliminateSingletonSBs x
eliminateSingletonSBs (StatementBlock xs) = error "Error! Functions are only meant to contain singleton statement blocks!"
	-- twace ("[T] eliminateSingletonSBs - " ++ (show xs)) $ error "Error! Functions are only meant to contain singleton statement blocks!"
eliminateSingletonSBs (PMatchIf i1 i2 thn els q) = (PMatchIf i1 i2 (eliminateSingletonSBs thn) (eliminateSingletonSBs els) q) 
eliminateSingletonSBs (LocalDec [] stmt atts) = (eliminateSingletonSBs stmt)
eliminateSingletonSBs (LocalDec lv stmt atts) = (LocalDec lv (eliminateSingletonSBs stmt) atts)
eliminateSingletonSBs (If gd thn els q) = (If gd (eliminateSingletonSBs thn) (eliminateSingletonSBs els) q)
eliminateSingletonSBs (Switch gd cases q) = (Switch gd cases' q)
	-- (\ x -> twace ("[T] eiminateSingletonSBs - " ++ (show (Switch gd cases q))) (Switch gd cases q)) (Switch gd cases' q)
where
	cases' = map (\ (x,y) -> (x, (eliminateSingletonSBs y))) cases
eliminateSingletonSBs (Return x q) = (Return x q)
eliminateSingletonSBs (StructReturn x y z) = (StructReturn x y z)
eliminateSingletonSBs (Write x y z) = (Write x y z)
eliminateSingletonSBs (Void) = (Void)
eliminateSingletonSBs x = error $ show x

consolidateLDecs :: Statement -> Statement
consolidateLDecs (StatementBlock xs) = (StatementBlock (map consolidateLDecs xs))
consolidateLDecs (PMatchIf i1 i2 thn els q) = (PMatchIf i1 i2 (consolidateLDecs thn) (consolidateLDecs els) q) 
consolidateLDecs (LocalDec lv (LocalDec lv' stmt atts') atts) = consolidateLDecs (LocalDec (lv ++ lv') stmt (atts ++ atts'))
consolidateLDecs (LocalDec [] stmt atts) = consolidateLDecs stmt 
consolidateLDecs (LocalDec lv stmt atts) = (LocalDec lv (consolidateLDecs stmt) atts)
consolidateLDecs (If gd thn els q) = (If gd (consolidateLDecs thn) (consolidateLDecs els) q)
consolidateLDecs (Switch gd cases q) = (Switch gd cases' q)
	-- (\ x -> twace ("[T] eiminateSingletonSBs - " ++ (show (Switch gd cases q))) (Switch gd cases q)) (Switch gd cases' q)
where
	cases' = map (\ (x,y) -> (x, (consolidateLDecs y))) cases
consolidateLDecs (Return x q) = (Return x q)
consolidateLDecs (StructReturn x y z) = (StructReturn x y z)
consolidateLDecs (Write x y z) = (Write x y z)
consolidateLDecs (Void) = (Void)
consolidateLDecs (MethodCall w x y z) = (MethodCall w x y z)
consolidateLDecs x = error $ show x

-----------------------
-- Split statement blocks into individual statement trees
splitStatementBlocks :: BSVModuleDec -> [Statement] -> [Statement]
splitStatementBlocks m xs = catMaybes $ map (cleanup m) $ splitStatementBlocks' m $ applyLocalDecs $ lDecExpressionize xs
	-- catMaybes $ map cleanup $ splitStatementBlocks' $ applyLocalDecs $ lDecExpressionize xs
	-- trace ("\n\t[T] splitStatementBlock BEFORE - " ++ (show xs) ++ "\n\n\tAFTER - " ++ (show showy) ) splitStatementBlocks' $ applyLocalDecs $ lDecExpressionize xs
where
	tracy = "[splitStatementBlocks] - \n\n>>> BEFORE " ++ (show xs) ++ "\n\n>>> AFTER " ++ (show $ applyLocalDecs $ lDecExpressionize xs)
	-- (\x -> trace ("\n\t[T] splitStatementBlock BEFORE - " ++ (show xs) ++ "\n\n\tAFTER - " ++ (show x) ) x) $ splitStatementBlocks' $ map consolidateLDecs $ map elimRedundantLets $ applyLocalDecs xs
	-- (\x -> twace ("[T] splitStatementBlock - " ++ (show x) ) x) $ splitStatementBlocks' $ map consolidateLDecs $ applyLocalDecs xs

-- collectLocalDecs :: [Statement] -> [Statement]    
-- collectLocalDecs ((LocalDec (lvars) stmt z):ss) = (LocalDec lvars' stmt z) : (collectLocalDecs ss')
--    where
--      futureLDecs = [x | x <- xs, isThisLocalDec x]

cleanup :: BSVModuleDec -> Statement -> Maybe Statement
cleanup m (Void) = Nothing
cleanup m (StatementBlock (x:[])) = cleanup m x
cleanup m (StatementBlock xs) = Just (StatementBlock (catMaybes (map (cleanup m) xs)))
cleanup m (LocalDec [] x _) = cleanup m x
cleanup m (LocalDec l stmt z) = if stmt' == Void then Nothing else Just (LocalDec l'' stmt' z)
	where
		l' = nub l
		stmt' = maybe (Void) (id) $ cleanup m stmt
		l'' = orderLDecs m l'
		showy = "[cleanup] - \n" ++ (show l) ++ "\n\n----------------" ++ (show l'')
		l''' = filter (\ (x,_,_) -> elemWith x reados pathEq) l''
		reados =  getReadsByOverStatements m (\ x -> True) [] (stmt':[])
cleanup m (If gd x y z) = if x' == Void && y' == Void then Nothing else Just (If gd x' y' z)
	where 
		x' = maybe (Void) (id) $ cleanup m x
		y' = maybe (Void) (id) $ cleanup m y 
cleanup m (PMatchIf i i' x y z) = if x' == Void && y' == Void then Nothing else Just (PMatchIf i i' x' y' z)
	where
		x' = maybe (Void) (id) $ cleanup m x
		y' = maybe (Void) (id) $ cleanup m y 
cleanup m (Write x y z) = Just (Write x y z)
cleanup m (Return x y) = Just (Return x y)
cleanup m (StructReturn x y z) = Just (StructReturn x y z)
cleanup m (Switch gd cases z) = if null cases' then Nothing else Just (Switch gd cases' z)
	where
	cases' = crunch $ map (\ (x,y) -> (x,(maybe (Void) (id) ( cleanup m y)))) cases
	crunch [] = []
	crunch ((_,Void):xs) = crunch xs
	crunch ((x,y):xs) = (x,y) : (crunch xs)
cleanup m (MethodCall w x y z) = Just (MethodCall w x y z)
cleanup m x = error $ show x

orderLDecs :: BSVModuleDec -> [LocalVar] -> [LocalVar]
orderLDecs m ls = orderLDecs' dectuples
	where
		dectuples =  map (\(x,y,z) -> ((x,y,z), (getReadsBy m (\ q -> True) [] z))) ls

orderLDecs' :: [(LocalVar, [ID_Path])] -> [LocalVar]
orderLDecs' [] = []
orderLDecs' xs = (map fst clearLDecs) ++ (orderLDecs' notClearLDecs)
where
	names = map (\ (q,_,_) -> q ) $ map fst xs
	clearLDecs = [ x | x <- xs, (null (intersectBy pathEq names (snd x)))]
		-- (\ x -> trace ("[orderLDecs'] - clear - " ++ (show x)) x) $ [ x | x <- xs, (null (intersectBy pathEq names (snd x)))]
	notClearLDecs = foldl (\ p q -> delete q p) xs clearLDecs
		-- (\ x -> trace ("[orderLDecs'] - not clear - " ++ (show x)) x) $ foldl (\ p q -> delete q p) xs clearLDecs

splitStatementBlocks' :: BSVModuleDec -> [Statement] -> [Statement]
splitStatementBlocks' m xs = xs' -- trace showy $ xs' 
where
	xs' = concat $ map (\ x -> if (not (isFullySplit x)) then splitStatementBlock m x else x:[]) xs
	showy = "[splitStatementBlocks'] - " ++ (concat (map (\ x -> "\nBEFORE >>>\n" ++ (show x) ++ "\nIs Fully Split? " ++ (show (isFullySplit x)) ++  "\nAFTER >>> \n" ++ (show (splitStatementBlock m x))) xs))
	
applyLocalDecs :: [Statement] -> [Statement] 
applyLocalDecs xs = result
	-- trace ("[T] applyLocalDecs - \nBEFORE >>> \n\n" ++ (show xs) ++ "\n\nCrunchedLvars >>>\n\n"  ++ (show lvars) ++ "\n\n nonLDecs >>> \n\n" ++ (show nonLDecs) ++ "\n\nAFTER >>>\n\n" ++ (show result)) $ result 
where
	lDecs = [x | x <- xs, isLocalDec x]  
	nonLDecs = removeVoids $ rmLDecs xs
		-- removeVoids $ [(stepIntoStatement applyLocalDecs x) | x <- xs, (not (isLocalDec x))]  
	lvars = crunchLDecs lDecs 
	result = if (null lDecs)
				then nonLDecs
				else map (\ stmt -> (LocalDec lvars stmt [])) nonLDecs 

rmLDecs :: [Statement] -> [Statement]                
rmLDecs [] = []
rmLDecs ((LocalDec _ stmt _):xs) = stmt : (rmLDecs xs)
rmLDecs (x:xs) = (x:(rmLDecs xs))
				
lDecExpressionize :: [Statement] -> [Statement]
lDecExpressionize [] = []
lDecExpressionize ((LocalDec lvars stmt z):xs) = ((LocalDec lvars' stmt z):(lDecExpressionize xs'))
	where
		lvars' = map (\ x -> condenseLDecs x xs) lvars
		xs'    = map simplifyVoids $ foldl (removeLDecReferences) xs lvars
		showy  = "[lDecExpressionize] x' -- vars = " ++ (show (map (\ (x,y,z) -> x) lvars)) ++ "\nstmt = " ++ (show $ foldl (removeLDecReferences) xs lvars)
		-- q = weirdFold lDecExpressionize' xs lvars ([],[])
		-- q = (\ x -> trace ("[T] - " ++ (show (length (snd x)))) x) $ weirdFold lDecExpressionize' xs lvar ([],[])
lDecExpressionize ((If w x y z):xs) = (If w (head (lDecExpressionize (x:[]))) (head (lDecExpressionize (y:[]))) z) : (lDecExpressionize xs)
lDecExpressionize ((PMatchIf v w x y z):xs) = (PMatchIf v w (head (lDecExpressionize (x:[]))) (head (lDecExpressionize (y:[]))) z) : (lDecExpressionize xs)
lDecExpressionize ((Write x y z):xs) = (Write x y z):(lDecExpressionize xs) 
lDecExpressionize ((StatementBlock x):xs) = (StatementBlock (lDecExpressionize x)) : (lDecExpressionize xs) 
lDecExpressionize ((Void):xs) = (Void) : (lDecExpressionize xs)
lDecExpressionize ((Return x y):xs) = (Return x y) : (lDecExpressionize xs)
lDecExpressionize (x:xs) = (x : (lDecExpressionize xs))

removeLDecReferences ::  [Statement] -> LocalVar -> [Statement]
removeLDecReferences ss l = map (removeLDecReference l) ss

removeLDecReference :: LocalVar -> Statement -> Statement
removeLDecReference (x,y,z) (LocalDec ls stmt q) = (LocalDec ls' stmt q)
	where 
		ls' = filto ls --trace showy $ asdf
		--showy = "[removeLDecReferences] - x = " ++ (show x) ++ "\nls = " ++ (show ls) ++ "\nls' =" ++ (show asdf)
		--asdf = 
		filto [] = []
		filto ((a,b,c):ls) = if a `pathEq` x then filto ls else (a,b,c) : (filto ls)
removeLDecReference l (If w x y z) = (If w (removeLDecReference l x) (removeLDecReference l y) z)
removeLDecReference l (PMatchIf i i' x y z) = (PMatchIf i i' (removeLDecReference l x) (removeLDecReference l y) z)
removeLDecReference l (StatementBlock xs) = (StatementBlock (removeLDecReferences xs l))
removeLDecReference l (Write x y z) = (Write x y z)
removeLDecReference l (Void) = (Void)
removeLDecReference l (Return x y) = (Return x y)
removeLDecReference l (StructReturn x y z) = (StructReturn x y z)
removeLDecReference l (MethodCall w x y z ) = (MethodCall w x y z )
removeLDecReference l (Switch gd cases z) = (Switch gd cases' z)
where
	cases' = map (\ (x, y) -> (x, (removeLDecReference l y)) ) cases
removeLDecReference l x = error $ show $ x


		
condenseLDecs :: LocalVar -> [Statement] -> LocalVar
condenseLDecs (x,y,z) [] = (x,y,z)
condenseLDecs (x,y,z) ((LocalDec lvars stmt z'):ss) = if (null relevantLVars) then condenseLDecs (x,y,z) ss else condenseLDecs (x,y,newExp) ss
	where
		relevantLVars = filter (\ (a,b,c) -> x `pathEq` a) lvars
		newExp = (\(a,b,c) -> c) $ last relevantLVars
condenseLDecs (x,y,z) ((If gd thn els z'):ss) = condenseLDecs (x,y,exp') ss
	where 
		exp' = expressionize z $ simplifyVoids $ treefilter x (If gd thn els z')
		showy = (++) ("[condenseLDecs] "++ (show x) ++" - ") $ show $ expressionize z $ simplifyVoids $ treefilter x (If gd thn els z')
condenseLDecs (x,y,z) ((PMatchIf i i' thn els z'):ss) = condenseLDecs (x,y,exp') ss
	where 
		exp' = expressionize z $ simplifyVoids $ treefilter x (PMatchIf i i' thn els z')
		showy = (++) ("[condenseLDecs] "++ (show x) ++" - ") $ show $ expressionize z $ simplifyVoids $ treefilter x (PMatchIf i i' thn els z')
condenseLDecs l ((StatementBlock xs):ss) = condenseLDecs l (xs ++ ss)
condenseLDecs l ((Write _ _ _):ss) = condenseLDecs l ss
condenseLDecs l ((Void):ss) = condenseLDecs l ss
condenseLDecs l ((Switch gd cases atts):ss) = condenseLDecs l ((switchToIf gd cases atts) : ss )
condenseLDecs l ((Return _ _):ss) = condenseLDecs l ss
condenseLDecs l ((StructReturn _ _ _):ss) = condenseLDecs l ss
condenseLDecs l ((MethodCall _ _ _ _):ss) = condenseLDecs l ss
condenseLDecs (x,y,z) ((q):ss) = error $ show $ q
	-- condenseLDecs (x,y,z) ss

switchToIf :: Guard -> [Case] -> [StatementAttribute] -> Statement
switchToIf gd [] _ = Void
switchToIf gd ((lit, stmt):cs) atts = (If gd' thn els atts)
where
	gd' = (Equals gd lit) 
	thn = stmt
	els = switchToIf gd cs []

expressionize :: Expression -> Statement -> Expression
expressionize exp (Void) = exp
expressionize exp (If gd thn els _) = (Exp_If gd (expressionize exp thn) (expressionize exp els))
expressionize exp (PMatchIf i1 i2 thn els _) = (MaybeIf i1 i2 (expressionize exp thn) (expressionize exp els))
expressionize exp (LocalDec ((_,_,exp'):[]) _ _) = exp'
expressionize exp (LocalDec ((_,_,exp'):ls) p q) = expressionize exp' (LocalDec ls p q)
expressionize exp (StatementBlock []) = exp
expressionize exp (StatementBlock (x:xs)) = expressionize (expressionize exp x) (StatementBlock xs) 

treefilter :: ID_Path -> Statement -> Statement 
treefilter i (LocalDec lvars x y) = if null rLDs then Void else (LocalDec rLDs x y)
where
	rLDs = (filter (\ (a,b,c) -> i `pathEq` a) lvars)
treefilter i (If gd thn els z) = (If gd (treefilter i thn) (treefilter i els) z)
treefilter i (PMatchIf i1 i2 thn els z) = (PMatchIf i1 i2 (treefilter i thn) (treefilter i els) z)
treefilter i (StatementBlock xs) = (StatementBlock (map (treefilter i) xs))
treefilter _ (Write _ _ _) = Void
treefilter _ (Void) = Void
treefilter i (Return _ _ ) = Void
treefilter i (StructReturn _ _ _) = Void
treefilter i (MethodCall _ _ _ _) = Void
treefilter i (Switch gd cases z) = (Switch gd cases' z)
	where
		cases' = map (\(x,y)-> (x, (treefilter i y))) cases
treefilter i x = error $ "\n\n--------------------------------------\n\n" ++ (show x)


simplifyVoids :: Statement -> Statement 
simplifyVoids (If gd thn els z) = if thn' == Void && els' == Void then Void else (If gd thn' els' z)
	where
		thn' = simplifyVoids thn
		els' = simplifyVoids els
simplifyVoids (PMatchIf i i' thn els z) = if thn' == Void && els' == Void then Void else (PMatchIf i i' thn' els' z)
	where
		thn' = simplifyVoids thn
		els' = simplifyVoids els
simplifyVoids (StatementBlock []) = Void
simplifyVoids (StatementBlock xs) = if length xs' > 1 
		then (StatementBlock inner)
		else if length xs' == 1 then head xs'
					else Void
	where 
		inner = (map simplifyVoids xs)
		xs' = if (Void `elem` inner) then killVoids inner else inner 
simplifyVoids (Void) = (Void)
simplifyVoids (Write x y z) = (Write x y z)
simplifyVoids (LocalDec [] stmt z) = stmt
simplifyVoids (LocalDec x y z) = (LocalDec x y z)
simplifyVoids (Switch gd cases atts) = if (null cases') then Void else (Switch gd cases' atts)
where
	cases' = simplifyVoidCases cases 
simplifyVoids (Return x y) = (Return x y)
simplifyVoids (MethodCall w x y z) = (MethodCall w x y z)
simplifyVoids (StructReturn x y z) = (StructReturn x y z)
simplifyVoids x = error $ show x

simplifyVoidCases :: [Case] -> [Case]
simplifyVoidCases [] = []
simplifyVoidCases ((lit, stmt):cs) = if stmt' == Void then simplifyVoidCases cs else ( (lit, stmt') : (simplifyVoidCases cs ))
	where
		stmt' = simplifyVoids stmt

weirdFold :: ([a] -> b -> (b, [a])) -> [a] -> [b] -> ([b],[a]) -> ([b],[a])
weirdFold _ xs [] (z,_) = (z,xs)
weirdFold f xs (y:ys) (z,zs) = weirdFold f (snd f') ys (((fst f'):z),zs)
	where
		f' = f xs y

lDecExpressionize' :: [Statement] -> LocalVar -> (LocalVar, [Statement])
lDecExpressionize' stmts (n, q, exp) = (revisedLocalVar ,(crunchedStmts ++ notlDecStmts))
	where
		ldecStmts = filter (isThisLDec n) stmts
		notlDecStmts = filter (\ x -> not (isThisLDec n x)) stmts
		crunchedStmts = removeVoids $ map (removeLDec n) ldecStmts 
		justLdecStmts = removeVoids $ map (keepLDec n) ldecStmts 
		revisedLocalVar = combineLDecs (n,q,exp) justLdecStmts

combineLDecs :: LocalVar -> [Statement] -> LocalVar 
combineLDecs (n,q,exp) [] = (n,q,exp)
combineLDecs (n,q,exp) ((Void):xs) = (n,q,exp)
combineLDecs (n,q,exp) ((LocalDec ((n',q',exp'):[]) _ z):stmts) = combineLDecs (n',q',exp') stmts
-- combineLDecs (n,q,exp) ((LocalDec ((n',q',exp'):[]) what z):stmts) = error $ show $ what
combineLDecs (n,q,exp) ((If gd thn els z):stmts) = combineLDecs (n,q,(Exp_If gd thn' els')) stmts
where
	thn' = (\ (_,_,x) -> x) $ combineLDecs (n,q,exp) (thn:[])
	els' = (\ (_,_,x) -> x) $ combineLDecs (n,q,exp) (els:[])
combineLDecs (n,q,exp) ((PMatchIf i1 i2 thn els z):stmts) = combineLDecs (n,q,(MaybeIf i1 i2 thn' els')) stmts
where
	thn' = (\ (_,_,x) -> x) $ combineLDecs (n,q,exp) (thn:[])
	els' = (\ (_,_,x) -> x) $ combineLDecs (n,q,exp) (els:[])
		
removeVoids :: [Statement] -> [Statement]        
removeVoids [] = []
removeVoids ((Void):xs) = removeVoids xs
removeVoids ((If _ Void Void _):xs) = removeVoids xs
removeVoids ((PMatchIf _ _ Void Void _):xs) = removeVoids xs
removeVoids (x:xs) = x : (removeVoids xs)

keepLDec :: ID_Path -> Statement -> Statement
keepLDec i (LocalDec lvars stmt z) = if null newLVars then stmt else (LocalDec newLVars stmt z)
	where 
		newLVars = filter (\ (x,y,z) -> x `pathEq` i) lvars
keepLDec i (If gd thn els z) = if (thn' == Void) && (els' == Void) then Void else (If gd thn' els' z)
	where
		thn' = keepLDec i thn
		els' = keepLDec i els
keepLDec i (PMatchIf i1 i2 thn els z) = if (thn' == Void) && (els' == Void) then Void else (PMatchIf i1 i2 thn' els' z)
	where
		thn' = keepLDec i thn
		els' = keepLDec i els
keepLDec i (StatementBlock xs) = (StatementBlock (map (removeLDec i) xs))
keepLDec i x = x

removeLDec :: ID_Path -> Statement -> Statement
removeLDec i (LocalDec lvars stmt z) = if null newLVars then stmt else (LocalDec newLVars stmt z)
	where 
		newLVars = filter (\ (x,y,z) -> not (x `pathEq` i)) lvars
removeLDec i (If gd thn els z) = if (thn' == Void) && (els' == Void) then Void else (If gd thn' els' z)
	where
		thn' = removeLDec i thn
		els' = removeLDec i els
removeLDec i (PMatchIf i1 i2 thn els z) = if (thn' == Void) && (els' == Void) then Void else (PMatchIf i1 i2 thn' els' z)
	where
		thn' = removeLDec i thn
		els' = removeLDec i els
removeLDec i (StatementBlock xs) = (StatementBlock (map (removeLDec i) xs))
removeLDec i x = x
	
		
isThisLDec :: ID_Path -> Statement -> Bool
isThisLDec i (LocalDec lvars stmt z) = elemWith i (map (\ (x,_,_) -> x ) lvars) pathEq 
isThisLDec i (If _ thn els _) = isThisLDec i thn || isThisLDec i els
isThisLDec i (PMatchIf _ _ thn els _) = isThisLDec i thn || isThisLDec i els
isThisLDec i (StatementBlock xs) = or $ map (isThisLDec i) xs
isThisLDec i (Write _ _ _) = False
isThisLDec i (Void) = False
isThisLDec i x = error $ "\n\n\n\n\n\n\n\n" ++ (show x)

-- | Order is important
matchLVs :: [LocalVar] -> [LocalVar] -> Expression -> [LocalVar]
matchLVs [] [] _ = []
matchLVs [] ((i, t, exp):ys) gd = (i, t, (Exp_If gd Skip exp)) : (matchLVs [] ys gd)
matchLVs ((i, t, exp):xs) ys gd = (i, t, (Exp_If gd exp (matchingExp i ys))) : (matchLVs xs ys gd)

matchingExp :: ID_Path -> [LocalVar] -> Expression
matchingExp _ [] = Skip
matchingExp i ((i', _, exp):xs) = if (i `pathEq` i') then exp else matchingExp i xs
	
stepIntoStatement :: ([Statement] -> [Statement]) -> Statement -> Statement 
stepIntoStatement f (StatementBlock xs) = (StatementBlock (f xs))
	--(\ x -> twace ("[T] stepIntoStatement =BEFORE= " ++ (show (StatementBlock xs)) ++ "\n=AFTER= " ++ (show x)) x ) $ (StatementBlock (f xs))
stepIntoStatement f (PMatchIf i i' thn els q) = (PMatchIf i i' (stepIntoStatement f thn) (stepIntoStatement f els) q)
stepIntoStatement f (LocalDec lv stmt q) = (LocalDec lv (stepIntoStatement f stmt) q)
stepIntoStatement f (If gd thn els q) = (If gd (stepIntoStatement f thn) (stepIntoStatement f els) q)
stepIntoStatement f (Switch gd cases q) = (Switch gd (stepIntoCases f cases) q) 
stepIntoStatement f x = x

stepIntoCases :: ([Statement] -> [Statement]) -> [(Literal, Statement)] -> [(Literal, Statement)]
stepIntoCases _ [] = []
stepIntoCases f ((lit, stmt):xs) = (lit, (stepIntoStatement f stmt)) : (stepIntoCases f xs) -- twace ("[T] stepIntoCase - case =BEFORE= " ++ (show (lit, stmt)) ++ "\n=AFTER=" ++ (show (lit, (stepIntoStatement f stmt))) ++ "\n") $ (lit, (stepIntoStatement f stmt)) : (stepIntoCases f xs)
	-- (lit, (stepIntoStatement f stmt)) : (stepIntoCases f xs)
	
crunchLDecs :: [Statement] -> [LocalVar]
crunchLDecs [] = []
crunchLDecs ((LocalDec lvs _ _):xs) = lvs ++ (crunchLDecs xs)
crunchLDecs (_:xs) = (crunchLDecs xs)

isLocalDec :: Statement -> Bool 
isLocalDec (LocalDec _ _ _) = True
isLocalDec (If gd thn els _) = isLocalDec thn && isLocalDec els
isLocalDec (PMatchIf i i' thn els _) = isLocalDec thn && isLocalDec els
-- isLocalDec (Let _ _ _) = True
isLocalDec _ = False

-- elimRedundantLets :: Statement -> Statement 
-- elimRedundantLets (StatementBlock xs) = StatementBlock (map elimRedundantLets xs)
-- elimRedundantLets (PMatchIf i1 i2 thn els q) = (PMatchIf i1 i2 (elimRedundantLets thn) (elimRedundantLets els) q) 
-- elimRedundantLets (If gd thn els q) = (If gd (elimRedundantLets thn) (elimRedundantLets els) q)
-- elimRedundantLets (Switch gd cases q) = (Switch gd cases' q)
--   where
--     cases' = map (\ (x,y) -> (x, (elimRedundantLets y))) cases
-- elimRedundantLets (LocalDec lv stmt atts) = if (null reducedLVs) then (elimRedundantLets stmt) else (LocalDec reducedLVs (elimRedundantLets stmt) atts) 
--   where
--     reducedLVs = 
--          nub $ getDeps lv referencedLVs
--         -- (\ x -> trace ("[T] elimRedundantLets - reducedLVs : " ++ (show (map (\(q,_,_) -> q) x))) x) $ nub $ getDeps lv referencedLVs
--     referencedLVs = 
--          [ x | x <- lv, (isReferencedAnywhere x lv stmt)] 
--         --   (\ x -> trace ("[T] elimRedundantLets - referenced LVs :" ++ (show (map (\(q,_,_) -> q) x))) x) $ [ x | x <- lv, (isReferencedAnywhere x lv stmt)]
-- elimRedundantLets x = x

isReferencedAnywhere :: BSVModuleDec -> LocalVar -> [LocalVar] -> Statement -> Bool
isReferencedAnywhere m (n,_,_) [] stmt = result
	-- trace ("[T] isReferencedAnywhere - name = " ++ (show n) ++ "\nreads = " ++ (show referenced) ++ "\nstatement = \n" ++ (show stmt) ++ "\nRESULT = " ++ (show result)) result
	where
		--referenced = getReadsByOverStatements isTrue (stmt:[])
		referenced = getReadsByOverStatements m isTrue [] (stmt:[])
		isTrue x = True
		result = elemWith n referenced pathEq
isReferencedAnywhere m' (n,m,o) ((n',_,exp):xs) stmt = result
	-- trace ("[T] isReferencedAnywhere - name = " ++ (show n) ++ "\nreads = " ++ (show reads) ++ "\nchecking lvar " ++ (show n')++"...\nexpression = " ++ (show exp) ++ "\nRESULT = " ++ (show result')) result
	where
		reads =  getReadsBy m' isTrue [] exp
		-- reads = getReadsBy isTrue exp
		isTrue x = True
		result = if (elemWith n reads pathEq) then True else (isReferencedAnywhere m' (n,m,o) xs stmt)
		result' = if (elemWith n reads pathEq) then True else False
			
			-- $ getReadsBy (\ y -> n `pathEq` y) x

pathEq :: ID_Path -> ID_Path -> Bool
pathEq (ID_Submod_Struct i' p') (ID_Submod_Struct i p)  = i == i' && p `pathEq` p'
pathEq (ID_Submod_Struct i' p') (ID i)                  = i == i'
pathEq (ID_Submod_Struct i' p') (ID_Vect i n)           = i == i'
pathEq (ID i') (ID_Submod_Struct i p)                   = i == i'
pathEq (ID i') (ID i)                                   = i == i'
pathEq (ID i') (ID_Vect i n)                            = i == i'
pathEq (ID_Vect i' n') (ID_Submod_Struct i p)           = i == i'
pathEq (ID_Vect i' n') (ID i)                           = i == i'
pathEq (ID_Vect i' n') (ID_Vect i n)                    = i == i' && n == n'

pathTerminalEq :: ID_Path -> ID_Path -> Bool
pathTerminalEq (ID_Submod_Struct i' p') (ID_Submod_Struct i p)  = p' `pathTerminalEq` p
pathTerminalEq (ID_Submod_Struct i' p') (ID i)                  = p' `pathTerminalEq` (ID i)
pathTerminalEq (ID_Submod_Struct i' p') (ID_Vect i n)           = p' `pathTerminalEq` (ID_Vect i n)
pathTerminalEq (ID i') (ID_Submod_Struct i p)                   = p `pathTerminalEq` (ID i')
pathTerminalEq (ID i') (ID i)                                   = i == i'
pathTerminalEq (ID i') (ID_Vect i n)                            = False
pathTerminalEq (ID_Vect i' n') (ID_Submod_Struct i p)           = p `pathTerminalEq` (ID_Vect i' n')
pathTerminalEq (ID_Vect i' n') (ID i)                           = False
pathTerminalEq (ID_Vect i' n') (ID_Vect i n)                    = i == i' && n == n'

getDeps :: BSVModuleDec -> [LocalVar] -> [LocalVar] -> [LocalVar]
getDeps m lib [] = []
getDeps m lib ((x,y,exp):xs) =  (getDeps m lib newDeps) ++ ((x,y,exp) : []) ++ (getDeps m lib xs)
where 
	referenced = getReadsBy m (\ x -> True) [] exp
	newDeps = [ z | z <- lib, ((\ (q,_,_) -> q `elem` referenced) z) ]
	
isReferencedinExp :: BSVModuleDec -> LocalVar -> Expression -> Bool
isReferencedinExp m (n,_,_) exp = n `elem` referenced
where
	referenced = getReadsBy m isTrue [] exp
	isTrue x = True
	
isReferencedin :: BSVModuleDec -> LocalVar -> Statement -> Bool
isReferencedin m (n,_,_) stmt = n `elem` referenced 
where
	referenced = getReadsByOverStatements m isTrue [] (stmt:[])
	isTrue x = True

splitStatementBlock :: BSVModuleDec -> Statement -> [Statement]
splitStatementBlock m (StatementBlock (x:[])) = splitStatementBlock m x
splitStatementBlock m (StatementBlock xs) = if (and (map isFullySplit result)) then result else error $ show $ result
where
	result = splitStatementBlocks m xs 
splitStatementBlock m (PMatchIf i1 i2 thn els q) = if (and (map isFullySplit result)) then result else error $ show $ result
where 
	result = if isFullySplit (PMatchIf i1 i2 thn els q) 
	then (PMatchIf i1 i2 thn els q):[] 
	else map (\ (x, y) -> (PMatchIf i1 i2 x y q)) (getStatementPairs splitThen splitElse) 
	splitThen = if (isFullySplit thn) then thn:[] else splitStatementBlock m thn
	splitElse = if (isFullySplit els) then els:[] else splitStatementBlock m els
splitStatementBlock m (LocalDec [] stmt _) = splitStatementBlock m stmt
splitStatementBlock m (LocalDec lv (StatementBlock xs) atts) = result --trace showy $ result
where
	result = map (\ x -> (LocalDec lv x atts)) $ splitStatementBlocks m xs
	showy = "[splitStatementBlock] - LocalDec + Statement Block - lvs - " ++ (show ((map (\ (x,y,z) -> x)) lv)) ++ "\nResult - " ++ (intercalate "\n\t>> " (map show result))
splitStatementBlock m (LocalDec lv stmt atts) = result -- trace showy $ result
where
	result = map (\ x -> (LocalDec lv x atts)) $ splitStatementBlock m stmt
	showy = "[splitStatementBlock] - LocalDec - lvs - " ++ (show ((map (\ (x,y,z) -> x)) lv)) ++ "\nResult - " ++ (show result)
--     if (and (map isFullySplit result)) then result else error $ show $ result
--   where
--   result = if isFullySplit (LocalDec lv stmt atts) 
--     then ((LocalDec lv stmt atts):[])
--     else map (\ x -> LocalDec lv x atts) $ if (isFullySplit stmt) then stmt:[] else splitStatementBlock stmt
splitStatementBlock m (If gd thn els q) =  if (and (map isFullySplit result)) then result else error $ (show (If gd thn els q))++ "\nSplit into\n" ++ show result ++ "\nWhich apparently isn't far enough!"
where 
	result = if isFullySplit (If gd thn els q) 
	then (If gd thn els q):[] 
	else map (\ (x, y) -> (If gd x y q)) (getStatementPairs splitThen splitElse) 
	splitThen = if (isFullySplit thn) then thn:[] else splitStatementBlock m thn
	splitElse = if (isFullySplit els) then els:[] else splitStatementBlock m els
	showy = "[splitStatementBlock] - guard = " ++ (show gd) ++ "\n\nThen before >>>\n\n" ++ (show thn) ++ "\nThen after >>>\n\n" ++ (show splitThen) ++ "\nElse before >>>\n\n" ++ (show els) ++ "Else After >>>\n\n" ++ (show splitElse)
splitStatementBlock m (Switch gd cases q) = if (and (map isFullySplit result)) then result else error $ show $ result
where 
result = if isFullySplit (Switch gd cases q)
	then (Switch gd cases q):[]
	else map (\ x -> (Switch gd x q)) (getCaseMatches m cases) 
splitStatementBlock m (Return x q) = (Return x q):[]
splitStatementBlock m (Write x y z) = (Write x y z):[]
splitStatementBlock m (Void) = (Void):[]
splitStatementBlock m (MethodCall w x y z) = (MethodCall w x y z) : []
splitStatementBlock m x = error $ show x
--splitStatementBlock x = x:[]

killVoids :: [Statement] -> [Statement]
killVoids [] = []
killVoids (Void:xs) = killVoids xs
killVoids (x:xs) = x : (killVoids xs)

getCaseMatches :: BSVModuleDec -> [Case] -> [[Case]]
getCaseMatches m (xs) = result -- map (\ (_,x) -> getMatchingCaseStatements m xs x) xs
	where 
		result = getCasesMatching m splitCases 
		splitCases = concat $ map (splitCase m) xs
		
		-- tracy = "[getCaseMatches]" ++ (intercalate "\n" ( map (\ (_,x) -> show (getMatchingCaseStatements m xs x)) xs))
		tracy = "[getCaseMatches] input = " ++ (intercalate "\n" (map show xs)) ++ "\n\nsplitCases = " ++ (intercalate "\n" (map show splitCases)) ++ "\n\nresult = " ++ (intercalate "\n" (map show result))

splitCase :: BSVModuleDec -> Case -> [Case]        
splitCase m (i, stmt) = map (\ x -> (i, x)) $ removeVoids $ splitStatementBlock m stmt

getCasesMatching :: BSVModuleDec -> [Case] -> [[Case]]
getCasesMatching _ [] = []
getCasesMatching m ((i, stmt):xs) = ((i, stmt):(fst splitter)) : (getCasesMatching m (snd splitter))
	where
		splitter = filterCases m stmt (xs) 

-- First result is matching, second is not matching
filterCases :: BSVModuleDec -> Statement -> [Case] -> ([Case], [Case])
filterCases m stmt cs = (yes, no)
	where 
		yes = [x | x <- cs, (\ (_,q) -> isStatementMatch stmt q) x]
		no = [x | x <- cs, (\ (_,q) -> not (isStatementMatch stmt q)) x]

		
getMatchingCaseStatements :: BSVModuleDec -> [Case] -> Statement -> [Case]
getMatchingCaseStatements m [] _ = []
getMatchingCaseStatements m ((ind,stmt'):xs) stmt = (ind, matched):(getMatchingCaseStatements m xs stmt)
where
	splitStmts' = splitStatementBlock m stmt'
	matched = findMatchingStatement stmt splitStmts' 
					

getStatementPairs :: [Statement] -> [Statement] -> [(Statement, Statement)]
getStatementPairs xs ys = nub $ xpairs ++ ypairs
where 
	xpairs = zip (xs) (map (\ x -> findMatchingStatement x ys) xs)
	ypairs = zip (map (\ y -> findMatchingStatement y xs) ys) (ys)
	showy = intercalate "\n" $ map (\ x -> "\n[getStatementPairs] - x = " ++ (show x) ++ "\n\nys = " ++ (intercalate "\n > " (map show ys)) ++ "\n\nMatching Statement = " ++ (show (findMatchingStatement x ys))) xs

-- operates on the assumption that all writes at the leaves of if-trees write to the same register.  Takes first match.
findMatchingStatement :: Statement -> [Statement] -> Statement
findMatchingStatement x [] = Void
findMatchingStatement x (y:ys) = if isStatementMatch x y then y else findMatchingStatement x ys

isStatementMatch :: Statement -> Statement -> Bool
isStatementMatch (StatementBlock (x:[])) y = isStatementMatch x y 
isStatementMatch (StatementBlock xs) y = if null boink
					then False
					else True
	where
		boink = filter (\ (p, q) -> p) $ map (\ x -> ((isStatementMatch x y), x)) xs
isStatementMatch x (StatementBlock (y:[])) = isStatementMatch x y
isStatementMatch x (StatementBlock ys) = if null boink
					then False
					else True
	where
		boink = filter (\ (p, q) -> p) $ map (\ y -> ((isStatementMatch y x), y)) ys
isStatementMatch Void _ = False
isStatementMatch _ Void = False
isStatementMatch (LocalDec _ x _) y = isStatementMatch x y
isStatementMatch x (LocalDec _ y _) = isStatementMatch x y
isStatementMatch (If _ x x' _) y = isStatementMatch x y || isStatementMatch x' y
isStatementMatch x (If _ y y' _) = isStatementMatch x y || isStatementMatch x y'
isStatementMatch (PMatchIf _ _ x x' _) y = isStatementMatch x y || isStatementMatch x' y
isStatementMatch x (PMatchIf _ _ y y' _) = isStatementMatch x y || isStatementMatch x y'
isStatementMatch (Write iPath _ _) (Write iPath' _ _) = iPath == iPath'
isStatementMatch (Return _ _) (Return _ _) = True
isStatementMatch (MethodCall sName _ _ _) (MethodCall sName' _ _ _) = sName == sName'
isStatementMatch (MethodCall sName _ _ _) y = False
isStatementMatch x (MethodCall sName _ _ _) = False
isStatementMatch x y = error $ "Error! Don't know how to compare x = " ++ (show x) ++ "\n and y = " ++ (show y)

isFullySplit :: Statement -> Bool 
isFullySplit (If gd thn els _) = isFullySplit thn && isFullySplit els
isFullySplit (Switch gd cases _) = and $ map isFullySplitCase cases
isFullySplit (LocalDec _ stmt _ ) = isFullySplit stmt
isFullySplit (PMatchIf _ _ stmt1 stmt2 _) = (isFullySplit stmt1) && (isFullySplit stmt2)
isFullySplit (StatementBlock xs) = False 
isFullySplit (Void) = True
isFullySplit (Return _ _) = True
isFullySplit (Write _ _ _) = True
isFullySplit (StructReturn _ _ _) = True
isFullySplit (MethodCall _ _ _ _) = True
isFullySplit x = error $ show x

--isFullySplit _ = True

isFullySplitCase :: Case -> Bool
isFullySplitCase (_, stmt) = isFullySplit stmt
-----------------------
genTransitionVars :: BSVPackage -> BSVModuleDec -> [TransitionVar]
genTransitionVars universe mod = concat [ preludes, methVars, submodVars ]
where 
	preludes = []
	inter = maybe (error "Error! Interface not found in function genTransitionVars") (id) $ findInterface (interfaces universe) (interfaceName mod)
	methVars = groupByType $ nub $ (genMethVars) (methods mod) ((\(_,x,_,_) ->x ) inter)
	submodVars = groupByType $ nub $ maybeToEmpty $ map genSubmodVars (state mod) 

maybeToEmpty :: [(Maybe a)] -> [a]
maybeToEmpty [] = [] 
maybeToEmpty ((Nothing):xs) = [] ++ (maybeToEmpty xs)
maybeToEmpty ((Just x):xs) = x : (maybeToEmpty xs)

findInterface :: [InterfaceDec] -> String -> Maybe InterfaceDec
findInterface [] _ = Nothing
findInterface (x:xs) nom = if (((\ (a,_,_,_) -> a) x) == nom) then Just x else findInterface xs nom

genMethVars :: [MethodBody] -> [MethodDec] -> [(PVSType, String)]
genMethVars [] ys = []
genMethVars (x:xs) ys = (genMethVars' x (matchingDec x ys)) ++ (genMethVars xs ys)
where 
	matchingDec w [] = error $ "BSV2PVS Error! No interface declaration of method \""++ (show ((\(q,_,_,_,_,_) -> q)  w)) ++ "\""
	matchingDec w (z:zs) = if ((((\(q,_,_,_,_,_) -> q)  w) == ((\(p,_,_,_) -> p)  z)) ) then z else matchingDec w zs

genMethVars' :: MethodBody -> MethodDec -> [(PVSType, String)]
genMethVars' (nom1, _, args1, _, _, _) (nom2, typ, args2, _) = genArgumentVars args1 args2 

genArgumentVars :: UTArgs -> [Argument] -> [(PVSType, String)]
genArgumentVars [] [] = []
genArgumentVars ((x,t):xs) (y:ys) = (genArgumentVars' x y) : (genArgumentVars xs ys) 

genArgumentVars' :: String -> Argument -> (PVSType, String)
genArgumentVars' x (nom, btyp, _) = ((b2pType btyp), x)

genSubmodVars :: BSVstateDec -> Maybe (PVSType, String)
genSubmodVars (BSV_Reg _ _ _) = Nothing
genSubmodVars (BSV_Fifo _ _ _) = Nothing
genSubmodVars (BSV_Vector _ _ _ _) = Nothing
genSubmodVars (BSV_RegFile _ _ _ _) = Nothing  
genSubmodVars (BSV_SubModuleDec interface name inst) = Just ((PVS_Custom interface), (interface ++ "_var"))
genSubmodVars (DWire _ _ _) = Nothing

groupByType :: [(PVSType, String)] -> [(PVSType, [String])]
groupByType [] = []
groupByType ((typ, nom):xs) = (typ, (nom : (matching))) : (groupByType notMatching)
where 
	matching = map snd $ filter (\(x, y) -> x == typ) xs
	notMatching = filter (\(x, y) -> not (x == typ)) xs

-- genTransitions' :: BSVPackage -> BSVModuleDec -> [String] -> [ModuleTransition]
-- genTransitions' universe mod reqMeths = genTransition universe mod 
--   where 
--     meths = onlyInputMethods $ methods mod 
--     methsCalled = (map (\ (x,_,_,_,_,_) -> [x]) meths) ++ reqMeths


onlyInputMethods :: [MethodBody] -> [MethodBody]
onlyInputMethods [] = []
onlyInputMethods ((u,(Action),w,x,y,z):ms) 	  = (u,(Action),w,x,y,z) : (onlyInputMethods ms)
onlyInputMethods ((u,(ActionValue t),w,x,y,z):ms) = (u,(ActionValue t),w,x,y,z) : (onlyInputMethods ms)
onlyInputMethods (m:ms) = onlyInputMethods ms


unMaybeList :: (Eq a) => [Maybe a] -> [a]
unMaybeList [] = []
unMaybeList ((Just x):xs) = x : (unMaybeList xs)
unMaybeList ((Nothing):xs) = unMaybeList xs



typos :: BSVPackage -> BSVModuleDec -> String -> [PVSType]
typos pkg mod n = map b2pType $ map typs $ args $ rightMeth (meths inter) n
where
	inter = maybe (error "Error! Interface not found in function typos") (id) $ findInterface (interfaces pkg) (interfaceName mod) 
	meths (_, x, _, _) = x
	rightMeth :: [MethodDec] -> String -> MethodDec
	rightMeth [] _ = error "Error! Corresponding method declaration not found!"
	rightMeth ((n,x,y,z):xs) n' = if n == n' then (n,x,y,z) else rightMeth xs n'
	args (_,_,x,_) = x
	typs (_,t,_) = t
	

-- | Converts called methods to rules and updates the top level BSV module
propagateMethodCalls :: BSVPackage -> BSVModuleDec -> [ID_Path] -> [Replacement] -> BSVModuleDec 
propagateMethodCalls universe mod meths replacements = update mod
where
	methds = map applyNumbering $ zip [0..(length meths)] $ map (\ x -> maybe (error "Error! Method not found!") (id) (findMethod' mod (methods mod) x)) meths
	ruleList = (rules mod) ++ (applyReplacements replacements (unMaybeList (map convertMethodToRule methds)))
	methList = filter (\ (x,_,_,_,_,_) -> not (x `elem` (map showIDPath meths))) (methods mod)
	stmts = concat $ map (\ (_,_,x,_) -> x) ruleList
	mCalls = (getMethCalls universe mod True stmts)
	newInstances = map (\ x -> propagateMethodCalls 
					universe 
					x 
					(map (\ (Exp_MethodCall _ y _ _) -> (ID y)) (nub (filterForModule mCalls x)))
					(getReplacements universe x (nub (filterForModule mCalls x))) 
					) (instances mod)
	update x = x {rules = ruleList, methods = methList, instances = newInstances}
	tracy = "[propagateMethodCalls] - methodNames = " ++ (show meths) ++ "\nmethods = " ++ (show methds) ++ "\nrule list = " ++ (intercalate "\n- " (map show ruleList)) ++ "\nMCalls = " ++ (intercalate "\n- " (map show mCalls))
	h1 = (applyReplacements replacements (unMaybeList (map convertMethodToRule methds)))

applyNumbering :: (Int, MethodBody) -> MethodBody
applyNumbering (num, (nom, typ, args, gd, stmts, atts)) = (nom, typ, newArgs, gd, newStmts, atts)
where
	newArgs = map (\ (afst, asnd) -> ((afst ++ "_" ++ (show num)), asnd)) args
	reps = zip (map (\ x -> (Identifier (ID x))) (map fst args)) (map (\ x -> (Identifier (ID x))) (map fst newArgs))
	newStmts = map (\ z -> foldl (\ x y -> applyReplacementStmt y x) z reps ) stmts
	
getReplacements :: BSVPackage -> BSVModuleDec -> [Expression] -> [Replacement]
getReplacements _ _ [] = []
getReplacements universe mod ((Exp_MethodCall iName mName exps q):xs) = (zip args exps) ++ (getReplacements universe mod xs)
where 
	meth = maybe (error "Error! Method Call Not Found!") (id) $ findMethod (methods mod) mName 
	args = map (\ y -> (Identifier (ID y))) ((\ (_,_,x,_,_,_) -> map fst x) meth)

applyReplacements :: [Replacement] -> [RuleDec] -> [RuleDec]
applyReplacements reps ruls = map (applyReplacement reps) ruls

applyReplacement :: [Replacement] -> RuleDec -> RuleDec
applyReplacement reps rule = foldl (\ x y -> applyReplacement' y x) (rule) (reps)
--map (\ x -> applyReplacement' x rule) reps

applyReplacement' :: Replacement -> RuleDec -> RuleDec 
applyReplacement' rep (x, y, stmts, z) = (x, y, (map (applyReplacementStmt rep) stmts), z)

applyReplacementStmt :: Replacement -> Statement -> Statement
applyReplacementStmt rep (Write x exp z) = (Write x (applyReplacementExp rep exp) z)
applyReplacementStmt rep (MethodCall x y exps z) = (MethodCall x y (map (applyReplacementExp rep) exps) z)
applyReplacementStmt rep (ActionCall x y) = (ActionCall x y)
applyReplacementStmt rep (Return exp y) = (Return (applyReplacementExp rep exp) y) 
applyReplacementStmt rep (If x y z atts) = (If (applyReplacementExp rep x) (applyReplacementStmt rep y) (applyReplacementStmt rep z) atts) 
applyReplacementStmt rep (ForLoop w x y z atts) = (ForLoop (map (applyReplacementUStmt rep) w) (applyReplacementExp rep x) (map (applyReplacementUStmt rep) y) (applyReplacementStmt rep z) atts)
applyReplacementStmt rep (Switch x y atts) = (Switch (applyReplacementExp rep x) (map (applyReplacementCase rep) y) atts)
-- applyReplacementStmt rep (Let x exp atts) = (Let x (applyReplacementExp rep exp) atts)
applyReplacementStmt rep (Void) = (Void)
applyReplacementStmt rep (StatementBlock xs) = (StatementBlock (map (applyReplacementStmt rep) xs))
applyReplacementStmt rep (LocalDec lvars stmt atts) = (LocalDec lvars' (applyReplacementStmt rep stmt) atts) 
where
	lvars' = map (\ (x,y,z) -> (x,y,(applyReplacementExp rep z))) lvars
applyReplacementStmt rep x = error $ show x

applyReplacementUStmt :: Replacement -> UStatement -> UStatement 
applyReplacementUStmt rep (DeclAssign x y exp) = (DeclAssign x y (applyReplacementExp rep exp))
applyReplacementUStmt rep (UAssign x exp) = (UAssign x (applyReplacementExp rep exp))

applyReplacementCase :: Replacement -> Case -> Case 
applyReplacementCase rep (x, stmt) = (x, (applyReplacementStmt rep stmt))

applyReplacementExp :: Replacement -> Expression -> Expression 
applyReplacementExp (r1, r2) (Negative x)	= if (r1 == (Negative x)) then (RPFlag r2) else (Negative (applyReplacementExp (r1, r2) x))
applyReplacementExp (r1, r2) (Not x) 		= if (r1 == (Not x)) then (RPFlag r2) else (Not (applyReplacementExp (r1, r2) x))
applyReplacementExp (r1, r2) (Equals x y) 	= if (r1 == (Equals x y)) then (RPFlag r2) else (Equals (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (NotEquals x y) 	= if (r1 == (NotEquals x y)) then (RPFlag r2) else (NotEquals (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (GreaterEquals x y)= if (r1 == (GreaterEquals x y)) then (RPFlag r2) else (GreaterEquals (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (LessEquals x y) 	= if (r1 == (LessEquals x y)) then (RPFlag r2) else (LessEquals (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (Greater x y) 	= if (r1 == (Greater x y)) then (RPFlag r2) else (Greater (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (Less x y) 	= if (r1 == (Less x y)) then (RPFlag r2) else (Less (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (And x y) 		= if (r1 == (And x y)) then (RPFlag r2) else (And (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (Or x y) 		= if (r1 == (Or x y)) then (RPFlag r2) else (Or (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (BitwiseAND x y) 	= if (r1 == (BitwiseAND x y)) then (RPFlag r2) else (BitwiseAND (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (BitwiseOR x y) 	= if (r1 == (BitwiseOR x y)) then (RPFlag r2) else (BitwiseOR (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (BitwiseXOR x y) 	= if (r1 == (BitwiseXOR x y)) then (RPFlag r2) else (BitwiseXOR (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (LShift x y) 	= if (r1 == (LShift x y)) then (RPFlag r2) else (LShift (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (RShift x y) 	= if (r1 == (RShift x y)) then (RPFlag r2) else (RShift (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (Multiply x y) 	= if (r1 == (Multiply x y)) then (RPFlag r2) else (Multiply (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (Divide x y) 	= if (r1 == (Divide x y)) then (RPFlag r2) else (Divide (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (Modulo x y)       = if (r1 == (Modulo x y)) then (RPFlag r2) else (Modulo (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (Add x y) 		= if (r1 == (Add x y)) then (RPFlag r2) else (Add (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (Subtract x y) 	= if (r1 == (Subtract x y)) then (RPFlag r2) else (Subtract (applyReplacementExp (r1, r2) x) (applyReplacementExp (r1, r2) y))
applyReplacementExp (r1, r2) (Literal x) 	= if (r1 == (Literal x)) then (RPFlag r2) else (Literal x)

applyReplacementExp ((Identifier r1), r2) (Identifier x)	= if (r1 `pathPrefixEq` x) then (RPFlag (pathPrefixReplacement x (Just r1) r2)) else (Identifier x)
applyReplacementExp (r1, r2) (Identifier x)	= (Identifier x)

applyReplacementExp (r1, r2) (Exp_MethodCall inst meth zs q) = if (r1 == (Exp_MethodCall inst meth zs q)) then (RPFlag r2) else (Exp_MethodCall inst meth (map (applyReplacementExp (r1, r2)) zs) q)
applyReplacementExp (r1, r2) (Skip) = if (r1 == (Skip)) then (RPFlag r2) else (Skip)
applyReplacementExp (r1, r2) (RPFlag x) = (RPFlag x)
applyReplacementExp (r1, r2) (Exp_FunctionCall x ys) = if (r1 == (Exp_FunctionCall x ys)) then (RPFlag r2) else (Exp_FunctionCall x (map (applyReplacementExp (r1,r2)) ys))
applyReplacementExp (r1, r2) (MaybeValue x) = if (r1 == (MaybeValue x)) then (RPFlag r2) else (MaybeValue (applyReplacementExp (r1,r2) x))
applyReplacementExp (r1, r2) (Tagged typ mbtag) = if (r1 == (Tagged typ mbtag)) then (RPFlag r2) else (Tagged typ (splort mbtag))
where
	splort (Valid x) = (Valid (applyReplacementExp (r1, r2) x))
	splort (Invalid) = (Invalid)
	splort (MaybeContainer x) = (MaybeContainer (applyReplacementExp (r1, r2) x)) 
applyReplacementExp x y = error $ (show x) ++ "\n\n" ++ (show y)

-- genTransTable''' :: BSVPackage -> BSVModuleDec -> [MethodArg] -> [String] -> WriteSchedule -> RuleTree
-- genTransTable''' universe mod mArgs prefixes (RegSchedule sName (rules)) = makePreemptTree (mName mod) $ restoreTransitive (killDeadRefs (genRuleGraph rules) (genRuleGraph rules)) 0
-- genTransTable''' universe mod mArgs prefixes (VectSchedule sName (rules)) = makePreemptTree (mName mod) $ restoreTransitive (killDeadRefs (genRuleGraph rules) (genRuleGraph rules)) 0

pathPrefixEq :: ID_Path -> ID_Path -> Bool
pathPrefixEq (ID_Submod_Struct x y) (ID_Submod_Struct x' y') = if (x == x') then pathPrefixEq y y' else False
pathPrefixEq (ID_Submod_Struct x y) (ID x') = x == x'
pathPrefixEq (ID x) (ID_Submod_Struct x' y') = x == x'
pathPrefixEq (ID x) (ID x') = x == x'

-- Argument 1 is the original ID path (containing the suffix).  
-- Argument 2 is the prefix path to be removed from argument 1.  
-- Argument 3 is the path to which the suffix will be attached.
pathPrefixReplacement :: ID_Path -> Maybe ID_Path -> Expression -> Expression 
pathPrefixReplacement x Nothing (Identifier y) = Identifier $ mergeIDPaths y x
pathPrefixReplacement x Nothing y =  (FieldAccess y x)
pathPrefixReplacement (ID_Submod_Struct x y) (Just (ID_Submod_Struct x' y')) z = pathPrefixReplacement y (Just y') z
pathPrefixReplacement (ID_Submod_Struct x y) (Just (ID x')) z = pathPrefixReplacement y Nothing z
pathPrefixReplacement (ID x) (Just (ID x')) z = z

genTransTable :: BSVPackage -> BSVModuleDec -> [MethodArg] -> [String] -> [RuleSchedule] -> [BSVstateDec] -> [TransitionTable] 
genTransTable universe mod mArgs prefixes sched states = map (genTrans universe mod [] prefixes (mArgs ++ wireNames) treeHeap) states
where 
	tracy = "[genTransTable] - Rule Heap = " ++ (show ruleHeap)
	ruleHeap = genRuleHeap sched []
	treeHeap = genTreeHeap ruleHeap
	wireNames = getWireNames (state mod)

genRuleHeap :: [RuleSchedule] -> [String] -> RuleHeap 
genRuleHeap rs path = (RHeap nom rs' subheaps) 
	where
		tracy = "[genRuleHeap] - nom = " ++ (show nom) ++ "\nrs = " ++ (intercalate "\n\t" (map (show.rName) rs)) ++ "\nrs' = " ++ (intercalate "\n\t" (map (show.rName) rs'))
		tracy2 = "[genRuleHeap] - nom = " ++ (show nom) ++ "\nrawRuleNames = " ++ (show threep) ++ "\nsubheap Names = " ++ (show nextNames)
		threep = map (\q -> ap2string (rName q)) rs 
		nom = if null path then "root" else last path 
		rs' = getRelevantSchedules rs path
		nextNames = nub $ filter (isNext path) $ map (\q -> ap2string (rName q)) rs 
		subheaps = map (genRuleHeap rs) nextNames 

-- snoc :: [a] -> a -> [a]        
-- snoc [] a = [a]
-- snoc as a = reverse (a:(reverse as))
		
getRelevantSchedules :: [RuleSchedule] -> [String] -> [RuleSchedule]
getRelevantSchedules rs path = filter (\ q -> isAboveOrAt path (ap2string (rName q))) rs 

-- First arg is the full path
isAboveOrAt :: [String] -> [String] -> Bool
isAboveOrAt [] [] = True
isAboveOrAt [] ys = False
isAboveOrAt xs [] = True
isAboveOrAt (x:xs) (y:ys) = if (x == y) then isAboveOrAt xs ys else False 

isNext :: [String] -> [String] -> Bool
isNext [] [] = False
isNext [] (y:[]) = True
isNext [] ys = False
isNext xs [] = False
isNext (x:xs) (y:ys) = if (x == y) then isNext xs ys else False 


ap2string :: ActionPath -> [String]
ap2string (RuleNameAP x)         = []
ap2string (ActionNameAP x)       = []
ap2string (MethodNameAP x)       = []
ap2string (SubmoduleNameAP x ap) = x : (ap2string ap)

		
genTreeHeap :: RuleHeap -> TreeHeap
genTreeHeap (RHeap nom sched subHeaps) = (THeap nom ttree subHeaps') 
	where
		tracy = "[genTreeHeap] - nom = " ++ (show nom) ++ "tree = " ++ (show ttree)
		ttree = genTotalTree sched 0
		subHeaps' = map genTreeHeap subHeaps
	
genTrans :: BSVPackage -> BSVModuleDec -> [String] -> [String] -> [String] -> TreeHeap -> BSVstateDec -> TransitionTable 
genTrans _ mod absPath prefixes mArgs (THeap n' tree _) (BSV_RegFile n w t l) = error $ "Error! This case should not exist!"
genTrans _ mod absPath prefixes mArgs (THeap n' tree _) (BSV_Vector n x y z) = (TransVect n y tables)
where
	vectorWrites (ID r) = nub $ getVectorWrites r tree 
	tables = map (\ q -> ((applyRootPrefix mArgs prefixes q), (tablify (Just q)))) (vectorWrites n)
	n' = if absPath /= [] then mergeIDPaths (strings2idpath absPath) n else n
	tablify p = makeTreesSpecific mod (BSV_Vector n' x y z) prefixes mArgs tree p
genTrans uni mod absPath prefixes mArgs (THeap n tree _) (DWire x y z) = trace tracy $ (TransDWire x (scrubDwireSkips (redefault) specificTree) z) -- Records? 
	where 
		tracy = if (show x) == "wr_WdPointer" 
				then "[genTrans] wire = " ++ (show x) ++ "totalTree = " ++ (show tree) ++ "\n\nspecific tree = " ++ (show specificTree)
				else ""
		x' = if absPath /= [] then mergeIDPaths (strings2idpath absPath) x else x
		redefault = if (z == (Literal LitStructConstructor)) then constructDefaultValue uni mod y else z
		specificTree = makeTreesSpecific mod (DWire x' y z) prefixes mArgs tree Nothing
genTrans universe mod absPath prefixes mArgs (THeap nom tree hs) (BSV_Reg n x y) = if (n == (ID_Submod_Struct "rg_InitReqDataCount" (ID "lastdata"))) 
					then trace tracy $ result 
					else result
where 
	tracy = "[genTrans] register = " ++ (show n) ++ "\nmoduleInstance = " ++ (show (instanceName mod)) ++ "\nstructName = " ++ (show struct) ++ "\nIs a submod = " ++ (show (isSubmod n)) ++ "\nTree Heap Name = " ++ (show nom) ++ "\nrefactored tree = " ++ (show newTree)
	n' = if absPath /= [] then mergeIDPaths (strings2idpath absPath) n else n
	result = if not (struct == Nothing) 
				then (TransStruct n'' (map (genTrans universe mod absPath prefixes' mArgs (THeap nom tree hs)) newState)) 
				else if (isSubmod n) 
					then (TransReg n (applyFieldAccesses newTree fields))
					else (TransReg n newTree)
	structs = bsv_instDefs universe
	isSubmod (ID q) = False
	isSubmod (ID_Submod_Struct q r) = True
	fields = strings2idpath $ tail $ idpath2strings n
	structType = getStructType (bsv_typedefs universe) x
	struct = x `getStruct` structs 
	--structType = x `getStruct2` structTypes
	prefixes' = prefixes --reverse ((showIDPath n) : (reverse prefixes))
	newState = makeStatesFromStruct (structs) (structType) n
	newTree = makeTreesSpecific mod (BSV_Reg n' x y) prefixes mArgs tree Nothing
	n'' = lastID n
genTrans universe mod absPath prefixes mArgs (THeap n tree heaps) (BSV_SubModuleDec iName mNom inst) = (TransMod inst (map (genTrans universe actualNewMod newAbsPath newPrefixes (mArgs ++ newWires) newHeap) (state actualNewMod)))
where 
	newHeap = maybe ((THeap n tree heaps)) (id) $ getHeap inst heaps
	errorMsg = "Heep not Creeped! Beep Beep!\niName = " ++ (show iName) ++ "\nmNom = " ++ (show mNom) ++ "\ninst = " ++ (show inst) ++ "Heap Name = " ++ (show n) ++ "\nsubheapNames = " ++ (show (map (\ (THeap x _ _) -> x) heaps))
	newAbsPath = inst : absPath 
	structs = bsv_instDefs universe 
	newMod [] = error $ "BSV2PVS Error!  Uninstantiated submodule: " ++ inst 
	newMod (x:xs) = if ((instanceName x) == inst) then x else newMod xs 
	actualNewMod = (newMod (instances mod))
	newPrefixes = reverse (inst : (reverse prefixes))
	newWires = getWireNames (state actualNewMod)
genTrans uni mod prefixes absPath mArgs (THeap n' tree _) (BSV_Fifo f n x) = (TransFIFO n enqTree deqTree clearTree)
where
	n' = if absPath /= [] then mergeIDPaths (strings2idpath absPath) n else n
	showy = "[T] - genTrans - clearTree " ++ (show clearTree)
	enqTree = makeCallTreesSpecific mod n' "enq" prefixes mArgs tree
		-- (\ x -> twace ("[T] genTrans - " ++ (show tree) ++  "\nenqTree - " ++ (show x)) x) $ makeCallTreesSpecific mod n "enq" prefixes mArgs tree 
	deqTree = makeCallTreesSpecific mod n' "deq" prefixes mArgs tree 
		-- (\ x -> twace ("[T] genTrans - deqTree - " ++ (show x)) x) $ makeCallTreesSpecific mod n "deq" prefixes mArgs tree 
	clearTree = makeCallTreesSpecific mod n' "clear" prefixes mArgs tree
		-- (\ x -> twace ("[T] genTrans - clearTree - " ++ (show x)) x) $ makeCallTreesSpecific mod n "clear" prefixes mArgs tree 

applyFieldAccesses :: SpecificTree -> ID_Path -> SpecificTree
applyFieldAccesses (SpecStem gd (Left texp) ftree) fs = (SpecStem gd (Left (applyFieldAccessesXP texp fs)) (applyFieldAccesses ftree fs)) 
applyFieldAccesses (SpecStem gd (Right ttree) ftree) fs = (SpecStem gd (Right (applyFieldAccesses ttree fs)) (applyFieldAccesses ftree fs)) 
applyFieldAccesses (SpecLeaf gd texp fexp) fs = (SpecLeaf gd (applyFieldAccessesXP texp fs) (applyFieldAccessesXP fexp fs))
applyFieldAccesses (SpecEx exp) fs = (SpecEx (applyFieldAccessesXP exp fs))

applyFieldAccessesXP :: Expression -> ID_Path -> Expression
applyFieldAccessesXP (Identifier id) id' 
	| (idpath2strings id') `isSuffixOf` (idpath2strings id) = (Identifier id)
	| otherwise = (FieldAccess (Identifier id) id')
applyFieldAccessesXP (Skip) fs = (Skip)
applyFieldAccessesXP exp fs = (FieldAccess exp fs)
where
	tracy = show (FieldAccess exp fs)
		
getHeap :: String -> [TreeHeap] -> Maybe TreeHeap
getHeap n [] = Nothing
getHeap n ((THeap n' t h):hs) = if (n' == n) then Just (THeap n' t h) else getHeap n hs
where
	tracy = "[getHeap] n' = " ++ (show n')
		
lastID :: ID_Path -> String    
lastID (ID x) = x
lastID (ID_Submod_Struct x y) = lastID y
		
{-mergeFifoWriteTree :: SpecificTree -> SpecificTree -> SpecificTree
mergeFifoWriteTree (SpecStem gdd (Left expD) ftd) (SpecStem gdc (Left expC) ftc) 
	= (SpecStem gde (Left exp') ft')
where
	exp' = decideFifoExpression expD expC
	ft' = mergeFifoWriteTree ftd ftc
mergeFifoWriteTree (SpecStem gdd (Left expD) ftd) (SpecStem gdc (Right ttc) ftc) 
	= (SpecStem gde (Left expD) ft')
where
	ft' = mergeFifoWriteTree fte ftd ftc      
mergeFifoWriteTree (SpecStem gdd (Right ttd) ftd) (SpecStem gdc (Left expC) ftc) 
	= (SpecStem gde (Left expC) ft')
where
	ft' = mergeFifoWriteTree fte ftd ftc      
mergeFifoWriteTree (SpecStem gdd (Right ttd) ftd) (SpecStem gdc (Right ttc) ftc) 
	= (SpecStem gde (Right tt') ft')
where
	tt' = mergeFifoWriteTree ttd ttc
	ft' = mergeFifoWriteTree ftd ftc      
mergeFifoWriteTree (SpecLeaf gdd td fd) (SpecLeaf gdc tc fc)
	= (SpecLeaf gde texp' fexp' )
where
	texp' = decideFifoExpression td tc
	fexp' = decideFifoExpression fd fc
mergeFifoWriteTree (SpecEx exD) (SpecEx exC) = (SpecEx (decideFifoExpression exD exC))
mergeFifoWriteTree x y z = error $ "Error! Fifo Trees have different structures!\n\nenq - " ++ (show x) ++ "\n\ndeq - " ++ (show y) ++ "\n\nclear - " ++ (show z)

decideFifoExpression :: Expression -> Expression -> Expression 
decideFifoExpression (Skip) (Skip) = (Skip)
decideFifoExpression d (Skip) = d
decideFifoExpression (Skip) c = c
decideFifoExpression y z = z -}
	
constructDefaultValue :: BSVPackage -> BSVModuleDec -> BSVType -> Expression
constructDefaultValue uni mod t = def2exp dv t
	where
		dvs = bsv_instDefs uni 
		dv = lookupInst' dvs t
		
def2exp :: BSVInstDef -> BSVType -> Expression 
def2exp (n, fs) t = (StructCluster (Right (b2pType t)) fs)

	
getVectorWrites :: String -> TotalTree -> [Expression]    
getVectorWrites n (TotalStem _ stmts tt ft) = (concat (map (getVectIndex n) stmts)) ++ (getVectorWrites n tt) ++ (getVectorWrites n ft)
getVectorWrites n (TotalLeaf _ stmts) = concat $ map (getVectIndex n) stmts

getVectIndex :: String -> Statement -> [Expression]
getVectIndex n (Write (ID_Vect n' i) y z) = if (n == n') then i:[] else []
getVectIndex n (If w x y z) 		= (getVectIndex n x) ++ (getVectIndex n y)
getVectIndex n (PMatchIf v w x y z)  	= (getVectIndex n x) ++ (getVectIndex n y)	
getVectIndex n (ForLoop v w x y z) 	= getVectIndex n y
getVectIndex n (Switch x y z) 		= concat $ map (getVectIndex n) (map snd y)
getVectIndex n (LocalDec x y z) 	= (getVectIndex n) y
getVectIndex n (StatementBlock x) 	= concat $ map (getVectIndex n) x
getVectIndex n x 			= []


makeStatesFromStruct ::  [BSVInstDef] -> BSVTypeDef -> ID_Path -> [BSVstateDec]
makeStatesFromStruct insts (BSV_Struct n fs) localPath = makeStateFromStruct inst (BSV_Struct n fs) localPath
where 
	inst = maybe (error ("Couldn't find instance definition for struct : " ++ (show n) ++ "\n\nList of instances : " ++ (intercalate "\n" (map show insts)))) (id) $ getInstDef insts n

makeStateFromStruct :: BSVInstDef -> BSVTypeDef -> ID_Path -> [BSVstateDec]
makeStateFromStruct _ (BSV_Struct _ []) _ = []
makeStateFromStruct (n', fs) (BSV_Struct n ((f, t):xs)) localPath = (BSV_Reg address t (unMaybe (lookup f fs))) : (makeStateFromStruct (n', fs) (BSV_Struct n xs) localPath)
where 
	address =  mergeIDPaths localPath (ID f)
	unMaybe (Just x) = x
	unMaybe Nothing = error $ "BSV2PVS Error! Specific Structure field lacks instantiation!"

getInstDef :: [BSVInstDef] -> String -> Maybe BSVInstDef
getInstDef [] _ = Nothing 
getInstDef ((n, fs):xs) n' = if n == n' then Just (n, fs) else getInstDef xs n' 
	-- trace ("n = " ++ (show n) ++ "\nn' = " ++ (show n') ++ "result = " ++ (show (n == n'))) $ if n == n' then Just (n, fs) else getInstDef xs n' 
	
getStructType :: [BSVTypeDef] -> BSVType -> BSVTypeDef
getStructType [] _ = error $ "BSV2PVS Error! Searching for a structure type definition that doesn't exist!"
getStructType ((BSV_Struct n fs):xs) (BSV_Custom y) = if n == y then (BSV_Struct n fs) else getStructType xs (BSV_Custom y)
getStructType (x:xs) (BSV_Custom y) = getStructType xs (BSV_Custom y)
getStructType _ y = error $ "BSV2PVS Error! Tried to access a record type definition for a non-record type: " ++ (show y)

getStruct :: BSVType -> [BSVInstDef] -> Maybe BSVInstDef
getStruct _ [] = Nothing
getStruct (BSV_Custom n) ((n', m):xs) = if (n == n') then (Just (n', m)) else (BSV_Custom n) `getStruct` xs
getStruct x _ = Nothing

-- getStruct2 :: BSVType -> [BSVTypeDef] -> BSVTypeDef
-- getStruct2 _ [] = error $ "Error, struct not found!"
-- getStruct2 (BSV_Custom n) ((BSV_Struct n' m):xs) = if (n == n') then (BSV_Struct n' m) else (BSV_Custom n) `getStruct2` xs
-- getStruct2 y (x:xs) = y `getStruct2` xs
-- getStruct2 x _ = error $ "Error, this is not a custom type!"

getWireNames :: [BSVstateDec] -> [String]    
getWireNames [] = []
getWireNames ((DWire x y z):xs) = (showIDPath x) : (getWireNames xs)
getWireNames (x:xs) = (getWireNames xs)
	
scrubDwireSkips :: Expression -> SpecificTree -> SpecificTree
scrubDwireSkips deft (SpecStem w (Right y) z) = (SpecStem w (Right (scrubDwireSkips deft y)) (scrubDwireSkips deft z))
scrubDwireSkips deft (SpecStem w (Left x) z) = (SpecStem w (Left x) (scrubDwireSkips deft z))
scrubDwireSkips deft (SpecLeaf x y z) = (SpecLeaf x (repWDefault (scrubDwireSkips' deft y)) (repWDefault (scrubDwireSkips' deft z)) )
where
	repWDefault x = if x == (Skip) then deft else x 
scrubDwireSkips deft (SpecEx x) = (SpecEx (if x == (Skip) then deft else scrubDwireSkips' deft x)) 

scrubDwireSkips' :: Expression -> Expression -> Expression 
scrubDwireSkips' x (Skip) = x
scrubDwireSkips' x y = y
	
deReferenceStructure :: String -> TransitionTable -> TransitionTable 
deReferenceStructure [] (TransStruct l ws) = (TransStruct l (map (deReferenceStructure l) ws))
deReferenceStructure s (TransVect x n y) = if isIDPrefix s x then (TransVect (removeStructPrefix x) n y) else (TransVect x n y)
deReferenceStructure s (TransReg x y) = if isIDPrefix s x then (TransReg (removeStructPrefix x) y) else (TransReg x y) 
deReferenceStructure s (TransStruct l ws) = (TransStruct s (map (deReferenceStructure s) ws))

isIDPrefix :: String -> ID_Path -> Bool
isIDPrefix _ (ID _) = False
isIDPrefix x (ID_Submod_Struct x' y) = x == x'

removeStructPrefix :: ID_Path -> ID_Path
removeStructPrefix (ID _ ) = error $ "BSV2PVS Error! Attempting to remove a structure prefix from a prefixless Identifier"
removeStructPrefix (ID_Submod_Struct x y) = y

addStructPrefix :: ID_Path -> [TransitionTable] -> [TransitionTable]
addStructPrefix _ [] = []
addStructPrefix n ((TransReg n' rs):xs) =  (TransReg (applyIDPrefix n n') rs) : (addStructPrefix n xs)
addStructPrefix n ((TransVect n' x rs):xs) =  (TransVect (applyIDPrefix n n') x rs) : (addStructPrefix n xs)
addStructPrefix n ((TransMod n' ws):xs) =  error $ "BSV2PVS Error! Structures are not permitted to contain submodule declarations!"--(SubMod (ID_Submod_Struct n n') sub rs) : (addStructPrefix nom xs)
addStructPrefix n ((TransStruct n' ws):xs) =  (TransStruct n' (addStructPrefix n ws)) : (addStructPrefix n xs)
addStructPrefix n ((TransDWire n' rs dv):xs) =  (TransDWire (applyIDPrefix n n') rs dv) : (addStructPrefix n xs)

getVectSize :: [BSVstateDec] -> ID_Path -> Integer
getVectSize [] vName = error $ "BSV2PVS Error! Vector \"" ++ (show vName) ++ "\" not found in state list!"
getVectSize ((BSV_Vector nom typ n init):xs) vName = if (nom == vName) then n else getVectSize xs vName 
getVectSize (x:xs) vName = getVectSize xs vName


killDeadRefs :: [RuleNode] -> [RuleNode] -> [RuleNode]
killDeadRefs _ [] = []
killDeadRefs nodes (x:xs) = (RuleNode {rnName = (rnName x), rnPreempts = newPreempts}) : (killDeadRefs nodes xs)
where 
	noms = map (\ x -> RuleNameAP x) $ map (rnName) nodes
	newPreempts = filter (\x -> x `elem` noms) (rnPreempts x)

-- extractTreeExpression :: BSVModuleDec -> [MethodArg] -> ID_Path -> [String] -> RuleName -> Expression
-- extractTreeExpression mod mArgs sName prefixes rName = exp stmt 
--   where 
--     rule = findRule (rules mod) rName 
--     stmt = findStmt ((\(_,_,x,_) -> x) rule) (actions mod) sName
--     exp (Just x) = extractExpression mArgs sName prefixes x
--     exp (Nothing) = error $ "BSV2PVS Error! Expression does not exist in current context: " ++ rName 

extractExpression :: [MethodArg] -> ID_Path -> [String] -> Statement -> Expression 
extractExpression mArgs _ prefixes (Write nom exp atts) = applyRootPrefix mArgs prefixes exp
extractExpression mArgs nom prefixes (MethodCall w'' x y z) = (Exp_MethodCall (ID w') x (map (applyRootPrefix mArgs prefixes) y) (Just []))
	where
		w = showIDPath w''
		w' = if x == "deq" 
				then w ++ "_enq"
				else if x == "clear"
					then w ++ "_deq"
					else if x == "enq"
					then "pre`" ++ w
					else w
extractExpression mArgs nom prefixes (ActionCall _ _ ) = error $ "BSV2PVS Error! Something has been badly borked! Action call considered as register write!" 
extractExpression mArgs nom prefixes (Return exp _ ) = applyRootPrefix mArgs prefixes exp
extractExpression mArgs nom prefixes (StructReturn x fs _) = (StructCluster (Right (b2pType x)) (map (\ (x, y) -> (x, applyRootPrefix mArgs prefixes y)) fs))
extractExpression mArgs nom prefixes (If guard thn els atts) = (Exp_If (applyRootPrefix mArgs prefixes guard) (extractExpression mArgs nom prefixes thn) (extractExpression mArgs nom prefixes els))
extractExpression mArgs nom prefixes (ForLoop _ _ _ _ _) = error $ "BSV2PVS Error! Not handling For Loops quite yet..."
extractExpression mArgs nom prefixes (Switch guard cases atts) = CasesOf (applyRootPrefix mArgs prefixes guard) (map (\ (x, y)-> (x, (extractExpression mArgs nom prefixes y)) ) cases)
-- extractExpression mArgs nom prefixes (Switch guard [] atts) = error $ "BSV2PVS Error! Swich case contains no cases!" 
-- extractExpression mArgs nom prefixes (Switch guard (c:[]) atts) = (Exp_If (Equals (applyRootPrefix mArgs prefixes guard) lit) (extractExpression mArgs nom prefixes stmt) (Identifier nom))
--   where 
--     lit = ( \ (x,_) -> x ) c
--     stmt = ( \ (_,x) -> x ) c
-- extractExpression mArgs nom prefixes (Switch guard (c:cs) atts) = (Exp_If (Equals (applyRootPrefix mArgs prefixes guard) lit) (extractExpression mArgs nom prefixes stmt) (extractExpression mArgs nom prefixes (Switch guard cs atts)))
--   where 
--     lit = ( \ (x,_) -> x ) c
--    stmt = ( \ (_,x) -> x ) c
extractExpression mArgs nom prefixes (StatementBlock (s:[])) = extractExpression mArgs nom prefixes s
extractExpression mArgs nom prefixes (StatementBlock ss) = maybe (Skip) (extractExpression mArgs nom prefixes) boink
where
	boink = getStatement (ID (lastID nom)) ss
extractExpression _ _ _ (Void) = (Skip)
extractExpression mArgs nom prefixes (PMatchIf i1 i2 stmt1 stmt2 atts) = (MaybeIf (stripExp (applyRootPrefix mArgs prefixes (Identifier i1))) i2 (extractExpression ((showIDPath i2):mArgs) nom prefixes stmt1) (extractExpression mArgs nom prefixes stmt2))
	where 
		stripExp (Identifier x) = x
		-- showy = "[extractExpression] - PMatchIf - " ++ (show i2)
extractExpression mArgs nom prefixes (LocalDec vars stmt atts) = (Binding (map update vars) (extractExpression (mArgs ++ newExclusions) nom prefixes stmt))
where
	update (i, t, exp) = (i, t, applyRootPrefix (mArgs++newExclusions) prefixes exp)
	newExclusions = map (\ (x,_,_) -> showIDPath x) vars
	-- showy = "[extractExpression] - LocalDec -"
	
	--(ID_Path, (Either (Maybe BSVType) (Maybe PVSType)), Expression)
	
getStatement :: ID_Path -> [Statement] -> Maybe Statement
getStatement _ [] = Nothing
getStatement n' ((Write n x y):xs) = if n `pathEq` n' then Just (Write n x y) else (getStatement n' xs)
getStatement n' ((If gd thn els z):xs) = if thn' == Void && els' == Void 
					then (getStatement n' xs)
					else Just (If gd thn' els' z)
	where
		thn' = maybe (Void) (id) $ getStatement n' (thn:[])
		els' = maybe (Void) (id) $ getStatement n' (els:[])
getStatement n' ((Void):xs) = (getStatement n' xs)
getStatement n x = error $ (show x) ++ "\n\n\n" ++ (show n)

applyRootPrefix :: [MethodArg] -> [String] -> Expression -> Expression
applyRootPrefix m p (Negative x)	= (Negative (applyRootPrefix m p x))
applyRootPrefix m p (Not x) 		= (Not (applyRootPrefix m p x))
applyRootPrefix m p (Equals x y) 	= (Equals (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (NotEquals x y) 	= (NotEquals (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (GreaterEquals x y) = (GreaterEquals (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (LessEquals x y) 	= (LessEquals (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (Greater x y) 	= (Greater (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (Less x y) 		= (Less (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (And x y) 		= (And (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (Or x y) 		= (Or (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (BitwiseAND x y) 	= (BitwiseAND (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (BitwiseOR x y) 	= (BitwiseOR (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (BitwiseXOR x y) 	= (BitwiseXOR (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (LShift x y) 	= (LShift (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (RShift x y) 	= (RShift (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (BitSelect x y)     = (BitSelect (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (BitSelectRange x y z) = (BitSelectRange (applyRootPrefix m p x) (applyRootPrefix m p y) (applyRootPrefix m p z))
applyRootPrefix m p (BitConcat xs) 	= (BitConcat (map (applyRootPrefix m p) xs))
applyRootPrefix m p (Multiply x y) 	= (Multiply (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (Divide x y) 	= (Divide (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (Modulo x y)  = (Modulo (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (Add x y) 		= (Add (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (Subtract x y) 	= (Subtract (applyRootPrefix m p x) (applyRootPrefix m p y))
applyRootPrefix m p (Literal x) 		= (Literal x)
applyRootPrefix m p (Identifier (ID_Submod_Struct inst path))	= if (elemWith inst m stringPathEq) 
					then (Identifier (ID_Submod_Struct inst path))
					else (Identifier (applyRootPrefix' (ID_Submod_Struct inst path) p))
applyRootPrefix m p (Identifier (ID_Vect str index)) = if (elemWith str m stringPathEq) 
					then (Identifier (ID_Vect str (applyRootPrefix m p index))) 
					else (Identifier (applyRootPrefix' (ID_Vect str (applyRootPrefix m p index)) p)) 
applyRootPrefix m p (Identifier (ID str)) 	= if (elemWith str m stringPathEq) then (Identifier (ID str)) else (Identifier (applyRootPrefix' (ID str) p)) 
applyRootPrefix m p (ValueMethodCall w x y z) = (ValueMethodCall w x y z)
applyRootPrefix m p (Exp_MethodCall inst meth zs q) = (Exp_MethodCall inst meth (map (applyRootPrefix m p) zs) q){-if (meth == "first") || (meth == "deq") || (meth == "clear")
	then (Exp_MethodCall (applyRootPrefix' inst p) meth (map (applyRootPrefix m p) zs) q)
	else (Exp_MethodCall inst meth (map (applyRootPrefix m p) zs) q)-}
applyRootPrefix m p (Exp_If x y z) = (Exp_If (applyRootPrefix m p x) (applyRootPrefix m p y) (applyRootPrefix m p z))
applyRootPrefix m p (Skip) = (Skip)
applyRootPrefix m p (RPFlag x) = applyRootPrefix m p x
applyRootPrefix m p (Tagged t (Valid x)) = Tagged t $ Valid $ applyRootPrefix m p x
applyRootPrefix m p (Tagged t Invalid) = Tagged t Invalid
applyRootPrefix m p (MaybeIf i j x y) = 
	(MaybeIf 
		(if (elemWith (showIDPath i) m stringPathEq) then i else (applyRootPrefix' i p)) 
		j 
		(applyRootPrefix ((showIDPath j):m) p x) 
		(applyRootPrefix ((showIDPath j):m) p y)
	)
applyRootPrefix m p (FromMaybe (ID_Submod_Struct x y) z) = if (elemWith (showIDPath (ID_Submod_Struct x y)) m stringPathEq)
	--trace ("[T] applyRootPrefix - m = " ++ (show m) ++ "\n\n" ++ (show showy2) ++ "\n\n" ++ (show showy) ++ "\n\n") $ 
	then (FromMaybe (ID_Submod_Struct x y) (applyRootPrefix m p z))
	else (FromMaybe (applyRootPrefix' (ID_Submod_Struct x y) p) (applyRootPrefix m p z))
	where
		showy = stringPathEq (head m) (showIDPath (ID_Submod_Struct x y))
		showy2 = split (== '`') (showIDPath (ID_Submod_Struct x y))
applyRootPrefix m p (FromMaybe (ID_Vect str index) z) = if (elemWith str m stringPathEq) 
					then (FromMaybe (ID_Vect str (applyRootPrefix m p index)) (applyRootPrefix m p z)) 
					else (FromMaybe (applyRootPrefix' (ID_Vect str (applyRootPrefix m p index)) p) (applyRootPrefix m p z)) 
applyRootPrefix m p (FromMaybe (ID str) y) 	= if (elemWith str m stringPathEq) then (FromMaybe (ID str) y) else (FromMaybe (applyRootPrefix' (ID str) p) (applyRootPrefix m p y)) 
applyRootPrefix m p (Binding vars exp) = (Binding vars' (applyRootPrefix m' p exp))
where
	m' = m ++ ( map (\(x,_,_) -> showIDPath x) vars)
	vars' = map (\ (x,y,z) -> (x,y,(applyRootPrefix m' p z))) vars
applyRootPrefix m p (Exp_FunctionCall n args) = (Exp_FunctionCall n (map (applyRootPrefix m p) args))
applyRootPrefix m p (StructCluster t ss) = (StructCluster t (map (\ (s1, s2) -> (s1, (applyRootPrefix m p s2)) ) ss))
applyRootPrefix m p (IsValid x) = (IsValid (boink x))
where
	boink (Valid z) = (Valid (applyRootPrefix m p z))
	boink (Invalid) = (Invalid)
	boink (MaybeContainer z) = (MaybeContainer (applyRootPrefix m p z))
applyRootPrefix m p (MaybeValue x) 		= (MaybeValue (applyRootPrefix m p x))
applyRootPrefix m p (FieldAccess exp i) = (FieldAccess (applyRootPrefix m p exp) i)
applyRootPrefix _ _ x = error $ show $ x
	
elemWith :: (Eq a) => a -> [a] -> (a -> a -> Bool) -> Bool
elemWith _ [] _ = False 
elemWith x (y:ys) f = if f x y 
					then True 
					else elemWith x ys f
	
stringPathEq :: String -> String -> Bool
stringPathEq xs ys = stringPathEq' xss yss
	where
		xss = split (== '`') xs -- (\ x -> trace ("[T] stringPathEq - xss = " ++ (show x)) x) $ splitOn '`' xs
		yss = split (== '`') ys -- (\ x -> trace ("[T] stringPathEq - yss = " ++ (show x)) x) $ splitOn '`' ys

		
stringPathEq' :: [String] -> [String] -> Bool
stringPathEq' [] [] = True
stringPathEq' x [] = True
stringPathEq' [] y = True
stringPathEq' (x:xs) (y:ys) = if x == y
					then stringPathEq' xs ys
					else False
	
rmRF :: Expression -> Expression
rmRF (Negative x)   	=  (Negative (rmRF x))
rmRF (Not x) 		= (Not (rmRF x))
rmRF (Equals x y)   	= (Equals (rmRF x) (rmRF y))
rmRF (NotEquals x y) 	= (NotEquals (rmRF x) (rmRF y))
rmRF (GreaterEquals x y)= (GreaterEquals (rmRF x) (rmRF y))
rmRF (LessEquals x y) 	= (LessEquals (rmRF x) (rmRF y))
rmRF (Greater x y)  	= (Greater (rmRF x) (rmRF y))
rmRF (Less x y) 	= (Less (rmRF x) (rmRF y))
rmRF (And x y) 		= (And (rmRF x) (rmRF y))
rmRF (Or x y) 		= (Or (rmRF x) (rmRF y))
rmRF (BitwiseAND x y) 	= (BitwiseAND (rmRF x) (rmRF y))
rmRF (BitwiseOR x y) 	= (BitwiseOR (rmRF x) (rmRF y))
rmRF (BitwiseXOR x y) 	= (BitwiseXOR (rmRF x) (rmRF y))
rmRF (LShift x y)   	= (LShift (rmRF x) (rmRF y))
rmRF (RShift x y)   	= (RShift (rmRF x) (rmRF y))
rmRF (BitSelect x y)    = (BitSelect (rmRF x) (rmRF y))
rmRF (BitSelectRange x y z) = (BitSelectRange (rmRF x) (rmRF y) (rmRF z))
rmRF (BitConcat xs) 	= (BitConcat (map rmRF xs))
rmRF (Multiply x y) 	= (Multiply (rmRF x) (rmRF y))
rmRF (Divide x y)   	= (Divide (rmRF x) (rmRF y))
rmRF (Modulo x y)       = (Modulo (rmRF x) (rmRF y))
rmRF (Add x y) 		= (Add (rmRF x) (rmRF y))
rmRF (Subtract x y) 	= (Subtract (rmRF x) (rmRF y))
rmRF (Literal x) 	= (Literal x)
rmRF (Identifier x)	= (Identifier x)
rmRF (Exp_If x y z)     = (Exp_If (rmRF x) (rmRF y) (rmRF z))
rmRF (Exp_MethodCall inst meth zs q) = (Exp_MethodCall inst meth (map rmRF zs) q)
rmRF (Exp_FunctionCall a b) = (Exp_FunctionCall a (map rmRF b))
rmRF (ValueMethodCall a b c d) = (ValueMethodCall a b c d)
rmRF (Skip) = (Skip)
rmRF (RPFlag x) = x

getReadsBy :: BSVModuleDec -> (ID_Path -> Bool) -> [String] -> Expression  -> [ID_Path]
getReadsBy m p i (Negative x)   	= getReadsBy m p i x
getReadsBy m p i (Not x) 		= getReadsBy m p i x
getReadsBy m p i (Equals x y)   	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (NotEquals x y) 	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (GreaterEquals x y)= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (LessEquals x y) = (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (Greater x y)  	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (Less x y) 	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (And x y) 	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (Or x y) 	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (BitwiseAND x y) = (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (BitwiseOR x y) 	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (BitwiseXOR x y) = (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (LShift x y)   	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (RShift x y)   	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (Multiply x y) 	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (Divide x y)   	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (Modulo x y)     = (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (Add x y) 	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (Subtract x y) 	= (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (BitSelect x y)    = (getReadsBy m p i x) ++ (getReadsBy m p i y) 
getReadsBy m p i (BitSelectRange x y z) = (getReadsBy m p i x) ++ (getReadsBy m p i y) ++ (getReadsBy m p i z)
getReadsBy m p i (BitConcat xs) 	= concat $ map (getReadsBy m p i) xs
getReadsBy m p i (Literal x) 	= []
getReadsBy m p i (Identifier x)	= if (p x) then (x'):[] else []
where
	x' = if not (null i) then (mergeIDPaths (strings2idpath i) x) else x
getReadsBy m p i (Exp_If x y z)   = (getReadsBy m p i x) ++ (getReadsBy m p i y) ++ (getReadsBy m p i z)
getReadsBy m p i (Exp_FunctionCall a b) 	= concat $ map (getReadsBy m p i) b
getReadsBy m p i (ValueMethodCall a b c d)= []
getReadsBy m p i (Skip) 		= []
getReadsBy m p i (Tagged _ (Valid x)) = (getReadsBy m p i x)
getReadsBy m p i (Tagged _ Invalid) = []
getReadsBy m p i (RPFlag x) 	= (getReadsBy m p i x)
getReadsBy m p i (FromMaybe x y)	= if (p x) then x : (getReadsBy m p i y) else (getReadsBy m p i y)
getReadsBy m p i (StructCluster t ss) = concat $ map (getReadsBy m p i) $ map snd ss 
--   where
--       showy = "[T] getReadsBy - " ++ (show t) ++ "\n\n" ++ (show (concat $ map (getReadsBy p) $ map snd ss ))
getReadsBy m p i (Binding lvs x) = (concat (map (getReadsByLVs m p i) lvs)) ++ (getReadsBy m p i x)
getReadsBy m p i (MaybeIf i1 i2 x y) = (if (p i1) then i1:[] else [])   ++ (getReadsBy m p i x) ++ (getReadsBy m p i y)
getReadsBy m p i (IsValid x) = boink x
	where
		boink (Valid z) = getReadsBy m p i z
		boink (Invalid) = []
		boink (MaybeContainer z) = getReadsBy m p i z
getReadsBy m p i (CasesOf xp cases) = (getReadsBy m p i xp) ++ (getReadsByCases m p i cases)
getReadsBy m p i (Exp_MethodCall path "enq" zs _) = (concat $ map (getReadsBy m p i) zs)
getReadsBy m p i (Exp_MethodCall path "deq" zs _) = (concat $ map (getReadsBy m p i) zs)
getReadsBy m p i (Exp_MethodCall path "first" zs _) = (concat $ map (getReadsBy m p i) zs)
getReadsBy m p i (Exp_MethodCall path "clear" zs _) = (concat $ map (getReadsBy m p i) zs)
getReadsBy mod p i (Exp_MethodCall path meth zs (Nothing)) = (map applyI deepReads) ++ (concat $ map (getReadsBy mod p i) zs)
where 
	tracy = "[getReadsBy] meth - " ++ (show path) ++ "." ++ (show meth) ++ "\nMethod Body = " ++ (show (snd pair)) ++ "\ndeep Reads = " ++ (show deepReads)
	pair = maybe (error errMsg) (id) $ findSubModbyInst' (instances mod) path meth
	mod' = fst pair    
	stmts = (\ (_,_,_,_,x,_) -> x ) $ snd pair
	deepReads = getReadsByOverStatements mod' p (i ++ (idpath2strings path)) stmts 
	applyI str = if not (null i) then (mergeIDPaths (strings2idpath i) str) else str
	errMsg = "Error! subModule not found! Searching for " ++ (show (Exp_MethodCall path meth zs Nothing)) ++ "\nin " ++ (show mod)
getReadsBy m p i (Exp_MethodCall path meth zs (Just theAnswer)) = theAnswer
getReadsBy m p i (MaybeValue x) = (getReadsBy m p i x)
getReadsBy m p i (FieldAccess exp i') = map (\ x -> mergeIDPaths x i') $ getReadsBy m p i exp
getReadsBy m p i x = error $ show $ x

getReadsByCases :: BSVModuleDec -> (ID_Path -> Bool) -> [String] -> [ExpCase] -> [ID_Path]
getReadsByCases _ _ _ [] = []
getReadsByCases m p i ((lit, xp):cs) = (getReadsBy m p i xp) ++ (getReadsByCases m p i cs)

getReadsByLVs :: BSVModuleDec -> (ID_Path -> Bool) -> [String] -> LocalVar -> [ID_Path]
getReadsByLVs m p i (_, _, x) = getReadsBy m p i x

addModuleInfo :: BSVPackage -> BSVModuleDec -> [String] -> Expression -> Expression
addModuleInfo u s p (Negative x)   	=  (Negative (addModuleInfo u s p x))
addModuleInfo u s p (Not x) 		= (Not (addModuleInfo u s p x))
addModuleInfo u s p (Equals x y)   	= (Equals (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (NotEquals x y) 	= (NotEquals (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (GreaterEquals x y)   = (GreaterEquals (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (LessEquals x y) 	= (LessEquals (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (Greater x y)  	= (Greater (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (Less x y) 	        = (Less (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (And x y) 		= (And (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (Or x y) 		= (Or (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (BitwiseAND x y) 	= (BitwiseAND (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (BitwiseOR x y) 	= (BitwiseOR (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (BitwiseXOR x y) 	= (BitwiseXOR (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (LShift x y)   	= (LShift (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (RShift x y)   	= (RShift (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (Multiply x y) 	= (Multiply (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (Divide x y)   	= (Divide (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (Modulo x y)          = (Modulo (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (Add x y) 		= (Add (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (Subtract x y) 	= (Subtract (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (Literal x) 	        = (Literal x)
addModuleInfo u s p (Identifier x)	= (Identifier x)
addModuleInfo u s p (Exp_MethodCall inst meth [] q) = (Exp_MethodCall newInst meth [] q) -- val
where
	newInst = strings2idpath $ p ++ (idpath2strings inst)
	tracy = "[addModuleInfo] exp method call = " ++ (show (Exp_MethodCall inst meth [] q))
--     where 
--         path = (findModPathbyInst (state s) inst)
--         val = if null path 
--                 then (Exp_MethodCall inst meth [] q)
--                 else (\ x -> (ValueMethodCall path x (showIDPath inst) meth))
addModuleInfo u s p (Exp_MethodCall inst meth zs q) = (Exp_MethodCall inst meth (map (addModuleInfo u s p) zs) q)
addModuleInfo u s p (Exp_If x y z)   = (Exp_If (addModuleInfo u s p x) (addModuleInfo u s p y) (addModuleInfo u s p z))
addModuleInfo u s p (Skip) = (Skip)
addModuleInfo u s p (StructCluster t ss) = (StructCluster t (map (\(s1, s2) -> (s1, (addModuleInfo u s p s2))) ss))
addModuleInfo u s p (Binding lvs x) =  (Binding (map (addLVmoduleInfo u s p) lvs) (addModuleInfo u s p x))
addModuleInfo u s p (MaybeIf id1 id2 x y) = (MaybeIf id1 id2 (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (Tagged w x) = (Tagged w (addTagModuleInfo u s p x))
addModuleInfo u s p (BitSelect x y) = (BitSelect (addModuleInfo u s p x) (addModuleInfo u s p y))
addModuleInfo u s p (BitSelectRange x y z) = (BitSelectRange (addModuleInfo u s p x) (addModuleInfo u s p y) (addModuleInfo u s p z))
addModuleInfo u s p (BitConcat xs) = (BitConcat (map (addModuleInfo u s p) xs))
addModuleInfo u s p x = error $ "Function \"addModuleInfo\" contains no case for the following expression type: " ++ (show x)

addTagModuleInfo :: BSVPackage -> BSVModuleDec -> [String] -> MaybeTag -> MaybeTag
addTagModuleInfo u s p (Valid x) = (Valid (addModuleInfo u s p x))
addTagModuleInfo _ _ _ (Invalid) = (Invalid)

addLVmoduleInfo :: BSVPackage -> BSVModuleDec -> [String] -> LocalVar -> LocalVar
addLVmoduleInfo u s p (x, y, exp) = (x, y, addModuleInfo u s p exp)

applyRootPrefix' :: ID_Path -> [String] -> ID_Path
applyRootPrefix' str [] = str
applyRootPrefix' str (p:ps) = (ID_Submod_Struct p (applyRootPrefix' str ps))

removeAtIndex :: Int -> [a] -> [a]
removeAtIndex i xs = front ++ back'
where 
	front = fst $ splitAt i xs 
	back  = snd $ splitAt i xs 
	back' = if (not (null back)) then (tail back) else []

deStruct :: [RuleSchedule] -> [RuleSchedule]
deStruct [] = []
deStruct (x:xs) = (update x) : (deStruct xs)
where 
	update y = y {writesTo = newWrites}
	newWrites = map (stripIDPrefix) (writesTo x)

stripIDPrefix :: ID_Path -> ID_Path
stripIDPrefix (ID i) = (ID i)
stripIDPrefix (ID_Submod_Struct x y) = y 
	
unMaybeWithDefault :: Maybe a -> a -> a 
unMaybeWithDefault (Just x) _ = x
unMaybeWithDefault (Nothing) x = x
	
genFakeRegs :: ID_Path -> [BSVTypeDef] -> [BSVInstDef] -> String -> String -> [BSVstateDec]
genFakeRegs _ [] _ n _ = error $ "BSV2PVS Error! Some foolish register has a type ("++ n ++") that might be an undeclared structure!  At least, I didn't find the structure it references."
genFakeRegs prefix ((BSV_Struct n1 fs):xs) insts n2 instName = if n1 == n2 
	then ( map (\ (x , t) -> genFakeReg (ID_Submod_Struct instName prefix) (lookupInst insts n1) (x,t)) fs )
	else genFakeRegs prefix xs insts n2 instName
genFakeRegs prefix (x:xs) insts n2 instName = genFakeRegs prefix xs insts n2 instName

lookupInst :: [BSVInstDef] -> String -> BSVInstDef
lookupInst [] n = error $ "BSV2PVS Error! Structure \"" ++ n ++ "\" has no corresponding default value instantiation!"
lookupInst ((n1, fs):xs) n2 = if n1 == n2 then (n1, fs) else lookupInst xs n2

lookupInst' :: [BSVInstDef] -> BSVType -> BSVInstDef
lookupInst' [] n = error $ "BSV2PVS Error! Structure \"" ++ (show n) ++ "\" has no corresponding default value instantiation!"
lookupInst' x (BSV_Maybe y) = lookupInst' x y
lookupInst' ((n1, fs):xs) (BSV_Custom n2) = if n1 == n2 then (n1, fs) else lookupInst xs n2
lookupInst' _ x = error $ "BSV2PVS Error! Looking up definition of non structure type: " ++ (show x)

genFakeReg :: ID_Path -> BSVInstDef -> BSV_Field -> BSVstateDec
genFakeReg prefix (_, fs) (n, t) = (BSV_Reg (applyIDPrefix prefix (ID n)) t ini)
where
	ini = unMaybe $ lookup n fs
	unMaybe (Just x) = x
	unMaybe (Nothing) = error $ "BSV2PVS Error! Initialization value for field \""++ n ++"\" not found in corresponding default value instantiation!"

applyIDPrefix :: ID_Path -> ID_Path -> ID_Path
applyIDPrefix (ID _) x = x
applyIDPrefix (ID_Submod_Struct x y) z = (ID_Submod_Struct x (applyIDPrefix y z))
	
filterForModule :: [Expression] -> BSVModuleDec -> [Expression]
filterForModule [] _ = []
filterForModule ((Exp_MethodCall inst nom exps q):xs) mod = if ((showIDPath inst) == (instanceName mod)) then ((Exp_MethodCall inst nom exps q): (filterForModule xs mod)) else (filterForModule xs mod)
filterForModule (_:xs) mod = filterForModule xs mod
	
getMethNames :: Expression -> String
getMethNames (Exp_MethodCall mName _ _ _) = showIDPath mName
getMethNames x = ""

{- noMethodConflicts :: [(String,[String])] -> Bool
noMethodConflicts xs = null (intersect meths confList)
where 
	meths = fst $ unzip xs
	confList = concat $ snd $ unzip xs 

getMethodConfList :: BSVPackage -> BSVModuleDec -> [RuleDec] -> [(String,[String])]
getMethodConfList universe mod ruleMeths = ruleConflicts rsnc
where 
	atts = attributes mod
	writesTuple = zip (map (\(x,_,_,_) -> x ) ruleMeths) (map (getWrites universe mod) (map (\ (_,_,x,_) -> x) ruleMeths))
	rulesSansNonConflicts [] = []
	rulesSansNonConflicts ((rName, writes):xs) = (rName, (removeSelf rName (removeNonConflicts atts ruleMeths rName)), writes) : (rulesSansNonConflicts xs)
	rsnc = rulesSansNonConflicts writesTuple
	ruleConflicts [] = [] 
	ruleConflicts ((rName, ruls, writes):xs) = (rName, (getConflicts universe mod ruls writes)) : (ruleConflicts xs) -}
	
removeSelf :: String -> [RuleDec] -> [RuleDec]
removeSelf _ [] = []
removeSelf rName ((x,y,z,w):xs) = (if (not (rName == x)) then ((x,y,z,w):[]) else [] ) ++ (removeSelf rName xs)

convertMethodToRule :: MethodBody -> Maybe RuleDec 
convertMethodToRule (x,(Action),_,y,z,_) = Just (x, y, z,[])
convertMethodToRule (x,(ActionValue _),_,y,z,_) = Just (x, y, z,[])
convertMethodToRule (x,(Value _),_,y,z,_) = Nothing

convertMethodToRule' :: MethodBody -> RuleDec 
convertMethodToRule' (x,_,_,y,z,_) = (x, y, z,[])

-- genTransitionGuards' :: BSVPackage -> [TransitionGuard]
-- genTransitionGuards' universe = concat $ map (\x -> genTransitionGuards universe (rulizeMethods x) (getMethNames' x)) (bsv_modules universe)

getMethNames' :: BSVModuleDec -> [String]
getMethNames' mod = map (\ (x,_,_,_,_,_) -> x) meths 
where 
	meths = methods mod

-- genTransitionGuards :: BSVPackage -> BSVModuleDec -> [String] -> [TransitionGuard]
-- genTransitionGuards universe mod meths = zip3 nom modName expr
--   where 
--     nom = map (\ (x,_,_,_) -> x) (rules mod)
--     modName = replicate (length nom) (mName mod)
--     schedule = genRuleSchedules universe mod [] meths
--     expr = map (extractGuardExpression universe mod) schedule

rulizeMethods :: BSVModuleDec -> BSVModuleDec 
rulizeMethods mod = update mod
where
	newRules = (rules mod) ++ (map (convertMethodToRule') (methods mod))
	update x = x {rules = newRules, methods = []}

extractGuardExpression :: BSVPackage -> BSVModuleDec -> RuleSchedule -> Expression 
extractGuardExpression universe mod sched = if (not (null preemptList)) 
	then (And (baseGuard) (Not (andChain)))
	else baseGuard -- Hurray for lazy evaluation! 
where 
	baseGuard = applyRootPrefix [] (( (mName mod) ++ "_var"):[]) (rGuard sched)
	preemptList = isPreemptedBy sched
	preemptGuards = map (fetchGuardByRule universe mod) preemptList
	andChain = applyRootPrefix [] (( (mName mod) ++ "_var"):[]) (andChainExps preemptGuards)

andChainExps :: [Expression] -> Expression
andChainExps (x:[]) = x
andChainExps (x:xs) = (And x (andChainExps xs))

fetchGuardByRule :: BSVPackage -> BSVModuleDec -> ActionPath -> Expression
fetchGuardByRule universe mod (ActionNameAP x) = (Literal (LitBool True))
fetchGuardByRule universe mod (MethodNameAP x) = (\ (_,_,_,x,_,_) -> x) $ maybe (error "Error! Method Not Found!") (id) $ findMethod (methods mod) x
fetchGuardByRule universe mod (RuleNameAP x) = (\ (_,x,_,_) -> x ) (findRule (rules mod) x)
fetchGuardByRule universe mod (SubmoduleNameAP inst x) = fetchGuardByRule universe nextMod x
where 
	nextMod = maybe (error "This really shouldn't happen...") (id) $ fmap (\ x -> maybe (error "Module not found!") (id) (findMod universe x)) $ findModbyInst (state mod) (ID inst)

-- | While generating submodule schedules, it is not necessary to invoke any action methods, as all action methods at this point have been subbed in 
-- | using samStmt etc.  Conflicts are handled by the transferring of implicit method conditions to the supermodule. We only need rule firings for submodules. 
-- | I expect adding the recursive element here will blow up translator runtime quite substatially.  Thankfully, this can be mitigated by merging rules that have no conflicts and the same guards.  

genRuleSchedules :: BSVPackage -> BSVModuleDec -> [String] -> [String] -> [RuleSchedule]
genRuleSchedules universe mod path meths = (schedules) ++ (recursing)
where
	tracy = "[genRuleSchedules] mod - " ++ (mName mod) ++ "\nmeths - " ++ (intercalate "\n\t\t" meths) ++ "\nrules - " ++ (intercalate "\n\t" (map (\(x,_,_,_) -> x) (rules mod))) ++ "\nmethodInvokations = " ++ (intercalate "\n\t\t\t" (map show methodInvokations))
	schedules = amalgamateRules' $ map (genRuleSchedule universe mod path meths) (rules mod)
	methodInvokations = concat $ map actionMethodsCalled schedules
	recursing = concat $ map (\ q -> genRuleSchedules universe q (path ++ ((instanceName q):[])) (filterActionMethodsByInstance (instanceName q) (methodInvokations))) (instances mod) 

filterActionMethodsByInstance :: String -> [Expression] -> [String]
filterActionMethodsByInstance inst [] = []
filterActionMethodsByInstance inst (x:xs) = if (inst == plork x) 
					then (plink x) : (filterActionMethodsByInstance inst xs)
					else (filterActionMethodsByInstance inst xs)
where
	plork (Exp_MethodCall w x y z) = lastID w
	plink (Exp_MethodCall w x y z) = x
	
amalgamateRules' :: [RuleSchedule] -> [RuleSchedule]
amalgamateRules' rs = rs'
	where
		proc = amalgamateRules rs ([], [])
		rs' = map (applyRepl'' (snd proc)) $ fst proc
	
amalgamateRules :: [RuleSchedule] -> ([RuleSchedule], [([ActionPath], ActionPath)]) -> ([RuleSchedule], [([ActionPath], ActionPath)])
amalgamateRules [] q = q
amalgamateRules (r:rs) (rs', repl) = amalgamateRules nonmergable ((rs' `snoc` merg), repl')
	where
		tracy = "[amalgamateRules] repl = " ++ (show repl)
		mergable = filter (ruleCorrespondance r) rs
		nonmergable = filter (\ q -> not (ruleCorrespondance r q)) rs
		merged = ruleMerge (r:mergable) repl
		merg = fst merged
		repl' = if length (fst (snd merged)) > 1 then (snd merged) : (repl) else repl

ruleCorrespondance :: RuleSchedule -> RuleSchedule -> Bool
ruleCorrespondance x y = and [c1,c2,c3,c5,c6,c7,c8]
	where
		tracy = "[ruleCorrespondance] rGuard x - " ++ (show (rGuard x)) ++ "\n rGuard y - " ++ (show (rGuard y)) ++
			"\n implicitConditions x - " ++ (show (implicitConditions x)) ++ "\n implicitConditions y - " ++ (show (implicitConditions y)) ++
			"\n conflictsWith x - " ++ (show (conflictsWith x)) ++ "\n conflictsWith y - " ++ (show (conflictsWith y)) ++
			"\n noConflictsWith x - " ++ (show (noConflictsWith x)) ++ "\n noConflictsWith y - " ++ (show (noConflictsWith y))
		c1 = rGuard x == rGuard y
		c2 = implicitConditions x == implicitConditions y
		c3 = conflictsWith x == conflictsWith y
		--c4 = (delete (rName y) (noConflictsWith x)) == (delete (rName x) (noConflictsWith y))
		c5 = preempts x == preempts y
		c6 = isPreemptedBy x == isPreemptedBy y
		c7 = executesAfter x == executesAfter y
		c8 = executesBefore x == executesBefore y
		
ruleMerge :: [RuleSchedule] -> [([ActionPath], ActionPath)] -> (RuleSchedule, ([ActionPath], ActionPath))
ruleMerge (r:rs) repl = (rule, ( names , nom ))
	where 
		names = nub $ (map rName (r:rs))
		nom = mergeActionPaths names
		tracy = "[ruleMerge] merge names - " ++ (show names) ++ "\nmerged name = " ++ (show nom)
		repl' = ( names , nom ) : repl
		rule = RuleSchedule { rName = nom
			, rGuard = rGuard r
			, rStatements = concat (map rStatements (r:rs))
			, writesTo = nub $ concat (map writesTo (r:rs))
			, dWireWrites = nub $ concat (map dWireWrites (r:rs))
			, dWireReads = nub $ concat (map dWireReads (r:rs))
			, fifoEnqs = nub $ concat (map fifoEnqs (r:rs))
			, fifoDeqs = nub $ concat (map fifoDeqs (r:rs))
			, fifoClears = nub $ concat (map fifoClears (r:rs))
			, fifoFirsts = nub $ concat (map fifoFirsts (r:rs))
			, actionMethodsCalled = nub $ concat (map actionMethodsCalled (r:rs))
			, implicitConditions = implicitConditions r
			, conflictsWith = conflictsWith r
			, noConflictsWith =  noConflictsWith r
			, preempts = preempts r
			, isPreemptedBy = isPreemptedBy r
			, executesAfter = executesAfter r
			, executesBefore = executesBefore r
			} 

applyRepl'' :: [([ActionPath], ActionPath)] -> RuleSchedule -> RuleSchedule
applyRepl'' reps r = RuleSchedule { rName = rName r
			, rGuard = rGuard r
			, rStatements = rStatements r
			, writesTo = writesTo r
			, dWireWrites = dWireWrites r
			, dWireReads = dWireReads r
			, fifoEnqs = fifoEnqs r
			, fifoDeqs = fifoDeqs r
			, fifoClears = fifoClears r
			, fifoFirsts = fifoFirsts r
			, actionMethodsCalled = actionMethodsCalled r
			, implicitConditions = implicitConditions r
			, conflictsWith = applyRepl (rName r) reps $ conflictsWith r
			, noConflictsWith = applyRepl (rName r) reps $ noConflictsWith r
			, preempts = applyRepl (rName r) reps $ preempts r
			, isPreemptedBy = applyRepl (rName r) reps $ isPreemptedBy r
			, executesAfter = applyRepl (rName r) reps $ executesAfter r
			, executesBefore = applyRepl (rName r) reps $ executesBefore r
			} 
			
applyRepl :: ActionPath -> [([ActionPath], ActionPath)] -> [ActionPath] -> [ActionPath]
applyRepl nom repls xs = nub $ xs'
where
	tracy = "[applyRepl] xs = " ++ (show xs) ++ "\n\n repls = " ++ (intercalate "\n\t\t" (map show repls))  ++ "\n xs' = " ++ (show xs')
	xs' = foldl (\ y x -> applyRepl' nom x y ) xs repls 

			
applyRepl' :: ActionPath -> ([ActionPath], ActionPath) -> [ActionPath] -> [ActionPath]
applyRepl' _ _ [] = []
applyRepl' nom (ys,r) (x:xs) = if x `elem` ys 
					then if r /= nom
					then r : (applyRepl' nom (ys,r) xs)
					else (applyRepl' nom (ys,r) xs)
					else x : (applyRepl' nom (ys,r) xs)
	where
		tracy = "[applyRepl'] x = " ++ (show x) ++ "\nys = " ++ (intercalate "\n" (map show (ys))) ++ "\n x `elem` ys = " ++ (show (x `elem` ys)) ++ "\n r = " ++ (show r)
		
			
mergeActionPaths :: [ActionPath] -> ActionPath 
mergeActionPaths ap = mergeAP path $ intercalate "_" lastAPs
	where
		lastAPs = map lastAP ap
		path = getAPPath (head ap)

mergeAP :: [String] -> String -> ActionPath
mergeAP [] y = (RuleNameAP y)
mergeAP (x:xs) y = (SubmoduleNameAP x (mergeAP xs y))
		
getAPPath :: ActionPath -> [String]
getAPPath (ActionNameAP x) = [] 
getAPPath (RuleNameAP x)   = []
getAPPath (MethodNameAP x) = [] 
getAPPath (SubmoduleNameAP x ap) = (x:(getAPPath ap))
		
lastAP :: ActionPath -> String
lastAP (ActionNameAP x) = x 
lastAP (RuleNameAP x)   = x
lastAP (MethodNameAP x) = x 
lastAP (SubmoduleNameAP _ ap) = lastAP ap
	
genRuleSchedule :: BSVPackage -> BSVModuleDec -> [String] -> [String] -> RuleDec -> RuleSchedule
genRuleSchedule universe mod path meths (nom, guard, stmts, rAtts) = RuleSchedule { rName = makeActionPath (path ++ nom:[])
			, rGuard = guard
			, rStatements = stmts
			, writesTo = fst sortedWrites
			, dWireWrites = snd sortedWrites
			, dWireReads = wireReads
			, fifoEnqs = enqs
			, fifoDeqs = deqs
			, fifoClears = clears
			, fifoFirsts = firsts
			, actionMethodsCalled = methCalls
			, implicitConditions = imps
			, conflictsWith = map (\ q -> makeActionPath (path ++ q:[]) ) confs
			, noConflictsWith = map (\ q -> makeActionPath (path ++ q:[]) ) nonConfs
			, preempts = (fst prempts) ++ implicitPreempts
			, isPreemptedBy = snd prempts 
			, executesAfter = priors
			, executesBefore = anteriors 
			} 
where 
	tracy = "\n\n\n[GenRuleSchedule] rule - " ++ (show (makeActionPath (path ++ nom:[]))) ++ "\n\nstatements = " ++ (intercalate "\n" (map show stmts)) 
	--"\nmeths = " ++ (show meths) ++ "\nwritesTo = " ++ (show (fst sortedWrites)) ++ "\nDWire Writes = " ++ (show (snd sortedWrites)) ++ "\ndWireReads = " ++ (show wireReads) ++ "\n" ++ "\n\nstatements = " ++ (intercalate "\n" (map show stmts)) ++ "\n\npreempts = " ++ (show prempts)
	writesTo = getWrites universe mod stmts 
	methCalls = getMethCalls universe mod False stmts 
	imps = if (check4imps rAtts) then [] else getImplicitConditions universe mod stmts
	confs = getConflicts universe mod (removeSelf nom (removeNonConflicts (attributes mod) (rules mod) nom)) writesTo (enqs, deqs)
	nonConfs = map (\ (x,_,_,_) -> x) $ filter (\ (x,_,_,_) -> not (x `elem` confs)) (removeSelf nom (rules mod))
	prempts = if finalIDPathEqWithStrings nom meths then ( (getPreempts' universe mod path confs meths) , [] ) else getPreempts universe mod path nom confs meths implicitPreempts
	-- tracy = "[getPreempts'] - meths = " ++ (show meths) ++ "\nnom = " ++ (show nom) ++ "\nresult = " ++ (show (elemWith nom meths (finalIDPathEqWithStrings) ))
	rawWrites = nub $ getWrites universe mod stmts 
	sortedWrites = sortWires mod rawWrites 
	wireReads = filterForWires mod $ (getReadsByOverStatements mod (\ x -> True) [] stmts) ++ (getReadsBy mod (\ x -> True) [] guard)
	priors = getPriors universe mod path (removeSelf nom (rules mod)) wireReads (enqs,deqs,firsts,clears)
		-- (\ x -> twace ("[T] genRuleSchedule - for rule "++(show nom)++" - \nexecutes after : " ++ (show x)) x ) $ getPriors universe mod (removeSelf nom (rules mod)) wireReads (enqs,deqs,firsts,clears)
	anteriors = nub $ getAnteriors universe mod path (snd sortedWrites) (enqs,deqs,firsts,clears)
		-- (\ x -> twace ("[T] genRuleSchedule - for rule "++(show nom)++" - \nexecutes before : " ++ (show x)) x ) $ nub $ getAnteriors universe mod (snd sortedWrites) (enqs,deqs,firsts,clears)
	implicitPreempts = (map (\ q -> makeActionPath (path ++ q:[])) confs) `intersect` anteriors 
	enqs = concat $ map (getMCalls universe mod "enq") stmts 
		--(\ x -> twace ("[genRuleSchedule] - "++ (show nom) ++ "\n[genRuleSchedule] - enqs = " ++ (show x)) x) $ concat $ map (getMCalls universe mod "enq") stmts 
	deqs = concat $ map (getMCalls universe mod "deq") stmts 
		-- (\ x -> twace ("[genRuleSchedule] - deqs = " ++ (show x)) x) $ concat $ map (getMCalls universe mod "deq") stmts 
	clears = concat $ map (getMCalls universe mod "clear") stmts 
		-- (\ x -> twace ("[genRuleSchedule] - clears = " ++ (show x)) x) $ concat $ map (getMCalls universe mod "clear") stmts 
	firsts = methCalls `getAllInvokationsOf` "first"
		-- (\ x -> twace ("[genRuleSchedule] - firsts = " ++ (show x)) x) $ methCalls `getAllInvokationsOf` "first"

filterForWires :: BSVModuleDec -> [ID_Path] -> [ID_Path]
filterForWires mod = id
		
finalIDPathEqWithStrings :: String -> [String] -> Bool
finalIDPathEqWithStrings _ [] = False 
finalIDPathEqWithStrings x (y:ys) = if x' == y' then True else finalIDPathEqWithStrings x ys
	where
		x' = lastID $ string2IDPath x
		y' = lastID $ string2IDPath y
		tracy = "[fidpeqwstr] x = " ++ (show x) ++ "\nys = " ++ (show (y:ys)) ++ "\nx' = " ++ (show x') ++ "\nresult = " ++ (show (x' == y'))
		
getAllInvokationsOf :: [Expression] -> String -> [ID_Path]
getAllInvokationsOf [] _ = []
getAllInvokationsOf ((Exp_MethodCall inst meth _ _ ):xs) s = if s == meth
					then inst : (getAllInvokationsOf xs s)
					else getAllInvokationsOf xs s
	
getMCalls :: BSVPackage -> BSVModuleDec -> String -> Statement -> [ID_Path]
getMCalls universe mod s (MethodCall inst meth exps atts) = if (meth == s) 
		then (inst:[])
		else (map (\ x -> mergeIDPaths inst x) (concat (map (getMCalls universe mod s) stmts)))
where 
	methMod = findModbyInst (state mod) inst
	meth2 = maybe (Nothing) (id) $ fmap (\ q -> findMethod (methods (maybe (error "Module not found") (id) (findMod universe q))) meth) methMod
	stmts = maybe ([]) (\ (_,_,_,_,x,_) -> x ) meth2
getMCalls universe mod s (ActionCall nom atts) = (concat (map (getMCalls universe mod s) stmts))
where 
	stmts = (\ (_,x,_) -> x) (findAction (actions mod) nom) 
getMCalls universe mod s (If g t e atts) = (getMCalls universe mod s t)++(getMCalls universe mod s e)
getMCalls universe mod s (ForLoop inits guard incs stmt atts) = (getMCalls universe mod s stmt) 
getMCalls universe mod s (Switch x cases atts) = (concat (map (\ x ->getMCalls universe mod s x) (snd (unzip cases))))
getMCalls universe mod s (StatementBlock ss) = concat $ map (getMCalls universe mod s) ss
getMCalls universe mod s (PMatchIf _ _ x y _) = (getMCalls universe mod s x) ++ (getMCalls universe mod s y)
getMCalls universe mod s (LocalDec _ x _) = (getMCalls universe mod s x)
getMCalls universe mod _ _ = [] 


getAnteriors :: BSVPackage -> BSVModuleDec -> [String] -> [ID_Path] -> ([ID_Path],[ID_Path],[ID_Path],[ID_Path]) -> [ActionPath]
getAnteriors uni mod path wires (enqs, deqs, firsts, clears) = (map (\ (z,_,_,_) -> makeActionPath (path ++ z:[]) ) (wirs ++ fifs1 ++ fifs2))
where
	wirs = filter (\ y -> not (null (isWireAnterior mod y wires))) (rules mod)
	fifs1 = filter (\ y -> not (null (isClearAnterior uni mod y clears))) (rules mod)
	fifs2 = filter (\ y -> not (null (isDeqAnterior uni mod y deqs))) (rules mod)
	
isWireAnterior :: BSVModuleDec -> RuleDec -> [ID_Path] -> [ID_Path]
isWireAnterior m (rName, gd, stmts, _) wires = ((getReadsBy m (\ x -> x `elem` wires) [] gd) ++ (getReadsByOverStatements m (\ x -> x `elem` wires) [] stmts))

isClearAnterior :: BSVPackage -> BSVModuleDec -> RuleDec -> [ID_Path] -> [ID_Path]
isClearAnterior universe mod (rName, gd, stmts, _) clears = filter (\ x -> x `elem` (enqs ++ deqs ++ firsts)) clears
	where
		enqs = concat $ map (getMCalls universe mod "enq") stmts 
		deqs = concat $ map (getMCalls universe mod "deq") stmts 
		firsts = (getMethCalls universe mod False stmts ) `getAllInvokationsOf` "first"

isDeqAnterior :: BSVPackage -> BSVModuleDec -> RuleDec -> [ID_Path] -> [ID_Path]
isDeqAnterior universe mod (rName, gd, stmts, _) deqs = filter (\ x -> x `elem` (firsts)) deqs
	where
		firsts = (getMethCalls universe mod False stmts ) `getAllInvokationsOf` "first"

getPriors :: BSVPackage -> BSVModuleDec -> [String] -> [RuleDec] -> [ID_Path] -> ([ID_Path],[ID_Path],[ID_Path],[ID_Path]) -> [ActionPath]
getPriors _ _ _ [] _ _ = []
getPriors universe mod path ((name, _, stmts, _):xs) writes (enqs,deqs,firsts,clears) = if or [writeCond, enqCond, deqCond, firstCond, clearCond]
	then ((makeActionPath (path ++ name:[])) : (getPriors universe mod path xs writes (enqs,deqs,firsts,clears))) 
	else getPriors universe mod path xs writes (enqs,deqs,firsts,clears)
where 
	methCalls = getMethCalls universe mod False stmts
	writes' = getWrites universe mod stmts 
	enqs' = concat $ map (getMCalls universe mod "enq") stmts 
		-- (\ x -> twace ( "[T] getPriors - enqs' - "++ (show name) ++" = " ++ (show x) ++ "\nenqs = " ++ (show enqs) ) x) $ concat $ map (getMCalls universe mod "enq") stmts 
	deqs' = concat $ map (getMCalls universe mod "deq") stmts 
		--(\ x -> twace ( "[T] getPriors - deqs' - "++ (show name) ++" = " ++ (show x) ++ "\ndeqs = " ++ (show deqs) ) x) $ concat $ map (getMCalls universe mod "deq") stmts 
	firsts' = methCalls `getAllInvokationsOf` "first"
		--(\ x -> twace ( "[T] getPriors - firsts' - "++ (show name) ++" = " ++ (show x) ++ "\nfirsts = " ++ (show firsts) ) x) $ methCalls `getAllInvokationsOf` "first"
	clears' = concat $ map (getMCalls universe mod "clear") stmts 
		--(\ x -> twace ( "[T] getPriors - clears' - "++ (show name) ++" = " ++ (show x) ++ "\nclears = " ++ (show clears) ) x) $ concat $ map (getMCalls universe mod "clear") stmts 
	writeCond = (not(null (intersect writes writes')))
	enqCond = False
	deqCond = (not (null (intersect deqs (firsts'))))
	firstCond = False
	clearCond = (not (null (intersect clears (enqs' ++ deqs' ++ firsts'))))
	

genTotalTree :: [RuleSchedule] -> Integer -> TotalTree
genTotalTree [] _ = TotalLeaf (Literal (LitBool True)) (Void:[])
genTotalTree (r:[]) _ = TotalLeaf (rGuard r) (rStatements r)
genTotalTree rs t = (TotalStem (rGuard sup) (rStatements sup) leftTree rightTree )
where
	tracy = "[genTotalTree] rank # = " ++ (show t) ++ "\nrule supremum = " ++ (show (rName sup)) ++ "\n\nstatements = " ++ (intercalate "\n" (map show (rStatements sup)))
	tracy2 = "[genTotalTree] node rank # = " ++ (show t)  ++ if (rGuard sup == (Literal (LitBool True))) then " * " else ""
	nullLeaf = (TotalLeaf (Literal (LitBool True)) (Void:[]))
	sortedRules = findSupremum rs
	sup = (\ (x,_,_) -> x) sortedRules
	trueBase = (\ (_,x,_) -> x) sortedRules
	falseBase = (\ (_,_,x) -> x) sortedRules    
	leftTree = (genTotalTree trueBase (t+1))
	rightTree = if (rGuard sup == (Literal (LitBool True))) then nullLeaf else (genTotalTree falseBase (t+1))

-- This operation should never be performed on a cluster type.  
makeTreesSpecific :: BSVModuleDec -> BSVstateDec -> [String] -> [String] -> TotalTree -> (Maybe Expression) -> SpecificTree
makeTreesSpecific mod (BSV_Reg n t ini) prefixes ex tree _       = tree'
	where
		tree_mid = makeSpecificTree mod n prefixes ex tree 
		tree' = simplifySpecificTree' $ tree_mid
		tracy = error $ "[makeTreesSpecific] name - " ++ (show n) ++ "\nmidtree =" ++ (show tree_mid) ++ "\ntree' = " ++ (show tree')
makeTreesSpecific mod (DWire n t ini) prefixes ex tree _         = simplifySpecificTree' $ makeSpecificTree mod n prefixes ex tree 
where
	tree_mid = makeSpecificTree mod n prefixes ex tree 
	tree' = simplifySpecificTree' $ tree_mid
	tracy = if ((show n) == "rio_ModInsIOPktGeneration.wr_RxReady_In") 
			then "[makeTreesSpecific] name - " ++ (show n) ++ "\nmidtree =" ++ (show tree_mid) ++ "\ntree' = " ++ (show tree')
			else ""
makeTreesSpecific mod (BSV_Fifo f n t) prefixes ex tree _          = simplifySpecificTree' $ makeSpecificTree mod n prefixes ex tree 
makeTreesSpecific mod (BSV_Vector (ID n) t num ini) prefixes ex tree (Just exp) = simplifySpecificTree' $ makeSpecificTree mod (ID_Vect n exp) prefixes ex tree 
makeTreesSpecific mod (BSV_Vector n t num ini) prefixes ex tree (Nothing) = error "Internal BSV2PVS Error!" 

makeCallTreesSpecific :: BSVModuleDec -> ID_Path -> String -> [String] -> [String] -> TotalTree -> SpecificTree
makeCallTreesSpecific mod fifoName methName prefixes ex tree = simplifySpecificTree' $ makeCallSpecificTree mod fifoName methName prefixes ex tree  
	-- simplifySpecificTree' $ makeCallSpecificTree mod fifoName methName prefixes ex tree 

makeCallSpecificTree :: BSVModuleDec -> ID_Path -> String -> [String] -> [String] -> TotalTree -> SpecificTree
makeCallSpecificTree mod x meth prefixes exclusions (TotalStem g ss tt ft) = (SpecStem g' (unMaybe mStmt) (makeCallSpecificTree mod x meth prefixes exclusions ft))
where
	mStmt = findCallStmt ss (actions mod) x meth
		-- (\ x -> twace ( "[T] makeCallSpecificTree - mStmt - "++ (show x) ) x) $ findCallStmt ss (actions mod) x meth
	unMaybe (Nothing) = (Right (makeCallSpecificTree mod x meth prefixes exclusions tt))
	unMaybe (Just z) = (Left (extractExpression exclusions x prefixes z))
	g' = applyRootPrefix exclusions prefixes g
makeCallSpecificTree mod x meth prefixes exclusions (TotalLeaf g ss) = (SpecLeaf g' (unMaybe mStmt) (Skip))
where
	mStmt = findCallStmt ss (actions mod) x meth 
	unMaybe (Nothing) = extractExpression exclusions x prefixes Void
	unMaybe (Just z) = extractExpression exclusions x prefixes z
	g' = applyRootPrefix exclusions prefixes g

--simplifySpecificTree' = id
simplifySpecificTree' = simplifySpecificTree

makeSpecificTree :: BSVModuleDec -> ID_Path -> [String] -> [String] -> TotalTree -> SpecificTree
makeSpecificTree mod x prefixes exclusions (TotalStem g ss tt ft) = (SpecStem g' (unMaybe mStmt) (makeSpecificTree mod x prefixes exclusions ft))
	{-if (show x == "rio_ModInsIOPktGeneration.wr_RxReady_In") 
					then twace tracy $ (SpecStem g' (unMaybe mStmt) (makeSpecificTree mod x prefixes exclusions ft))
					else (SpecStem g' (unMaybe mStmt) (makeSpecificTree mod x prefixes exclusions ft))-}
where
	mStmt = findStmt ss (actions mod) x  -- %%% 
	unMaybe (Nothing) = (Right (makeSpecificTree mod x prefixes exclusions tt))
	unMaybe (Just z) = (Left (extractExpression exclusions x prefixes z))
	g' = applyRootPrefix exclusions prefixes g
	tracy = "[makeSpecificTree] x - " ++ (show x) ++ "\nss = " ++ (show ss) ++ "\nmStmt = " ++ (show mStmt)
makeSpecificTree mod x prefixes exclusions (TotalLeaf g ss) = (SpecLeaf g' (unMaybe mStmt) (Skip))
where
	mStmt = findStmt ss (actions mod) x
	unMaybe (Nothing) = extractExpression exclusions x prefixes Void
	unMaybe (Just z) = extractExpression exclusions x prefixes z
	g' = applyRootPrefix exclusions prefixes g

simplifySpecificTree :: SpecificTree -> SpecificTree 
simplifySpecificTree (SpecStem gd (Left y) t2) = if (gd == (Literal (LitBool True)))
	then (SpecEx y)
	else if (gd == (Literal (LitBool False)))
		then (simplifySpecificTree t2)
	else (SpecStem gd (Left y) (simplifySpecificTree t2))
simplifySpecificTree (SpecStem gd (Right t1) t2) = if (gd ==  (Literal (LitBool True)))
	then (simplifySpecificTree t1)
	else if (gd == (Literal (LitBool False)))
		then (simplifySpecificTree t2)
	else if ((simplifySpecificTree t1) == (simplifySpecificTree t2)) 
	then (simplifySpecificTree t1) 
	else (SpecStem gd (Right (simplifySpecificTree t1)) (simplifySpecificTree t2))
simplifySpecificTree (SpecLeaf gd y z) = if (gd ==  (Literal (LitBool True))) 
	then (SpecEx y)
	else if (gd == (Literal (LitBool False))) 
		then (SpecEx z)
	else if (y == z) 
	then (SpecEx y) 
	else (SpecLeaf gd y z)

-- if gd == gd', the true path of the false branch will never be executed.	  
	
----------------------------------------------------------------------------------------------------------------------------------------
-- (supremum, trueSet, falseSet)
findSupremum :: [RuleSchedule] -> (RuleSchedule, [RuleSchedule], [RuleSchedule])
findSupremum [] = error $ "BSV2PVS Error!  Tried to find supremum of empty list!"
findSupremum (rs) = (supremum, (winnowRuleSchedules (rName supremum) baseSet), baseSet)
where 
	noPriors = filter (\ x -> null (executesAfter x)) rs
	noPreempts  = filter (\ x -> null (isPreemptedBy x)) rs
	supremum = arbitrateRuleSchemes noPreempts noPriors
	baseSet = killSupremumReferences (rName supremum) $ delete supremum rs
	
-- Preemptions -> Priors -> 
arbitrateRuleSchemes :: [RuleSchedule] -> [RuleSchedule] -> RuleSchedule
arbitrateRuleSchemes [] x = error $ "Scheduling BSV2PVS Error! Circular preemption scheme! Abort! Abort!\n\n" ++ (intercalate "\n\n" (map show x))
arbitrateRuleSchemes _ [] = error "Scheduling BSV2PVS Error! Circular wire accesses! Mayday! Mayday!"
arbitrateRuleSchemes noPreempts noPriors 
| length both >= 1 		=  head both
| otherwise                   = error $ "Scheduling BSV2PVS Error! Conflict Preemption scheme inverts ordering produced by wire accesses!\n\nRules with no preemptions: " ++ (intercalate "\n\t" (map show noPreempts)) ++ "\n\nRules at top of wire ordering" ++ (intercalate "\n\t" (map show noPriors))
where
	both = noPreempts `intersect` noPriors
	
--error $ "Rules with no preemptions: \n\t" ++ (intercalate "\n\t" (map show noPreempts)) ++ "\n\nRules at top of wire ordering : \n\t" ++ (intercalate "\n\t" (map show noPriors))--

killSupremumReferences :: ActionPath -> [RuleSchedule] -> [RuleSchedule]
killSupremumReferences _ [] = []
killSupremumReferences x rs = map (killSupremumReference x) rs

killSupremumReference :: ActionPath -> RuleSchedule -> RuleSchedule
killSupremumReference x r = update r
where
	update y = y { isPreemptedBy = (filter (x /=) (isPreemptedBy r))
				, executesAfter = (filter (x /=) (executesAfter r))
				, executesBefore = (filter (x /=) (executesBefore r))
				, preempts = (filter (x /=) (preempts r))
				}  

winnowRuleSchedules :: ActionPath -> [RuleSchedule] -> [RuleSchedule]
winnowRuleSchedules x rs = winnowRuleSchedules' x rs rs

winnowRuleSchedules' :: ActionPath -> [RuleSchedule] -> [RuleSchedule] -> [RuleSchedule]
winnowRuleSchedules' _ [] ts = ts
winnowRuleSchedules' x (r:rs) ts = if (x `elem` (conflictsWith r))
then winnowRuleSchedules' x rs (map (killSupremumReference (rName r)) (delete r ts)) 
else winnowRuleSchedules' x rs ts 

getReadsByOverStatements :: BSVModuleDec -> (ID_Path -> Bool) -> [String] -> [Statement] -> [ID_Path]
getReadsByOverStatements m p i [] = []
getReadsByOverStatements m p i ((Write _ exp _):xs) = (getReadsBy m p i exp) ++ (getReadsByOverStatements m p i xs)
getReadsByOverStatements m p i ((ActionCall x y):xs) = error "I need to go home and rethink my life..."
getReadsByOverStatements m p i ((Return exp _):xs) = (getReadsBy m p i exp) ++ (getReadsByOverStatements m p i xs)
getReadsByOverStatements m p i ((If exp stmt1 stmt2 _):xs) = (getReadsBy m p i exp) ++ (getReadsByOverStatements m p i (stmt1:(stmt2:[]))) ++ (getReadsByOverStatements m p i xs) 
getReadsByOverStatements m p i ((ForLoop _ _ _ stmt _):xs) = (getReadsByOverStatements m p i (stmt:[])) ++ (getReadsByOverStatements m p i xs)
getReadsByOverStatements m p i ((Switch _ cases _):xs) = (getReadsByOverStatements m p i (map snd cases)) ++ (getReadsByOverStatements m p i xs)
-- getReadsByOverStatements p ((Let nom exp _):xs) = (getReadsBy p exp) ++  (getReadsByOverStatements p xs)
getReadsByOverStatements m p i ((Void):xs) = getReadsByOverStatements m p i xs   
getReadsByOverStatements m p i ((PMatchIf i' _ x y _):xs) = (getReadsBy m p i (Identifier i')) ++ (getReadsByOverStatements m p i (x:y:xs))
getReadsByOverStatements m p i ((LocalDec x stmt z):xs) = (getReadsByOverStatements m p i (stmt:[])) ++ (getReadsByOverStatements m p i xs)
getReadsByOverStatements m p i ((StatementBlock ys):xs) = (getReadsByOverStatements m p i ys) ++ (getReadsByOverStatements m p i xs)
getReadsByOverStatements m p i ((MethodCall inst meth exps _):xs) = (concat (map (getReadsBy m p i) exps)) ++ (getReadsByOverStatements m p i xs)


-- fst -> Registers, snd -> DWires    
sortWires :: BSVModuleDec -> [ID_Path] -> ([ID_Path], [ID_Path])
sortWires mod ids = (regs, wires)
where 
	regs = [x | x <- ids, (not (isDWire mod x))] 
	wires = [x | x <- ids, isDWire mod x] 
	
isDWire :: BSVModuleDec -> ID_Path -> Bool 
isDWire mod (ID x) = isDWire' states (ID x)
	where
		states = state mod
-- isDWire mod (ID_Submod_Struct x (ID y)) = isDWire inst (ID y)
--     where
--         inst = maybe (mod) (id) $ findSubModbyInst (instances mod) x 
isDWire mod (ID_Submod_Struct x y) = inst
	where
		inst = maybe (isDWire' (state mod) y) (\ q -> isDWire q y) $ findSubModbyInst (mod : (instances mod)) x 

isDWire' :: [BSVstateDec] -> ID_Path -> Bool
isDWire' [] _ = False
isDWire' ((DWire i _ _):xs) i' = if (i `pathEq` i') then True else isDWire' xs i'
isDWire' (_:xs) n = isDWire' xs n

getPreempts' :: BSVPackage -> BSVModuleDec -> [String] -> [String] -> [String] -> [ActionPath]
getPreempts' _ _ _ [] _ = []
getPreempts' universe mod path (c:cs) meths = if finalIDPathEqWithStrings c meths then (getPreempts' universe mod path cs meths) else ((makeActionPath (path ++ c:[])) : (getPreempts' universe mod path cs meths))

getMethCalls :: BSVPackage -> BSVModuleDec -> Bool -> [Statement] -> [Expression]
getMethCalls universe mod v [] = []
getMethCalls universe mod v ((Write _ exp _):xs) = (getMethCallsExp universe mod v exp) ++ (getMethCalls universe mod v xs)
getMethCalls universe mod v ((MethodCall inst meth exps _):xs) = (Exp_MethodCall inst meth exps (Just [])) : (concat (map (getMethCallsExp universe mod v) exps))
getMethCalls universe mod v ((ActionCall aName _):xs) = (getMethCalls universe mod v stmts) ++ (getMethCalls universe mod v xs)
where 
	stmts = (\ (_,x,_) -> x) (findAction (actions mod) aName)
getMethCalls universe mod v ((Return exp _):xs) = (getMethCallsExp universe mod v exp) ++ (getMethCalls universe mod v xs)
getMethCalls universe mod v ((If _ stmt1 stmt2 _):xs) = (getMethCalls universe mod v (stmt1:(stmt2:[]))) ++ (getMethCalls universe mod v xs)
getMethCalls universe mod v ((ForLoop _ _ _ stmt _):xs) = (getMethCalls universe mod v (stmt:[])) ++ (getMethCalls universe mod v xs)
getMethCalls universe mod v ((Switch _ cases _):xs) = (getMethCalls universe mod v stmts) ++ (getMethCalls universe mod v xs)
where 
	stmts = snd $ unzip cases 
-- getMethCalls universe mod ((Let nom exp _):xs) = (getMethCallsExp universe mod exp) ++  (getMethCalls universe mod xs)
getMethCalls universe mod v ((Void):xs) = getMethCalls universe mod v xs
getMethCalls universe mod v ((PMatchIf _ _ x y _):xs) = getMethCalls universe mod v (x:y:xs)
getMethCalls universe mod v ((StatementBlock stmts):xs) = (getMethCalls universe mod v stmts) ++ (getMethCalls universe mod v xs) 
getMethCalls universe mod v ((LocalDec _ stmt _):xs) = (getMethCalls universe mod v (stmt:[])) ++ (getMethCalls universe mod v xs) 

getMethCallsExp :: BSVPackage -> BSVModuleDec -> Bool -> Expression -> [Expression]
getMethCallsExp u m v (Negative x) 	        = (getMethCallsExp u m v x)
getMethCallsExp u m v (Not x)                    = (getMethCallsExp u m v x)
getMethCallsExp u m v (Equals x y)       	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (NotEquals x y)            = (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (GreaterEquals x y)        = (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (LessEquals x y)   	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (Greater x y)      	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (Less x y)  		= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (And x y)  		= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (Or x y)	                = (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (BitwiseAND x y)   	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (BitwiseOR x y)    	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (BitwiseXOR x y)    	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (BitConcat xs)		= concat $ map (getMethCallsExp u m v) xs
getMethCallsExp u m v (BitSelect x y)		= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (BitSelectRange x y z)	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y) ++ (getMethCallsExp u m v z)
getMethCallsExp u m v (LShift x y)       	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (RShift x y)       	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (Multiply x y)     	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (Divide x y)       	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (Modulo x y)       	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (Add x y)  		= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (Subtract x y)      	= (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (Literal x)  		= []
getMethCallsExp u m v (Identifier x)     	= []
getMethCallsExp u m v (ValueMethodCall w x y z) = []
getMethCallsExp u m v (Exp_If x y z) = (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y) ++ (getMethCallsExp u m v z)
getMethCallsExp u m v (RPFlag x) = (getMethCallsExp u m v x)
getMethCallsExp u m v (Tagged _ (Valid x))         = (getMethCallsExp u m v x)
getMethCallsExp u m v (Tagged _ Invalid)           = []
getMethCallsExp u m v (FromMaybe i x) = (getMethCallsExp u m v x)
getMethCallsExp u m v (MaybeIf i j x y)          = (getMethCallsExp u m v x) ++ (getMethCallsExp u m v y)
getMethCallsExp u m v (Exp_MethodCall x "enq" z q)     = (concat (map (\ x -> getMethCallsExp u m v x) z))
getMethCallsExp u m v (Exp_MethodCall x "deq" z q)     = (concat (map (\ x -> getMethCallsExp u m v x) z))
getMethCallsExp u m v (Exp_MethodCall x "clear" z q)     = (concat (map (\ x -> getMethCallsExp u m v x) z))
getMethCallsExp u m v (Exp_MethodCall x "first" z q)     = (concat (map (\ x -> getMethCallsExp u m v x) z))
getMethCallsExp u m v (Exp_MethodCall x y z q)     = (if not v
					then (if (isActionMeth ) 
					then ((Exp_MethodCall x y z q):[]) 
					else []) 
					else (Exp_MethodCall x y z q):[])
					++ (concat (map (\ x -> getMethCallsExp u m v x) z))
where
	sourceMod = maybe (maybe (error errorMsg) (id) $ ultimateModuleFinder u m (tailID (mergeIDPaths x (ID y)))) (id) $ ultimateModuleFinder u m (mergeIDPaths x (ID y))
	errorMsg = "Module Not Found!\n\npath = " ++ (show x) ++ "\nmethod = " ++ (show y) ++ "\nModule found by instance names :" ++ (show (ultimateModuleFinder u m (mergeIDPaths x (ID y))))
	errorMsg2 = "Method not found!\n\npath = " ++ (show x) ++ "\nmethod = " ++ (show y) ++ "\nModule found by instance names :" ++ (show (ultimateModuleFinder u m (mergeIDPaths x (ID y)))) ++ "" 
	meth = maybe (error errorMsg) (id) $ findMethod (methods sourceMod) y
	isActionMeth = isAction meth  
	isAction (_, Action, _,_,_,_) = True
	isAction (_, ActionValue _, _,_,_,_) = True
	isAction (_, Value _, _,_,_,_) = False
getMethCallsExp u m v (Exp_FunctionCall n xs) = concat $ map (getMethCallsExp u m v) xs
getMethCallsExp u m v (StructCluster t sts) = concat $ map (getMethCallsExp u m v) $ map snd sts
getMethCallsExp u m v (Binding lvars exp) = (getMethCallsExp u m v exp) ++ (concat (map (getMethCallsLVar u m v) lvars))
getMethCallsExp u m v (Skip) = []
getMethCallsExp u m v (CasesOf exp cases) = ((getMethCallsExp u m v exp) ++ (concat (map (getMethCallsCases u m v) cases)))
getMethCallsExp u m v (IsValid mtag) = borp mtag
	where
		borp (Valid exp) = (getMethCallsExp u m v exp)
		borp (Invalid) = []
		borp (MaybeContainer exp) = (getMethCallsExp u m v exp)
getMethCallsExp u m v (MaybeValue x) = (getMethCallsExp u m v x)
getMethCallsExp u m v (FieldAccess exp i)  = (getMethCallsExp u m v exp)
getMethCallsExp u m v x = error $ show $ x

getMethCallsCases :: BSVPackage -> BSVModuleDec -> Bool -> ExpCase -> [Expression]
getMethCallsCases u m v (l, exp) = (getMethCallsExp u m v exp)

getMethCallsLVar :: BSVPackage -> BSVModuleDec -> Bool -> LocalVar -> [Expression]
getMethCallsLVar u m v (_,_,exp) = getMethCallsExp u m v exp

tailID :: ID_Path -> ID_Path
tailID (ID x) = error "This function is being used improperly"
tailID (ID_Submod_Struct x y) = y

getPreempts :: BSVPackage -> BSVModuleDec -> [String] -> String -> [String] -> [String] -> [ActionPath] -> ([ActionPath],[ActionPath])
getPreempts universe mod path rName confs meths implicits = ( (nub ((fst sortedUrgs) ++ apPrempting)), (nub ((snd sortedUrgs) ++ apPrempted)))  
-- twace ("[T] - getPreempts - mod " ++ (show mod)) $
	{- if (addressesAllConflicts' confs' premptingRaw premptedbyRaw) 
	then ( (nub ((fst sortedUrgs) ++ apPrempting))
		, (nub ((snd sortedUrgs) ++ apPrempted))
		) 
		else error  $ "BSV2PVS Error! Unresolved rule conflicts for rule "++ (show rName) ++", in module \""++ (mName mod) ++ "\".  Please add either \"preempt\" or \"descending_urgency\" attributes to your BSV Source!\n\n" ++  "rName : " ++ (show rName) ++ "\nConflicts: "  ++ (show confs') ++ "\nPreemptedBy: " ++ (show preemptedBy) ++ "\nsortedUrgs: " ++ (show sortedUrgs') -- -}
where 
	atts = attributes mod 
	preempting = (findPreemptingList atts rName) ++ (map removeActionPath implicits)
	preemptedBy = findPreemptedByList atts rName ++ meths
	confs' = filter (\ x -> not (x `elem` preempting)) confs
	desUrgs = findDescendingUrgency atts rName
	sortedUrgs = encapsulateUrgs universe mod path $ sortUrgency desUrgs confs rName
	sortedUrgs' = encapsulateUrgs' universe mod $ sortUrgency desUrgs confs rName 
	premptingRaw = (nub ((fst sortedUrgs') ++ preempting))
	premptedbyRaw = (nub ((snd sortedUrgs') ++ preemptedBy))
	apPrempting = map (\ q -> makeActionPath (path ++ (q:[]))) premptingRaw
	apPrempted = map (\ q -> makeActionPath (path ++ (q:[]))) premptedbyRaw

makeActionPath :: [String] -> ActionPath
makeActionPath (x:[]) = (RuleNameAP x)
makeActionPath (x:xs) = (SubmoduleNameAP x (makeActionPath xs))
	
makeActionPath' :: BSVPackage -> BSVModuleDec -> String -> ActionPath
makeActionPath' universe mod x
| isAction mod x = (ActionNameAP x) 
| isRule mod x = (RuleNameAP x)
| isMethod mod x = (MethodNameAP x)
| isInSubmod universe mod x = (SubmoduleNameAP (mName nextMod) (makeActionPath' universe nextMod x))
| isSubMethod mod (string2IDPath x) = MethodNameAP $ lastID $ string2IDPath x
| otherwise = error $ "BSV2PVS Error! I just tried to create an action path for something that doesn't exist!\n\n" ++ (show x) ++ "\n\n" ++ (show mod)
where 
	nextMod = isInWhichSubmod universe mod x

string2IDPath :: String -> ID_Path 
string2IDPath xs = strings2idpath $ splitOn "`" xs

strings2idpath :: [String] -> ID_Path 
strings2idpath (x:[]) = (ID x)
strings2idpath (x:xs) = (ID_Submod_Struct x (strings2idpath xs))

isSubMethod :: BSVModuleDec -> ID_Path -> Bool
isSubMethod mod (ID_Submod_Struct x y) = isSubMethod mod y
isSubMethod mod (ID y) = isMethod mod y
	
removeActionPath :: ActionPath -> String 
removeActionPath (ActionNameAP x) = x
removeActionPath (RuleNameAP x) = x
removeActionPath (MethodNameAP x) = x
removeActionPath (SubmoduleNameAP x y) = x ++ "." ++ (removeActionPath y)

isInWhichSubmod :: BSVPackage -> BSVModuleDec -> String -> BSVModuleDec 
isInWhichSubmod universe mod x = maybe (error "Error! Module not found!") (id) $ findMod universe thisMod
where 
	modList = extractSubmods (state mod)
	mappedMods = zip (modList) (map (isInSubmod universe mod) modList)
	thisMod = fst $ head $ filter (\(_,y) -> y) mappedMods

isInSubmod :: BSVPackage -> BSVModuleDec -> String -> Bool 
isInSubmod universe mod x = foldl (\ x y -> x || y) (False) (map inThere mods)
where 
	submods = extractSubmods (state mod) 
	mods = catMaybes $ map (findMod universe) submods
	inThere q = (isAction q x) || (isRule q x) || (isMethod q x) || (isInSubmod universe q x)

extractSubmods :: [BSVstateDec] -> [String]
extractSubmods [] = []
extractSubmods ((BSV_SubModuleDec inter nom inst):xs) = nom : (extractSubmods xs)
extractSubmods (x:xs) = extractSubmods xs

isAction :: BSVModuleDec -> String -> Bool
isAction mod x = x `elem` (map (\ (x,_,_) -> x) (actions mod))

isRule :: BSVModuleDec -> String -> Bool 
isRule mod x = x `elem` (map (\ (x,_,_,_) -> x) (rules mod))

isMethod :: BSVModuleDec -> String -> Bool
isMethod mod x = x `elem` (map (\ (x,_,_,_,_,_) -> x) (methods mod))

addressesAllConflicts :: [String] -> ([String],[String]) -> Bool
addressesAllConflicts [] _ = True
addressesAllConflicts (x:xs) (ys, zs) = if ((x `elem` ys) || (x `elem` zs)) then (addressesAllConflicts xs (ys, zs)) else False

addressesAllConflicts' :: [String] -> [String] -> [String] -> Bool
addressesAllConflicts' [] _ _ = True
addressesAllConflicts' (x:xs) ys zs = if ((x `elem` ys) || (x `elem` zs)) then (addressesAllConflicts' xs ys zs) else False

encapsulateUrgs :: BSVPackage -> BSVModuleDec -> [String] -> [([String],[String])] -> ([ActionPath],[ActionPath])
encapsulateUrgs universe mod path x = ( map (\ q -> makeActionPath (path ++ (q:[]))) (nub ((concat ( map (fst) x))))
				, map (\ q -> makeActionPath (path ++ q:[])) (nub ((concat ( map (snd) x))))
				)

encapsulateUrgs' :: BSVPackage -> BSVModuleDec -> [([String],[String])] -> ([String],[String])
encapsulateUrgs' universe mod x = ( nub ((concat ( map (fst) x)))
				, nub ((concat ( map (snd) x)))
				)

findDescendingUrgency :: [ModuleAttribute] -> String -> [ModuleAttribute]
findDescendingUrgency [] _ = []
findDescendingUrgency ((Descending_Urgency ys):xs) rName = if (rName `elem` ys) 
	then ((Descending_Urgency ys): (findDescendingUrgency xs rName))
	else (findDescendingUrgency xs rName)
findDescendingUrgency (x:xs) rName = findDescendingUrgency xs rName

sortUrgency :: [ModuleAttribute] -> [String] -> String -> [([String],[String])]
sortUrgency [] _ _ = []
sortUrgency ((Descending_Urgency (ys)):xs) confs rName = (pres, posts) : (sortUrgency xs confs rName)
where 
	posts  = filter (\z -> (z `elem` confs)) (takeWhile (\ x -> not (x == rName)) ys)
	pres = filter (\z -> (z `elem` confs)) (if not (null boink) then tail boink else [])
	boink = dropWhile (\x -> not (x == rName)) ys
sortUrgency (x:xs) confs rName = sortUrgency xs confs rName

findPreemptingList :: [ModuleAttribute] -> String -> [String]
findPreemptingList [] _ = []
findPreemptingList ((Mod_Preempts ys):xs) rName = if (rName `elem` ys) && (not $ null boink)
	then tail boink
	else findPreemptingList xs rName
where
	boink = (dropWhile (\ x -> not (x == rName)) ys)
findPreemptingList (x:xs) rName = findPreemptingList xs rName

findPreemptedByList :: [ModuleAttribute] -> String -> [String]
findPreemptedByList [] _ = []
findPreemptedByList ((Mod_Preempts ys):xs) rName = if (rName `elem` ys) 
	then takeWhile (\ x -> not (x == rName)) ys 
	else findPreemptedByList xs rName
findPreemptedByList (x:xs) rName = findPreemptedByList xs rName

removeNonConflicts :: [ModuleAttribute] -> [RuleDec] -> String -> [RuleDec]
removeNonConflicts [] rules rName = rules 
removeNonConflicts ((Mod_Conflict_Free ys):xs) rules rName = if (rName `elem` ys)
	then removeNonConflicts xs remainder rName
	else removeNonConflicts xs rules rName
where 
	remainder = filter (\ (x,_,_,_) -> not (x `elem` ys)) rules 
removeNonConflicts ((Mod_Mutually_Exclusive ys):xs) rules rName = if (rName `elem` ys)
	then removeNonConflicts xs remainder rName
	else removeNonConflicts xs rules rName
where 
	remainder = filter (\ (x,_,_,_) -> not (x `elem` ys)) rules 
removeNonConflicts (_:xs) rules rName = removeNonConflicts xs rules rName

getConflicts :: BSVPackage -> BSVModuleDec -> [RuleDec] -> [ID_Path] -> ([ID_Path], [ID_Path]) -> [String]
getConflicts _ _ [] _ _ = []
getConflicts universe mod ((name, _, stmts, _):xs) writes (fifoEnqs, fifoDeqs) = if or [writeCond, enqCond, deqCond]
		then (name:(getConflicts universe mod xs writes (fifoEnqs, fifoDeqs))) 
		else getConflicts universe mod xs writes (fifoEnqs, fifoDeqs)
where 
	writes' = getWrites universe mod stmts 
	fifoEnqs' = concat $ map (getMCalls universe mod "enq") stmts 
	fifoDeqs' = concat $ map (getMCalls universe mod "deq") stmts 
	writeCond = (not(null (intersect writes writes')))
	enqCond = (not(null (intersect fifoEnqs fifoEnqs')))
	deqCond = (not(null (intersect fifoDeqs fifoDeqs')))
	
getNonConflicts :: BSVPackage -> BSVModuleDec -> [RuleDec] -> [ID_Path] -> ([ID_Path], [ID_Path]) -> [String]
getNonConflicts _ _ [] _ _ = []
getNonConflicts universe mod ((name, _, stmts, _):xs) writes (fifoEnqs, fifoDeqs) = if (null (writes `intersect` writes')) 
	then (name:(getConflicts universe mod xs writes (fifoEnqs, fifoDeqs))) 
	else getConflicts universe mod xs writes (fifoEnqs, fifoDeqs)
where 
	writes' = getWrites universe mod stmts 

getImplicitConditions :: BSVPackage -> BSVModuleDec -> [Statement] -> [Expression]
getImplicitConditions _ _ [] = []
getImplicitConditions universe mod ((MethodCall inst meth exps atts):xs) = if meth == "enq" 
					then ((Exp_MethodCall inst "notFull" [] (Just [])) : recurse)
					else if meth == "deq"
					then ((Exp_MethodCall inst "notEmpty" [] (Just [])) : recurse)
					else if meth == "first"
					then ((Exp_MethodCall inst "notEmpty" [] (Just [])) : recurse)
					else (guard:[]) ++ val ++ recurse
where
	recurse = (getImplicitConditions universe mod xs)
	methMod = findModbyInst (state mod) inst
	mod2 = maybe (error "Module not found!") (id) $ fmap (findMod universe) methMod
	meth2 = maybe (Nothing) (id) $ fmap (\ x -> (findMethod (methods x)) meth) mod2
	guard = maybe (Literal (LitBool True)) (\ (_,_,_,x,_,_) -> x ) meth2
	stmts = maybe [] (\ (_,_,_,_,x,_) -> x ) meth2
	val = maybe [] (\ x -> getImplicitConditions universe x stmts) mod2
getImplicitConditions universe mod (x:xs) = getImplicitConditions universe mod xs 

getWrites :: BSVPackage -> BSVModuleDec -> [Statement] -> [ID_Path]
getWrites _ _ [] = [] 
getWrites universe mod ((Write x y atts):xs) = x:(getWrites universe mod xs)
getWrites universe mod ((MethodCall inst meth exps atts):xs) = if (null stmts)
then (getWrites universe mod xs)
else (getWrites universe mod' stmts) ++ (getWrites universe mod xs)
where 
	methMod = findModbyInst (state mod) inst
	stmts = maybe ([]) (\ (_,_,_,_,x,_) -> x ) (meth methMod)
	meth (Nothing) = Nothing
	meth (Just mm) = findMethod ( methods (maybe (error "Module not found!") (id) (findMod universe mm)) ) mm 
	mod' = maybe (error "Module not found!") (id) $ findMod universe $ maybe (error "This should never happen") (id) methMod
getWrites universe mod ((ActionCall nom atts):xs) = (getWrites universe mod stmts) ++ (getWrites universe mod xs)
where 
	stmts = (\ (_,x,_) -> x) (findAction (actions mod) nom) 
getWrites universe mod ((Return x atts):xs) = getWrites universe mod xs
getWrites universe mod ((If g t e atts):xs) = (getWrites universe mod (t:[]))++(getWrites universe mod (e:[]))++(getWrites universe mod xs)
getWrites universe mod ((ForLoop inits guard incs stmt atts):xs) = (getWrites universe mod (stmt:[])) ++ (getWrites universe mod xs)
getWrites universe mod ((Switch x cases atts):xs) = (concat (map (\ x ->getWrites universe mod (x:[])) (snd (unzip cases)))) ++ (getWrites universe mod xs)
--getWrites universe mod ((Let nom exp atts):xs) = getWrites universe mod xs 
getWrites universe mod ((StatementBlock ss):xs) = (\ x -> (x ss) ++ (x xs)) (getWrites universe mod)
getWrites universe mod ((Void):xs) = getWrites universe mod xs 
getWrites universe mod ((PMatchIf _ _ x y _):xs) = (getWrites universe mod (x:[])) ++ (getWrites universe mod (y:[])) ++ getWrites universe mod xs 
getWrites universe mod ((LocalDec _ x _) : xs) = (getWrites universe mod (x:[])) ++ getWrites universe mod xs 

check4imps :: [RuleAttribute] -> Bool
check4imps [] = False 
check4imps ((No_Implicit_Conditions):xs) = True
check4imps ((_):xs) = check4imps xs 

showIDexpr :: Expression -> String
showIDexpr (Identifier x) = showIDPath x

prependIDPath :: String -> ID_Path -> ID_Path
prependIDPath x y = (ID_Submod_Struct x y)

mergeIDPaths :: ID_Path -> ID_Path -> ID_Path
mergeIDPaths (ID_Submod_Struct m p) y  = (ID_Submod_Struct m (mergeIDPaths p y))
mergeIDPaths (ID x) y = (ID_Submod_Struct x y)
--mergeIDPaths (ID_Vect x n) y = 


showIDPath :: ID_Path -> String
showIDPath (ID_Submod_Struct m p) = m ++ "`" ++ (showIDPath p)
showIDPath (ID x) = x
showIDPath (ID_Vect x n) = x ++ "[" ++ (show n) ++ "]"


b2pStateDecs :: BSVPackage -> [PVSstateDec]
b2pStateDecs universe = map b2pStateDec mods
where 
	mods = bsv_modules universe 
	b2pStateDec x = ((mName x),(concat (map b2pState (state x))))
		-- (\ x' -> twace ("[T] b2pStateDecs " ++ (show x')) x') $ ((mName x),(concat (map b2pState (state x))))

b2pState :: BSVstateDec -> [PVSstate]
b2pState (BSV_Reg (ID n) (BSV_Custom t) i) = if (i == (Literal LitStructConstructor)) 
	then (PVS_SubModuleDec t [] n):[]
	else (PVS_Reg (ID n) (b2pType (BSV_Custom t)) i):[]
b2pState (BSV_Reg n t i) = (PVS_Reg n (b2pType t) i):[]
b2pState (BSV_Fifo f n t) = (PVS_Fifo f n (b2pType t)):[]
b2pState (BSV_Vector n t s i) = (PVS_Vector n (b2pType t) s i):[]
b2pState (BSV_SubModuleDec i1 n i2) = (PVS_SubModuleDec i1 n i2):[]
b2pState (DWire n t i) = (PVS_DWire n (b2pType t) i):[]

-- | converts a BSV constant declaration into a pvs package declaration at the abstract syntax level
b2pConstDecl :: BSVConstantDec -> PVSConstantDec  
b2pConstDecl (nom, typ, lit) = (nom, (b2pType typ), lit)

-- | converts a BSV type definition into a pvs type definition at the abstract syntax level
b2pTypeDef :: BSVTypeDef -> PVSTypeDef
b2pTypeDef (BSV_Synonym nom typ) = (PVS_Synonym nom (b2pType typ))
b2pTypeDef (BSV_Enumeration nom enums) = (PVS_Enumeration nom enums)
b2pTypeDef (BSV_Struct nom fields) = (PVS_Struct nom (map (\ (x,y)-> (x, (b2pType y))) fields))

-- b2pVectType :: BSVstateType -> PVSstate
-- b2pVectType (BST_Reg typ) = (PVS_Reg (ID "") (b2pType typ) (Literal (LitInt 0)))



-- | Converts a lexer-bound PVS type to a lexer-bound BSV type.  PVS interprets Bit types as Int. 
b2pType (BSV_Bool) = (PVS_Bool)
b2pType (BSV_Bit n) = (PVS_Bit n)
b2pType (BSV_Int n) = (PVS_Int n)
b2pType (BSV_UInt n) = (PVS_UInt n)
b2pType (BSV_Real) = (PVS_Real)
b2pType (BSV_Custom n) = (PVS_Custom n)
b2pType (BSV_Maybe x) = (PVS_Maybe (b2pType x))

-- | Takes a list of omnibuses generated by parsing individual BSV files and groups them together into a single omnibus.
omniFlatten :: Maybe String -> [BSVPackage] -> BSVPackage
omniFlatten (Just topMod) omnibuss = recordifyBSV (name, imps, incl, (expandSubInterfaces ints ints), const, typs, mods, insts, funcs, macros, hexes)
where 
	omnibuses = map preprocMidModInts omnibuss
	name = getTopPackageName' topMod omnibuses
	imps = concat (map imports omnibuses)
	ints = concat (map interfaces omnibuses)
	const = concat (map bsv_constants omnibuses)
	typs = concat (map bsv_typedefs omnibuses)
	insts = concat (map bsv_instDefs omnibuses)
	mods = nub $ concat (map bsv_modules omnibuses)
	funcs = concat (map bsv_functions omnibuses) 
	incl = concat (map including omnibuses) 
	macros = concat (map bsv_macros omnibuses)
	hexes = concat $ map hexFiles omnibuses
omniFlatten Nothing omnibuses = recordifyBSV (name, imps, incl, (expandSubInterfaces ints ints), const, typs, mods, insts, funcs, macros, hexes)
where 
	name = bsv_packageName $ head $ reverse omnibuses 
	imps = concat (map imports omnibuses)
	ints = concat (map interfaces omnibuses)
	const = concat (map bsv_constants omnibuses)
	typs = concat (map bsv_typedefs omnibuses)
	insts = concat (map bsv_instDefs omnibuses)
	mods = nub $ concat (map bsv_modules omnibuses)
	funcs = concat (map bsv_functions omnibuses) 
	incl = concat (map including omnibuses) 
	macros = concat (map bsv_macros omnibuses)
	hexes = concat $ map hexFiles omnibuses

preprocMidModInts :: BSVPackage -> BSVPackage
preprocMidModInts x = update x
where
	update z = z { interfaces = newInterfaces, bsv_modules = newMods }
	newInterfaces = (interfaces x) ++ (concat (map genInterfaces (bsv_modules x)))
	newMods = map purgeMidModDecs $ bsv_modules x
	
purgeMidModDecs :: BSVModuleDec -> BSVModuleDec 
purgeMidModDecs x = update x
where
	update z = z {methods = newMeths}
	newMeths = (methods x) ++ (concat (map (\ (_,_,q) -> q) (interfaceDecs x)))
	
genInterfaces :: BSVModuleDec -> [InterfaceDec]
genInterfaces x = map genInterfaces' (interfaceDecs x)

genInterfaces' :: MidModInterfaceDec -> InterfaceDec
genInterfaces' (x, y, ms) = (x, (map mBody2MDec ms), [], [])

mBody2MDec :: MethodBody -> MethodDec
mBody2MDec (nom, typ, args, _, _, _) = (nom, typ, (utArg2Arg args), [])

utArg2Arg :: UTArgs -> [Argument]
utArg2Arg [] = []
utArg2Arg ((n, Nothing):xs) = error "Error! Untyped argument used in mid-module interface declaration!"    
utArg2Arg ((n, Just x):xs) = (n,x,[]) : (utArg2Arg xs)
	
expandSubInterfaces :: [InterfaceDec] -> [InterfaceDec] -> [InterfaceDec]
expandSubInterfaces [] _ = []
expandSubInterfaces ((nom, mDecs, iDecs, atts):is) lib = (nom, (mDecs ++ (concat newMDecs)), iDecs, atts) : (expandSubInterfaces is lib)
where
	newMDecs = map (getMDecs lib lib) (map fst iDecs)
	
getMDecs :: [InterfaceDec] -> [InterfaceDec] -> String -> [MethodDec]
getMDecs _ [] _ = error $ "BSV2PVS Error! requested sub-interface is never declared! Check your spelling please!"
getMDecs lib ((n, mDecs, iDecs, _):xs) n' = if (n' == n)
then concat (mDecs : ( map (getMDecs lib lib) (map fst iDecs) ))
else getMDecs lib xs n'
	
getTopPackageName' :: String -> [BSVPackage] -> String 
getTopPackageName' nom [] = error $ "BSV2PVS Error! Corresponding package not found for module named :" ++ nom
getTopPackageName' nom (x:xs) = if (isJust (safeGetMod nom (bsv_modules x))) then (bsv_packageName x) else (getTopPackageName' nom xs) --error $ (show nom) ++ "\n\n\n" ++ (show x) ++ "\n\n\n" ++ (show xs)


-- | Converts a list of tuples containing the omnibus information to a permutated declaration record containing the same information.
recordifyBSV :: (PackageName, [PackageName], [String], [InterfaceDec], [BSVConstantDec], [BSVTypeDef], [BSVModuleDec], [BSVInstDef], [BSVFunction], [BSVMacro], [HexFile]) -> BSVPackage
recordifyBSV (nom, imp, incl, inter, str, typ, mods, insts, funcs, macros, hexes) = BSVPackage 
				{ bsv_packageName = nom
				, imports = imp
				, interfaces = inter
				, bsv_constants = str
				, bsv_typedefs = typ
				, bsv_modules = mods
				, bsv_instDefs = insts
				, bsv_functions = funcs
				, including = incl
				, bsv_macros = macros 
				, hexFiles = hexes
--				, requestedTransitions = []
				}

recordifyPVS :: (PackageName, [PVSConstantDec], [PVSTypeDef], [PVStransition], [PVSstateDec], [PVSInstDef], [PVSFunction]) -> PVSPackage
recordifyPVS (nom, str, typ, trans, state, insts, funcs) = PVSPackage 
				{ pvs_packageName = nom
				, pvs_constants = str
				, pvs_typedefs = typ
				, transitions = trans
				, pvs_state = state
				, pvs_instantiations = insts
				, pvs_functions = funcs
				}

-- Helper Functions -------------------------------------------
findMod :: BSVPackage -> String -> Maybe BSVModuleDec
findMod omnibus modRef = getMod modRef mods -- twace ("[T] findMod " ++ (show modRef) ++ " in " ++ (show (map mName mods))) $ getMod modRef mods 
where 
	mods = bsv_modules omnibus 


-- Accepts full ID_Path including Identifier, returns the module containing it.
ultimateModuleFinder :: BSVPackage -> BSVModuleDec -> ID_Path -> Maybe BSVModuleDec 
ultimateModuleFinder u m (ID x) = result 
where
	tracy = "[ultimateModuleFinder] mname = " ++ (mName m) ++ "\nmodules - " ++ (show (map mName (bsv_modules u))) ++ "\nresult = " ++ (if result == Nothing then "Failure" else "Success")
	result = getMod (mName m) (bsv_modules u)
ultimateModuleFinder u m (ID_Submod_Struct x y) = if mname == Nothing 
					then Nothing
					else ultimateModuleFinder u nextMod y
where
	tracy = "[ultimateModuleFinder] looking for : " ++ x ++ "\nIn state list : \n\t" ++ (intercalate "\n\t" (map show states)) 
	states = state m 
	mname = findModbyInst states (ID x) 
	nextMod = maybe (error "module not found") (id) $ (\ x -> getMod x (instances m)) $ maybe (error "huh?") (id) mname
	
getMod :: String -> [BSVModuleDec] -> Maybe BSVModuleDec
getMod name [] = Nothing
getMod name (x:xs) = if ((interfaceName x) == name) || ((mName x) == name) then (Just x) else getMod name xs
where
	tracy = "[getMod] name = " ++ (show name) ++ "\ninterfacName = " ++ (show (interfaceName x))

safeGetMod :: String -> [BSVModuleDec] -> Maybe BSVModuleDec
safeGetMod name [] = Nothing
safeGetMod name (x:xs) = if (interfaceName x) == name then (Just x) else safeGetMod name xs

findModbyInst :: [BSVstateDec] -> ID_Path -> Maybe String
findModbyInst [] y = Nothing
findModbyInst ((BSV_Reg _ _ _):xs) y = findModbyInst xs y
findModbyInst ((BSV_Fifo _ x _):xs) y = if x == y then Nothing else findModbyInst xs y
findModbyInst ((BSV_Vector _ _ _ _):xs) y = findModbyInst xs y
findModbyInst ((DWire _ _ _):xs) y = findModbyInst xs y
findModbyInst ((BSV_SubModuleDec inter nom y'):xs) i = if (lastID i) == y' then Just inter else findModbyInst xs i
--findModbyInst ((BSV_SubModuleDec inter nom y'):xs) (ID y) = if y == y' then Just inter else findModbyInst xs (ID y)
--findModbyInst ((BSV_SubModuleDec inter nom y'):xs) (ID_Submod_Struct y q) = if y == y' then Just inter else findModbyInst xs (ID_Submod_Struct y q)
	where
		result = (lastID i) == y'
		tracy = "[findModbyInst] i = " ++ (show i) ++ "\ny' = " ++ (show y') ++ "\nresult = " ++ (show result)

--findModbyInst (x:xs) y = error $ show x

findSubModbyInst :: [BSVModuleDec] -> String -> Maybe BSVModuleDec
findSubModbyInst [] inst = Nothing
findSubModbyInst (x:xs) inst = if (inst == (instanceName x)) then Just x else findSubModbyInst (xs ++ submods) inst
where 
	submods = instances x 
	
findSubModbyInst' :: [BSVModuleDec] -> ID_Path -> String -> Maybe (BSVModuleDec, MethodBody)
findSubModbyInst' [] i m = Nothing
findSubModbyInst' (x:xs) (ID y) m = if (y == (instanceName x)) then Just (x, meth) else findSubModbyInst' xs (ID y) m
	--trace ("1 - " ++ tracy) $ if (y == (instanceName x)) then Just (x, meth) else findSubModbyInst' xs (ID y) m
	where
		meth = maybe (error "Error! Corresponding Method not declared in pointed to submodule!") (id) $ findMethod (methods x) m
		tracy = "[findSubModbyInst'] path = " ++ (show (ID y)) ++ "\ninstanceName - " ++ (show (instanceName x)) ++ "\ninterfaceNames = " ++ (show (interfaceDecs x))
findSubModbyInst' (x:xs) (ID_Submod_Struct y (ID z)) m
| (y == instanceName x) && (midmod /= Nothing) =  Just (x, meth)
	-- trace ("2 - " ++ tracy) $ Just (x, meth)
| (y == instanceName x) && (midmod == Nothing) = findSubModbyInst' (instances x) (ID z) m
	-- trace ("3 - " ++ tracy) $ findSubModbyInst' (instances x) (ID z) m
| otherwise                                    =  findSubModbyInst' xs (ID_Submod_Struct y (ID z)) m
	-- trace ("4 - " ++ tracy) $ findSubModbyInst' xs (ID_Submod_Struct y (ID z)) m
where
	midmod = interfaceLookup 2 z (interfaceDecs x) 
	meth = maybe (error "Error! Corresponding Method not found in subinterface declaration!") (id) $ findMethod (fromJust midmod) m
	tracy = "\n[findSubModbyInst']\n path = " ++ (show (ID_Submod_Struct y (ID z))) ++ "\ninstanceName - " ++ (show (instanceName x)) ++ "\ninterfaceNames = " ++ (show (map (\ (x,y,z) -> x ++ "/" ++ y) (interfaceDecs x))) ++ "\nmidmod = " ++ (show midmod) ++ "\nmname = " ++ (show m) ++ (if (midmod /= Nothing) then ("\nmeth = " ++ (show meth)) else (""))
findSubModbyInst' (x:xs) (ID_Submod_Struct y z) m = if (y == instanceName x) then findSubModbyInst' (instances x) z m else findSubModbyInst' xs (ID_Submod_Struct y z) m  
	-- trace ("5 - " ++ tracy) $ if (y == instanceName x) then findSubModbyInst' (instances x) z m else findSubModbyInst' xs (ID_Submod_Struct y z) m
	where
		midmeths = interfaceLookup 2 y (interfaceDecs x)
		midmeth = maybe (Nothing) (\ q -> findMethod q m) midmeths
		tracy = "[findSubModbyInst'] path = " ++ (show (ID_Submod_Struct y z)) ++ "\ninstanceName - " ++ (show (instanceName x)) ++ "\ninterfaceNames = " ++ (show (interfaceDecs x))

interfaceLookup :: Int -> String -> [MidModInterfaceDec] -> Maybe [MethodBody]   
interfaceLookup 1 n xs = lookup n xs'
	where
		xs' = map (\ (x,y,z) -> (x,z)) xs
interfaceLookup 2 n xs = lookup n xs'
	where
		xs' = map (\ (x,y,z) -> (y,z)) xs
		
findMethod :: [MethodBody] -> String -> Maybe MethodBody
findMethod [] x = Nothing -- error $ "BSV2PVS Error! Method not found: " ++ x
findMethod ((u,v,w,x,y,z):ms) methName = if u == methName then Just (u,v,w,x,y,z) else findMethod ms methName 

findMethod' :: BSVModuleDec -> [MethodBody] -> ID_Path -> Maybe MethodBody
findMethod' _ [] x = Nothing -- error $ "BSV2PVS Error! Method not found: " ++ x    
findMethod' mod ((u,v,w,x,y,z):ms) (ID i) = if u == i' then Just (u,v,w,x,y,z) else findMethod' mod ms (ID i') 
	where 
		i' = if "_" `isPrefixOf` i then (tail i) else i
		showy = "[findMethod'] i = " ++ (show i) ++ "\nu = " ++ (show u)
findMethod' mod ms (ID_Submod_Struct x y) = findMethod' mod midmod y
	where
		midmod = maybe (error ("Error! No corresponding subinterface declaration for x : " ++ (show x) ++ "\ninterfaceDecs = " ++ (show (interfaceDecs mod)))) (id) $ interfaceLookup 1 x (interfaceDecs mod)



findAction :: [ActionDec] -> String -> ActionDec 
findAction [] x = error $ "BSV2PVS Error! Action not found: " ++ x
findAction ((name, statements, atts):as) actName = if name == actName then (name, statements, atts) else findAction as actName 

findRule :: [RuleDec] -> String -> RuleDec 
findRule [] x = error $ "BSV2PVS Error! Rule not found: " ++ x
findRule ((w,x,y,z):xs) name = if (w == name) then (w,x,y,z) else findRule xs name

findStmt :: [Statement] -> [ActionDec] -> ID_Path -> Maybe Statement
findStmt [] _ x = (Nothing)
findStmt ((Write n exp atts):xs) acts nom = if (n `pathEq` nom) then (Just (Write n exp atts)) else (findStmt xs acts nom) 
where
	tracy = "[findStmt] mem name = " ++ (show n) ++ "\nmatching = " ++ (show nom)
findStmt ((MethodCall submod meth exps atts):xs) acts nom = findStmt xs acts nom
findStmt ((ActionCall aName atts):xs) acts nom = (findStmt (((\(_,x,_) -> x) (findAction acts aName)) ++ xs) acts nom)
findStmt ((Return exp atts):xs) acts nom = findStmt xs acts nom
findStmt ((If g t e atts):xs) acts nom = if ((findStmt (t:[]) acts nom) == Nothing && (findStmt (e:[]) acts nom) == Nothing) 
					then findStmt xs acts nom
					else (Just (If g t e atts))
findStmt ((PMatchIf i1 i2 x y atts):xs) acts nom = if ((findStmt (x:[]) acts nom) == Nothing && (findStmt (y:[]) acts nom) == Nothing) 
					then findStmt xs acts nom
					else (Just (PMatchIf i1 i2 x y atts))
findStmt ((ForLoop inits guard incs stmt atts):xs) acts nom = if ((findStmt (stmt:[]) acts nom) == Nothing)
					then findStmt xs acts nom
					else (Just (ForLoop inits guard incs stmt atts))
findStmt ((Switch guard cases atts):xs) acts nom = if ((findStmt (map (\(_,x)-> x) cases) acts nom) == Nothing) 
					then findStmt xs acts nom
					else (Just (Switch guard (filter (\(_,x) -> (notNothing (findStmt (x:[]) acts nom))) cases) atts) )
--findStmt ((Let n exp atts):xs) acts nom = if ((ID n) == nom) then (Just (Let n exp atts)) else (findStmt xs acts nom)
findStmt ((StatementBlock ss):xs) acts nom = findStmt (ss ++ xs) acts nom 
findStmt ((Void):xs) acts nom = findStmt xs acts nom
findStmt ((LocalDec vars stmt atts):xs) acts nom = if ((findStmt (stmt:[]) acts nom) == Nothing) 
		then (findStmt xs acts nom) 
		else (Just (LocalDec vars stmt atts)) 

findCallStmt :: [Statement] -> [ActionDec] -> ID_Path -> String -> Maybe Statement
findCallStmt [] _ x _ = (Nothing)
findCallStmt ((MethodCall submod meth exps atts):xs) acts nom meth' = if (submod == nom && meth == meth') 
	-- (\ x -> twace ("[T] findCallStmt - " ++ (show x)) x) $ if (submod == (showIDPath nom) && meth == meth') 
					then (Just (MethodCall submod meth exps atts)) 
					else findCallStmt xs acts nom meth'
findCallStmt ((ActionCall aName atts):xs) acts nom meth' = (findCallStmt (((\(_,x,_) -> x) (findAction acts aName)) ++ xs) acts nom meth')
findCallStmt ((If g t e atts):xs) acts nom meth' = if ((findCallStmt (t:[]) acts nom meth') == Nothing && (findCallStmt (e:[]) acts nom meth') == Nothing) 
					then findCallStmt xs acts nom meth'
					else (Just (If g t e atts))
findCallStmt ((PMatchIf i1 i2 x y atts):xs) acts nom meth' = if ((findCallStmt (x:[]) acts nom meth') == Nothing && (findCallStmt (y:[]) acts nom meth') == Nothing) 
					then findCallStmt xs acts nom meth'
					else (Just (PMatchIf i1 i2 x y atts))
findCallStmt ((ForLoop inits guard incs stmt atts):xs) acts nom meth' = if ((findCallStmt (stmt:[]) acts nom meth') == Nothing)
					then findCallStmt xs acts nom meth'
					else (Just (ForLoop inits guard incs stmt atts))
findCallStmt ((Switch guard cases atts):xs) acts nom meth' = if ((findCallStmt (map (\(_,x)-> x) cases) acts nom meth') == Nothing) 
					then findCallStmt xs acts nom meth'
					else (Just (Switch guard (filter (\(_,x) -> (notNothing (findCallStmt (x:[]) acts nom meth'))) cases) atts) )
findCallStmt ((StatementBlock ss):xs) acts nom meth' = findCallStmt (ss ++ xs) acts nom meth'
findCallStmt ((LocalDec vars stmt atts):xs) acts nom meth' = if ((findCallStmt (stmt:[]) acts nom meth') == Nothing) 
		then (findCallStmt xs acts nom meth') 
		else (Just (LocalDec vars stmt atts)) 	    
findCallStmt (x:xs) acts nom meth' = (findCallStmt xs acts nom meth')	    

notNothing :: Maybe a -> Bool 
notNothing (Just x) = True
notNothing (Nothing) = False

p2bType :: PVSType -> BSVType
p2bType (PVS_Bool) = (BSV_Bool)
p2bType (PVS_Bit n) = (BSV_Bit n)
p2bType (PVS_Int n) = (BSV_Int n)
p2bType (PVS_UInt n) = (BSV_UInt n)
p2bType (PVS_Real) = (BSV_Real)
p2bType (PVS_Custom n) = (BSV_Custom n)

idpath2strings :: ID_Path -> [String]
idpath2strings (ID_Submod_Struct x y) = x : (idpath2strings y)
idpath2strings (ID x) = x : []

idpath2string :: ID_Path -> String
idpath2string (ID_Submod_Struct x y) = x ++ "`" ++ (idpath2string y)
idpath2string (ID x) = x

