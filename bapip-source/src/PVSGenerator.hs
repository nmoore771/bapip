{-# OPTIONS_GHC -fno-warn-tabs #-}

module PVSGenerator where

-- import Debug.Hoed.Observe
import Debug.Trace 
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe 
import LexerTypes
--import Language.Haskell.Pretty
import qualified Data.Text as T
import BSV2PVS (b2pType, getReadsBy, strings2idpath, idpath2strings, lastID, mergeIDPaths)

showPVSPackages :: [PVSPackage] -> [File]
showPVSPackages x = concat $ map (\ y -> showPVSPackage y Nothing) x

showPVSPackage :: PVSPackage -> Maybe TSPpackage -> [File]
--showPVSPackage pkg tsp = error $ show pkg
showPVSPackage pkg tsp = [ ( ("TypeDefinitions.pvs") , (showPVSTypedefs pkg nom const typedefs definsts funcs))
                         , ( (nom ++ ".pvs") , (showPVSTop pkg nom trans tsp))
                         ] ++ if (null trans) then [] else [ ( ("State.pvs") , (showPVSStates pkg nom state) )
                         , ( ("Methods.pvs") , (showPVSMethods pkg trans) )
                         , ( ("Transitions.pvs") , (showPVSTrans pkg nom trans))
			 ]
  where
    nom = pvs_packageName pkg 
    const = pvs_constants pkg
    typedefs = pvs_typedefs pkg
    definsts = pvs_instantiations pkg
    trans = transitions pkg
    state = pvs_state pkg
    funcs = pvs_functions pkg
    

showPVSMethods :: PVSPackage -> [PVStransition] -> String 
showPVSMethods uni trans = intercalate "\n\n\n\t" [ header, (intercalate "\n\n\t"  shownValueMethods), footer]
  where
    header = "Methods : theory\n\nbegin\n\n\timporting State"
    vals = nubBy sameName $ concat $ map (\ (_,_,v,_) -> v) trans
    shownValueMethods = (map (showValueMethod uni (pvs_state uni) trans) vals) 
    footer = "\nend Methods"
    
sameName :: ValueMethod -> ValueMethod -> Bool
sameName (x,x',_,_,_,_,_) (y,y',_,_,_,_,_) = x == y && x' == y' 
  
    
showPVSTypedefs :: PVSPackage -> PackageName -> [PVSConstantDec] -> [PVSTypeDef] -> [PVSInstDef] -> [PVSFunction] -> String
showPVSTypedefs uni nom consts tds dis funcs = intercalate "\n\n\t" [ header
                                                      , shownConsts
                                                      , shownTDs
                                                      , shownDefInsts
                                                      , shownFunctions
                                                      , footer
                                                      ]
  where 
    header = "TypeDefinitions : theory\n\nbegin\n\n\timporting arith_bitwise\n\timporting Maybe\n\timporting FIFO\n\n\tInt(n : int): TYPE = {i:int | -(2^(n-1)) <= i AND i < 2^(n-1)}\n\tUInt(n : int): TYPE = {i:int | 0 <= i AND i < 2^n}\n\tBit(n : int): TYPE = {i:int | 0 <= i AND i < 2^n}\n\n\tmkInt(n : int) : Int(n) = 0\n\tmkUInt(n : int) : UInt(n) = 0\n\tmkBit(n : int) : Bit(n) = 0"
    footer = "\nend TypeDefinitions"
    shownConsts = intercalate "\n\t" $ map (showPVSConstantDeclaration uni) consts
    shownTDs = showPVSTypedefs' uni tds -- intercalate "\n\t" $ map showPVSTypedef tds
    shownDefInsts = intercalate "\n\n\t" $ map (showPVSDefInst uni) $ sortDefInsts dis tds
    shownFunctions = intercalate "\n\n\t" $ map (showPVSFunc uni) funcs
    
sortDefInsts :: [PVSInstDef] -> [PVSTypeDef] -> [PVSInstDef] 
sortDefInsts xs ys = map lookItUp order
  where
    order = map (\ (PVS_Struct z _) -> z) $ sortStructs [ y | y <- ys, isStruct' y]
    lookItUp x = (x, maybe ([]) (id) (lookup x xs))
    
preproc :: PVSPackage -> PVSPackage 
preproc pkg = update pkg
  where 
    update x = x {pvs_state = newState}
    newState = map (\ (x, xs) -> if (x == pvs_packageName pkg) then ("pre", xs) else (x, xs) ) (pvs_state pkg)
    
showPVSDefInst :: PVSPackage -> PVSInstDef -> String
showPVSDefInst uni (nom, fields) = if (null fields) then "" else intercalate "\n\t" [header, mid, footer]
    -- if (null fields) then "" else intercalate "\n\t" [header1, header, mid]
  where
    header1 = nom ++ "_var : var " ++ nom ++ "\n\n\t"
    header = "mk" ++ nom ++ " : " ++ nom
        -- "mk" ++ nom ++ "(" ++ nom ++ "_var) : bool "  
    mid = "= (# " ++ (intercalate "\n         , " $ map (showPVSInstanceDef uni nom) fields)
    footer = "#)"
    
showPVSFunc :: PVSPackage -> PVSFunction -> String
showPVSFunc uni (nom, args, typ, exp) = intercalate "\n\t" [dec, result]
  where 
    dec = nom ++ " (" ++ (intercalate ", " (map (showPVSArgument uni) args)) ++ ") : " ++ showPVSType uni typ
    result = "\t= " ++ showPVSExpression uni (map (args2LVars uni) args) (Just (Identifier (ID ("mk" ++ (showPVSType uni typ))))) (Just typ) exp Nothing ""

args2LVars :: PVSPackage -> PVSArgument -> LocalVar
args2LVars uni (x, y) = ((ID x), (Right (Just y)), Skip)

showPVSTypedefs' :: PVSPackage -> [PVSTypeDef] -> String
showPVSTypedefs' uni xs = intercalate "\n\n\t" [syns, enums, structs]
  where
    syns = intercalate "\n\t" [ showPVSTypedef uni x | x <- xs, isSynonym x]
    enums = intercalate "\n\t" [ showPVSTypedef uni x | x <- xs, isEnum x]
    structs = intercalate "\n\n\t" $ map (showPVSTypedef uni) $ sortStructs [ x | x <- xs, isStruct' x]

sortStructs :: [PVSTypeDef] -> [PVSTypeDef]
sortStructs [] = []
sortStructs xs = x : (sortStructs xs')
  where
    x = head $ filter (\ x -> x `doesntNeed` xs) xs  
    xs' = xs \\ (x:[])

doesntNeed :: PVSTypeDef -> [PVSTypeDef] -> Bool
doesntNeed _ [] = True
doesntNeed (PVS_Struct x ys) ((PVS_Struct x' _):xs) = x' `isntIn` ys && doesntNeed (PVS_Struct x ys) xs
    
isntIn :: String -> [PVS_Field] -> Bool
isntIn _ [] = True
isntIn x ((_,(PVS_Custom y)):ys) = if x == y then False else isntIn x ys
isntIn x ((_,_):ys) = isntIn x ys
    
isSynonym :: PVSTypeDef -> Bool
isSynonym (PVS_Synonym _ _) = True
isSynonym x = False

isEnum :: PVSTypeDef -> Bool
isEnum (PVS_Enumeration _ _) = True
isEnum x = False

isStruct' :: PVSTypeDef -> Bool
isStruct' (PVS_Struct _ _) = True
isStruct' x = False
    
--PVS_Synonym Name PVSType | PVS_Enumeration Name [Enumerat] | PVS_Struct Name [PVS_Field]  

showPVSArgument :: PVSPackage -> PVSArgument -> String
showPVSArgument uni (n, t) = n ++ ": " ++ ( showPVSType uni t )
    
showPVSInstanceDef :: PVSPackage -> String -> (String, Literal) -> String
-- showPVSInstanceDef uni nom (n, (Identifier (ID x))) = n ++ " := " ++ 
showPVSInstanceDef uni nom (n, val) =  n ++ " := " ++ exp 
    -- "("++nom++"_var`"++n++" = " ++ exp++")"
  where
    typ = checkForSynonyms (pvs_typedefs uni) nom
    typ' = maybe (error "Error! This should be impossible!") (id) typ
    typ'' = maybe (error "Another Impossible Error!") (id) (getFieldType n typ')
    dv (PVS_Custom x) = Just $ Identifier $ ID $ "mk"++x
    dv x = Nothing
    exp = (showPVSExpression uni [] (dv typ'') (Just typ'') val (Nothing) "")
    
getFieldType :: String -> PVSTypeDef -> Maybe PVSType 
getFieldType n (PVS_Struct _ fs) = lookup n fs
    
showPVSStates :: PVSPackage -> PackageName -> [PVSstateDec] -> String
showPVSStates uni nom state = intercalate "\n\n\t" [ header
                                               , stateType
                                               , stateInstantiator
                                               , footer
                                               ]
  where 
    header = "State : theory\n\nbegin\n\n\timporting TypeDefinitions"
    footer = "\nend State" 
    stateType = (intercalate "\n\n\t" (map (showPVSStateTypes uni) (orderStates state))) {- if notEmpty uni 
                   then (intercalate "\n\n\t" (map (showPVSStateTypes uni) (reverse state)))
                   else "" -}
    stateInstantiator = (intercalate "\n\n\t" (map (showPVSStateInstance uni) (orderStates state))) {- if notEmpty uni
                           then (intercalate "\n\n\t" (map (showPVSStateInstance uni) (reverse state))) 
                           else "" -}

orderStates :: [PVSstateDec] -> [PVSstateDec]  
orderStates [] = []
orderStates ss = stateInfimum : (orderStates statesLessInfimum)
  where
    stateTuples = map (\ s -> makeStateDepTuples s ss) ss
    showy = "[orderStates] - " ++ (intercalate "\n" (map (\ (x,y) -> (show (fst y)) ++ "->" ++ (show x)) stateTuples)) ++ "\n\ninfimum = " ++ (show (fst stateInfimum))
    stateInfimum = maybe (error "Circular State References?!") (id) $ lookup [] stateTuples 
    statesLessInfimum = delete stateInfimum ss

makeStateDepTuples :: PVSstateDec -> [PVSstateDec] -> ([String], PVSstateDec)
makeStateDepTuples (s, sts) ss = ( ssPresent , (s,sts))
    where
        tracy = "[makeStateDepTuples] - State = " ++ (show s) ++ "\n\nDependencies = " ++ (show ssPresent)
        ssNameList = (\ x -> x ++ (map (\ y -> "mk" ++ y) x)) $ map fst ss
        ssPresent = catMaybes $ map getName sts
        getName (PVS_SubModuleDec _ x _) = if (x `elem` ssNameList) 
                                              then Just x
                                              else Nothing
        getName _ = Nothing
    
showPVSStateTypes :: PVSPackage -> PVSstateDec -> String
showPVSStateTypes uni (nom, xs) = concat [ header
                                     , intercalate "\n\t\t , " (map (showPVSStateDef' uni) (killStateWires xs))
                                     , footer
                                     ] -- (\ q -> trace ("[T] showPVSStateTypes - " ++ (show q)) q) $
  where 
    header = nom ++ " : type =\n\t\t[# "
    footer = "\n\t\t#]"

killStateWires :: [PVSstate] -> [PVSstate] 
killStateWires [] = []
killStateWires ((PVS_DWire _ _ _):xs) = killStateWires xs
killStateWires (x:xs) = x : (killStateWires xs)
    
showPVSStateDef' :: PVSPackage -> PVSstate -> String
showPVSStateDef' uni (PVS_Reg nom typ init) = (showIDPath uni [] Nothing nom "") ++ " : " ++ (showPVSType uni typ)
    -- trace ("[showPVSStateDef'] - " ++ (show nom) ++ "\ntype = " ++ (show typ)) $ (showIDPath uni [] Nothing nom) ++ " : " ++ (showPVSType uni typ)
showPVSStateDef' uni (PVS_Fifo f nom typ) = (showIDPath uni [] Nothing nom "") ++ " : FIFO[" ++ (showPVSType uni typ) ++ "]"
showPVSStateDef' uni (PVS_Vector nom typ size i) = (showIDPath uni [] Nothing nom "") ++ " : [ nat -> "++ (showPVSType uni typ) ++"]"
showPVSStateDef' uni (PVS_SubModuleDec interface nom inst) = if (null nom ) 
                                                                then inst ++ " : " ++ interface
                                                                else inst ++ " : " ++ (tail (tail nom))
--showPVSStateDef' uni (PVS_DWire _ _ _) = ""

showPVSStateInstance :: PVSPackage -> PVSstateDec -> String
showPVSStateInstance uni (nom, xs) = concat [ header1
                                        , header2 
                                        , middleBit
                                        , footer
                                        ]
  where
    header1 = nom ++ "_var : var " ++ nom ++ "\n\n\t"
    header2 = "mk"++nom++" ("++nom++"_var) : bool\n\t=\t"
    footer = "\n"
    middleBit = if (notEmpty uni) && (not (null (killStateWires xs)))
                   then intercalate "\n\tAND\t" (map (showPVSStateInstance' uni nom) (killStateWires xs))
                   else "True"
                    
showPVSStateInstance' :: PVSPackage -> String -> PVSstate -> String
showPVSStateInstance' uni nom (PVS_Reg n (PVS_Custom y) (Literal (LitStructConstructor))) = nom ++ "_var`"++ (showIDPath uni [] Nothing n "") ++ "\t = mk" ++ (showPVSType uni (PVS_Custom y))
showPVSStateInstance' uni nom (PVS_Reg n (PVS_Custom y) i) = nom ++ "_var`"++(showIDPath uni [] Nothing n "")++"\t = " ++ (showPVSExpression uni [] Nothing (Just (PVS_Custom y)) i (Nothing) "")
showPVSStateInstance' uni nom (PVS_Reg n t i) = nom ++ "_var`"++(showIDPath uni [] Nothing n "")++"\t = " ++ (showPVSExpression uni [] Nothing (Just t) i (Nothing) "")
showPVSStateInstance' uni nom (PVS_Fifo f n t) = "mkFIFO(" ++ nom ++ "_var`"++(showIDPath uni [] Nothing n "")++", " ++ (showPVSExpression uni [] Nothing (Just t) size Nothing "") ++ ")"
  where
      size = maybe (Literal (LitInt (-1))) (id) $ getFifoSize f 
showPVSStateInstance' uni nom (PVS_Vector nom2 t num i) = nom ++ "_var`"++(showIDPath uni [] Nothing nom2 "")++ " = "++ nom ++ "_var`"++(showIDPath uni [] Nothing nom2 "") ++" WITH \n\t\t\t" ++ init
  where 
    init = getVectorInit uni (fromIntegral num) i
showPVSStateInstance' uni nom (PVS_SubModuleDec int n inst) = if (isStruct (pvs_typedefs uni) int)
    -- trace ("[showPVSStateInstance] - " ++ (show (PVS_SubModuleDec int n inst))) $ if (isStruct (pvs_typedefs uni) int)
    then nom ++ "_var`"++ inst ++ "\t = mk" ++ int
    else if (null n) 
      then "mk" ++ int ++" ("++ nom ++ "_var`"++ inst ++ ")"
      else n ++" ("++ nom ++ "_var`"++ inst ++ ")"
--showPVSStateInstance' _ _ (PVS_DWire _ _ _) = ""

getFifoSize :: FifoType -> Maybe Literal 
getFifoSize (SizedFIFO x) = Just x
getFifoSize (SizedFIFOF x) = Just x
getFifoSize (SizedBypassFIFOF x) = Just x
getFifoSize x = Nothing

isStruct :: [PVSTypeDef] -> String -> Bool
isStruct [] _ = False
isStruct ((PVS_Struct n _):xs) n' = if (n == n') then True else isStruct xs n'
isStruct (x:xs) n'                = isStruct xs n'

getVectorInit :: PVSPackage -> Int -> VectorInit -> String
getVectorInit pkg size (Replicate i) = intercalate "\n\t\t\t" ((header ++ middle) : (footer : []))
  where 
    header = "[ "
    middle = intercalate "\n\t\t\t, " $ map (getVectorInit' pkg) (zip [0..(size)] (repeat i))
    footer = "] "
getVectorInit pkg _ (Explicit xs) = intercalate "\n\t\t\t" ((header ++ middle) : (footer : []))
  where 
    header = "[ "
    middle = intercalate "\n\t\t\t, " $ map (getVectorInit' pkg) (zip [0..(length xs)] xs)
    footer = "] "

getVectorInit' :: PVSPackage -> (Int, Literal) -> String
getVectorInit' pkg (n, l) = (show n) ++ " := " ++ showPVSExpression pkg [] Nothing badType l Nothing ""
    
defaultVal :: PVSType -> String 
defaultVal (PVS_Bool) = "false"
defaultVal (PVS_Bit _) = "0"
defaultVal (PVS_Int _) = "0"
defaultVal (PVS_UInt _) = "0"
defaultVal (PVS_Real) = "0.0"
defaultVal (PVS_Custom nom) = "0"

showPVSTrans :: PVSPackage -> PackageName -> [PVStransition] -> String
showPVSTrans uni nom trans = intercalate "\n\n\n\t" [ header 
       --                                   , variables
       --                                   , shownValueMethods
                                          , shownValTransitions
                                          , shownTransitions
                                          , footer
                                          ]
  where
    header = "Transitions : theory\n\nbegin\n\n\timporting Methods"
    vals = nub $ concat $ map (\ (_,_,v,_) -> v) trans
    -- vars = nub $ concat $ map (\ (_,_,v,_) -> v) trans
    -- variables = intercalate "\n\t" $ ("pre, post, "++nom++"_var : VAR "++nom) : (map showPVSvariable vars)
    -- shownValueMethods = intercalate "\n\n\t" (map (showValueMethod uni trans) vals)
    shownTransitions = showPVSTransition' uni nom False trans
    shownValTransitions = showPVSTransition' uni nom True trans
    footer = "\nend Transitions"

showPVSTransition' :: PVSPackage -> String -> Bool -> [PVStransition] -> String
showPVSTransition' uni nom mode transitions = intercalate "\n\n\t" generated -- error $ show $ map (map (\ (x,_,_,_) -> x)) groupedTransitions --
  where
    st = maybe (error "Error! State declaration for root module not found!") (\ x -> (nom, x)) (lookup nom (pvs_state uni))
    groupedTransitions = groupByArgs uni transitions
    generated = map (genNewTransition uni st nom mode) groupedTransitions
    
genNewTransition :: PVSPackage -> PVSstateDec -> String -> Bool -> [PVStransition] -> String
genNewTransition uni st nom mode ts = if (notEmpty uni) 
    then intercalate "\n\t\t" [header, middle, footer, footer2]
    else "" 
  where                       
    mArgs = getMArgs ts
    methodArgs = if null mArgs 
                   then [] 
                   else ", " ++ (intercalate ", " ( map (\(n,t) -> n++" : "++(showPVSType uni t)) mArgs))  
    methodArgs2 = if null mArgs 
                   then [] 
                   else ", " ++ (intercalate ", " ( map (\(n,t) -> n ) mArgs))
    header = if (not mode) 
		then "transition ( index : nat, pre, post : " ++ nom ++ (methodArgs) ++ ") : bool ="
                else "transition_val (index : nat, pre : " ++ nom ++ (methodArgs) ++ ") : "++(nom)++" ="
    ifTuples = crunchTuples $ zip (map (\ (x,_,_,_) -> x) ts) (map (showPVSTransitionTable uni st nom [] mode) ts)
    footer = if (not mode)
		then "" 
		else "ELSE pre"
    footer2 = if (not mode)
                 then ""
                 else concat $ replicate ((length ifTuples)) "ENDIF " -- <M> concat $ replicate ((length ifTuples) - 1) "ENDIF "
    middle = if (not mode)
                then "post = transition_val (index, pre" ++ (methodArgs2) ++ ")"
                else intercalate "\n\t\tELSE " $ genIf ifTuples
    qbert [] = ""
    qbert ((_,x):[]) = x

notEmpty :: PVSPackage -> Bool
notEmpty uni = not $ null $ wireFilter $ pvs_state uni

wireFilter :: [PVSstateDec] -> [PVSstateDec]
wireFilter [] = []
wireFilter ((x, ss):xs) = if null filtered 
                             then wireFilter xs
                             else (x, filtered) : (wireFilter xs)
    where
        filtered = killStateWires ss

truplePad :: (a,b) -> (a,b,[c])
truplePad (x,y) = (x,y,[])
    
getMArgs :: [PVStransition] -> [(MethodArg, PVSType)]		
getMArgs [] = [] 
getMArgs ((_,[],_,_):xs) = []
getMArgs ((_,x,_,_):xs) = concat $ map snd x


showPVSTransitionTable :: PVSPackage -> PVSstateDec -> String -> [ID_Path] -> Bool -> PVStransition -> String
showPVSTransitionTable uni st nom ex mode (ind, methods, _, transTables)  = concat [ transStates ]
  where
    tracy = "[showPVSTransitionTable] transition Heirarchy = " ++ (showTransitionHierarchy transTables "\t")
    tracy2 = "[showPVSTransitionTable]"
    -- fifos = (\ x -> trace ("[showPVSTransitionTable] All - " ++ (show transTables) ++ "\n\nFIFOs " ++ (show x)) x) $ extractFifos transTables
    --lets = genLets uni ( orderLets st $ (extractWires' transTables) ++ fifos ) (show ind)
    transStates = (showPVSTransStateCluster uni st "" "" ex (TransMod "pre" transTables) (show ind) mode)
    -- footer = "\n\t\t  ]"

showTransitionHierarchy :: [TransitionTable] -> String -> String
showTransitionHierarchy ts spacer = intercalate "\n" (map (sth spacer) ts)

sth :: String -> TransitionTable -> String 
sth spacer (TransMod n ts) = spacer ++ n ++ "\n" ++ spacer ++ (intercalate ("\n"++spacer) (map (sth (spacer ++ "  ")) ts))
sth spacer (TransStruct n ts) = spacer ++ n ++ "\n" ++ spacer ++ (intercalate ("\n"++spacer) (map (sth (spacer ++ "  ")) ts))
sth spacer t = justTheName' t 

    
groupByArgs :: PVSPackage -> [PVStransition] -> [[PVStransition]] 
groupByArgs uni [] = []
groupByArgs uni (x:xs) = yup : (groupByArgs uni nope)
  where
    yup  = x : [ y | y <- xs, argsMatch uni x y]
    nope = [ y | y <- xs, (not (argsMatch uni x y))]

argsMatch :: PVSPackage -> PVStransition -> PVStransition -> Bool
argsMatch uni (_,x,_,_) (_,y,_,_) = isMatch (boom x) (boom y)
  where
    boom z = map (snd) z
    d zs = map (desynonymize' (pvs_typedefs uni)) zs
    isMatch xs ys = null (d xs \\ d ys) && null (d ys \\ d xs)
  
desynonymize :: [PVSTypeDef] -> [(MethodName, [(MethodArg, PVSType)])] -> [(MethodName, [(MethodArg, PVSType)])]
desynonymize _ [] = []
desynonymize tds ((x, ys):xs) = (x, (desynonymize' tds ys)) : (desynonymize tds xs)

desynonymize' :: [PVSTypeDef] -> [(MethodArg, PVSType)] -> [(MethodArg, PVSType)]
desynonymize' _ [] = []
desynonymize' tds ((x, y):xs) = (x, (drillOutSynonyms tds y)) : (desynonymize' tds xs)

drillOutSynonyms :: [PVSTypeDef] -> PVSType -> PVSType
drillOutSynonyms tds (PVS_Custom n) = desyn syn
  where
    syn = checkForSynonyms tds n
    desyn Nothing = (PVS_Custom n)
    desyn (Just (PVS_Enumeration _ _)) = (PVS_Custom n)
    desyn (Just (PVS_Struct _ _)) = (PVS_Custom n)
    desyn (Just (PVS_Synonym n t)) = drillOutSynonyms tds t
drillOutSynonyms tds x = x

--PVS_Synonym Name PVSType | PVS_Enumeration Name [Enumerat] | PVS_Struct Name [PVS_Field] 



    
showValueMethod :: PVSPackage -> [PVSstateDec] -> [PVStransition] -> ValueMethod -> String
showValueMethod uni stdecs trans (name, name2, namo, q, ret, exp, wires) = (intercalate "\n\n\t" vms)
  where
    stdec = maybe (error "?!?!") (\ x -> (name2, x)) $ lookup name2 stdecs
    val = (name, name2, namo, q, ret, exp, wires)
    -- wireTest = (null (getReadsBy (isDWire stdec) exp) )
    ifTruples = genIfTruple uni stdec val trans
    ifTruples' = crunchTruples ifTruples
    ifTuples = map fst ifTruples'
    headers = map (genValueMethodHeader uni True val) (map (\ (_,x) -> x) ifTruples')  --if wireTest 
                -- then (genValueMethodHeader uni False val []):[]
                -- else map (genValueMethodHeader uni True val) (map (\ (_,x) -> x) ifTruples') 
    middles = map genIf' ifTuples -- if wireTest 
                -- then replicate (length headers) (showPVSExpression uni [] Nothing (Just ret) exp Nothing)
                -- else map genIf' ifTuples
    endifs = map (\ x -> concat (replicate ((length x) - 1) "ENDIF ")) ifTuples -- if wireTest 
               -- then replicate (length headers) ""
               -- else map (\ x -> concat (replicate ((length x) - 1) "ENDIF ")) ifTuples
    footers = replicate (length ifTruples) $ "\n\t\tELSE " ++ (showPVSExpression uni [] Nothing (Just ret) (kludgeInit'' uni ret) Nothing "") ++ "\n\t\tENDIF "  -- if wireTest 
                 --then replicate (length headers) ""
                 --else replicate (length ifTruples) $ "\n\t\tELSE " ++ (showPVSExpression uni [] Nothing (Just ret) (kludgeInit'' uni ret) Nothing) ++ "\n\t\tENDIF " 
    vms =  zipWith (++) (zipWith (++) (zipWith (++) headers middles) footers) endifs

kludgeInit'' :: PVSPackage -> PVSType -> Expression
kludgeInit'' uni (PVS_Maybe x) = (Tagged (Just x) (Invalid))
kludgeInit'' uni x = kludgeInit uni x
    
kludgeInit :: PVSPackage -> PVSType -> Expression
kludgeInit _ (PVS_Bool) = Literal (LitBool False)
kludgeInit _ (PVS_Bit _) = Literal (LitInt 0)
kludgeInit _ (PVS_Int _) = Literal (LitInt 0)
kludgeInit _ (PVS_UInt _) = Literal (LitInt 0)
kludgeInit _ (PVS_Real) = Literal (LitReal 0.0)
kludgeInit uni (PVS_Custom n) = maybe (ifNotInst) ((\ x -> def2exp x (PVS_Custom n))) inst
    where 
      inst     = lookupInst (pvs_instantiations uni) (PVS_Custom n)
      ifNotInst = kludgeInit uni (deAlias (pvs_typedefs uni) (PVS_Custom n))
kludgeInit uni (PVS_Maybe x) = kludgeInit uni x

kludgeInit' :: PVSPackage -> PVSType -> String
kludgeInit' _ (PVS_Bool) = "False"
kludgeInit' _ (PVS_Bit _) = "0"
kludgeInit' _ (PVS_Int _) = "0"
kludgeInit' _ (PVS_UInt _) = "0"
kludgeInit' _ (PVS_Real) = "0.0"
kludgeInit' uni (PVS_Custom n) = maybe (kludgeInit' uni (deAlias (pvs_typedefs uni) (PVS_Custom n))) ((\ x -> def2string uni td x)) inst
    where 
      td       = findTypeDef (pvs_typedefs uni) (PVS_Custom n)
      inst     = lookupInst (pvs_instantiations uni) (PVS_Custom n)
kludgeInit' uni (PVS_Maybe x) = kludgeInit' uni x

lookupInst :: [PVSInstDef] -> PVSType -> Maybe PVSInstDef
lookupInst [] n = Nothing
-- lookupInst xs (PVS_Custom "DoorBell") = error $ show $ xs 
lookupInst x (PVS_Maybe y) = lookupInst x y -- trace ("[T] - lookupInst " ++ (show x) ++ "\n\n" ++ (show y)) $ lookupInst x y
lookupInst ((n1, fs):xs) (PVS_Custom "Type") = if n1 == "Tpye" then (Just (n1, fs)) else lookupInst xs (PVS_Custom "Type")
lookupInst ((n1, fs):xs) (PVS_Custom n2) = if n1 == n2 then (Just (n1, fs)) else lookupInst xs (PVS_Custom n2)
lookupInst _ x = Nothing 

def2exp :: PVSInstDef -> PVSType -> Expression 
def2exp (n, fs) t = (StructCluster (Right t) fs)

def2string :: PVSPackage -> PVSTypeDef -> PVSInstDef -> String
def2string uni td (n, fs) = "(# " ++ (intercalate "\n\t\t\t, " (map (def2string' uni td) fs)) ++ "\n\t\t\t#)"

def2string' :: PVSPackage -> PVSTypeDef -> (Name, Literal) -> String
def2string' uni td (n, (Literal (LitStructConstructor))) = n ++ " := " ++ inito
  where
    fieldType = maybe (error "Error! Field type not found! ") (id) $ getFieldType n td
    inst      = maybe (error "Error! Instantiation not found!") (id) $ lookupInst (pvs_instantiations uni) fieldType
    td'       = findTypeDef (pvs_typedefs uni) fieldType
    inito     = def2string uni td' inst
def2string' uni td (n, x) = n ++ " := " ++ (show x)



-- PVS_Bool | PVS_Bit N | PVS_Int N | PVS_UInt N | PVS_Real | PVS_Custom Name | PVS_Maybe PVSType
    
genIfTruple :: PVSPackage -> PVSstateDec -> ValueMethod -> [PVStransition] -> [(Integer, String, [(MethodArg, PVSType)])]
genIfTruple _ _ _ [] = []
genIfTruple uni stdec val ((w,x,y,z):ts) = (w, midbit, args) : (genIfTruple uni stdec val ts)
    where
--        z' = orderLets stdec z 
        n = pvs_packageName uni
        st = (n, (maybe (error "I am Error!") (id) (lookup n (pvs_state uni))))
        midbit = genMethodCase uni st val (w,x,y,z)
        args = map (deAliasArg (pvs_typedefs uni)) $ getMArgs ((w,x,y,z):[])
        
deAliasArg :: [PVSTypeDef] -> (MethodArg, PVSType) -> (MethodArg, PVSType)
deAliasArg tds (n, t) = (n, deAlias tds t)

deAlias :: [PVSTypeDef] -> PVSType -> PVSType
deAlias tds (PVS_Custom x) = tdLookup tds x
    -- trace ("[T] deAlias - \n\t\tcustom type : " ++ (show x) ++ "\n\t\ttypedefs : " ++ (intercalate "\n\t\t\t" (map show tds)) ) $ tdLookup tds x
deAlias tds x = x

tdLookup :: [PVSTypeDef] -> String -> PVSType
tdLookup ((PVS_Synonym n t):xs) n' = if (n == n') then t else tdLookup xs n'
tdLookup (x:xs) n' = tdLookup xs n'
tdLookup [] n = (PVS_Custom n)
    
tripleConc :: [String] -> [String] -> [String] -> [String]
tripleConc [] [] [] = [] 
tripleConc [] [] (z:zs) = (z) : (tripleConc [] [] zs)
tripleConc [] (y:ys) [] = (y) : (tripleConc [] ys [])
tripleConc (x:xs) [] [] = (x) : (tripleConc xs [] [])
tripleConc [] (y:ys) (z:zs) = (y ++ z) : (tripleConc [] ys zs)
tripleConc (x:xs) [] (z:zs) = (x ++ z) : (tripleConc xs [] zs)
tripleConc (x:xs) (y:ys) [] = (x ++ y) : (tripleConc xs ys [])
tripleConc (x:xs) (y:ys) (z:zs) = (x ++ "\n\t\t" ++ y ++ "\n\t\t" ++ z) : (tripleConc xs ys zs)
    
genValueMethodHeader :: PVSPackage -> Bool-> ValueMethod -> [(MethodArg, PVSType)] -> String 
genValueMethodHeader uni includeIndex (name, name2, namo, _, ret, exp, wires) args = name ++ " (" ++ (intercalate ", " decs) ++ ") : " ++ (showPVSType uni ret) ++ " = "
    where 
        decs = if includeIndex 
                   then [("index : nat"), ( "pre : " ++ namo), ("mod : " ++ name2)]  ++ (map (showPVSArgument uni) args)
                   else [( "pre : " ++ namo), ("mod : " ++ name2)]  ++ (map (showPVSArgument uni) args)
        
		

crunchTuples :: (Eq a, Eq b) => [(a, b)] -> [([a], b)]
crunchTuples [] = []
crunchTuples (x:xs) = ((fst x):(map fst eqs) , snd x) : (crunchTuples neqs)
  where
    eqs  = [ y | y <- xs, snd x == snd y]
    neqs = [ y | y <- xs, snd x /= snd y]
    
crunchTruples :: (Eq a, Eq b, Eq c) => [(a, b, c)] -> [([(a, b)], c)]
crunchTruples [] = []
crunchTruples (x:xs) = (((\(p,q,_)->(p,q)) x):(map (\(p,q,_)->(p,q)) eqs) , ((\(_,_,q)->q) x)) : (crunchTruples neqs)
  where
    eqs  = [ y | y <- xs, (\ (_,_,q)-> q) x == (\ (_,_,q)-> q) y]
    neqs = [ y | y <- xs, (\ (_,_,q)-> q) x /= (\ (_,_,q)-> q) y]    

genIf' :: [(Integer, String)] -> String
genIf' [] = []
--genIf ((is, str):[]) = str:[]
genIf' ((i, str):xs) = "IF (index = " ++ (show i) ++ ") \n\t  THEN " ++ str ++ (if (null xs) then "" else "\n\t  ELSE " ++ (genIf' xs) ) {-++ "\n\t  ENDIF"-}
    
genIf :: [([Integer], String)] -> [String]
genIf [] = []
--genIf ((is, str):[]) = str:[]
genIf ((is, str):xs) = ("IF " ++ (intercalate " OR " (map (\ x -> "(index = " ++ (show x) ++ ")") is)) ++ " THEN " ++ str) : (genIf xs)
-- 
-- genIf' :: [([Integer], [String], [(MethodArg, PVSType)])] -> [[PVStransition]] -> [String]
-- genIf' [] = []
-- genIf' ((is, (str:ss), _):[]) _   = str:[]
-- genIf' ((is, (str:ss), _):xs) tss = ("IF " ++ (intercalate " OR " (map (\ x -> "(index = " ++ (show x) ++ ")") is)) ++ " THEN " ++ str') : (genIf' xs)
--     where 
--         ts = findByIndexes is tss
--         str' = 
--         
-- findByIndexes :: [Integer] -> [[PVStransition]] -> [PVStransition]        
-- findByIndexes [] = []
-- findByIndexes is (ts:tss) = if (all (\ x -> x `elem` is) its) then ts else findByIndexes is tss
--     where 
--         its = map (\ (x,_,_,_) -> x ) ts
    
genMethodCase :: PVSPackage -> PVSstateDec -> ValueMethod -> PVStransition -> String
genMethodCase uni st (name, name2, path, _, ret, exp, wires) (index,x,y,tables) = result 
  where
    tracy = "\n[genMethodCase] - name = " ++ (show name) 
         ++ "\nname2 = " ++ (show name2) 
         ++ "\nraw exp = " ++ (show exp) 
         ++ "\nreads = "++ (show (map idTopLevel (getReadsBy mkBSVModule (\ x -> True) [] exp))) 
         ++ "\nwiresInExp = " ++ (intercalate "\n" (map show wirs)) 
         ++ "\npath = " ++ (show path)
         ++ "\n\nresult = " ++ (result) 
    tables' = getTablesBySubmod uni name2 tables
    wirs = nub $ getWireDeps tables' (Just exp) []
    letsInExp = (wirs)  ++ (extractFifos tables')
        -- (filterWires (getReadsBy (isDWire st) exp) st tables)  ++ (extractFifos tables)
        -- (\ x -> trace ("[genMethodCase] letsInExp - \n" ++ (show x)) x) $ (wirs)  ++ (extractFifos tables')
    allWires = orderLets uni st [] tables letsInExp
        -- (\ x -> trace ("[genMethodCase] allWires - \n" ++ (show x)) x) $ orderLets st letsInExp 
    wireStuff = concat $ map (\ a -> showLet uni a "pre" (show index)) allWires
    lets = if (null wireStuff) then "" else ("LET " ++ (intercalate "\n\t\t, " wireStuff)) ++ "\n\t\t IN " --if (name == "ireq_rdy_n_" ) 
      --then error ("LET " ++ (intercalate "\n\t\t, " wireStuff) ++ "\n\t\t IN " )
      --else ("LET " ++ (intercalate "\n\t\t, " wireStuff)) ++ "\n\t\t IN " 
    theRest = showPVSExpression uni [] Nothing (Just ret) exp Nothing (show index)
    result = lets ++ theRest
        -- error $ show $ showPVSExpression uni [] Nothing exp Nothing

getWireDeps :: [TransitionTable] -> Maybe Expression -> [TransitionTable] -> [TransitionTable] 
getWireDeps tables Nothing [] = []
getWireDeps tables (Just exp) [] = wirs ++ (getWireDeps tables Nothing wirs)
    where
        wirs = nub $ filterWires (map idTopLevel (getReadsBy mkBSVModule (\ x -> True) [] exp)) tables
getWireDeps tables Nothing (t:ts) = wirs ++ (getWireDeps tables Nothing (ts ++ wirs))
    where
        wirs = getWireDeps' tables t
        tracy = "[getWireDeps] - found wires = " ++ (intercalate "\n\t" (map show wirs)) 
        
getWireDeps' :: [TransitionTable] -> TransitionTable -> [TransitionTable]
getWireDeps' tables (TransDWire i tree dv) = filterWires (map idTopLevel q) tables
  where
      q = nub (getReadsByOverTree (\ x -> True) tree)
      tracy = "[getWireDeps'] - name = " ++ (show i) ++ "\nreads = " ++ (show q) ++ "\nfiltered Reads = " ++ (show (filterWires (map idTopLevel q) tables))
        
idTopLevel :: ID_Path -> ID_Path 
idTopLevel (ID x) = (ID x)
idTopLevel (ID_Submod_Struct x y) = (ID x)
idTopLevel (ID_Vect x n) = (ID x)
        
getTablesBySubmod :: PVSPackage -> String -> [TransitionTable] -> [TransitionTable]
getTablesBySubmod uni name tables = seekTable tables path  
  where
    itree = constructInvokationTree uni
    path = findPathToName itree name 

seekTable :: [TransitionTable] -> [String] -> [TransitionTable]
seekTable ts [] = ts
seekTable ts ("root":xs) = seekTable ts xs
seekTable ((TransMod n ts):ts') (x:xs) = if (n == x) 
                                            then seekTable ts xs
                                            else seekTable ts' xs
seekTable (t:ts) xs = seekTable ts xs

    
findPathToName :: InvokationTree -> String -> [String] 
findPathToName (Istem x y ts) z  = if (y == z) 
                                      then x : []
                                      else findPathToName (head possibles) z
  where
    possibles = filter (\ q -> branchTest q z) ts
findPathToName (Ileaf x y) z     = if y == z then x : [] else error "What the frick people?"

branchTest :: InvokationTree -> String -> Bool
branchTest (Istem _ x ts) y = if (x == y) then True else or (map (\ q -> branchTest q y) ts)
branchTest (Ileaf _ x) y = x == y
    
constructInvokationTree :: PVSPackage -> InvokationTree 
constructInvokationTree uni = constructInvokationTree' uni "root" rootStdec 
    where
        rootStdec = maybe (error "?!?!") (\ x -> (pvs_packageName uni, x)) $ lookup (pvs_packageName uni) (pvs_state uni)
        
constructInvokationTree' :: PVSPackage -> String -> PVSstateDec -> InvokationTree
constructInvokationTree' uni inst (n, sts) = if (null submods) 
                                                then (Ileaf inst n)
                                                else (Istem inst n (map (\ (x,y) -> constructInvokationTree' uni x y) submodTuples))
  where
      tracy = "[constructInvokationTree'] submods = "++ (show submods) ++"\n\nsubmodNames - " ++ (show submodNames)
      submods = filter (isSubmod) sts
      getSubmodNames (PVS_SubModuleDec _ x _) = if ("mk" `isPrefixOf` x) then tail $ tail x else x
      getSubmodInsts (PVS_SubModuleDec _ _ x) = x
      submodNames = map getSubmodNames submods
      submodInsts = map getSubmodInsts submods 
      submodStates = map (\ x -> (x, maybe (error "?!") (id) (lookup x (pvs_state uni)))) submodNames 
      submodTuples = zip submodInsts submodStates 
      

isSubmod :: PVSstate -> Bool
isSubmod (PVS_SubModuleDec _ "" _) = False
isSubmod (PVS_SubModuleDec _ _ _) = True
isSubmod x = False

filterWires :: [ID_Path] -> [TransitionTable] -> [TransitionTable]
filterWires [] _ = []
filterWires (x:xs) tables = if table == Nothing 
                               then (filterWires xs tables)
                               else (maybe (error "Weird Error!") (id) table) : (filterWires (xs ++ wireDeps) tables)
  where
    table = wut $ lookupTable' x tables
    wut Nothing = Nothing
    wut (Just (TransDWire x y z)) = (Just (TransDWire x y z))
    wut (Just x) = Nothing
    wireDeps = maybe ([]) (id) $ fmap nub $ fmap (getReadsByOverTree (\ x -> True)) (fmap getTree table) 
    
filterWires' :: [TransitionTable] -> [ID_Path] -> [ID_Path]
filterWires' _ [] = []
filterWires' tables (x:xs) = if table == Nothing 
                               then (filterWires' tables xs )
                               else (idTopLevel x) : (filterWires' tables (xs ++ wireDeps) )
  where
    tracy = "[filterWires'] - x = " ++ (show x) ++ "\ntables =" ++ (intercalate "\n" (map justTheName' tables)) ++ "\ntable = " ++ (show (lookupTable' x tables))
    table = wut $ lookupTable' x tables
    wut Nothing = Nothing
    wut (Just (TransDWire x y z)) = (Just (TransDWire x y z))
    wut (Just x) = Nothing
    wireDeps = maybe ([]) (id) $ fmap nub $ fmap (getReadsByOverTree (\ x -> True)) (fmap getTree table)     

getTree :: TransitionTable -> SpecificTree
getTree (TransReg x y) = y
getTree (TransDWire x y z) = y

lookupTable' :: ID_Path -> [TransitionTable] -> Maybe TransitionTable
lookupTable' i ts = lookupTable i $ concat $  map (expandTable Nothing) ts

expandTable :: Maybe ID_Path -> TransitionTable -> [TransitionTable]
expandTable (Just p) (TransMod x ts)     = concat $ map (expandTable (Just (mergeIDPaths p (ID x)))) ts
expandTable (Just p) (TransStruct x ts)  = concat $ map (expandTable (Just (mergeIDPaths p (ID x)))) ts
expandTable (Just p) (TransReg x y)      = (TransReg (mergeIDPaths p x) y):[]
expandTable (Just p) (TransVect x y z)   = (TransVect (mergeIDPaths p x) y z):[]
expandTable (Just p) (TransDWire x y z)  = (TransDWire x y z):[]
expandTable (Just p) (TransFIFO w x y z) = (TransFIFO (mergeIDPaths p w) x y z):[]
expandTable (Nothing) (TransMod x ts)     = concat $ map (expandTable (Just (ID x))) ts
expandTable (Nothing) (TransStruct x ts)  = concat $ map (expandTable (Just (ID x))) ts
expandTable (Nothing) (TransReg x y)      = (TransReg x y):[]
expandTable (Nothing) (TransVect x y z)   = (TransVect x y z):[]
expandTable (Nothing) (TransDWire x y z)  = (TransDWire x y z):[]
expandTable (Nothing) (TransFIFO w x y z) = (TransFIFO w x y z):[]

lookupTable :: ID_Path -> [TransitionTable] -> Maybe TransitionTable 
lookupTable i [] = Nothing 
lookupTable (ID i) ((TransMod _ ts):xs) = maybe (lookupTable (ID i) xs) (\ x -> Just x) $ lookupTable (ID i) ts
lookupTable (ID_Submod_Struct x' y') ((TransMod x y):xs) = if (x' == x) then (lookupTable y' y) else (lookupTable (ID_Submod_Struct x' y') xs)
lookupTable i ((TransReg x y):xs) = if (i == x) then Just (TransReg x y) else (lookupTable i xs)
lookupTable i ((TransVect x y z):xs) = if (i == x) then Just (TransVect x y z) else (lookupTable i xs)
lookupTable (ID i) ((TransStruct _ _):xs) = lookupTable (ID i) xs
lookupTable (ID_Submod_Struct x' y') ((TransStruct x y):xs) = if (x' == x) then (lookupTable y' y) else (lookupTable (ID_Submod_Struct x' y') xs)
lookupTable i ((TransDWire x y z):xs) = if (i `pathEq` x) then Just (TransDWire x y z) else (lookupTable i xs)
  where
      tracy = "[lookupTable] i = " ++ (show i) ++ "\nx = " ++ (show x) ++ "\n result = " ++ (show (i `pathEq` x))
lookupTable i ((TransFIFO w x y z):xs) = if (i `pathEq` w) then Just (TransFIFO w x y z) else (lookupTable i xs)

pathEq :: ID_Path -> ID_Path -> Bool
pathEq (ID_Submod_Struct i' p') (ID_Submod_Struct i p)  = i == i' && p `pathEq` p'
pathEq (ID_Submod_Struct i' p') (ID i)                  = i == i'
pathEq (ID i') (ID_Submod_Struct i p)                   = i == i'
pathEq (ID i') (ID i)                                   = i == i'
        
orderLets :: PVSPackage -> PVSstateDec -> [ID_Path] ->[TransitionTable] -> [TransitionTable] -> [TransitionTable]
orderLets uni st exclude tables ts = result
    where
        q = nub $ map (depTuples (pvs_state uni) tables) ts
        depTups =  map (eliminateSubModEntities exclude st) $ q
        result = orderWires' depTups
        tracy = "[T] orderLets' dependency tuples = " ++ (intercalate "\n" (map show q)) ++ "\nphase 2 = " ++ (intercalate "\n" (map show depTups))
            -- ( \ x -> trace ("[T] orderLets' - " ++ (concat (map justTheName x))) x) $ nub $ map (depTuples st) ts
        
orderWires' :: [(TransitionTable, [ID_Path])] -> [TransitionTable]
orderWires' [] = []
orderWires' ts = ((inf) : (orderWires' ts'))
    --trace ("[T] orderWires' - " ++ (concat (map justTheName ts))) $ ((inf) : (orderWires' ts'))
  where
    tracy = "[T] orderWires' - infimum = " ++ (show inf') ++ "\nthe rest = " ++ (concat (map justTheName ts))
    inf' =  pickInfimum ts
        -- (\ x-> trace ("[T] orderWires' - inf' " ++ (show x) ++ "\n\nts = " ++ (concat (map justTheName ts)) ) x) $ pickInfimum ts
    inf = maybe (error ("selected infimum: " ++ (show inf') ++ "\n\n" ++ (concat (map justTheName ts)))) (id) inf'
    nom (TransDWire i _ _) = i
    nom (TransFIFO i _ _ _) = i
    nom (TransReg i _) = i
    nom (TransMod i _) = ID i
    nom (TransStruct i _) = ID i
    nom (TransVect i _ _) = i
    ts' = killRefs (deleteTable ts inf) (nom inf)

killRefsWithNoTransition :: [(TransitionTable, [ID_Path])] -> [(TransitionTable, [ID_Path])]
killRefsWithNoTransition ts = ts' 
-- trace ("[killRefsWithNoTransition] - \n\n" ++ (show ts) ++ "\n\n" ++ (show ts')) ts'
  where
      noms = map nom $ map fst ts
      nom (TransDWire i _ _) = i
      nom (TransFIFO i _ _ _) = i
      nom (TransReg i _) = i
      nom (TransMod i _) = ID i
      nom (TransStruct i _) = ID i
      nom (TransVect i _ _) = i
      fsts = map fst ts
      snds = map snd ts 
      snds' = map (\ q -> filter (\ x -> x `elem` noms) q) snds
      ts' = zip fsts snds'

justTheName :: (TransitionTable,[ID_Path]) -> String
justTheName ((TransMod x _),is) = x ++ "\n\t\t" ++ (show is) ++ "\n"
justTheName ((TransReg i _),is) = (show i)++ "\n\t\t" ++ (show is)++ "\n"
justTheName ((TransVect i _ _ ),is)  = (show i)++ "\n\t\t" ++ (show is)++ "\n"
justTheName ((TransStruct x _),is)   = x++ "\n\t\t" ++ (show is)++ "\n"
justTheName ((TransDWire i _ _ ),is) = (show i)++ "\n\t\t" ++ (show is)++ "\n"
justTheName ((TransFIFO i _ _ _),is) = (show i)++ "\n\t\t" ++ (show is)++ "\n"

justTheName' :: TransitionTable -> String
justTheName' (TransMod x _) = x 
justTheName' (TransReg i _) = (show i)
justTheName' (TransVect i _ _ )  = (show i)
justTheName' (TransStruct x _)   = x
justTheName' (TransDWire i _ _ ) = (show i)
justTheName' (TransFIFO i _ _ _) = (show i)

deleteTable :: [(TransitionTable, [ID_Path])] -> TransitionTable -> [(TransitionTable, [ID_Path])]
deleteTable [] _ = []
deleteTable (t:ts) t' = if (fst t) == t' 
                           then deleteTable ts t' 
                           else t : (deleteTable ts t')
    
killRefs :: [(TransitionTable, [ID_Path])] -> ID_Path -> [(TransitionTable, [ID_Path])]
killRefs [] _ = []
killRefs ((x, y):xs) i = (x, (filter (\ q -> not (q `pathEq` i)) y)) : (killRefs xs i)
    
pickInfimum :: [(TransitionTable, [ID_Path])] -> Maybe TransitionTable
pickInfimum [] = Nothing
pickInfimum ((t, _):[]) = Just t
pickInfimum ((t, []):ts) = Just t
pickInfimum ((t, xs):ts) = pickInfimum ts

eliminateSubModEntities :: [ID_Path] -> PVSstateDec -> (TransitionTable, [ID_Path]) -> (TransitionTable, [ID_Path])
eliminateSubModEntities excludes st (t, ids) = (t, (filter (\ x -> (isNotSubModEntity (snd st) x) && (not (x `elem` excludes))) ids))

isNotSubModEntity :: [PVSstate] -> ID_Path -> Bool
isNotSubModEntity st (ID x) = True
isNotSubModEntity st (ID_Submod_Struct x y) = (isStruct'' st (ID_Submod_Struct x y))

isStruct'' :: [PVSstate] -> ID_Path -> Bool
isStruct'' [] _ = False
isStruct'' ((PVS_Reg i _ _):ss) i' = if i `pathEq` i' then True else isStruct'' ss i'
isStruct'' ((PVS_DWire i _ _):ss) i' = if i `pathEq` i' then True else isStruct'' ss i'
isStruct'' (s:ss) i' = isStruct'' ss i'


depTuples :: [PVSstateDec] -> [TransitionTable] -> TransitionTable -> (TransitionTable, [ID_Path])        
depTuples st tables (TransDWire i tree dv) = ((TransDWire i tree dv), deps)
  where
      deps = nubBy (pathEq) $ filterWires' tables (getReadsByOverTree (\ q -> True) tree)
depTuples st tables (TransFIFO i x y z) = ((TransFIFO i x y z), deps)
  where
      deps = nubBy (pathEq) $ filterWires' tables $ (getReadsByOverTree (\ q -> True) x) ++ (getReadsByOverTree (\ q -> True) y)
depTuples st tables (TransMod n ts) = ((TransMod n ts), deps)
  where
      deps = nubBy (pathEq) $ concat $ map getReads' ts
      getReads' (TransReg _ t) = filterWires' tables (getReadsByOverTree (\ q -> True) t)
      getReads' (TransVect _ _ ts) = concat $ map getReadsOverVector ts
      getReads' (TransStruct _ ts) = concat $ map getReads' ts
      getReads' (TransDWire _ t _) = filterWires' tables (getReadsByOverTree (\ q -> True) t) 
      getReads' (TransFIFO _ x y _) = filterWires' tables $ (getReadsByOverTree (\ q -> True) x) ++ (getReadsByOverTree (\ q -> True) y)
      getReads' (TransMod _ ts) = concat $ map getReads' ts
      getReadsOverVector (exp, tree) = filterWires' tables $ (getReadsBy mkBSVModule (\ q -> True) [] exp) ++ (getReadsByOverTree (\ q -> True) tree)
depTuples st tables (TransReg x y) = ((TransReg x y), deps)
  where
    deps = nubBy (pathEq) $ filterWires' tables (getReadsByOverTree (\ q -> True) y) 
depTuples _ _ x = error $ show x

isWireOrFifo'' :: [PVSstateDec] -> ID_Path -> Bool
isWireOrFifo'' ss i = or $ map (\ x -> isWireOrFifo x i) ss

isWireOrFifo :: PVSstateDec -> ID_Path -> Bool
isWireOrFifo (_, ss) i = or $ map (isWireOrFifo' i) ss

isWireOrFifo' :: ID_Path -> PVSstate -> Bool
isWireOrFifo' i (PVS_DWire i' _ _) = i `pathEq` i'
isWireOrFifo' i (PVS_Fifo _ i' _) = i `pathEq` i'
isWireOrFifo' _ x = False

isDWire :: PVSstateDec -> ID_Path -> Bool
isDWire (_, ss) i = (or $ map (isDWire' i) ss)
    -- trace ("[isDWire] - " ++ (show i) ++ "\n\n" ++ (show ss) ++ "\n\n result = " ++ (show (or $ map (isDWire' i) ss))) $ or $ map (isDWire' i) ss

isDWire' :: ID_Path -> PVSstate -> Bool
isDWire' i (PVS_DWire i' _ _) = i `pathEq` i'
isDWire' _ x = False
      
getReadsByOverTree :: (ID_Path -> Bool) -> SpecificTree -> [ID_Path]
getReadsByOverTree f (SpecStem gd (Left tExp) fTree) = (getReadsBy mkBSVModule f [] gd) ++ (getReadsBy mkBSVModule f [] tExp) ++ (getReadsByOverTree f fTree)
getReadsByOverTree f (SpecStem gd (Right tTree) fTree) = (getReadsBy mkBSVModule f [] gd) ++ (getReadsByOverTree f tTree) ++ (getReadsByOverTree f fTree)
getReadsByOverTree f (SpecLeaf gd tExp fExp) = (getReadsBy mkBSVModule f [] gd) ++ (getReadsBy mkBSVModule f [] tExp) ++ (getReadsBy mkBSVModule f [] fExp)
getReadsByOverTree f (SpecEx exp) = getReadsBy mkBSVModule f [] exp
            
getTrans :: [TransitionTable] -> ID_Path -> TransitionTable
getTrans [] _ = error "Error! Transition Table not found!!"
getTrans ((TransMod n ts):xs) (ID_Submod_Struct x y) 	= if x == n then (TransMod n ts) else getTrans xs (ID_Submod_Struct x y)
getTrans ((TransReg n t):xs) (ID x)			= if (ID x) == n then (TransReg n t) else getTrans xs (ID x)
getTrans ((TransVect n s t):xs) (ID_Vect x n')		= if (ID x) == n then (TransVect n s t) else getTrans xs (ID_Vect x n')
getTrans ((TransVect n s t):xs) (ID x)			= if (ID x) == n then (TransVect n s t) else getTrans xs (ID x)
getTrans ((TransStruct n ts):xs) (ID_Submod_Struct x y) = if x == n then (TransStruct n ts) else getTrans xs (ID_Submod_Struct x y)
getTrans ((TransDWire n t z):xs) (ID x)			= if (ID x) == n then (TransDWire n t z) else getTrans xs (ID x)
getTrans (x:xs) n = getTrans xs n
		      

--TransMod Name [TransitionTable] | TransReg ID_Path SpecificTree | TransVect ID_Path Size [(Expression, SpecificTree)] | TransStruct Name [TransitionTable] | TransDWire ID_Path
      

showPVSvariable :: PVSPackage -> TransitionVar -> String
showPVSvariable uni (typ , noms) = (intercalate ", " noms) ++ " : var " ++ (showPVSType uni typ) 

-- showPVSTransition :: PVSPackage -> String -> Bool -> ModuleTransition -> String
-- showPVSTransition uni nom isValueTransition (methods, mArgs, transTables)  = concat [ header
--                                                      , wireLets 
--                                               --       , vectorStates 
--                                                      , header2 
--                                                      , transStates
--                                                      , footer
--                                                      ]
--   where
--     header = if (not isValueTransition) 
-- 		then if (null methods) 
--                        then nom ++ "_t (pre, post" ++ (methodArgs ) ++ ") : bool =\n\t\t"
--                        else nom ++ "_t_m_" ++ (convertMeths2Name methods) ++ " (pre, post" ++ (methodArgs ) ++ ") : bool =\n\t\t"
--                 else if (null methods) 
--                        then nom ++ "_t_val (pre" ++ (methodArgs ) ++ ") : "++(nom)++" =\n\t\t"
--                        else nom ++ "_t_m_" ++ (convertMeths2Name methods)++ "_val (pre" ++ (methodArgs ) ++ ") : "++(nom)++" =\n\t\t  "
--     wireLets = genLets uni (extractWires' (transTables))
--     -- vectTables = getVectorTables transTables
--     header2 = if (not isValueTransition) 
-- 		  then "(post = pre with\n\t\t  ["
-- 		  else "(pre with\n\t\t  ["
--     transStates = (showPVSTransStates uni"pre" "" (killWires transTables))
--     footer = "\n\t\t  ]\n\t\t)"
--     methodArgs = if null mArgs 
--                    then [] 
-- 		   else ", " ++ (intercalate ", " ( map (\(n,t) -> n++" : "++(showPVSType t)) mArgs))
--     vectorStates = if (null vectTables) 
-- 	then []
--         else "\n\t\t" ++ (intercalate "\n\t\tAND " (map (showPVSVectorState uni) vectTables)) ++ "\n\t\tAND "

-- getVectorTables :: [TransitionTable] -> [TransitionTable]
-- getVectorTables [] = []
-- getVectorTables ((TransVect x y z):xs) = (TransVect x y z) : (getVectorTables xs)
-- getVectorTables (x:xs) = (getVectorTables xs)

-- showPVSVectorState :: PVSPackage -> TransitionTable -> String 
-- showPVSVectorState pkg (TransVect nom size tabls) = concat [ header, tables, footer ]
--   where
--     header = "(Forall (i : nat) : \n\t\t  i < " ++ (show size) ++ " AND\n\t\t  post`" ++ (showIDPath pkg Nothing nom) ++ "(i) = if "
--     tables =  (intercalate "\n\t\t    else if" (map (showVectTable pkg nom) tabls))
--     footer = "\n\t\t    else pre`" ++ (showIDPath pkg Nothing nom) ++ "(i)\n\t\t endif)"
  
-- showVectTable :: PVSPackage -> ID_Path -> (Expression, SpecificTree) -> String
-- showVectTable pkg (ID nom) (ex, tr) = "(i = "++(showPVSExpression pkg ex Nothing)++") then " ++ (showPVSTransTree pkg (ID_Vect nom ex) "pre" "      " tr)
    
genLets :: PVSPackage -> [TransitionTable] -> String -> String -> String
genLets _ [] _ _ = ""
genLets uni xs prefix i = concat [ header
                    , synonyms
                    , footer
                    ]
  where 
    header = "LET "
    synonyms = intercalate "\n\t\t  , " (concat (map (\ a -> showLet uni a prefix i ) xs))
    footer = "\n\t\tIN "

showLet :: PVSPackage -> TransitionTable -> String -> String -> [String]
showLet uni (TransDWire nom tree dv) prefix i = ((showIDPath uni [] Nothing nom i) ++ " : " ++ (showPVSType uni typ) ++ " = " ++ (showPVSTransTree uni nom prefix "    " (Just dv) typ tree i)):[]
  where
      mod = getState (pvs_state uni) (showIDPath uni [] Nothing nom i)
      typ = getTypeFromState uni (pvs_typedefs uni) (pvs_state uni) [] Nothing (ID_Submod_Struct (fst mod) nom)
showLet uni (TransFIFO nom enq deq clear) prefix i = [ namo ++ "_enq = " ++ (showPVSTransTree uni nom prefix "    " (Just (Identifier (ID_Submod_Struct prefix nom))) typ enq i)
                                            , namo ++ "_deq = " ++ (showPVSTransTree uni nom prefix "    " (Just (Identifier (ID (namo ++ "_enq")))) typ deq i)
                                            , namo ++ "_clear = " ++ (showPVSTransTree uni nom prefix "    " (Just (Identifier (ID (namo ++ "_deq")))) typ clear i)
                                            ]
  where
    tracy = "[showLet] clearTree - " ++ (show clear)
    namo = (showIDPath uni [] Nothing nom i)
    typ = getTypeFromState uni (pvs_typedefs uni) (pvs_state uni) [] Nothing nom
    
    
killWires :: [TransitionTable] -> [TransitionTable]
killWires ((TransDWire _ _ _):xs) = (killWires xs) 
killWires ((TransMod x ts):xs) = (TransMod x (killWires ts)):(killWires xs) 
killWires (x:xs) = x : (killWires xs)
killWires [] = []

killWires' :: TransitionTable -> TransitionTable
killWires' (TransMod x ts) = (TransMod x (killWires ts))


extractWires :: PVStransition -> [TransitionTable]
extractWires (_,_,_,xs) = extractWires' xs

extractWires' :: [TransitionTable] -> [TransitionTable]
extractWires' [] = []
extractWires' ((TransDWire x y z):xs) = (TransDWire x y z) : (extractWires' xs)
extractWires' ((TransMod x ts):xs) = (extractWires' xs)
extractWires' ((TransStruct x ts):xs) = (extractWires' ts) ++ (extractWires' xs)
extractWires' (x:xs) = (extractWires' xs)

extractFifos :: [TransitionTable] -> [TransitionTable]
extractFifos  [] = []
extractFifos ((TransFIFO x y z w):xs) = (TransFIFO x y z w) : (extractFifos xs)
extractFifos ((TransMod x ts):xs) = (extractFifos xs)
extractFifos ((TransStruct x ts):xs) = (extractFifos ts) ++ (extractFifos xs)
extractFifos (x:xs) = (extractFifos xs)

convertMeths2Name :: [String] -> String
convertMeths2Name = intercalate "_m_"

showPVSTransStateCluster :: PVSPackage -> PVSstateDec-> String -> String -> [ID_Path] -> TransitionTable -> String -> Bool ->  String
showPVSTransStateCluster uni st prefix spacer ex (TransMod nom tables) i mode = if (noStateTables tables)
    then "\n\t\t" ++ header
    else "\n\t\t" ++ header ++ " with\n\t\t  " ++ spacer ++ "  [ " ++ (showPVSTransStates uni newPrefix (spacer ++ "  ") ex' tables i )  ++ "\n\t\t  " ++ spacer ++ "  ]"
    where
        tracy = "[showPVSTransStateCluster] nom = " ++ (show nom) ++ "prefix lets = " ++ prefixLets
        tracy2 = "[showPVSTransStateCluster] nom = " ++ (show nom) ++ "\nNo state tables? " ++ (show (noStateTables tables)) ++ "\ntable hierarchy = " ++ (showTransitionHierarchy tables "  ")
        -- tracy = "[showPVSTransStateCluster] nom = " ++ (show nom) ++ "\ntable hierarchy = " ++ (showTransitionHierarchy tables "  ") ++ "\nPrefixed Lets = " ++ (show (map justTheName' lets))
        lets = (extractWires' tables) ++ (extractFifos tables)
        ex' = ex ++ (getExclusionNames lets)
        tables' = getTransitionGlobalTables i (transitions uni)
        newPrefix = if (prefix /= "") then (prefix ++ "`" ++ nom) else nom
        prefixLets = genLets uni ( orderLets uni st ex tables' $ lets) (prefix ++ "`" ++ nom) i
        -- trace ("[showPVSTransStateCluster] let prefix -" ++ (show (prefix ++ "`" ++ nom))) $ 
        header = prefixLets ++ (if not (null prefix) then prefix ++ "`" ++ nom else nom)
        -- if mode -- && nom == "pre"
                    -- then "pre "
                    -- else (if (nom == "pre") then "post" else nom) ++ (if (nom == "pre") then " = " else " := " ) ++ prefixLets ++ (if not (null prefix) then prefix ++ "`" ++ nom else nom)
showPVSTransStateCluster uni st prefix spacer ex (TransStruct nom tables) i mode = if (noStateTables tables)
    then "\n\t\t" ++ header
    else "\n\t\t" ++ header ++ " with\n\t\t  " ++ spacer ++ "  [ " ++ (showPVSTransStates uni newPrefix (spacer ++ "  ") ex' (killSurfaceWires tables) i )  ++ "\n\t\t  " ++ spacer ++ "  ]"
    where
        lets = (extractWires' tables) ++ (extractFifos tables)
        ex' = ex ++ (getExclusionNames lets)
        tables' = getTransitionGlobalTables i (transitions uni)
        newPrefix = if (prefix /= "") then (prefix ++ "`" ++ nom) else nom
        prefixLets = genLets uni ( orderLets uni st ex tables' $ lets ) (prefix ++ "`" ++ nom) i
        -- trace ("[showPVSTransStateCluster] let prefix -" ++ (show (prefix ++ "`" ++ nom))) $ 
        header = if mode && nom == "pre"
                    then "pre "
                    else (if (nom == "pre") then "post" else nom) ++ (if (nom == "pre") then " = " else " := " ) ++ prefixLets ++ (if not (null prefix) then prefix ++ "`" ++ nom else nom)                    

noStateTables :: [TransitionTable] -> Bool
noStateTables [] = True
noStateTables ((TransDWire _ _ _):ts) = (noStateTables ts)
noStateTables (t:ts) = False
                    
getExclusionNames :: [TransitionTable] -> [ID_Path]                    
getExclusionNames [] = []
getExclusionNames ((TransDWire n _ _):ts) = n : (getExclusionNames ts)
getExclusionNames ((TransFIFO n _ _ _):ts) = (map (\ x -> addSuffix n x) ["_enq","_deq","_clear"]) ++ (getExclusionNames ts)
            
addSuffix :: ID_Path -> String -> ID_Path
addSuffix (ID x) y = (ID (x ++ y))
addSuffix (ID_Submod_Struct x y) z = (ID_Submod_Struct x (addSuffix y z))
            
killSurfaceWires :: [TransitionTable] -> [TransitionTable]
killSurfaceWires [] = []
killSurfaceWires ((TransDWire _ _ _):xs) = (killWires xs) 
killSurfaceWires (x:xs) = x : (killWires xs)

getTransitionGlobalTables :: String -> [PVStransition] -> [TransitionTable]
getTransitionGlobalTables i [] = error $ "Error! No transition with index " ++ i
getTransitionGlobalTables i ((i', _, _, tables):ts) = if (i == (show i')) then tables else getTransitionGlobalTables i ts
        
showPVSTransStates :: PVSPackage -> String -> String -> [ID_Path] -> [TransitionTable] -> String -> String
showPVSTransStates uni prefix spacer ex tables i = intercalate ("\n\t\t  "++ spacer ++", ") $ killVoids $ map (\ a -> showPVSTransState uni prefix spacer ex a i) tables
--    where 
--        tables' = elimRedundantEntries tables 

elimRedundantEntries :: [TransitionTable] -> [TransitionTable]
elimRedundantEntries [] = []
elimRedundantEntries ((TransReg i (SpecEx (Identifier i'))):xs) = if i `tailEq` i'
    --trace ("[T] elimRedundantEntries - \ni = " ++ (show i) ++ "\ni' = " ++ (show i') ++ "\nresult = " ++ (show (i `tailEq` i'))) $ if i `tailEq` i'
                                                   then elimRedundantEntries xs 
                                                   else (TransReg i (SpecEx (Identifier i'))) : (elimRedundantEntries xs )
elimRedundantEntries (x:xs) = x : (elimRedundantEntries xs)
    
tailEq :: ID_Path -> ID_Path -> Bool
tailEq x y = tailEq' x' y' 
    where 
        x' = invertID x
        y' = invertID y

tailEq' :: [String] -> [String] -> Bool
tailEq' [] _ = True
tailEq' _ [] = True
tailEq' (x:xs) (y:ys) = if x == y 
                           then tailEq' xs ys
                           else False
        
invertID :: ID_Path -> [String]
invertID (ID x) = x : []
invertID (ID_Submod_Struct x y) = (invertID y) ++ (x:[]) 


killVoids :: [String] -> [String]
killVoids [] = []
killVoids ((""):xs) = killVoids xs
killVoids (x:xs) = x : (killVoids xs)

unMaybeList :: [Maybe a] -> [a]
unMaybeList [] = [] 
unMaybeList ((Just x):xs) = x : (unMaybeList xs)
unMaybeList ((Nothing):xs) = unMaybeList xs

showPVSTransState :: PVSPackage -> String -> String -> [ID_Path] -> TransitionTable -> String -> String 
showPVSTransState uni prefix spacer ex (TransMod nom tables) i = if (not (null tables)) 
    then nom ++ " := " ++ (showPVSTransStateCluster uni st prefix spacer ex (TransMod nom tables) i False)
    else (nom ++ " := " ++ "pre`" ++ prefix ++ "`" ++ nom)
    where 
        n = nom
        st = (n, (maybe (error "I am Error!") (id) (lookup n (pvs_state uni))))
showPVSTransState uni prefix spacer ex (TransStruct nom tables) i = showPVSTransStateCluster uni st prefix spacer ex (TransStruct nom tables) i False
    where 
        n = nom
        st = (n, (maybe (error "I am Error!") (id) (lookup n (pvs_state uni))))
showPVSTransState uni prefix spacer ex (TransReg nom tree) i = nom' ++ (showPVSTransTree uni nom prefix spacer Nothing typ tree i)
    {-if ( nom' == "rg_InitReqDataCount") 
                                                                  then nom' ++ (showPVSTransTree uni nom prefix spacer Nothing typ tree i)
                                                                  else nom' ++ (showPVSTransTree uni nom prefix spacer Nothing typ tree i)-}
    where 
      nom' = ((showIDPath uni [] rep (getIDRoot nom) i) ++ " := ")
      tracy = "[showPVSTransState] - " ++ (show tree)
      rep = (Just (prefix, showIDPath uni [] Nothing nom i))
      typ = getTypeFromState uni (pvs_typedefs uni) (pvs_state uni) [] rep nom 
showPVSTransState uni prefix spacer ex (TransVect nom size subTrees) i = (showIDPath uni [] rep (getIDRoot nom) i) ++ " := " ++ prefix ++ "`" ++ (showIDPath uni [] rep nom i) ++ " with" ++ "\n\t\t  " ++ spacer ++ "  [ " ++ (showPVSVectorState uni nom prefix (spacer ++ "  ") typ subTrees i) ++ "\n\t\t" ++ spacer ++ "    ]"  
  where
      typ = getTypeFromState uni (pvs_typedefs uni) (pvs_state uni) [] rep nom
      rep = (Just (prefix, showIDPath uni [] Nothing nom i))
showPVSTransState uni prefix spacer ex (TransFIFO nom _ _ _) i = nomo ++ " := " ++ nomo ++ "_clear"
    where 
        nomo = (showIDPath uni [] (Just (prefix, showIDPath uni [] Nothing nom i)) (getIDRoot nom) i)
showPVSTransState _ _ _ _ (TransDWire _ _ _) _ = ""


showPVSVectorState :: PVSPackage -> ID_Path -> String -> String -> PVSType -> [(Expression, SpecificTree)] -> String -> String 
showPVSVectorState uni nom prefix spacer typ (xs) i = intercalate ("\n\t\t" ++ spacer ++ ", ") $ map (\ (y,z) -> (showPVSExpression uni [] Nothing (Just typ) y (Just (prefix, showIDPath uni [] Nothing nom i)) i) ++ " := " ++ (showPVSTransTree uni (addVectExptoID nom y) prefix spacer Nothing typ z i) ) xs

addVectExptoID :: ID_Path -> Expression -> ID_Path
addVectExptoID (ID_Submod_Struct x y) z = (ID_Submod_Struct x (addVectExptoID y z))
addVectExptoID (ID x) y = (ID_Vect x y)

getIDRoot :: ID_Path -> ID_Path 
getIDRoot (ID_Submod_Struct x y ) = getIDRoot y
getIDRoot (ID x) = (ID x)

showPVSTransTree :: PVSPackage -> ID_Path -> String -> String -> Maybe DefaultValue -> PVSType -> SpecificTree -> String-> String
showPVSTransTree uni nom prefix spacer dv typ (SpecStem gd (Left exp) fTree) i = showIf ++ space2 ++ showThen ++ space2 ++ showElse ++ space1 ++ "endif"
  where
    showIf =  "if ("++ (showPVSExpression uni [] Nothing (Just typ) gd (Just (prefix, showIDPath uni [] Nothing nom i)) i) ++ ")" 
    showThen = "then " ++ (showPVSExpression uni [] dv (Just typ) (addPreIfMissing' exp) (Just (prefix, showIDPath uni [] Nothing nom i)) i)
    showElse = "else " ++ (showPVSTransTree uni nom prefix (spacer ++ "  ") dv typ fTree i) 
    space1 = "\n\t\t  " ++ spacer
    space2 = "\n\t\t  " ++ spacer ++ "  "
showPVSTransTree uni nom prefix spacer dv typ (SpecStem gd (Right tTree) fTree) i = showIf ++ space2 ++ showThen ++ space2 ++ showElse ++ space1 ++ "endif"
  where
    showIf =  "if ("++ (showPVSExpression uni [] Nothing (Just typ) gd (Just (prefix, showIDPath uni [] Nothing nom i)) i) ++ ")" 
    showThen = "then " ++ (showPVSTransTree uni nom prefix (spacer ++ "  ") dv typ tTree i) 
    showElse = "else " ++ (showPVSTransTree uni nom prefix (spacer ++ "  ") dv typ fTree i) 
    space1 = "\n\t\t  " ++ spacer
    space2 = "\n\t\t  " ++ spacer ++ "  "
showPVSTransTree uni nom prefix spacer dv typ (SpecLeaf gd texp fexp) i = showIf ++ space2 ++ showThen ++ space2 ++ showElse ++ space1 ++ "endif" 
  where
    showIf =  "if ("++ (showPVSExpression uni [] Nothing (Just typ) gd (Just (prefix, showIDPath uni [] Nothing nom i)) i) ++ ")" 
    showThen = "then " ++ (showPVSExpression uni [] dv (Just typ) (addPreIfMissing' texp) (Just (prefix, showIDPath uni [] Nothing nom i)) i) 
    showElse = "else " ++ (showPVSExpression uni [] dv (Just typ) (addPreIfMissing' fexp) (Just (prefix, showIDPath uni [] Nothing nom i)) i) 
    space1 = "\n\t\t  " ++ spacer
    space2 = "\n\t\t  " ++ spacer ++ "  "
showPVSTransTree uni nom prefix spacer dv typ (SpecEx x) i = (showPVSExpression uni [] dv (Just typ) (addPreIfMissing' x)  (Just (prefix, showIDPath uni [] Nothing nom i)) i) 

addPreIfMissing' :: Expression -> Expression
addPreIfMissing' (Identifier (ID x)) = if ("wr_" `isPrefixOf` x || "value_" `isPrefixOf` x)
    then (Identifier (ID x))
    else Identifier $ addPreIfMissing (ID x)
addPreIfMissing' (Identifier x) = Identifier $ addPreIfMissing x
addPreIfMissing' x = x

showPVSTop :: PVSPackage -> PackageName -> [PVStransition] -> Maybe TSPpackage -> String
showPVSTop uni nom ts tsp = intercalate "\n\n\t" [ header 
                                            , imports 
                                            , vars
                                            , tabspecs
                                            , doc
                                            , theor
                                            , consistencies
                                            , footer
                                            ]
  where 
    header = "Theorems[(IMPORTING Time) delta_t:posreal] : theory\n\nbegin"
    imports = if (null ts) then "importing TypeDefinitions" else "importing Transitions\n\timporting ClockTick[delta_t]\n\n\tt: VAR tick"
--    maybeImports = intercalate "\n\t" $ map genMaybeImport $ nub $ justTheMaybes $ gatherMaybeTypes uni 
    vars = if (null ts) 
              then ""
              else if (stateless (pvs_packageName uni) (pvs_state uni)) 
                then "" 
                else intercalate "\n\t" $ ("s : VAR [tick -> "++ nom ++"]") : ("pre, post, "++nom++"_var : VAR " ++ nom) : []
        
        
        --trace ("[T] showPVSTop : " ++ (show (null ts))) $ if ((null ts) || (stateless (pvs_packageName uni) (pvs_state uni))) then "" else intercalate "\n\t" $ ("s : VAR [tick -> "++ nom ++"]") : ("pre, post, "++nom++"_var : VAR " ++ nom) : [] -- (gatherPredVars nom trans)
    tabspecs = generateTabspecs uni tsp
    doc = genDocumentation uni ts
    theor = showProofTheory uni nom ts tsp
    --modTrans = (\(_,_,_,x) -> x ) trans
    consistencies = intercalate "\n\n\t" $ genConsTheorems uni ts nom
    footer = "\nend Theorems"

genConsTheorems :: PVSPackage -> [PVStransition] -> String -> [String]
genConsTheorems uni ts nom = if null ts 
                                then [] 
                                else if (stateless (pvs_packageName uni) (pvs_state uni)) 
                                    then []
                                    else map (genConsTheorem uni nom) groupedTransitions
    where
      groupedTransitions = groupByArgs uni ts

      
stateless :: String -> [PVSstateDec] ->  Bool
stateless n xs = stateless' $ maybe (error (show n)) (id) (lookup n xs)
      
stateless' :: [PVSstate] -> Bool
stateless' [] = True
stateless' ((PVS_DWire _ _ _):xs) = stateless' xs
stateless' (x:xs) = False

genConsTheorem :: PVSPackage -> String -> [PVStransition] -> String
genConsTheorem uni nom ts = intercalate "\n\t" [proof, l1, l2, l3, l4]
  where 
    proof = genConsProof ts
    is = map (\ (x,_,_,_) -> x) ts
    args = concat $ map snd $ (\ (_,x,_,_) -> x) $ head ts
    args1 = if (null args) then "" 
                           else ", " ++ (intercalate ", " (map (displayArgs uni) args))
    args2 = if (null args) then "" 
			   else ", " ++ (intercalate ", " (map fst args))
    l1 = "consistency_" ++ (intercalate "_" (map show is)) ++ " : Theorem"
    l2 = "  FORALL (i : nat, pre : " ++ nom ++ args1 ++ ") :"
    l3 = "    EXISTS (post : "++nom++") : "
    l4 = "      transition(i, pre, post" ++ args2 ++")"

genConsProof :: [PVStransition] -> String
genConsProof ts = intercalate "\n%|- " [l1, l2, l3, l4, l5, l6, l7]
  where
    is = map (\ (x,_,_,_) -> x) ts
    args = concat $ map snd $ (\ (_,x,_,_) -> x) $ head ts
    args1 = if (null args) then "" 
			   else (++) ", " $ intercalate ", " $ map displayArgs' $ map fst args -- (intercalate "!1, " (map fst args)) ++ "!1"
    l1 = "\n%|- consistency_" ++ (intercalate "_" (map show is)) ++ " : PROOF"
    l2 = "(then (skolem!)"
    l3 = "      (inst + \"transition_val (i!1,pre!1"++ args1 ++ ")\")"
    l4 = "      (rewrite transition)"
    l5 = "      (rewrite transition_val)"
    l6 = "      (assert))"
    l7 = "QED"

displayArgs' :: String -> String
displayArgs' xs = maybe (xs ++ "!1" ) (\ x -> nom ++ "!" ++ x) num
  where
    nom = reverse $ if (not $ null boink) then tail boink else []
    boink = dropWhile (/= '_') $ reverse xs
    num = getNumericSuffix xs 
    
    
getNumericSuffix :: String -> Maybe String
getNumericSuffix [] = Nothing
getNumericSuffix xs = if (and (map isNumber suffix)) then Just suffix else Nothing
  where 
    suffix = show $ (+1) $ read $ reverse $ takeWhile (\ x -> x /= '_') $ reverse xs
    
genDocumentation :: PVSPackage -> [PVStransition] -> String
genDocumentation uni ts = if (null ts) 
                             then intercalate "\n%  " [header', scheduleDisplay, f1', f2', f3']
                             else if (stateless (pvs_packageName uni) (pvs_state uni)) 
                                then intercalate "\n%  " [header', scheduleDisplay, f1', f2', f3']
                                else intercalate "\n%  " [header, scheduleDisplay, footer, f2, f3, f4, f5, f6, f7]
  where
    header = "\n%  The following transitions have been scheduled."
    header' = "\n%  The following schedules have been generated."
    scheduleDisplay = if (null ts) then "\n% None!" else intercalate "\n%  " $ map (displaySchedule uni) ts
    footer = "The following arguments must be supplied to invoke the transition predicate."
    f2 = "\tindex -> The index number corresponding to the schedule you wish to invoke"
    f3 = "\tpre -> The pre-state of the transition predicate"
    f4 = "\tpost -> The post-state of the transition predicate"
    f5 = "\tmethod arguments -> supply the arguments given in the above list of schedules, in the order they appear."
    f6 = ""
    f7 = "For an example of how this is intended to work, take a look at the auto-generated consistency theorems below."
    f1'= "Apparently, however, although the module under examination is exists, it is not stateful!"
    f2'= "This might be because it is full of Wires and not state holding elements like Registers etc."
    f3'= "As such, no transitions have actually been generated.  "
    
displaySchedule :: PVSPackage -> PVStransition -> String
displaySchedule uni (i, args, mths, _) = intercalate "\n%  " [l1, l2, "" ]
  where
    mths' = map fst args
    l1 = if (null mths') 
	    then (show i) ++ "\t: Methods Invoked = {none} "
	    else (show i) ++ "\t: Methods Invoked = {" ++ (intercalate ", " mths') ++ "}"
    args' = concat $ map snd args
    args'' = unlines $ (\ (y:z) -> y : (map (\ x -> "%" ++ x) z)) $ lines $ intercalate ", " $ map (displayArgs uni) args'
    l2 = if (null args')
	    then "\t: Input Args = {none} " 
	    else "\t: Input Args = {" ++ args'' ++ "%}"
    

displayArgs :: PVSPackage -> (MethodArg, PVSType) -> String    
displayArgs uni (a, t) = a ++ " : " ++ (showPVSType uni t)
    
justTheMaybes :: [PVSType] -> [PVSType]
justTheMaybes ((PVS_Maybe x):xs) = (PVS_Maybe x) : (justTheMaybes xs)
justTheMaybes (x:xs) = (justTheMaybes xs)
    
showProofTheory :: PVSPackage -> PackageName -> [PVStransition] -> Maybe TSPpackage -> String
showProofTheory uni _ _ Nothing = "%test1 : theorem <antecedents>\n\t\t%implies <consequents>"
showProofTheory uni nom ts (Just tsp) = proof ++ "\n\n" ++ ( header ++ transitions ++ transHistory ++ tables {- ++ history -} ++ initialCondition ++ results)
  where 
    tracy = "[showProofTheory] nom = " ++ nom 
    iTrans = getInputTrans ts
    (ind, var, val, modtrans) = iTrans
    transo = (ind, var, val, modtrans)
    proof = genFuncProof nom
    pair = getPair uni (tsps tsp) (ind, var, val, modtrans)
    header = nom ++ "_REQ : THEOREM "
    transitions = "\n\t     " ++ ( gatherTransPred'' nom iTrans)
    transHistory = if (or (map (needsHistory uni) (transo:[]))) 
 		      then "\n\t AND " ++ gatherPastTransPred nom iTrans 
 		      else []
    tables = (++) "\n\t AND " $ intercalate "\n\t AND " $ map genTabSpecCall (tsps tsp)
    results = (++) "\n\t IMPLIES " $ pair (createEquivalencePairings uni nom) 
    initialCondition = (++) "\n\t AND " $ genInit nom

needsHistory :: PVSPackage -> PVStransition -> Bool
needsHistory uni (_,_,_,[]) = False
needsHistory uni (y,z,w,((TransReg n _):xs)) = if (isSuffixOf "_prev" nom) then True else needsHistory uni (y,z,w,xs)
  where 
    nom = showIDPath uni [] Nothing n ""
needsHistory uni (y,z,w,x:xs) = needsHistory uni (y,z,w,xs)
   
    
genInit :: String -> String 
genInit x = "(init(t) IMPLIES mk" ++ x ++ "(s(t)))" 
    
getInputTrans :: [PVStransition] -> PVStransition
getInputTrans [] = error "PVSgen Error! Input method transition not found during theorem generation!"
getInputTrans ((x, (("set_Inputs",ms):[]), y, z):xs) =  (x, (("set_Inputs", ms):[]), y, z)
getInputTrans ((w, x, y, z):xs) = getInputTrans xs
  where
      tracy = "[genInputTrans] everything = " ++ (show ((w, x, y, z):xs))
    
getPair :: PVSPackage -> [TSPTable] -> PVStransition -> ((ID_Path, String) -> String) -> String
getPair uni tsps trans transform = intercalate "\n\t     AND " (map transform pairs)
  where 
    outVars = map (\ (_,x,_,_,_,_) -> x ) tsps
    outs = map fst outVars
    meths = map (getGetMeth uni ((\ (_,_,x,_) -> x ) trans)) outs
    pairs = zip outs meths

createEquivalencePairings :: PVSPackage -> String -> (ID_Path, String) -> String
createEquivalencePairings uni nom (x, y) = (showPVSExpression uni [] Nothing badType (Identifier x) Nothing "") ++ "(next(t)) = " ++ y ++ "(0, s(next(t)), s(next(t)))"

    
createHistoryPairings :: String -> (String, String) -> String
createHistoryPairings nom (x, y) = x ++ "(t) = " ++ nom ++ "_" ++ y ++ "(s(t))"

getGetMeth :: PVSPackage -> [ValueMethod] -> ID_Path -> String 
getGetMeth _ [] _ = error "PVSgen Error!No matching value method found when attempting to match with table output variable during theorem generation!" 
getGetMeth uni ((methName, modName, _, _, _, _, _):xs) matcher 
  | (showIDPath uni [] Nothing matcher "") `isSuffixOf` methName  = methName
  | otherwise 					= getGetMeth uni xs matcher

showIDPath :: PVSPackage -> [LocalVar] -> Maybe (String, String) -> ID_Path -> String -> String
showIDPath pkg lv q (ID_Submod_Struct m p) i = m ++ "`" ++ (showIDPath pkg lv q p i)
showIDPath pkg _ q (ID x) i = x
showIDPath pkg lv q (ID_Vect x n) i = x ++ "(" ++ (showPVSExpression pkg lv Nothing badType n q i) ++ ")"  
  
showIDPath' :: PVSPackage -> [LocalVar] -> Maybe (String, String) -> ID_Path -> String -> String  
showIDPath' pkg lv q (ID_Submod_Struct m p) i = m ++ "_" ++ (showIDPath' pkg lv q p i)
showIDPath' pkg _ q (ID x) i = x
showIDPath' pkg lv q (ID_Vect x n) i = x ++ "(" ++ (showPVSExpression pkg lv Nothing badType n q i) ++ ")"  
  
genTabSpecCall :: TSPTable -> String
genTabSpecCall (nom, (_, outTime), _, _, invars, _) = nom ++ "(" ++ (intercalate "," invars) ++ ")" ++ (if (outTime == (N_Time 0)) then "(next(t))" else "(t)")
-- %% 
    
    
genFuncProof :: String -> String
genFuncProof nom = intercalate "\n%|- " [header, line1, footer]
  where
    header = "\n%|- " ++ nom ++ "_REQ : PROOF"
    line1 = "(then (grind))"
    footer = "QED"
    
gatherPastTransPred :: String -> PVStransition -> String
gatherPastTransPred nom (ind, methods, vals, tables) = nom ++ "_t_m_" ++ methodNames ++ " (s(pre(t)), s(t)" ++ methodArgs ++ ")"
  where
    mNames = map fst methods
    mArgs = concat $ map snd methods    
    methodNames = intercalate "_" mNames
    mArgs' =  map ( \ x -> init (reverse (dropWhile (/= '_') (reverse x)))) $ map fst mArgs
    methodArgs = if null mArgs' 
                   then [] 
		   else ", " ++ (intercalate "(t), " (mArgs')) ++ "(pre(t))"    
    
gatherTransPred'' :: String -> PVStransition -> String
gatherTransPred'' nom (ind, methods, vals, tables) = "transition (1, s(t), s(next(t))" ++ methodArgs ++ ")"
  where
    mNames = map fst methods
    mArgs = concat $ map snd methods
    methodNames = intercalate "_" mNames
    mArgs' =  map ( \ x -> init (reverse (dropWhile (/= '_') (reverse x)))) (map fst mArgs)
    mArgs'' = map ( \ x -> init (reverse (dropWhile (/= '_') (reverse x)))) mArgs'
    methodArgs = if null mArgs''
                   then [] 
		   else ", " ++ (intercalate "(t), " (mArgs'')) ++ "(t)"
    
genTabLine :: TSPTable -> String
genTabLine (nom, _, _, _, invars, _) = nom ++ "(" ++ (intercalate "," invars) ++ ")(t)" 
    
generateTabspecs :: PVSPackage -> Maybe TSPpackage -> String 
generateTabspecs uni (Nothing) = []
generateTabspecs uni (Just x) = intercalate "\n\n\t" [ vars, tabspecs ]
  where 
    vars = genTabVars uni (varDecs x)
    tabspecs = intercalate "\n\n\t" (map (genTabSpec uni) (tsps x))

genTabVars :: PVSPackage -> [TVarDec] -> String
genTabVars uni xs = intercalate "\n\t" (map (genTabVar uni) xs)

genTabVar :: PVSPackage -> TVarDec -> String
genTabVar uni (vars, typ) = if not (null vars') then (intercalate ", " vars') ++ " : VAR [ tick -> " ++ (showPVSType uni typ) ++ " ]" else ""
  where 
    vars' = filter (\x -> not (x == "t")) vars
    
genTabSpec :: PVSPackage -> TSPTable -> String
genTabSpec uni (nom, (outNom, outTime), expr, synonyms, invars, lins ) = intercalate "\n\t" $ unMaybeList [header, assignment, initializer, lets, actualContent, footer ]
  where 
    header = Just $ nom ++ "(" ++ (intercalate "," invars) ++ ")(t) : bool =" 
    assignment = Just $ "  " ++ (showPVSExpression uni [] Nothing badType (Identifier outNom) Nothing "")++ "("++ (showOutTime outTime) ++") = "
    initializer = if (expr /= (Literal (LitVoid))) 
		     then Just $ "    IF init(t) THEN " ++ (showPVSExpression uni [] Nothing badType expr Nothing "")
		     else Nothing
    lets = if (null synonyms) 
	      then if (expr /= (Literal (LitVoid))) 
	        then Just "    ELSE"
		else Nothing
	      else if (expr /= (Literal (LitVoid))) 
	        then Just $ "    ELSE LET " ++ (showReplacementsAsEquations uni synonyms) ++ " IN"
	        else Nothing
    actualContent = Just $ intercalate "\n\t" (("      TABLE":[]) ++ (map (genTableLine uni) lins) ++ ("      ENDTABLE":[]))
    footer = if (expr /= (Literal (LitVoid))) 
	       then Just "    ENDIF"
	       else Nothing
  
showOutTime :: Temporal -> String
showOutTime (N_Time n) = if (n < 0) 
	then "pre("++ (showOutTime (N_Time (n+1))) ++")"
	else if (n > 0) 
	  then "next("++ (showOutTime (N_Time (n-1))) ++")"
	  else "t"

badType :: Maybe PVSType
badType = (Just (PVS_Custom "Loto"))
	  
showReplacementsAsEquations :: PVSPackage -> [Replacement] -> String
showReplacementsAsEquations uni rs = intercalate ", " $ map (\ (exp1, exp2) -> (showPVSExpression uni [] Nothing badType exp1 Nothing "") ++ " = " ++ (showPVSExpression uni [] Nothing badType exp2 Nothing "")) rs

genTableLine :: PVSPackage -> TSPLine -> String
genTableLine uni (gd, ex) = "        | " ++ (showPVSExpression uni [] Nothing Nothing gd Nothing "") ++ " | " ++ (showPVSExpression uni [] Nothing Nothing ex Nothing "") ++ " ||" 
    
-- genConsTheorem :: String -> PVStransition -> String
-- genConsTheorem nom (ind, methods, vals, tables) = intercalate "\n\t\t" [proofscript, header, universal, existential, transition]
--   where
--     mNames = map fst methods
--     mArgs = map snd methods
--     proofscript = genConsProof nom (mNames, mArgs, tables)
--     header = if (null mNames)
--                then nom ++ "_t_consistency : Theorem "
--                else nom ++ "_t_m_" ++ (convertMeths2Name mNames)++ "_consistency : Theorem"
--     methodArgs = if null mArgs 
--                    then "pre" 
-- 		   else "pre, " ++ (intercalate ", " (map (\(n,t) -> n++"_nt : "++(showPVSType t)) mArgs))
--     universal = "FORALL (" ++ methodArgs ++ ") : "
--     existential = "EXISTS (post) : "
--     transition = gatherTransPred nom True (mNames, mArgs, tables)

-- genConsProof :: String -> PVStransition -> String
-- genConsProof nom (ind, methods, vals, tables) = intercalate "\n%|- " [header, line1, line2, line3, footer]
--   where
--     mNames = map fst methods
--     mArgs = map snd methods
--     header = "\n%|- " ++ if (null mNames)
--                then nom ++ "_t_consistency : PROOF"
--                else nom ++ "_t_m_" ++ (convertMeths2Name mNames)++ "_consistency : PROOF"
--     line1 = "(then (skolem!)"
--     line2 = "      (inst + \"" ++ (gatherTransPred' nom (mNames, mArgs, tables)) ++ "\")"
--     line3 = "      (grind))"
--     footer = "QED"

-- gatherPredVars :: String -> PVStransition -> [String]
-- gatherPredVars nom (vars, _, _) = (map gatherPredVar vars) ++ (map gatherTimelessPredVar vars)
    
-- gatherPredVar :: TransitionVar -> String
-- gatherPredVar (typ, vars) = (intercalate ", " vars) ++ " : VAR [tick -> " ++ (showPVSType typ) ++ "]"

-- gatherTimelessPredVar :: TransitionVar -> String
-- gatherTimelessPredVar (typ, vars) = (intercalate ", " (map (\ x -> x ++ "_nt" ) vars)) ++ " : VAR " ++ (showPVSType typ)

-- gatherStatePreds :: String -> String
-- gatherStatePreds nom = "mk"++ nom ++ " : (pre)"

-- gatherMethPreds :: String -> PVStransition -> [String]
-- gatherMethPreds nom (n, _, meths, _) = (map gatherMethPred (filter (isMethTop nom) meths))

isMethTop :: String -> ValueMethod -> Bool
isMethTop y (_,x,_,_,_,_,_) = x == y

-- gatherMethPred :: ValueMethod -> String 
-- gatherMethPred (name, name2, ret, exp, wires) = name2 ++"_"++ name ++" (" ++ name2 ++ "_var) : "++ (showPVSType ret)

-- gatherTransPreds :: String -> PVStransition -> [String]
-- gatherTransPreds nom (n, _, _, modTrans) =  (map (gatherTransPred nom False) modTrans)

-- gatherTransPred :: String -> Bool -> PVStransition -> String
-- gatherTransPred nom mode (ind, methods, vals, tables) = if null mNames 
--                                                 then nom ++ "_t (pre, post)"
--                                                 else nom ++ "_t_m_" ++ methodNames ++ " (pre, post" ++ methodArgs ++ ")"
--   where
--     mNames = map fst methods
--     mArgs = map snd methods
--     methodNames = intercalate "_m_" mNames
--     methodArgs = if null mArgs 
--                    then [] 
-- 		   else ", " ++ (intercalate ", " (if (not mode) then map (\(n,t) -> n) mArgs else (map (\(n,t) -> n++"_nt ") mArgs)))

-- gatherTransPred' :: String -> PVStransition -> String
-- gatherTransPred' nom (ind, methods, vals, tables) = if null mNames 
--                                                 then nom ++ "_t_val (pre!1)"
--                                                 else nom ++ "_t_m_" ++ methodNames ++ "_val (pre!1" ++ methodArgs ++ ")"
--   where
--     mNames = map fst methods
--     mArgs = map snd methods
--     methodNames = intercalate "_" mNames
--     methodArgs = if null mArgs 
--                    then [] 
-- 		   else ", " ++ (intercalate ", " ((map (\(n,t) ->  n++"_nt!1 : " ++ (showPVSType t) ) mArgs)))

-- | Converts a lexer-bound constant declaration to a string in BSV syntax.
showPVSConstantDeclaration :: PVSPackage -> PVSConstantDec -> String
showPVSConstantDeclaration uni (nom, typ, lit) = nom ++ " : " ++ (showPVSType uni typ) ++ " = " ++ shownLit
  where 
    shownLit = showPVSExpression uni [] Nothing (Just typ) lit (Nothing) ""

showPVSTypedef :: PVSPackage -> PVSTypeDef -> String
showPVSTypedef uni (PVS_Synonym nom typ) = line1 ++ "\n\n\t" ++ line2
  where
    line1 = safetynom ++ " : type = " ++ (showPVSType uni typ)
    init = kludgeInit uni typ 
    line2 = "mk" ++ safetynom ++ " : " ++ safetynom ++ " = " ++ showPVSExpression uni [] Nothing (Just typ) init (Nothing) "" 
    safetynom = if nom == "Type" then "Tpye" else nom
showPVSTypedef uni (PVS_Enumeration nom enums) =  nom ++ " : type = {" ++ (intercalate ", " enums) ++ "}"
showPVSTypedef uni (PVS_Struct nom xs) = nom ++ " : type = \n\t [# " ++ (intercalate "\n\t  , " (map (\ (x,y) -> x ++ " : " ++ (showPVSType uni y)) xs)) ++ "\n\t #] "

-- | Converts an Expression into a string for use by the BSV file generator
showPVSExpression :: PVSPackage ->  [LocalVar] -> Maybe DefaultValue -> Maybe PVSType -> Expression -> Maybe (String, String) -> String -> String 
showPVSExpression uni lvars dv typ (Negative x) z i 	= "( - " ++ (showPVSExpression uni lvars dv typ x z i) ++ ")" 
showPVSExpression uni lvars dv typ (Not x) z i		= "(NOT " ++ (showPVSExpression uni lvars dv typ x z i) ++ ")" 
showPVSExpression uni lvars dv typ (Equals x y) z i	= "( " ++ (showPVSExpression uni lvars dv typ x z i) ++" = "++ (showPVSExpression uni lvars dv typ y z i) ++ " )" 
showPVSExpression uni lvars dv typ (NotEquals x y) z i	= "( " ++ (showPVSExpression uni lvars dv typ x z i) ++" /= "++ (showPVSExpression uni lvars dv typ y z i) ++ " )"
showPVSExpression uni lvars dv typ (GreaterEquals x y) z i = "( " ++ (showPVSExpression uni lvars dv typ x z i) ++" >= "++ (showPVSExpression uni lvars dv typ y z i) ++ " )"
showPVSExpression uni lvars dv typ (LessEquals x y) z i	= "( " ++ (showPVSExpression uni lvars dv typ x z i) ++" <= "++ (showPVSExpression uni lvars dv typ y z i) ++ " )"
showPVSExpression uni lvars dv typ (Greater x y) z i	= "( " ++ (showPVSExpression uni lvars dv typ x z i) ++" > "++ (showPVSExpression uni lvars dv typ y z i) ++ " )"
showPVSExpression uni lvars dv typ (Less x y) z i 	= "( " ++ (showPVSExpression uni lvars dv typ x z i) ++" < "++ (showPVSExpression uni lvars dv typ y z i) ++ " )"
showPVSExpression uni lvars dv typ (And x y) z i		= "( " ++ (showPVSExpression uni lvars dv typ x z i) ++" AND "++ (showPVSExpression uni lvars dv typ y z i) ++ " )"
showPVSExpression uni lvars dv typ (Or x y) z i   	= "( " ++ (showPVSExpression uni lvars dv typ x z i) ++" OR "++ (showPVSExpression uni lvars dv typ y z i) ++ " )"
showPVSExpression uni lvars dv typ (BitwiseAND x y) z i	= "( bwAND ("++(showPVSExpression uni lvars dv typ x z i)++","++(showPVSExpression uni lvars dv typ y z i)++"))"
showPVSExpression uni lvars dv typ (BitwiseOR x y) z i	= "( bwOR ("++(showPVSExpression uni lvars dv typ x z i)++","++(showPVSExpression uni lvars dv typ y z i)++"))" 
showPVSExpression uni lvars dv typ (BitwiseXOR x y) z i	= "( bwXOR ("++(showPVSExpression uni lvars dv typ x z i)++","++(showPVSExpression uni lvars dv typ y z i)++"))"
showPVSExpression uni lvars dv typ (LShift x y) z i	= "( lshift (" ++ (showPVSExpression uni lvars dv typ x z i) ++", "++ (showPVSExpression uni lvars dv typ y z i) ++ " ) )" 
showPVSExpression uni lvars dv typ (RShift x y) z i	= "( rshift (" ++ (showPVSExpression uni lvars dv typ x z i) ++ ", " ++ (showPVSExpression uni lvars dv typ y z i) ++ " ) )" 
showPVSExpression uni lvars dv typ (BitConcat (x:[])) z i = (showPVSExpression uni lvars dv typ x z i)
showPVSExpression uni lvars dv typ (BitConcat (x1:xs)) z i = "( bitConcat (" ++ (showPVSExpression uni lvars dv typ x1 z i) ++ ", " ++ recurse ++ ", " ++ rsize ++ "))"
  where 
    recurse = showPVSExpression uni lvars dv typ (BitConcat xs) z i
    rsize = show $ getExpressionBitSize (preproc uni) lvars z (BitConcat xs)
showPVSExpression uni lvars dv typ (BitSelect x y) z i	= "(bitSelect(" ++ (showPVSExpression uni lvars dv typ x z i) ++ ", " ++ (showPVSExpression uni lvars dv typ y z i) ++ "))"
showPVSExpression uni lvars dv typ (BitSelectRange x y z) q i = "(bitSelect(" ++ (showPVSExpression uni lvars dv typ x q i) ++ ", " ++ (showPVSExpression uni lvars dv typ y q i) ++ ", " ++ (showPVSExpression uni lvars dv typ z q i) ++"))"
showPVSExpression uni lvars dv typ (Multiply x y) z i	= "( " ++ (showPVSExpression uni lvars dv typ x z i ) ++" * "++ (showPVSExpression uni lvars dv typ y z i) ++ " )"
showPVSExpression uni lvars dv typ (Divide x y) z i	= "( div((" ++ (showPVSExpression uni lvars dv typ x z i ) ++" ), ("++ (showPVSExpression uni lvars dv typ y z i ) ++ ")) )"
showPVSExpression uni lvars dv typ (Modulo x y) z i	= "( mod((" ++ (showPVSExpression uni lvars dv typ x z i ) ++" ), ("++ (showPVSExpression uni lvars dv typ y z i ) ++ ")) )"
showPVSExpression uni lvars dv typ (Add x y) z i		= "( " ++ (showPVSExpression uni lvars dv typ x z i ) ++" + "++ (showPVSExpression uni lvars dv typ y z i ) ++ " )"
showPVSExpression uni lvars dv typ (Subtract x y) z i	= "( " ++ (showPVSExpression uni lvars dv typ x z i) ++" - "++ (showPVSExpression uni lvars dv typ y z i ) ++ " )"
showPVSExpression uni lvars dv typ (Literal x) z i		= showLit uni lvars dv typ x z i
--showPVSExpression uni lvars dv typ (Identifier (ID_StructRef strct path)) _ = strct ++ "`" ++ (showPVSExpression uni lvars dv typ (Identifier path) (Nothing) )
-- showPVSExpression uni lvars dv typ (Identifier (ID_Submod_Struct x (ID_Submod_Struct "first" y))) z = "first(" ++ x ++ ")`" ++ (showPVSExpression uni lvars dv typ (Identifier y) (Nothing))
showPVSExpression uni lvars dv typ (Identifier (ID_Submod_Struct x y)) _ i = if (not (containsFifoMethod id))
    then (getinst id') ++ "`" ++ (showPVSExpression uni lvars dv typ (Identifier (getPath id')) (Nothing) i)
    else intercalate "`" ((meth ++ "(" ++ (intercalate "`" before) ++ ")"):(after))
  where
    tracy = "[showPVSExpression] exp = " ++ (show (Identifier (ID_Submod_Struct x y))) 
    id = (ID_Submod_Struct x y)
    id' = id -- removePathOverlaps id
    getPath (ID_Submod_Struct x y ) = y
    getinst (ID_Submod_Struct x y ) = x
    split = splitAtFifoMethod id ("",[],[])
    meth = (\ (x,_,_) -> x ) split
    before = (\ (_,x,_) -> x ) split
    after =  (\ (_,_,x) -> x ) split
showPVSExpression uni lvars dv typ (Identifier (ID_Vect str index)) z i = str ++ "(" ++ (showPVSExpression uni lvars dv typ index z i) ++ ")"
showPVSExpression uni lvars dv typ (Identifier (ID str)) z i = str
-- showPVSExpression uni lvars dv typ (ValueMethodCall sMod mod inst meth) z i = if meth == "deq" || meth == "clear" || meth == "first"
--                                                                      then meth ++ "(" ++ (showIDPath uni lvars z (ID inst) i) ++")"
--                                                                      else meth ++ "(" ++ i ++ ",pre ,pre`"++ inst ++")"
showPVSExpression uni lvars dv typ (Exp_FunctionCall x ys) q i = x ++ "(" ++ (intercalate ", " (map (\z -> showPVSExpression uni lvars dv typ z q i) ys)) ++ ")" 
showPVSExpression uni lvars dv typ (Exp_MethodCall inst meth [] x) z i = if meth == "deq" || meth == "clear" || meth == "first"
                                                                     then meth ++ "(pre`" ++ (showIDPath uni lvars z inst i) ++")"
                                                                     else meth ++ "(" ++ i ++ ", pre, pre`" ++ (showIDPath uni lvars z inst i ) ++")"
  where
      tracy = "[showPVSExpression - 1519] exp = " ++ (show (Exp_MethodCall inst meth [] x))
--showPVSExpression uni lvars dv typ (Exp_MethodCall inst meth zs) z  = "pre`" ++ inst ++ "`" ++ meth ++ "( (" ++ (intercalate "), (" (map (\ x -> showPVSExpression uni lvars dv typ x (Nothing)) zs ) ) ++ ") )"
showPVSExpression uni lvars dv typ (Exp_If w x y) z i = "if " ++ (showPVSExpression uni lvars dv typ w z i) ++ "\n\t\t\t  then " ++ (showPVSExpression uni lvars dv typ x z i) ++ "\n\t\t\t  else " ++ (showPVSExpression uni lvars dv typ y z i) ++ "\n\t\t\tendif"
showPVSExpression uni lvars (Nothing) typ (Skip) (Nothing) i = error "PVSgen Error! I can't skip an expression at this point unless i know what state element I'm skipping!"
showPVSExpression uni lvars (Just dv) typ (Skip) z i = showPVSExpression uni lvars Nothing typ dv' z i
    where dv' = fixDV' $ fixDV typ dv
showPVSExpression uni lvars dv typ (Skip) (Just (prefix, nom)) i = removePathOverlaps' $ prefix ++ "`" ++ nom
    where
        tracy = "[showPVSExpression 1] - exp = Skip\nprefix = " ++ (show prefix)
showPVSExpression uni lvars dv typ (Tagged (Just t) (Valid (Literal (LitStructConstructor)))) z i = result
  where
      tracy = "[showPVSExpression 1] - exp = " ++ (show (LitStructConstructor)) ++ "\ntype = " ++ (show t) ++ "\n\nresult = " ++ (show result)
      inito = "mk" ++ (showPVSType uni (stripMaybe t))
      result = "Just["++ (showPVSType uni (stripMaybe t)) ++ "," ++ inito ++"](" ++ inito ++ ")"
      stripMaybe (PVS_Maybe x) = x
      stripMaybe x = x
showPVSExpression uni lvars dv typ (Tagged (Just t) (Valid x)) z i = result -- (\x -> trace showy x) $ result
  where
      tracy = "[showPVSExpression 2] - exp = " ++ (show (Valid x)) ++ "\ntype = " ++ (show t) ++ "\n\nresult = " ++ (show result)
      stripMaybe (PVS_Maybe x) = x
      stripMaybe x = x
      inito = if (stripMaybe t == PVS_Bool) then "False"
                                            else "mk" ++ (showPVSType uni (stripMaybe t))
      result = "Just["++ (showPVSType uni (stripMaybe t)) ++ "," ++ inito ++"](" ++ (showPVSExpression uni lvars dv typ x z i) ++ ")"
showPVSExpression uni lvars dv (Just t) (Tagged Nothing (Valid x)) z i = "Just["++ (showPVSType uni (stripMaybe t)) ++ "," ++ inito ++"](" ++ (showPVSExpression uni lvars dv (Just t) x z i) ++ ")"
  where
      tracy = "[showPVSExpression 3] - exp = " ++ (show (Valid x)) ++ "\ntype = " ++ (show t) 
      stripMaybe (PVS_Maybe x) = x
      stripMaybe x = x
      inito = "mk" ++ (showPVSType uni (stripMaybe t))
          -- showPVSExpression uni lvars dv (Just t) (kludgeInit uni (stripMaybe t)) z 
showPVSExpression uni lvars dv typ (Tagged (Just t) Invalid) z i = ("Nothing["++ (showPVSType uni (stripMaybe t)) ++ "," ++ inito ++"]")
  where
      tracy = "[showPVSExpression 4] - exp = " ++ (show (Invalid)) ++ "\ntype = " ++ (show t) 
      stripMaybe (PVS_Maybe x) = x
      stripMaybe x = x
      inito = if (stripMaybe t == PVS_Bool) then "False"
                                            else "mk" ++ (showPVSType uni (stripMaybe t))
          -- showPVSExpression uni lvars dv typ (kludgeInit uni (stripMaybe t)) z
showPVSExpression uni lvars dv (Just t) (Tagged (Nothing) Invalid) z i = "Nothing["++ (showPVSType uni (stripMaybe t)) ++ "," ++ inito ++"]"
  where
      tracy = "[showPVSExpression 5] - exp = " ++ (show (Invalid)) ++ "\ntype = " ++ (show t) 
      stripMaybe (PVS_Maybe x) = x
      stripMaybe x = x
      inito = if (stripMaybe t == PVS_Bool) then "False"
                                            else "mk" ++ (showPVSType uni (stripMaybe t))
          -- showPVSExpression uni lvars dv (Just t) (kludgeInit uni (stripMaybe t)) z
showPVSExpression uni lvars dv Nothing (Tagged (Nothing) Invalid) z i = error "Error! No context type provided!"
showPVSExpression uni lvars dv typ (Tagged whatever (MaybeContainer x)) z i = showPVSExpression uni lvars dv typ x z i
showPVSExpression uni lvars dv typ (MaybeIf i1 i2 exp1 exp2) z i = "IF ( " ++ (showIDPath uni lvars z i1 i) ++ "`valid ) \n\t\t\t  THEN " ++ (showPVSExpression uni (newLVar:lvars) dv typ binding z i) ++ "\n\t\t\t  ELSE " ++ (showPVSExpression uni lvars dv typ exp2 z i) ++ "\n\t\t\tENDIF"
    --"cases " ++ (showIDPath uni lvars z i1) ++ " of \n\t\t\t  Just(" ++ (showIDPath uni (newLVar:lvars) z i2) ++ ") : " ++ (showPVSExpression uni (newLVar:lvars) dv typ exp1 z) ++ "\n\t\t\t, Nothing : " ++ (showPVSExpression uni lvars dv typ exp2 z) ++ "\n\t\t\tendcases"
  where 
    stripMaybe (PVS_Maybe x) = x
    stripMaybe x = x
    newLVar = (i2, (Right (Just (stripMaybe (getTypeFromState uni (pvs_typedefs uni) (pvs_state uni) lvars z i1)))), (Identifier (ID_Submod_Struct (showIDPath uni lvars z i1 i) (ID "value"))))
    binding = (Binding (newLVar:[]) exp1)
showPVSExpression uni lvars dv typ (Binding [] exp) z i = showPVSExpression uni lvars dv typ exp z i
showPVSExpression uni lvars dv typ (Binding vars exp) z i = "LET " ++ (intercalate "\n\t\t\t, " (map (\ a -> showBinding uni lvars z a i) vars)) ++ "\n\t\t\tIN " ++ (showPVSExpression uni (lvars ++ vars) dv typ exp z i)
showPVSExpression uni lvars dv typ (CasesOf x cases) z i = intercalate "\n\t\t" [header, ("\t  " ++ (intercalate "\n\t\t\t, " (map middlebit cases))), footer]
  where
    header = "COND"
    middlebit :: (Literal, Expression) -> String
    middlebit ((Literal (LitStructConstructor)),b) = "True -> " ++ (showPVSExpression uni lvars dv typ b z i)
    middlebit (a,b) = (showPVSExpression uni lvars dv typ x z i) ++ " = " ++ (showPVSExpression uni lvars dv typ a z i) ++ " -> " ++ (showPVSExpression uni lvars dv typ b z i)
    footer = "ENDCOND"
showPVSExpression uni lvars dv typ (FromMaybe x y) z i = "IF "++ (showIDPath uni lvars z x i) ++"`valid THEN " ++ (showIDPath uni lvars z x i) ++"`value \n\t\t\t  ELSE " ++ (showPVSExpression uni lvars dv typ y z i) ++ "\n\t\t\tENDIF"
showPVSExpression uni lvars dv typ (StructCluster x y) q i = result -- trace showy $ result
  where
    showy = "[showPVSExpression] - clusterName = " ++ (show x) ++ "\n\nresult = " ++ (show result) ++ "\n\n" 
    x' = either (b2pType) (id) x
    stripMaybe (PVS_Maybe q) = q
    stripMaybe q = q
    inito = showPVSExpression uni lvars dv typ (kludgeInit uni (stripMaybe x')) q i
    l1 = "LET x : " ++ (showPVSType uni (stripMaybe x')) ++ " = mk" ++ (showPVSType uni (stripMaybe x')) 
    l2 = if (isMaybe x') then "IN Just["++ (showPVSType uni (stripMaybe x')) ++ "," ++ inito ++"]( x WITH [ " ++ (intercalate "\n\t\t\t\t, " (map (\ a -> showClusterField' uni lvars dv typ q x' a i) y))
                         else "IN x WITH [ " ++ (intercalate "\n\t\t\t\t, " (map (\ a -> showClusterField' uni lvars dv typ q x' a i) y))
    l3 = if (isMaybe x') then "])"
                         else "]"
    --l4 = "IN Just["++ (showPVSType uni (stripMaybe x')) ++ "," ++ inito ++  "](x)"
    isMaybe (PVS_Maybe _) = True
    isMaybe _ = False
    result = intercalate "\n\t\t\t  " [l1, l2, l3]-- if isNotDefault uni (StructCluster x y) 
                --                                              then intercalate "\n\t\t\t  " [l1, l2, l3]
                  --                                            else "mk" ++ (showPVSType uni (stripMaybe x')) 
showPVSExpression uni lvars dv typ (Exp_MethodCall mod meth args _) z i = meth ++ "( " ++ (showIDPath uni lvars z mod i) ++ (if null args then "" else ", " ++ ( intercalate ", " (map (\ x -> showPVSExpression uni lvars dv typ x z i) args))) ++ " )"
showPVSExpression uni lvars dv typ (IsValid x) z i = "Just?("++ (showPVSExpression uni lvars dv typ (Tagged Nothing x) z i) ++ ")"
showPVSExpression uni lvars dv typ (MaybeValue x) z i = "("++ (showPVSExpression uni lvars dv typ x z i) ++")`value"
showPVSExpression uni lvars dv typ (FieldAccess x p) z i = "(" ++ (showPVSExpression uni lvars dv typ x z i) ++ ")`" ++ (showIDPath uni lvars z p i)
showPVSExpression _ _ _ _ x _ _ = error $ show x 


addPreIfMissing :: ID_Path -> ID_Path
addPreIfMissing (ID_Submod_Struct x y) = if x == "pre" 
                                            then (ID_Submod_Struct x y)
                                            else (ID_Submod_Struct "pre" (ID_Submod_Struct x y))
addPreIfMissing (ID x) = (ID_Submod_Struct "pre" (ID x))
addPreIfMissing (ID_Vect x y) = (ID_Submod_Struct "pre" (ID_Vect x y))


fixDV' :: Expression -> Expression
fixDV' (Identifier i) =  (Identifier (removePathOverlaps i))
fixDV' x = x

removePathOverlaps' :: String -> String
removePathOverlaps' xs = intercalate "`" (nub xs')
    -- trace ("[removePathOverlaps'] i = " ++ (show xs') ++ "\ni' = " ++ (show (intercalate "`" (nub xs')))) $ intercalate "`" (nub xs')
  where
    xs' = splitOn "`" xs

removePathOverlaps :: ID_Path -> ID_Path 
removePathOverlaps id = strings2idpath $ nub str
  where
    str = idpath2strings id



-- strings2idpath :: [String] -> ID_Path 
-- strings2idpath (x:[]) = (ID x)
-- strings2idpath (x:xs) = (ID_Submod_Struct x (strings2idpath xs))

splitAtFifoMethod :: ID_Path -> (String, [String], [String]) -> (String, [String], [String])
splitAtFifoMethod (ID x) _ = error $ "Error! Detected Fifo method not found!"
splitAtFifoMethod (ID_Submod_Struct x y) (m, xs, ys) = if x == "first" || x == "deq" || x == "enq"  
                                                       then splitAtFifoMethod' y (x,xs,ys)
                                                       else splitAtFifoMethod y (m, (xs ++ (x:[])),ys)
    
splitAtFifoMethod' :: ID_Path -> (String, [String], [String]) -> (String, [String], [String])
splitAtFifoMethod' (ID x) (m, xs, ys)                 = (m, xs, (ys ++ (x:[])))
splitAtFifoMethod' (ID_Submod_Struct x y) (m, xs, ys) = splitAtFifoMethod' y (m, xs, (ys++(x:[])))
 

containsFifoMethod :: ID_Path -> Bool
containsFifoMethod (ID x) = x == "first" || x == "deq" || x == "enq" 
containsFifoMethod (ID_Submod_Struct x y) = if x == "first" || x == "deq" || x == "enq"  
                                               then True
                                               else containsFifoMethod y

isNotDefault :: PVSPackage -> Expression -> Bool
isNotDefault uni (StructCluster x y) = and $ map (\ q -> isNotDefault' uni q (snd instDef)) y
  where
    x' = either (b2pType) (id) x 
    td = findTypeDef (pvs_typedefs uni) x'
    instDef = maybe (error "!?") (id) $ lookupInst (pvs_instantiations uni) x'
    
isNotDefault' :: PVSPackage -> (String, Expression) -> [(String, Expression)] -> Bool
isNotDefault' uni (nom, (StructCluster x y)) _ = isNotDefault uni (StructCluster x y)
isNotDefault' uni (nom, exp) fs = maybe (False) (\ x -> exp == x) $ exp'
    where
        exp' = lookup nom fs

fixDV :: Maybe PVSType -> Expression -> Expression
fixDV Nothing x = x
fixDV (Just (PVS_Maybe t)) (Tagged x y) = (Tagged x y)
fixDV (Just (PVS_Maybe t)) x = (Tagged (Just t) (Valid x))
fixDV (Just t) x = x
    
-- showClusterField :: PVSPackage -> [LocalVar] -> Maybe DefaultValue -> Maybe (String, String) -> String -> (String, Expression) -> String
-- showClusterField uni lv dv typ z parent (x, (Literal (LitStructConstructor))) = x ++ " := " ++ (showPVSExpression uni lv dv typ instDef z)
--   where
--       state = getState (pvs_state uni) parent
--       temp = stLookup' (pvs_typedefs uni) (ID_Submod_Struct parent (ID x)) (snd state)
--       typ = if (x == "trespcntrl") then error (show temp)
--                                    else temp 
--       typ' = maybe (error ("Error! Didn't find a type for " ++ x) ) id typ
--       instDef = def2exp (lookupInst (pvs_instantiations uni) typ') typ'
-- showClusterField uni lv dv z p (x, y) = x ++ " := " ++ (showPVSExpression uni lv dv y z)

showClusterField' :: PVSPackage -> [LocalVar] -> Maybe DefaultValue -> Maybe PVSType -> Maybe (String, String) -> PVSType -> (String, Expression) -> String -> String
showClusterField' uni lv dv mTyp z typ (x, (Literal (LitStructConstructor))) q = x ++ " := " ++ (showPVSExpression uni lv dv fieldType typo z q)
  where
      typedef = findTypeDef (pvs_typedefs uni) typ
      fieldType = getFieldType x typedef 
      fieldType' = maybe (error "This shouldn't Happen...") (id) fieldType 
      fieldTypeDef = findTypeDef (pvs_typedefs uni) fieldType'
      ft' = (maybe (error "Error! Field Type not found!") (id) fieldType)
      instDef = def2exp (maybe (error ("impossible error! " ++ (show x))) (id) (lookupInst (pvs_instantiations uni) ft')) ft'
      isTypeSynonym (PVS_Synonym _ _) = True
      isTypeSynonym _ = False
      isNotCustom (PVS_Custom _) = not True
      isNotCustom _ = not False
      typo = if isNotCustom fieldType' 
                then kludgeInit uni fieldType'
                else if (isTypeSynonym fieldTypeDef)
                    then kludgeInit uni fieldType' 
                    else instDef 
showClusterField' uni lv dv mTyp z p (x, y) q = x ++ " := " ++ (showPVSExpression uni lv dv fieldType y z q)
  where
      typedef = findTypeDef (pvs_typedefs uni) p
      fieldType = getFieldType x typedef 

findTypeDef :: [PVSTypeDef] -> PVSType -> PVSTypeDef 
findTypeDef x (PVS_Maybe y) = findTypeDef x y
findTypeDef [] _ = error "Error! Type not declared!"
findTypeDef ((PVS_Synonym n t):xs) (PVS_Custom n') = if (n == n') then (PVS_Synonym n t) else (findTypeDef xs (PVS_Custom n'))
findTypeDef ((PVS_Enumeration n es):xs) (PVS_Custom n') = if (n == n') then (PVS_Enumeration n es) else (findTypeDef xs (PVS_Custom n'))
findTypeDef ((PVS_Struct n fs):xs) (PVS_Custom n') = if (n == n') then (PVS_Struct n fs) else (findTypeDef xs (PVS_Custom n'))
findTypeDef x y = error $ (show x) ++ "\n\n" ++ (show y)


-- | Returns the state declaration with the given name
getStateDec :: [PVSstateDec] -> String -> PVSstateDec
getStateDec x y = maybe (error "Error! State Declaration Not Found!") (\ z -> (y,z))  (lookup y x)

-- | Returns a state declaration containing the specified string
getState :: [PVSstateDec] -> String -> PVSstateDec
getState [] _ = error "This is an error!"
getState ((n, sts):xs) x' = unMaybe res
  where
    res = stLookup'' x' sts
    unMaybe Nothing = getState xs x'
    unMaybe (Just x) = (n, sts)

{-fieldLookup :: [PVSTypeDef] -> PVSstateDec -> String -> String -> PVSType    
fieldLookup tds (_, sts) parent child = stLookup' tds (ID_Submod_Struct parent (ID child)) st' 
    where 
        st' = stLookup'-} 
    
    
--"CASES " ++ (showPVSExpression uni lvars x z ) ++ " OF\n\t\t\t  " ++ (intercalate "\n\t\t\t,  " (map middlebit cases)) ++ "\n\t\t\t  ENDCASES"    
    

showBinding :: PVSPackage -> [LocalVar] -> Maybe (String, String) -> LocalVar -> String -> String
showBinding uni lv z (i, Right Nothing, exp) q = (showIDPath uni lv Nothing i q) ++ " = " ++ (showPVSExpression uni lv Nothing Nothing exp z q)
showBinding uni lv z (i, Right (Just x), exp) q = (showIDPath uni lv Nothing i q) ++ " : " ++ (showPVSType uni x) ++ " = " ++ (showPVSExpression uni lv Nothing (Just x) exp z q) 
showBinding uni lv z (i, Left Nothing, exp) q = (showIDPath uni lv Nothing i q) ++ " = " ++ (showPVSExpression uni lv Nothing Nothing exp z q)
showBinding uni lv z (i, Left (Just x), exp) q  = (showIDPath uni lv Nothing i q) ++ " : " ++ (showPVSType uni (b2pType x)) ++ " = " ++ (showPVSExpression uni lv Nothing (Just (b2pType x)) exp z q) 

showLit :: PVSPackage ->  [LocalVar] -> Maybe DefaultValue -> Maybe PVSType -> Lit -> Maybe (String, String) -> String -> String 
showLit _ _ _ _ (LitString x) _ _ = show x
showLit _ _ _ _ (LitEnum x) _ _ = x
showLit _ _ _ _ (LitInt x) _ _ = show x
showLit _ _ _ _ (LitBool x) _ _ = show x
showLit _ _ _ _ (LitReal x) _ _ = show x
showLit _ _ _ _ (LitChar x) _ _ = show x
showLit _ _ _ _ (LitSizedInt _ x) _ _ = show x
showLit _ _ _ _ (LitVoid) _ _ = ""
showLit uni lvars (Just x) Nothing (LitStructConstructor) z i = showPVSExpression uni lvars (Just x) Nothing x z i
showLit uni lvars (Just x) (Just typ) (LitStructConstructor) z _ = "mk" ++ (showPVSType uni typ)
showLit uni lvars Nothing (Just typ) (LitStructConstructor) z _ = "mk" ++ (showPVSType uni typ)
    where 
        x = kludgeInit uni typ 
showLit uni _ Nothing Nothing (LitStructConstructor) _ _ = error "Error! No constructor default value provided!"
--showLit x = error $ "PVSgen Error! I can't show this type of literal : " ++ (show x)

getExpressionBitSize :: PVSPackage -> [LocalVar] -> Maybe (String, String) -> Expression -> Integer
getExpressionBitSize uni lvars con (Negative x) 	= getExpressionBitSize uni lvars con x
getExpressionBitSize uni lvars con (Not x) 	= getExpressionBitSize uni lvars con x
getExpressionBitSize uni lvars con (Equals x y) 	= max (getExpressionBitSize uni lvars con x) (getExpressionBitSize uni lvars con y)
getExpressionBitSize uni lvars con (NotEquals x y) = max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y )  
getExpressionBitSize uni lvars con (GreaterEquals x y) = max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y )  
getExpressionBitSize uni lvars con (LessEquals x y)= max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y )  
getExpressionBitSize uni lvars con (Greater x y) 	= max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y )  
getExpressionBitSize uni lvars con (Less x y)  	= max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y )  
getExpressionBitSize uni lvars con (And x y) 	= max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y )  
getExpressionBitSize uni lvars con (Or x y)    	= max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y )  
getExpressionBitSize uni lvars con (BitwiseAND x y)= max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y ) 
getExpressionBitSize uni lvars con (BitwiseOR x y) = max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y ) 
getExpressionBitSize uni lvars con (BitwiseXOR x y)= max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y ) 
getExpressionBitSize uni lvars con (LShift x y) 	= (getExpressionBitSize uni lvars con x) 
getExpressionBitSize uni lvars con (RShift x y) 	= (getExpressionBitSize uni lvars con x) 
getExpressionBitSize uni lvars con (BitConcat xs)	= sum (map (getExpressionBitSize uni lvars con) xs)
getExpressionBitSize uni lvars con (BitSelect x y)	= 1
getExpressionBitSize uni lvars con (BitSelectRange x y z) = (readIntLit y) - (readIntLit z)
getExpressionBitSize uni lvars con (Multiply x y)	= max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y )  
getExpressionBitSize uni lvars con (Divide x y) 	= max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y ) 
getExpressionBitSize uni lvars con (Modulo x y) 	= max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y ) 
getExpressionBitSize uni lvars con (Add x y) 	= max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y )  
getExpressionBitSize uni lvars con (Subtract x y) = max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y ) 
getExpressionBitSize uni lvars con (Identifier x) = getTypeBitSize uni $ getTypeFromState uni (pvs_typedefs uni) (pvs_state uni) lvars con x
getExpressionBitSize uni lvars con (ValueMethodCall sMod mod inmod meth) = error "PVSgen Error!An unflattened value method reference somehow made it through!"-- getValueMethodBitSize uni lvars (ValueMethodCall sMod mod inmod meth) 
getExpressionBitSize uni lvars con (Exp_FunctionCall x ys) = getFunctionReturnSize uni (pvs_functions uni) x 
getExpressionBitSize uni lvars con (Exp_MethodCall inmod meth zs _)  = error "PVSgen Error!An unflattened value method reference somehow made it through!"--getValueMethodBitSize uni lvars (ValueMethodCall sMod mod inmod meth) 
getExpressionBitSize uni lvars con (Exp_If w x y) = max (getExpressionBitSize uni lvars con x ) (getExpressionBitSize uni lvars con y ) 
getExpressionBitSize uni lvars con (Skip)		= 0
getExpressionBitSize uni lvars con (Literal x)	= getLiteralBitSize x
getExpressionBitSize uni lvars con (Binding lvars' x) = getExpressionBitSize uni (lvars ++ lvars') con x

readIntLit :: Expression -> Integer
readIntLit (Literal (LitInt x))	= x
readIntLit (Literal (LitSizedInt n x))	= x
readIntLit (Multiply x y) = (*) (readIntLit x) (readIntLit y)
readIntLit (Divide x y)   = (quot) (readIntLit x) (readIntLit y)
readIntLit (Modulo x y)   = (mod) (readIntLit x) (readIntLit y)
readIntLit (Add x y)      = (+) (readIntLit x) (readIntLit y)
readIntLit (Subtract x y) = (-) (readIntLit x) (readIntLit y)
readIntLit x = error $ "PVSgen Error! Expected a literal instead of this : " ++ (show x)

-- getIdentifierBitSize :: PVSPackage -> [LocalVar] -> Context -> ID_Path -> Integer
-- getIdentifierBitSize uni lv con (ID_Submod_Struct "pre" y) = getIdentifierBitSize uni lv con y
-- getIdentifierBitSize uni lv con x = unMaybe lvTyp
--   where 
--     lvTyp = lookup x (map getLVType lv)
--     unMaybe Nothing = getIdentifierBitSize' uni lv con x 
--     unMaybe (Just z) = getTypeBitSize uni (unMaybe' z)
--     unMaybe' Nothing = error $ "PVSgen Error! Matching local variable not found.\n\n  Local Vars: " ++ (show lv) ++ "\n\n  Looking for : " ++ (show x)
--     unMaybe' (Just x) = x

getLVType :: LocalVar -> (ID_Path, Maybe PVSType)
getLVType (i, (Right x), _) = (i, x)
getLVType (i, (Left x), _) = (i, (fmap b2pType x))

-- getIdentifierBitSize' :: PVSPackage -> [LocalVar] -> Context -> ID_Path -> Integer
-- getIdentifierBitSize' uni lv con (ID_Submod_Struct "pre" y) = getIdentifierBitSize' uni [] con y
-- getIdentifierBitSize' uni lv con (ID_Submod_Struct x y) = getStructBitSize uni [] (pvs_typedefs uni) (getTypeFromState uni (pvs_state uni) lv con (ID x)) y
-- getIdentifierBitSize' uni lv con i = findPVSState uni (pvs_state uni) i
-- --getIdentifierBitSize' uni lv con i = findPVSState uni (pvs_state uni) i

getTypeFromState :: PVSPackage -> [PVSTypeDef] -> [PVSstateDec] -> [LocalVar] -> Maybe (String,String) -> ID_Path -> PVSType
getTypeFromState uni tds sts lvs context i
  | lvar /= Nothing 	= maybe (error "Impossible Error!") (id) lvar
  | st /= Nothing 	= maybe (error "Impossible Error!") (id) st
  | wire /= Nothing 	= maybe (error "Impossible Error!") (id) wire
  | stGlobal /= Nothing = maybe (error "Impossible Error!") (id) stGlobal
  | otherwise 		= error $ (show tds) ++ "\n\n" ++ (show sts) ++ "\n\n" ++ (show lvs) ++ "\n\n" ++ (show i) 
  where
    lvar = lvLookup tds i lvs
    st = stLookup uni tds i sts 
    stGlobal = stGlobalLookup tds i sts
    wire = wireLookup uni tds sts context i
    
wireLookup :: PVSPackage -> [PVSTypeDef] -> [PVSstateDec] -> Maybe (String, String) -> ID_Path -> Maybe PVSType
wireLookup _ _ _ Nothing _ = Nothing
-- wireLookup _ sts Nothing (ID x) = trace ("[T] - wireLookup - " ++ (show x) ++ "\n\n" ++ (show st)) $ stLookup'' x $ snd st
--   where
--     st = getState sts x
wireLookup pkg tds sts (Just (prefix,_)) x = stLookup pkg tds (ID_Submod_Struct alt x) sts -- trace ("[T] - wireLookup - " ++ (show alt ) ++ "." ++ (show x)) $ stLookup tds (ID_Submod_Struct alt x) sts
    where 
        alt = if prefix == "pre"
                 then pvs_packageName pkg
                 else prefix

stGlobalLookup :: [PVSTypeDef] -> ID_Path -> [PVSstateDec] -> Maybe PVSType
stGlobalLookup tds i sts = if (length findings) > 1 
                              then Nothing
                              else if (length findings) < 1
                                then Nothing
                                else Just (head findings)
    where
        findings = nub $ catMaybes $ map (stLookup' tds (ID (lastID i))) $ (map snd sts)
        
        

stLookup :: PVSPackage -> [PVSTypeDef] -> ID_Path -> [PVSstateDec] -> Maybe PVSType
stLookup pkg _ i [] = Nothing
stLookup pkg tds (ID x) (st:sts) = maybe (stLookup pkg tds (ID x) sts) (\ x -> Just (id x)) $ stLookup' tds (ID x) (snd st)
stLookup pkg tds (ID_Submod_Struct x y) st = maybe (Nothing) (\ z -> stLookup' tds y z) $ lookup alt st 
    where 
        alt = if x == "pre"
                 then pvs_packageName pkg
                 else x
    
stLookup' :: [PVSTypeDef] -> ID_Path -> [PVSstate] -> Maybe PVSType
stLookup' _ (ID x) sts = stLookup'' x sts
stLookup' _ (ID_Vect x n) sts = stLookup'' x sts
stLookup' tds (ID_Submod_Struct "pre" y) sts = stLookup' tds y sts
stLookup' tds (ID_Submod_Struct x y) sts = fmap (followStructs tds y) typ
  where 
    -- typ = maybe (error ("Error! No corresponding state entry found : " ++ (show (ID_Submod_Struct x y)) ++ "\n\n" ++ (intercalate "\n" (map show sts)))) (id) $ stLookup'' x sts
    typ = stLookup'' x sts


stLookup'' :: String -> [PVSstate] -> Maybe PVSType 
stLookup'' x [] = Nothing
stLookup'' x ((PVS_Reg i t _):ts) = if (ID x) == i then Just t else stLookup'' x ts
stLookup'' x ((PVS_Fifo f i t):ts) = if (ID x) == i then Just t else stLookup'' x ts
stLookup'' x ((PVS_Vector i t _ _ ):ts) = if (ID x) == i then Just t else stLookup'' x ts
stLookup'' x ((PVS_DWire i t _):ts) = if (ID x) == i then Just t else stLookup'' x ts
stLookup'' x ((PVS_SubModuleDec _ _ _):ts) = stLookup'' x ts
	
  


lvLookup :: [PVSTypeDef] -> ID_Path -> [LocalVar] -> Maybe PVSType
lvLookup _ i [] = Nothing
lvLookup tds (ID i) ((n, t, e):ls) 
  | (ID i) == n 	= getTypeFromLV (n,t,e)
  | otherwise   	= lvLookup tds (ID i) ls
lvLookup tds (ID_Submod_Struct x y) ((n, t, e):ls) 
  | n == (ID x)		= Just $ followStructs tds y typ
  | otherwise   	= lvLookup tds (ID_Submod_Struct x y) ls
  where 
    typ = maybe (error "Error! Structure declared without type association!?") (id) $ getTypeFromLV (n,t,e)
    
getTypeFromLV :: LocalVar -> Maybe PVSType
getTypeFromLV (i, (Right x), _) = x
getTypeFromLV (i, (Left x), _) = fmap b2pType x

followStructs :: [PVSTypeDef] -> ID_Path -> PVSType -> PVSType
followStructs tds (ID x) (PVS_Maybe y) = followStructs tds (ID x) y
followStructs tds (ID x) (PVS_Custom n) = maybe (error "Error! Field doesn't exist!") (id) $ lookup x (fields struct)
  where 
    struct = maybe (error "Error! Struct isn't declared!") (id) $ checkForSynonyms tds n
    fields (PVS_Struct _ fs) = fs
followStructs tds (ID_Submod_Struct x y) (PVS_Custom n) = followStructs tds y typ
  where
    struct = maybe (error "Error! Struct isn't declared!") (id) $ checkForSynonyms tds n
    fields (PVS_Struct _ fs) = fs
    typ = maybe (error "Error! Field doesn't exist!") (id) $ lookup x (fields struct)
followStructs tds x y = error $ (show x) ++ "\n\n" ++ (show y)

getTypeFromField :: [PVS_Field] -> ID_Path -> Maybe PVSType
getTypeFromField fs (ID x) = lookup x fs
getTypeFromField fs (ID_Vect x n) = lookup x fs
--getTypeFromField fs (ID_Submod_Struct x y) = lookup 

findPVSState :: PVSPackage -> [PVSstateDec] -> ID_Path -> Integer 
findPVSState uni [] x = error $ "PVSgen Error 101! Undeclared register : " ++ (show x)
findPVSState uni ((x, ys):xs) (ID_Submod_Struct p q) = if (x == p) 
  then findPVSState' uni ys q 
  else findPVSState uni xs (ID_Submod_Struct p q)

findPVSStateType :: PVSPackage -> [PVSstateDec] -> ID_Path -> Integer 
findPVSStateType uni [] x = error $ "PVSgen Error 105! Undeclared register : " ++ (show x)
findPVSStateType uni ((x, ys):xs) (ID_Submod_Struct p q) = if (x == p) 
  then findPVSState' uni ys q 
  else findPVSState uni xs (ID_Submod_Struct p q)  
  
--findPVSState uni ((x, ys):xs) (Nothing) i = findPVSState uni xs Nothing i  
  
findPVSState'' :: PVSPackage -> [PVSstateDec] -> Context -> ID_Path -> Integer 
findPVSState'' uni [] x y = error $ "PVSGen Error 102! Undeclared register : \n" ++ (show x) ++ "\n" ++ (show y)
findPVSState'' uni ((x, ys):xs) (Just (pre, n)) i = if (x == pre) 
  then getTypeBitSize uni (findPVSState''' uni ys i)
  else findPVSState'' uni xs (Just (pre,n)) i  
findPVSState'' uni ((x, ys):xs) (Nothing) i = findPVSState'' uni xs Nothing i  
  
findPVSState' :: PVSPackage -> [PVSstate] -> ID_Path -> Integer
findPVSState' uni [] x = 541-- error $ "PVSGen Error 103! Undeclared register : \n" ++ (show x)
findPVSState' uni ((PVS_Reg i typ _):xs) y = if (i == y) then getTypeBitSize uni typ else findPVSState' uni xs y
findPVSState' uni ((PVS_Fifo fType i typ):xs) y = if (i == y) then getTypeBitSize uni typ else findPVSState' uni xs y
findPVSState' uni ((PVS_Vector i typ _ _):xs) y = if (i == y) then getTypeBitSize uni typ else findPVSState' uni xs y
findPVSState' uni ((PVS_SubModuleDec i typ _):xs) y = findPVSState' uni xs y

findPVSState''' :: PVSPackage -> [PVSstate] -> ID_Path -> PVSType
findPVSState''' uni [] x = error $ "PVSGen Error 104! Undeclared register : \n" ++ (show x)
findPVSState''' uni ((PVS_Reg i typ _):xs) y = if (i == y) then typ else findPVSState''' uni xs y
findPVSState''' uni ((PVS_Fifo fType i typ):xs) y = if (i == y) then typ else findPVSState''' uni xs y
findPVSState''' uni ((PVS_Vector i typ _ _):xs) y = if (i == y) then typ else findPVSState''' uni xs y
findPVSState''' uni ((PVS_SubModuleDec _ _ _):xs) y = findPVSState''' uni xs y

getStructBitSize :: PVSPackage -> [LocalVar] -> [PVSTypeDef] -> PVSType -> ID_Path -> Integer 
--getStructBitSize _ x _ _ _ = error $ show x
getStructBitSize uni lv [] x y = error $ "I am PVSgen Error!\n" ++ (show x) ++ "\n" ++ (show y)
getStructBitSize uni lv ((PVS_Struct x fs):xs) (PVS_Custom y) z = if (y == x) then findField uni fs (showIDPath uni lv Nothing z "") else getStructBitSize uni lv xs (PVS_Custom y) z
getStructBitSize uni lv (x:xs) y z = getStructBitSize uni lv xs y z

findField :: PVSPackage -> [PVS_Field] -> String -> Integer
findField uni xs y = deMaybe $ lookup y xs
  where 
    deMaybe (Nothing) = error "Error!"
    deMaybe (Just x) = getTypeBitSize uni x

getFunctionReturnSize :: PVSPackage -> [PVSFunction] -> String -> Integer
getFunctionReturnSize _ [] _ = error "Error!!"
getFunctionReturnSize uni ((x, _, typ, _):xs) x' = if (x == x') then getTypeBitSize uni typ else getFunctionReturnSize uni xs x'

getLiteralBitSize :: Lit -> Integer
getLiteralBitSize (LitString str) 	= 8 * (fromIntegral (length str))
getLiteralBitSize (LitEnum enm)		= error "PVSgen Error!Literal type is incompatible with bit size acquisition operation! \nI found an Enum!"
getLiteralBitSize (LitInt int)		= ceiling $ logBase 2 (fromIntegral int)
getLiteralBitSize (LitBool bl)		= 1
getLiteralBitSize (LitReal rl)		= error "PVSgen Error!Literal type is incompatible with bit size acquisition operation! \nI found a Float!"
getLiteralBitSize (LitChar _ ) 		= 8
getLiteralBitSize (LitSizedInt n _) 	= n
getLiteralBitSize (LitStructConstructor)= error "PVSgen Error!Literal type is incompatible with bit size acquisition operation! \nI found a Structure Constructor Literal!"
getLiteralBitSize (LitVoid) 		= 0

getTypeBitSize :: PVSPackage -> PVSType -> Integer 
getTypeBitSize uni PVS_Bool = 1
getTypeBitSize uni (PVS_Bit n) = n
getTypeBitSize uni (PVS_Int n) = n
getTypeBitSize uni (PVS_UInt n) = n
getTypeBitSize uni PVS_Real = error "PVSgen Error!Type is incompatible with bit size acquisition operation! \nI found a Float!"
getTypeBitSize uni (PVS_Custom nom) = unMaybe syns
  where 
    syns = checkForSynonyms (pvs_typedefs uni) nom
    unMaybe Nothing  = error "PVSgen Error!Literal type is incompatible with bit size acquisition operation! \nI found an Enum!"
    unMaybe (Just x) = getTypedefBitSize uni x

checkForSynonyms :: [PVSTypeDef] -> String -> Maybe PVSTypeDef
checkForSynonyms [] _ = Nothing
checkForSynonyms ((PVS_Synonym n typ):xs) n' = if n == n' then (Just (PVS_Synonym n typ)) else checkForSynonyms xs n'
checkForSynonyms ((PVS_Enumeration n es):xs) n' = if n == n' then (Just (PVS_Enumeration n es)) else checkForSynonyms xs n'
checkForSynonyms ((PVS_Struct n fs):xs) n' = if n == n' then (Just (PVS_Struct n fs)) else checkForSynonyms xs n'

    
getTypedefBitSize :: PVSPackage -> PVSTypeDef -> Integer
getTypedefBitSize uni (PVS_Synonym n typ) = getTypeBitSize uni typ
getTypedefBitSize uni (PVS_Enumeration n es) = ceiling $ logBase 2 (fromIntegral (length es))
getTypedefBitSize uni (PVS_Struct n fs) = sum (map (getFieldBitSize uni) fs)

getFieldBitSize :: PVSPackage -> PVS_Field -> Integer
getFieldBitSize uni (_, typ) = getTypeBitSize uni typ
    
-- | converts a lexer-bound PVS type to a string useable in a generated PVS file.
showPVSType :: PVSPackage -> PVSType -> String
showPVSType uni (PVS_Bool) = "bool"
showPVSType uni (PVS_Bit n) = "Bit("++(show n)++")"
showPVSType uni (PVS_Int n) = "Int("++(show n)++")"
showPVSType uni (PVS_UInt n) = "UInt("++(show n)++")"
showPVSType uni (PVS_Real) = "real"
showPVSType uni (PVS_Maybe (PVS_Bool)) = "Maybe[bool, False ]" 
showPVSType uni (PVS_Maybe x) = "Maybe[" ++ (showPVSType uni x) ++ ", mk" ++ (showPVSType uni x) ++ "]"
showPVSType uni (PVS_Custom "Type") = "Tpye"
showPVSType uni (PVS_Custom "Integer") = "int"
showPVSType uni (PVS_Custom n) = n

