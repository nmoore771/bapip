{-# OPTIONS_GHC -fno-warn-tabs #-}

module BSVGenerator where

import Data.List
import Data.Char
import LexerTypes
--import Language.Haskell.Pretty
import qualified Data.Text as T
import qualified Data.Text as T

showBSVPackages :: [BSVPackage] -> [File]
showBSVPackages = (\ x -> map showBSVPackage x) 

-- | Converts a BSV package to a file.
showBSVPackage :: BSVPackage -> File
showBSVPackage pkg =(((bsv_packageName pkg) ++ ".bsv")
                  ,( (showBSVHeader (bsv_packageName pkg))
                  ++ (showBSVImports (imports pkg))
                  ++ (showBSVincludes (including pkg))
                  ++ (showBSVInterfaces (interfaces pkg))
                  ++ (showBSVConstants (bsv_constants pkg))
                  ++ (showBSVTypeDefs (bsv_typedefs pkg))
                  ++ (concat (map showDefInst (bsv_instDefs pkg)))
                  ++ (showBSVFunctions (bsv_functions pkg))
                  ++ (showBSVModules (bsv_modules pkg))
                  ++ showBSVfooter))

-- showBSVPackage' :: BSVPackage -> File
-- showBSVPackage' pkg = (((bsv_packageName pkg) ++ ".bsv")
--                   ,( (showBSVHeader (bsv_packageName pkg))
--                   ++ (showBSVImports (imports pkg))
--                   ++ (showBSVInterfaces (interfaces pkg))
--                   ++ (showBSVConstants (bsv_constants pkg))
--                   ++ (showBSVTypeDefs (bsv_typedefs pkg))
--                   ++ (showBSVModules (bsv_modules pkg))
--                   ++ showBSVfooter))

-- | Generates a BSV file header
showBSVHeader :: PackageName -> String
showBSVHeader x = "package " ++ x ++ ";\n\n"

showBSVfooter :: String 
showBSVfooter = "endpackage\n"

-- | Generates BSV import statements
showBSVImports :: [PackageName] -> String
showBSVImports [] = []
showBSVImports xs = (intercalate "\n\t" (map (showBSVImport) xs)) ++ "\n\n\n"

-- | Generates a single BSV import statement
showBSVImport :: PackageName -> String
showBSVImport x = "import " ++ x ++ " :: * ;"

showBSVincludes :: [String] -> String
showBSVincludes xs = "\t" ++ (intercalate "\n\t" (map (showBSVinclude ) xs)) ++ "\n\n"

showBSVinclude :: String -> String
showBSVinclude x = "`include \""++ x ++ "\"" 

-- | Generates BSV interface blocks
showBSVInterfaces :: [InterfaceDec] -> String
showBSVInterfaces [] = []
showBSVInterfaces xs = (intercalate "\n\n\t" (map (showBSVInterface) xs)) ++ "\n\n\n"

-- | Generates a single BSV Interface block
showBSVInterface :: InterfaceDec -> String
showBSVInterface (name, meths, subints, []) = "\t"++(showInterfaceHeader name) ++ "\n\t\t" 
                                  ++ (intercalate "\n\t\t" (map showBSVMethodDec meths))++ "\n\t\t"
                                  ++ (intercalate "\n\t\t" (map showBSVSubInt subints))
                                  ++ "\n\tendinterface"
showBSVInterface (name, meths, subints, atts) = (showInterfaceAttributes atts) ++ "\n\t"
                                    ++ (showInterfaceHeader name) ++ "\n\t" 
                                    ++ (intercalate "\n\t\t" (map showBSVMethodDec meths))++ "\n\t\t" 
                                    ++ (intercalate "\n\t\t" (map showBSVSubInt subints))
                                    ++ "\n\tendinterface"

showInterfaceAttributes :: [InterfaceAttribute] -> String
showInterfaceAttributes [] = ""
showInterfaceAttributes xs = "(* " ++ (intercalate ", " (map (showInterfaceAttribute) xs)) ++ " *)"

showInterfaceAttribute :: InterfaceAttribute -> String
showInterfaceAttribute (Int_Always_Ready) = "always_ready"
showInterfaceAttribute (Int_Always_Enabled) = "always_enabled"
showInterfaceAttribute (Int_Doc x) = "doc = \"" ++ x ++ "\""

showInterfaceHeader :: Name -> String
showInterfaceHeader x = "interface " ++ x ++ ";"

showBSVSubInt :: InterfaceRef -> String
showBSVSubInt (i, l) = "interface " ++ i ++ " " ++ l ++";"

showBSVMethodDec :: MethodDec -> String
showBSVMethodDec (name, typ, args, []) = "method " ++ (showBSVReturnType typ) ++ " " ++ name ++ " (" ++ (intercalate ", " (map showBSVArgument args)) ++ ");"
showBSVMethodDec (name, typ, args, atts) = (showMethodDecAttributes atts) ++ "method " ++ (showBSVReturnType typ) ++ " " ++ name ++ " (" ++ (intercalate ", " (map showBSVArgument args)) ++ ");"

showBSVArgument :: Argument -> String
showBSVArgument (name, typ, []) = (showBSVType typ) ++ " " ++ name
showBSVArgument (name, typ, atts) = (showArgumentAttributes atts) ++ (showBSVType typ) ++ " " ++ name

showArgumentAttributes :: [ArgumentAttribute] -> String 
showArgumentAttributes [] = "" 
showArgumentAttributes xs = "(* " ++ (intercalate ", " (map (showArgumentAttribute) xs)) ++ " *)"

showArgumentAttribute :: ArgumentAttribute -> String
showArgumentAttribute (Arg_Ready x) = "ready = \"" ++ x ++ "\""
showArgumentAttribute (Arg_Enable x) = "enable = \"" ++ x ++ "\""
showArgumentAttribute (Arg_Result x) = "result = \"" ++ x ++ "\""
showArgumentAttribute (Arg_Prefix x) = "prefix = \"" ++ x ++ "\""
showArgumentAttribute (Arg_Port x) = "port = \"" ++ x ++ "\""
showArgumentAttribute (Arg_Always_Ready) = "always_ready"
showArgumentAttribute (Arg_Always_Enabled) = "always_enabled"
showArgumentAttribute (Arg_Doc x) = "doc = \"" ++ x ++ "\""

showBSVReturnType :: ReturnType -> String
showBSVReturnType (Action) = "Action"
showBSVReturnType (ActionValue x) = "ActionValue#(" ++ (showBSVType x) ++ ")"
showBSVReturnType (Value x) = showBSVType x

showMethodDecAttributes :: [MethodDecAttribute] -> String 
showMethodDecAttributes [] = "" 
showMethodDecAttributes xs = "(* " ++ (intercalate ", " (map (showMethodDecAttribute) xs)) ++ " *)"

showMethodDecAttribute :: MethodDecAttribute -> String 
showMethodDecAttribute (MDc_Ready x) = "ready = \"" ++ x ++ "\""
showMethodDecAttribute (MDc_Enable x) = "enable = \"" ++ x ++ "\""
showMethodDecAttribute (MDc_Result x) = "result = \"" ++ x ++ "\""
showMethodDecAttribute (MDc_Prefix x) = "prefix = \"" ++ x ++ "\""
showMethodDecAttribute (MDc_Port x) = "port = \"" ++ x ++ "\""
showMethodDecAttribute (MDc_Always_Ready) = "always_ready"
showMethodDecAttribute (MDc_Always_Enabled) = "always_enabled"
showMethodDecAttribute (MDc_Doc x) = "doc = \"" ++ x ++ "\""

-- | Generates BSV Consant Declarations
showBSVConstants :: [BSVConstantDec] -> String
showBSVConstants [] = ""
showBSVConstants xs = (intercalate "\n\t" (map (showBSVConstant) xs)) ++ "\n\n\n"

-- | Converts a lexer-bound constant declaration to a string in BSV syntax.
showBSVConstant :: BSVConstantDec -> String
showBSVConstant (nom, typ, lit) = (showBSVType typ) ++ " " ++ nom ++ " = " ++ shownLit ++ ";"
  where 
    shownLit = expression2BSV lit

-- | Generates BSV type definitions
showBSVTypeDefs :: [BSVTypeDef] -> String
showBSVTypeDefs [] = ""
showBSVTypeDefs xs = "\t" ++ (intercalate "\n\n\t" (map (showBSVTypeDef) xs)) ++ "\n\n\n"

showBSVTypeDef :: BSVTypeDef -> String
showBSVTypeDef (BSV_Synonym nom typ) = "typedef " ++ (showBSVType typ) ++ " " ++ nom ++ ";"
showBSVTypeDef (BSV_Enumeration nom enums) =  "typedef enum {" ++ (intercalate ", " enums) ++ "} " ++ nom ++ ";"
showBSVTypeDef (BSV_Struct nom fields) = "typedef struct { " ++ (intercalate "\n\t  " (map (\ (x,y)-> (showBSVType y) ++ " " ++ x ++ ";") fields)) ++ "\n\t  } " ++ nom ++ " deriving (Bits, Eq) ;"
    
showDefInst :: BSVInstDef -> String
showDefInst (nom, fields) = intercalate "\n\t" [header, middle, footer]
  where
    header = "\tinstance DefaultValue#(" ++ nom ++ ");"
    middle = "  defaultValue = " ++ nom ++ " {" ++ listo ++ "};"
    listo = intercalate ", " $ map (\ (x,z) -> x ++ ":" ++ (expression2BSV z) ) fields
    footer = "endinstance\n\n"    

-- | Generates BSV Module declarations 
showBSVModules :: [BSVModuleDec] -> String
showBSVModules [] = ""
showBSVModules xs = (intercalate "\n\n\n" (map (showBSVModule) xs)) ++ "\n\n\n"

showBSVModule :: BSVModuleDec -> String
showBSVModule mod = (showModuleAttributes (attributes mod))
                 ++ (showBSVModuleHeader (mName mod) (interfaceName mod))
                 ++ (showBSVModuleStateDecs (state mod))
                 ++ (showBSVModuleActions (actions mod))
                 ++ (showBSVModuleRules (rules mod))
                 ++ (showBSVModuleMethods (methods mod))
                 ++ "\n\tendmodule"

showModuleAttributes :: [ModuleAttribute] -> String
showModuleAttributes [] = "" 
showModuleAttributes xs = "\t(* " ++ (intercalate ", " (map (showModuleAttribute) (killVoidModuleAttributes xs))) ++ " *)"

killVoidModuleAttributes :: [ModuleAttribute] -> [ModuleAttribute]
killVoidModuleAttributes ((MAvoid):xs) = killVoidModuleAttributes xs 
killVoidModuleAttributes (x:xs) = x : (killVoidModuleAttributes xs)

showModuleAttribute :: ModuleAttribute -> String
showModuleAttribute (Synthesize)                = "synthesize"  
showModuleAttribute (Mod_Always_Ready xs)       = "always_ready = \"" ++ (intercalate ", " xs) ++ "\""
showModuleAttribute (Mod_Always_Enabled xs)     = "always_enabled = \"" ++ (intercalate ", " xs) ++ "\""
showModuleAttribute (Descending_Urgency xs)     = "descending_urgency = \"" ++ (intercalate ", " xs) ++ "\""
showModuleAttribute (Mod_Execution_Order xs)    = "execution_order = \"" ++ (intercalate ", " xs) ++ "\""
showModuleAttribute (Mod_Mutually_Exclusive xs) = "mutually_exclusive = \"" ++ (intercalate ", " xs) ++ "\""
showModuleAttribute (Mod_Conflict_Free xs)      = "conflict_free = \"" ++ (intercalate ", " xs) ++ "\""
showModuleAttribute (Mod_Preempts xs)           = "preempts = \"" ++ (intercalate ", " xs) ++ "\"" 
showModuleAttribute (Clock_Prefix x)            = "clock_prefix = \"" ++ x ++ "\""
showModuleAttribute (Gate_Prefix x)             = "gate_prefix = \"" ++ x ++ "\""
showModuleAttribute (Reset_Prefix x)            = "reset_prefix = \"" ++ x ++ "\""
showModuleAttribute (Gate_Input_Clocks xs)      = "gate_input_clocks = \"" ++ (intercalate ", " xs) ++ "\""
showModuleAttribute (Gate_All_Clocks)           = "gate_all_clocks"
showModuleAttribute (Default_Clock_OSC x)       = "default_clock_osc = \"" ++ x ++ "\""
showModuleAttribute (Default_Clock_Gate x)      = "default_clock_gate = \"" ++ x ++ "\""
showModuleAttribute (Default_Gate_Inhigh)       = "default_gate_inhigh"
showModuleAttribute (Default_Gate_Unused)       = "default_gate_unused"
showModuleAttribute (Default_Reset x)           = "default_reset = \"" ++ x ++ "\""
showModuleAttribute (No_Default_Reset)          = "no_default_reset"
showModuleAttribute (Clock_Family xs)           = "clock_family = \"" ++ (intercalate ", " xs) ++ "\""
showModuleAttribute (Clock_Ancestors xs)        = "clock_ancestors = \"" ++ (intercalate ", " xs) ++ "\""
showModuleAttribute (Mod_Doc x)                 = "doc = \"" ++ x ++ "\""

showBSVModuleHeader :: String -> String -> String
showBSVModuleHeader name interface = "\tmodule " ++ name ++ " (" ++ interface ++ ");" 

showBSVModuleStateDecs :: [BSVstateDec] -> String
showBSVModuleStateDecs xs = "\n\t\t" ++ (intercalate "\n\t\t" (map (showBSVModuleStateDec) xs)) ++ "\n\n"

showBSVModuleStateDec :: BSVstateDec -> String
showBSVModuleStateDec (BSV_SubModuleDec inter name inst) = inter ++ " " ++ inst ++ " <- " ++ name ++ ";"
showBSVModuleStateDec (BSV_Reg name typ initial) = "Reg#(" ++ (showBSVType typ) ++ ") " ++ (showIDPath name) ++ " <- mkReg(" ++ (expression2BSV initial) ++ ");"
showBSVModuleStateDec (BSV_Fifo fType name typ) = "FIFO#(" ++ (showBSVType typ) ++ ") " ++ (showIDPath name) ++ " <- mkFIFO;"
showBSVModuleStateDec (BSV_Vector name typ size init) = "Vector#("++ (show size) ++ ", " ++ (showBSVType typ) ++") " ++ (showIDPath name) ++ "<- "++ (showVectInit init) ++ " ;"

showIDPath :: ID_Path -> String
showIDPath (ID x) = x
showIDPath (ID_Submod_Struct x y) = x ++ "." ++ (showIDPath y)

-- showBSVvectType :: BSVstateType -> String 
-- showBSVvectType (BST_Reg typ) = "Reg#("++(showBSVType typ)++")"

showVectInit :: VectorInit -> String
showVectInit (Replicate i) = "replicate"++(expression2BSV i)++"))"

showBSVModuleActions :: [ActionDec] -> String 
showBSVModuleActions [] = ""
showBSVModuleActions xs = "\n\t\t" ++ (intercalate "\n\n\t\t" (map (showBSVModuleAction) xs)) ++ "\n\n"

showBSVModuleAction :: ActionDec -> String
showBSVModuleAction (name, stmts, atts) = (showBSVActionAttributes atts)
                                              ++ (showBSVActionHeader name)
                                              ++ (showBSVStatements stmts)
                                              ++ "\t\tendaction\n\t)"

showBSVActionAttributes :: [ActionAttribute] -> String
showBSVActionAttributes [] = "" 
showBSVActionAttributes xs = "\t(* " ++ (intercalate ", " (map (showBSVActionAttribute) xs)) ++ " *)\n"

showBSVActionAttribute :: ActionAttribute -> String
showBSVActionAttribute (Act_Doc x) = "doc = \"" ++ x ++ "\""

showBSVActionHeader :: String -> String
showBSVActionHeader name = "\tAction " ++ name ++ "\n\t\t = ( action \n\t\t"

showBSVStatements :: [Statement] -> String
showBSVStatements [] = []
showBSVStatements xs = "\n\t\t\t" ++ (intercalate "\n\t\t\t" (map (\ x -> showBSVStatement x "") xs))

showBSVStatement :: Statement -> Spacing -> String
showBSVStatement (Write nom exp atts) spc = spc ++ (showBSVStatementAttributes atts) ++ (showIDPath nom) ++ " <= " ++ (expression2BSV exp) ++ ";"
showBSVStatement (MethodCall modNom methNom [] atts) spc = spc ++ (showBSVStatementAttributes atts) ++ (showIDPath modNom) ++ "." ++ methNom ++ ";"
showBSVStatement (MethodCall modNom methNom exps atts) spc = spc ++ (showBSVStatementAttributes atts) ++ (showIDPath modNom) ++ "." ++ methNom ++ "("++ (intercalate ", " (map (\ x -> "("++ x ++ ")") (map expression2BSV exps))) ++ ");"
showBSVStatement (ActionCall nom atts) spc = spc ++ (showBSVStatementAttributes atts) ++ nom ++ ";"
showBSVStatement (Return exp atts) spc = spc ++ (showBSVStatementAttributes atts) ++ "return " ++ " (" ++ (expression2BSV exp) ++");"
showBSVStatement (If guard the (Void) atts) spc = spc ++ (showBSVStatementAttributes atts) ++ "if (" ++ (expression2BSV guard) ++") \n\t\t\t" ++ (showBSVStatement the ('\t':spc))
showBSVStatement (If guard the els atts) spc = spc ++ (showBSVStatementAttributes atts) ++ "if (" ++ (expression2BSV guard) ++") \n\t\t\t" ++ (showBSVStatement the ('\t':spc)) ++ "\n\t\t\t"++spc++"else\n\t\t\t" ++ (showBSVStatement els ('\t':spc)) ++ "\n\t\t\t"
showBSVStatement (ForLoop insts guard incs stmt atts) spc = spc ++ (showBSVStatementAttributes atts) ++ "for (" ++ (intercalate ", " (map showUninterpretedStatement insts)) ++ "; " ++ (expression2BSV guard) ++ "; " ++ (intercalate ", " (map showUninterpretedStatement incs))++ ")\n\t\t\t" ++ (showBSVStatement stmt ('\t':spc))
showBSVStatement (Switch guard cases atts) spc = spc ++ (showBSVStatementAttributes atts) ++ "case (" ++ (expression2BSV guard) ++ ")\n\t\t\t" ++ (intercalate "\n\t\t\t" (map (\x -> showBSVCase x spc) cases)) ++ "\n\t\tendcase"

showBSVCase :: Case -> Spacing -> String
showBSVCase (lit, stmt) spc = (expression2BSV lit) ++ " : " ++ (showBSVStatement stmt ('\t':spc)) ++ ";"

showUninterpretedStatement :: UStatement -> String
showUninterpretedStatement (DeclAssign typ nom exp) = (showUType typ) ++ " " ++ nom ++ " = " ++ (expression2BSV exp)
showUninterpretedStatement (UAssign nom exp) = nom ++ " = " ++ (expression2BSV exp)

showUType :: UType -> String
showUType (U_Int) = "int"
showUType (U_String) = "string"

showBSVStatementAttributes :: [StatementAttribute] -> String 
showBSVStatementAttributes [] = []
showBSVStatementAttributes xs = "(* " ++ (intercalate ", " (map (showBSVStatementAttribute) xs)) ++ " *)\n\t\t\t"

showBSVStatementAttribute :: StatementAttribute -> String
showBSVStatementAttribute (Split) = "split"
showBSVStatementAttribute (NoSplit) = "nosplit"
showBSVStatementAttribute (Sta_Descending_Urgency xs) = "descending_urgency = \"" ++ (intercalate ", " xs) ++ "\""
showBSVStatementAttribute (Sta_Execution_Order xs) = "execution_order = \"" ++ (intercalate ", " xs) ++ "\""
showBSVStatementAttribute (Sta_Mutually_Exclusive xs) = "mutually_exclusive = \"" ++ (intercalate ", " xs) ++ "\""
showBSVStatementAttribute (Sta_Conflict_Free xs) = "conflict_free = \"" ++ (intercalate ", " xs) ++ "\""
showBSVStatementAttribute (Sta_Preempts xs) = "preempts = \"" ++ (intercalate ", " xs) ++ "\"" 
showBSVStatementAttribute (Sta_Doc x) = "doc = \"" ++ x ++ "\""

showBSVModuleRules :: [RuleDec] -> String
showBSVModuleRules [] = [] 
showBSVModuleRules xs = intercalate "\n\t\t" (map (showBSVModuleRule) xs) 

showBSVModuleRule :: RuleDec -> String
showBSVModuleRule (nom, (Literal (LitBool True)), stmts, atts) = "\n\t\t" ++ (showBSVRuleAttributes atts) ++ "\n\t\trule " ++ nom ++ ";\n\t\t\t" ++ (intercalate "\n\t\t\t" (map ( \ x -> showBSVStatement x "") stmts)) ++ "\n\t\tendrule" 
showBSVModuleRule (nom, guard, stmts, atts) = (showBSVRuleAttributes atts) ++ "\n\t\trule " ++ nom ++ "("++ (expression2BSV guard) ++ ");\n\t\t\t" ++ (intercalate "\n\t\t\t" (map ( \ x -> showBSVStatement x "") stmts)) ++ "\n\t\tendrule" 

showBSVRuleAttributes :: [RuleAttribute] -> String 
showBSVRuleAttributes [] = [] 
showBSVRuleAttributes xs =  "(* " ++ (intercalate ", " (map (showBSVRuleAttribute) xs)) ++ " *)\n"

showBSVRuleAttribute :: RuleAttribute -> String
showBSVRuleAttribute (Fire_When_Enabled) = "fire_when_enabled"
showBSVRuleAttribute (No_Implicit_Conditions) = "no_implicit_conditions"
showBSVRuleAttribute (Rul_Descending_Urgency xs) = "descending_urgency = \"" ++ (intercalate ", " xs) ++ "\""
showBSVRuleAttribute (Rul_Execution_Order xs) =  "execution_order = \"" ++ (intercalate ", " xs) ++ "\""
showBSVRuleAttribute (Rul_Mutually_Exclusive xs) =  "mutually_exclusive = \"" ++ (intercalate ", " xs) ++ "\""
showBSVRuleAttribute (Rul_Conflict_Free xs) = "conflict_free = \"" ++ (intercalate ", " xs) ++ "\""
showBSVRuleAttribute (Rul_Preempts xs) = "preempts = \"" ++ (intercalate ", " xs) ++ "\"" 
showBSVRuleAttribute (Rul_Doc x) =  "doc = \"" ++ x ++ "\""

showBSVModuleMethods :: [MethodBody] -> String
showBSVModuleMethods [] = [] 
showBSVModuleMethods xs = "\n\n\t\t" ++ (intercalate "\n\n\t\t" (map (showBSVModuleMethod) xs) )

showBSVModuleMethod :: MethodBody -> String
showBSVModuleMethod (name, typ, args, (Literal (LitBool True)), stmts, atts) = (showBSVMethodBodyAttributes atts) ++ "method " ++ (showBSVReturnType typ) ++ " " ++ name ++ "(" ++ (intercalate ", " (showUTArgs args)) ++ ");" ++ (showBSVStatements stmts) ++ "\n\t\tendmethod" 
showBSVModuleMethod (name, typ, args, guard, stmts, atts) = (showBSVMethodBodyAttributes atts) ++ "method " ++ (showBSVReturnType typ) ++ " " ++ name ++ "(" ++ (intercalate ", " (showUTArgs args)) ++ ") if ("++(expression2BSV guard)++");" ++ (showBSVStatements stmts) ++ "\n\t\tendmethod" 

showUTArgs :: UTArgs -> [String]
showUTArgs [] = []
showUTArgs((n, Nothing):xs) = n : (showUTArgs xs)
showUTArgs((n, Just x):xs) = ((showBSVType x) ++ " " ++ n) : (showUTArgs xs)
 
showBSVMethodBodyAttributes :: [MethodBodyAttribute] -> String
showBSVMethodBodyAttributes [] = []
showBSVMethodBodyAttributes xs = "(* " ++ (intercalate ", " (map (showBSVMethodBodyAttribute) xs)) ++ " *)\n"

showBSVMethodBodyAttribute :: MethodBodyAttribute -> String
showBSVMethodBodyAttribute (Met_Doc x) = "doc = \"" ++ x ++ "\""


-- | Converts an Expression into a string for use by the BSV file generator
expression2BSV :: Expression -> String 
expression2BSV (Negative x)		= "( - " ++ (expression2BSV x) ++ ")" 
expression2BSV (Not x)			= "( !" ++ (expression2BSV x) ++ ")" 
expression2BSV (Equals x y)		= "( " ++ (expression2BSV x) ++" == "++ (expression2BSV y) ++ " )" 
expression2BSV (NotEquals x y)		= "( " ++ (expression2BSV x) ++" != "++ (expression2BSV y) ++ " )"
expression2BSV (GreaterEquals x y)	= "( " ++ (expression2BSV x) ++" >= "++ (expression2BSV y) ++ " )"
expression2BSV (LessEquals x y)		= "( " ++ (expression2BSV x) ++" <= "++ (expression2BSV y) ++ " )"
expression2BSV (Greater x y)		= "( " ++ (expression2BSV x) ++" > "++ (expression2BSV y) ++ " )"
expression2BSV (Less x y)		= "( " ++ (expression2BSV x) ++" < "++ (expression2BSV y) ++ " )"
expression2BSV (And x y)		= "( " ++ (expression2BSV x) ++" && "++ (expression2BSV y) ++ " )"
expression2BSV (Or x y)			= "( " ++ (expression2BSV x) ++" || "++ (expression2BSV y) ++ " )"
expression2BSV (BitwiseAND x y)		= "( " ++ (expression2BSV x) ++" & "++ (expression2BSV y) ++ " )"
expression2BSV (BitwiseOR x y)		= "( " ++ (expression2BSV x) ++" | "++ (expression2BSV y) ++ " )"
expression2BSV (BitwiseXOR x y)		= "( " ++ (expression2BSV x) ++" ^ "++ (expression2BSV y) ++ " )"
expression2BSV (LShift x y)		= "( " ++ (expression2BSV x) ++" << "++ (expression2BSV y) ++ " )"
expression2BSV (RShift x y)		= "( " ++ (expression2BSV x) ++" >> "++ (expression2BSV y) ++ " )"
expression2BSV (Multiply x y)		= "( " ++ (expression2BSV x) ++" * "++ (expression2BSV y) ++ " )"
expression2BSV (Divide x y)		= "( " ++ (expression2BSV x) ++" / "++ (expression2BSV y) ++ " )"
expression2BSV (Modulo x y)		= "( " ++ (expression2BSV x) ++" % "++ (expression2BSV y) ++ " )"
expression2BSV (Add x y)		= "( " ++ (expression2BSV x) ++" + "++ (expression2BSV y) ++ " )"
expression2BSV (Subtract x y)		= "( " ++ (expression2BSV x) ++" - "++ (expression2BSV y) ++ " )"
expression2BSV (Literal x)		= showLit x
expression2BSV (Identifier x)		= idPath2BSV x
expression2BSV (Exp_MethodCall inst meth zs _) = "( " ++ (showIDPath inst) ++ "." ++ meth ++ "( (" ++ (intercalate "), (" (map expression2BSV zs) ) ++ ") )"
expression2BSV (Exp_FunctionCall x ys)  = "( " ++ x ++ "( (" ++ (intercalate "), (" (map expression2BSV ys) ) ++ ") )"
expression2BSV (ValueMethodCall w x y z)= z ++ "()"
expression2BSV (Exp_If x y z)           = (expression2BSV x) ++ " ? " ++ (expression2BSV y) ++ " : " ++ (expression2BSV z) 
expression2BSV (Skip)                   = ""
--expression2BSV (Struct_Call x y)        = x ++ "." ++ y
expression2BSV (RPFlag x)               = expression2BSV x 


idPath2BSV :: ID_Path -> String 
idPath2BSV (ID x) = x
idPath2BSV (ID_Submod_Struct x recur) = x ++ "." ++ (idPath2BSV recur)
--idPath2BSV (ID_StructRef x y) = x ++ "." ++ (idPath2BSV y)

-- | converts a lexer-bound BSV type to a string useable in a generated BSV file.
showBSVType :: BSVType -> String
showBSVType (BSV_Bool) = "Bool"
showBSVType (BSV_Bit n) = "Bit#("++(show n)++")"
showBSVType (BSV_Int n) = "Int#("++(show n)++")"
showBSVType (BSV_UInt n) = "UInt#("++(show n)++")"
showBSVType (BSV_Real) = "Real"
showBSVType (BSV_Custom n) = n

showBSVFunctions :: [BSVFunction] -> String 
showBSVFunctions [] = []
showBSVFunctions xs = "\t" ++ (intercalate "\n\n\t" (map (showBSVFunction) xs)) ++ "\n\n\n"

showBSVFunction :: BSVFunction -> String 
showBSVFunction (nom, args, typ, stmts) = intercalate "\n\t" [header, middle, footer]
  where
    header = "function " ++ (showBSVType typ) ++ " " ++ nom ++ " (" ++ (intercalate ", " (map showBSVArgument args)) ++ ");" 
    middle = intercalate "\n\t" $ map (\ x -> showBSVStatement x "  ") stmts
    footer = "endfunction"
    
showLit :: Lit -> String 
showLit (LitString x) = show x
showLit (LitEnum x) = x
showLit (LitInt x) = show x
showLit (LitBool x) = show x
showLit (LitReal x) = show x
showLit (LitChar x) = show x
showLit (LitSizedInt _ x) = show x
showLit x = error "Error!  I can't show this type of literal : " ++ (show x)
