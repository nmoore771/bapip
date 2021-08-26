{-# OPTIONS_GHC -fno-warn-tabs #-}

module LexerTypes where

--import Language.Haskell.Pretty
import Data.List
import Debug.Trace

-- | Organizational, distinguishes input file paths from other types of strings, mainly used by IO module (bsv2pvs).
type InputPath = String
-- | Organizational, distinguishes output file paths from other types of strings, mainly used by IO module (bsv2pvs).
type OutputDir = String
type Spacing = String

data Mode = B2P | P2B | B2B | P2P | T2B | T2P deriving (Eq, Show)

--------BSV Side-------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Data type defining valid BSV types (i.e., return types for value methods, register types, arguments, etc.)
data BSVType = BSV_Bool | BSV_Bit N | BSV_Int N | BSV_UInt N | BSV_Real | BSV_Custom Name | BSV_Maybe BSVType deriving (Eq, Show, Ord)
type N = Integer

-- | Record type containing all the post-parsing data for a parsed BSV file.  
data BSVPackage = BSVPackage  	{ bsv_packageName :: PackageName -- ^ The name of the package that has been parsed.  
				, imports :: [PackageName] -- ^ a list of packages which this package imports
				, including :: [String]
				, interfaces :: [InterfaceDec] -- ^ a list of interfaces declared in this package
				, bsv_constants :: [BSVConstantDec] -- ^ a list of constants which have been declared by the current package
				, bsv_typedefs :: [BSVTypeDef] -- ^ a list of type definitions declared in the current package
				, bsv_instDefs :: [BSVInstDef] -- ^ a list of default structure value instantiations.  
				, bsv_modules :: [BSVModuleDec] -- ^ a list of modules contained by the current package.
				, bsv_functions :: [BSVFunction]
				, bsv_macros :: [BSVMacro]
				, hexFiles :: [HexFile]
				--, requestedTransitions :: [[String]]
				} deriving (Eq, Show)

-- | Relabels a string as a Package Name for organization and clarity.
type PackageName = String

type HexFile = ( String , [Literal] ) 

data BSVMacro = SimpleMacro String String | CompoundMacro String [String] String deriving (Show,Eq)

-- | a parsed interface declaration.  Interface delcarations consist of a name, which modules may invoke, and a list of method declarations.  
type InterfaceDec = ( Name, [MethodDec] , [InterfaceRef], [InterfaceAttribute])
-- | a parsed method declaration.  Method declarations consist of a name, a return type (as distinct from the general BSV type), and a list of typed arguments.
type MethodDec = (Name, ReturnType, [Argument], [MethodDecAttribute]) 
-- | The return type of a method may be one of three possible types:
-- | Action types declare methods which impact the state of the module, but do not return anything.
-- | Value types declare mehtods which do not impact the state of the module, and return a value of the specified BSV type.
-- | ActionValue methods both impact the state of a module and return a value of the specified BSV type.
data ReturnType = Action | ActionValue BSVType | Value BSVType deriving (Eq, Show)
-- | An argument is simply an identifier with an associated BSV type.
type Argument = (Name, BSVType, [ArgumentAttribute])
type BSVInstDef = (Name, [(Name, Literal)])
type BSVFunction = (String, [Argument], BSVType, [Statement])
type InterfaceRef = (InterfaceName, String)

-- | Constant declarations declare a synonym for some literal value, of any valid literal type.  
type BSVConstantDec = (Name, BSVType, Literal)
type Literal = Expression
	
-- | Encodes a type definition declaration in BSV.  These can be either type synonyms, or relabelling of types, or enumerations.
data BSVTypeDef = BSV_Synonym Name BSVType | BSV_Enumeration Name [Enumerat] | BSV_Struct Name [BSV_Field] deriving (Eq, Show)
type Enumerat = String
type BSV_Field = (Name, BSVType)

-- expression 1 for expression 2
type Replacement = (Expression, Expression)

-- | A record containing all the information contained by a module.  BSV modules are parsed using permutation parsing, so the order elements occur in is lost in this format.  This occurs without the loss of any information, aside from misplacing comments, which the parser disregards anyways.  
data BSVModuleDec = BSVModuleDec{ mName :: String -- ^ The declared name of the module
				, instanceName :: String -- ^ A unique identifier, signifying this module's place in the module heirarchy.
				, instances :: [BSVModuleDec] -- ^ A list of submodules instantiated in this module.  
				, interfaceName :: String -- ^ the name of the interface that this module invokes.
				, interfaceDecs :: [MidModInterfaceDec]
				, attributes :: [ModuleAttribute] -- ^ the attributes declared by this module, which are attributable to the module itself, as opposed to any sub-components.
				, state :: [BSVstateDec] -- ^ The declared registers, fifos, etc.  declared by this module.  
				, actions :: [ActionDec] -- ^ The actions declared in this module
				, rules :: [RuleDec] -- ^ the rules declared in this module.  
				, methods :: [MethodBody] -- ^ a list of method bodies, which define the behaviour of methods declared in the invoked interface.
				} deriving (Eq)

instance Show BSVModuleDec where
    show mod = intercalate "\n\t\t" [ ("BSVModule Dec { module name = " ++ (mName mod))
                , ("instance name = " ++ (instanceName mod))
                , ("list of instances = [" ++ (intercalate ", " (map instanceName (instances mod))) ++ "]" )
                , ("interface name = " ++ (interfaceName mod))
                , ("list of actions = [" ++ (intercalate "\n\t\t\t, " (map (\ (x,_,_) -> x) (actions mod))) ++ "]" )
                , ("list of rules = [" ++ (intercalate "\n\t\t\t, " (map (\ (x,_,_,_) -> x) (rules mod))) ++ "]" )
                , ("list of methods = [" ++ (intercalate "\n\t\t\t, " (map (\ (x,_,_,_,_,_) -> x) (methods mod))) ++ "]" )
                ]

mkBSVModule :: BSVModuleDec 
mkBSVModule = BSVModuleDec { mName = ""
    , instanceName = ""
    , instances = []
    , interfaceName = ""
    , interfaceDecs = []
    , attributes = []
    , state = []
    , actions = []
    , rules = [] 
    , methods = []
    } 
    
    
data PermMod = PermMod	{ stateM :: [BSVstateDec]
			, actionsM :: [ActionDec]
			, rulesM :: [RuleDec]
			, methodsM :: [MethodBody]
			, intersM :: [MidModInterfaceDec]
			} deriving (Show)

type MidModInterfaceDec = (String, String, [MethodBody])	
			
-- | State, in the case of BSV modules, consists of "state-holding submodules."  Currently supported are Registers, FIFO buffers, and Vectors (which function more or less equivalently to arrays).  
data BSVstateDec = BSV_Reg ID_Path BSVType Init 
   | BSV_Fifo FifoType ID_Path BSVType 
   | BSV_Vector ID_Path BSVType N VectorInit 
   | BSV_RegFile ID_Path AddressWidth BSVType RegFileLoader 
   | BSV_SubModuleDec InterfaceName Name InstName 
   | DWire ID_Path BSVType Init 
   deriving (Eq, Show)
   
data FifoType = FIFO | FIFOF | SizedFIFO Literal | SizedFIFOF Literal | DepthParamFIFO | FIFO1 | FIFOF1 | LFIFO | LFIFOF | PipelineFIFO | PipelineFIFOF | BypassFIFO | BypassFIFOF | SizedBypassFIFOF Literal deriving (Eq, Show) 
   
--data BSVstateType = BST_Reg BSVType deriving (Eq, Show)
data RegFileLoader = RegFileLoad FileName MinIndex MaxIndex deriving (Eq, Show)
type MinIndex = Literal
type MaxIndex = Literal
type AddressWidth = BSVType
type Name = String
type Init = Expression
type InstName = String

-- | Actions are groups of statements that may be invoked as a statement in a rule or method (or other action).  
type ActionDec = ( ActionName, [Statement], [ActionAttribute] )
-- | Rules are the work-horse of the BSV module.  They have a guard expression, and may activate in clock cycles when the guard evaluates to true.  They have somewhat complex semantics, which are explained further in the file generator documentation.
type RuleDec = ( RuleName, Guard, [Statement], [RuleAttribute] )
-- | Methods are like rules, but they do not activate themselves.  Rather, they are called by a supermodule in order to interface with its submodules.  If guarded, they introduce implicit conditions on supermodules.
type MethodBody = ( MethodName, ReturnType, UTArgs, Guard, [Statement], [MethodBodyAttribute] )

-- | Statements can be of a number of different types, including register writes, method calls, action invokations, return expressions, if statements, for loops and switch-cases.  It should be noted that for loops are not in the imperitive sense, but used to iterate concurrent expressions over constructs such as Vectors.
data Statement = Write ID_Path Expression [StatementAttribute]  
--  | VectW ID_Path Index Expressio"LocalVar >> "n [StatementAttribute] 
  | MethodCall ID_Path MethodName [Expression] [StatementAttribute] 
  | ActionCall ActionName [StatementAttribute] 
  | Return Expression [StatementAttribute] 
  | StructReturn BSVType [(String, Expression)] [StatementAttribute]
  | If Guard Then Else [StatementAttribute] 
  | PMatchIf ID_Path ID_Path Statement Statement [StatementAttribute]
  | ForLoop [Inits] Guard [Increments] Statement [StatementAttribute] 
  | Switch Guard [Case] [StatementAttribute] 
--  | Let Name Expression [StatementAttribute] 
  | LocalDec [LocalVar] Statement [StatementAttribute]
  | StatementBlock [Statement] 
  | Void 
  deriving (Eq)
 
instance Show Statement where
    show (Write i ex _) = (show i) ++ " <= " ++ (show ex)
    show (MethodCall w x y _) =  "Method Call >> " ++ (show w) ++ "." ++ (show x) ++ "( " ++ (intercalate "\n\t\t\t, " (map show y)) ++ " )"  
    show (ActionCall x _) = "Action Call >> " ++ (show x) 
    show (Return x _) = "Return >> " ++ (show x)
    show (StructReturn t x _) = "Struct Return >> " ++ (show t) ++ " >> " ++ (intercalate "\n\t\t" (map show x))
    show (If g t e _) = "IF >> " ++ (show g) ++ "\nTHEN >> " ++ (show t) ++ "\nELSE >> " ++ (show e) 
    show (PMatchIf i1 i2 t e _) = "PMatchIf >> Maybe " ++ (show i1) ++ " Matching " ++ (show i2) ++ "\nTHEN >> " ++ (show t) ++ "\nELSE >> " ++ (show e) 
    show (ForLoop i g inc stmt _) = "ForLoop >> \n\tInits >> " ++ (show i) ++ "\n\tGuard >> " ++ (show g) ++ "\n\tIncrementers >> " ++ (show inc) ++ "\n" ++ (show stmt)
    show (Switch gd cases _) = "Switch >> \n\tGuard >> " ++ (show gd)++ "\nCases >> " ++ (intercalate "\n\t\t" (map show cases))
--    show (Let n exp _ ) = "LET >> " ++ (show n) ++ " = " ++ (show exp) 
    show (LocalDec lvs stmt _) = "LocalVar >> " ++ (intercalate "\nLocalVar >> " (map show lvs)) ++ "\n IN \n" ++ show stmt
    show (StatementBlock xs) = "BEGIN\n" ++ (intercalate "\n| " (map show xs)) ++ "\nEND" 
    show (Void) = "Void >>"
 
type Case = (Literal, Statement) 
type ExpCase = (Literal, Expression)
type Guard = Expression 
type Then = Statement
type Else = Statement
type Inits = UStatement 
type Increments = UStatement


-- | "Un" stands for "uninterpreted"
data UStatement = DeclAssign UType Name Expression | UAssign Name Expression deriving (Eq, Show)

data UType = U_Int | U_String deriving (Eq, Show)


-- | The File datatype is used to organize post-generation data which is ready to be written into the system's ROM.  
type File = ( FileName, FileContents )
type FileName = String
type FileContents = String

-- | Organizational renaming for low-level functions.
type Spacer = String 

-- | Expressions are collections of tokens denoting mathematical operations.  These include boolean operations such as eqality and other comparisions, bitwise operations, arithmetic operations, and others.  
data Expression = Negative Op
  | Not Op
  | Equals Op1 Op2
  | NotEquals Op1 Op2
  | GreaterEquals Op1 Op2
  | LessEquals Op1 Op2
  | Greater Op1 Op2
  | Less Op1 Op2
  | And Op1 Op2
  | Or Op1 Op2
  | BitwiseAND Op1 Op2
  | BitwiseOR Op1 Op2
  | BitwiseXOR Op1 Op2
  | LShift Op1 Op2
  | RShift Op1 Op2
  | BitSelect Op1 Op2
  | BitSelectRange Op Op1 Op2
  | BitConcat [Op]
  | Multiply Op1 Op2
  | Divide Op1 Op2
  | Modulo Op1 Op2
  | Add Op1 Op2
  | Subtract Op1 Op2
  | Literal Lit
  | Identifier ID_Path
  | Exp_MethodCall ID_Path MethodName [Expression] (Maybe Writes)
  | Exp_FunctionCall String [Expression]
  | ValueMethodCall SuperModuleName ModuleName InstanceName MethodName
  | Exp_If Guard Op1 Op2
  | Skip
  | RPFlag Op
  | MaybeIf Matching ID_Path Op1 Op2
  | Tagged (Maybe PVSType) MaybeTag
  | FromMaybe ID_Path Op
  | MaybeValue Op 
  | Binding [LocalVar] Expression
  | CasesOf Expression [ExpCase] 
  | StructCluster (Either BSVType PVSType) [(String, Expression)]
  | PMatch MaybeIDTag
  | IsValid MaybeTag
  | FieldAccess Op ID_Path
  deriving (Eq, Ord, Show)

  
-- instance Show Expression where
--     show (Negative x)           = " -" ++ (show x)
--     show (Not x)                = " !" ++ (show x)
--     show (Equals x1 x2)         = "( " ++ (show x1) ++ " == " ++ (show x2) ++ " )"
--     show (NotEquals x1 x2)      = "( " ++ (show x1) ++ " != " ++ (show x2) ++ " )"
--     show (GreaterEquals x1 x2)  = "( " ++ (show x1) ++ " >= " ++ (show x2) ++ " )"
--     show (LessEquals x1 x2)     = "( " ++ (show x1) ++ " <= " ++ (show x2) ++ " )"
--     show (Greater x1 x2)        = "( " ++ (show x1) ++ " > " ++ (show x2) ++ " )"
--     show (Less x1 x2)           = "( " ++ (show x1) ++ " < " ++ (show x2) ++ " )"
--     show (And x1 x2)            = "( " ++ (show x1) ++ " && " ++ (show x2) ++ " )"
--     show (Or x1 x2)             = "( " ++ (show x1) ++ " || " ++ (show x2) ++ " )"
--     show (BitwiseAND x1 x2)     = "( " ++ (show x1) ++ " & " ++ (show x2) ++ " )"
--     show (BitwiseOR x1 x2)      = "( " ++ (show x1) ++ " | " ++ (show x2) ++ " )"
--     show (BitwiseXOR x1 x2)     = "( " ++ (show x1) ++ " ^ " ++ (show x2) ++ " )"
--     show (LShift x1 x2)         = "( " ++ (show x1) ++ " << " ++ (show x2) ++ " )"
--     show (RShift x1 x2)         = "( " ++ (show x1) ++ " >> " ++ (show x2) ++ " )"
--     show (BitSelect x1 x2)      = "( " ++ (show x1) ++ "[" ++ (show x2) ++ "])"
--     show (BitSelectRange x x1 x2) = "( " ++ (show x) ++ "[" ++ (show x1) ++ ":" ++ (show x2) ++ "])"
--     show (BitConcat xs)        = "( " ++ (intercalate " o " (map show xs)) ++ ")"
--     show (Multiply x1 x2)       = "( " ++ (show x1) ++ " * " ++ (show x2) ++ " )"
--     show (Divide x1 x2)         = "( " ++ (show x1) ++ " / " ++ (show x2) ++ " )"
--     show (Modulo x1 x2)         = "( " ++ (show x1) ++ " % " ++ (show x2) ++ " )"
--     show (Add x1 x2)            = "( " ++ (show x1) ++ " + " ++ (show x2) ++ " )"
--     show (Subtract x1 x2)       = "( " ++ (show x1) ++ " - " ++ (show x2) ++ " )"
--     show (Literal x)            = show x
--     show (Identifier x)         = show x
--     show (Exp_MethodCall mod meth exps _) = (show mod) ++ "." ++ (show meth) ++ "(" ++ (intercalate ", " (map show exps)) ++ ") "
--     show (Exp_FunctionCall func exps) = (show func) ++ "(" ++ (intercalate ", " (map show exps)) ++ ")"
--     show (ValueMethodCall superMod mod inst meth) =  (show inst) ++ "." ++ (show meth) ++ "() "
--     show (Exp_If guard x1 x2)   = "if " ++ (show guard) ++ " then " ++ (show x1) ++ " else " ++ (show x2)
--     show (Skip)                 = "skip!"
--     show (RPFlag x)             = "|" ++ (show x) ++ "|"
--     show (MaybeIf m i x1 x2)    = " if " ++ (show i) ++ " matches " ++ (show m) ++ " then " ++ (show x1) ++ " else " ++ (show x2)
--     show (Tagged _ x)             = show x
--     show (FromMaybe i x)        = "UnMaybe " ++ (show i) ++ " => default = " ++ (show x)
--     show (Binding lvs exp)      = "Let " ++ (show lvs) ++ " in " ++ (show exp)
--     show (CasesOf exp cases)    = "Switch " ++ (show exp) ++ "\n\t\t" ++ (intercalate "\n\t\t" (map show cases))
--     show (StructCluster typ cluster) = "Struct " ++ (show typ) ++ "{\n\t\t" ++ (intercalate "\n\t\t" (map show cluster)) ++ "\n\t\t}"
--     show (PMatch x)             = "PMatch " ++ (show x)
--     show (MaybeValue x)         = (show x)++ "`value"
--     show (IsValid x)            = "isValid " ++ (show x)
    
type Writes = [ID_Path]
    
-- | Op is short for "operand"
-- type Exp_Case = (Literal, Exp)
data Lit = LitString String | LitEnum String | LitInt Integer | LitBool Bool | LitReal Float | LitChar Char | LitSizedInt N Integer | LitStructConstructor | LitVoid deriving (Eq,Ord)

instance Show Lit where
    show (LitString x) = show x
    show (LitEnum x) = ":" ++ show x ++ ":"
    show (LitInt x) = show x
    show (LitBool x) = show x
    show (LitReal x) = show x
    show (LitChar x) = x:[]
    show (LitSizedInt x y) = (show x) ++ "`" ++ (show y)
    show (LitStructConstructor) = "--Struct Constructor--"
    show (LitVoid) = "--Void Literal--"

data MaybeIDTag = ValidID ID_Path | InvalidID deriving (Eq, Ord)
data MaybeTag = Valid Expression | Invalid | MaybeContainer Expression deriving (Eq, Ord)

instance Show MaybeIDTag where
    show (ValidID x) = "Just("++ (show x) ++")"
    show (InvalidID) = "Nothing"
    
instance Show MaybeTag where
    show (Valid x) = "Just("++ (show x) ++ ")"
    show (Invalid) = "Nothing"
    show (MaybeContainer x) = "MaybeContainer(" ++ (show x) ++ ")"

type LocalVar = (ID_Path, (Either (Maybe BSVType) (Maybe PVSType)), Expression)
type Matching = ID_Path
type Op = Expression
type Op1 = Expression
type Op2 = Expression
type ModuleName = String
type SuperModuleName = String
type InstanceName = String
type MethodName = String
type UTArgs = [(String, Maybe BSVType)] -- ^ untyped args, just identifiers. for method block declarations.
data ID_Path = ID_Submod_Struct ModuleInst ID_Path | ID String | ID_Vect String Index deriving (Eq, Ord)
type ModuleInst = String
type Index = Expression
type StructName = String
type FieldName = String

instance Show ID_Path where
    show (ID_Submod_Struct x y) = x ++ "." ++ (show y)
    show (ID x) = x
    show (ID_Vect x y) = x ++ "[" ++ (show y) ++ "]"

data ActionPath = ActionNameAP String | RuleNameAP String | MethodNameAP String | SubmoduleNameAP String ActionPath deriving (Eq)

instance Show ActionPath where
    show (ActionNameAP x) = "< " ++  x ++ " >"
    show (RuleNameAP x) = "< " ++  x ++ " >"
    show (MethodNameAP x) = "< " ++  x ++ " >"
    show (SubmoduleNameAP x y) = x ++ " => " ++ (show y)
    

type ActionName = String

-- | Data type containing possible values of module attributes
data ModuleAttribute = Synthesize | Mod_Always_Ready [String] | Mod_Always_Enabled [String] | Descending_Urgency [String] | Mod_Execution_Order [String] | Mod_Mutually_Exclusive [String] | Mod_Conflict_Free [String] | Mod_Preempts [String] | Clock_Prefix String | Gate_Prefix String | Reset_Prefix String | Gate_Input_Clocks [String] | Gate_All_Clocks | Default_Clock_OSC String | Default_Clock_Gate String | Default_Gate_Inhigh | Default_Gate_Unused | Default_Reset String | No_Default_Reset | Clock_Family [String] | Clock_Ancestors [String] | Mod_Doc String | MAvoid deriving (Eq, Show)
-- | Data type containing possible values of interface attributes
data InterfaceAttribute = Int_Always_Ready | Int_Always_Enabled  | Int_Doc String deriving (Eq, Show)
-- | Data type containing possible values of argument attributes
data ArgumentAttribute = Arg_Ready String | Arg_Enable String | Arg_Result String | Arg_Prefix String | Arg_Port String | Arg_Always_Ready | Arg_Always_Enabled | Arg_Doc String deriving (Eq, Show)
-- | Data type containing possible values of Method Declaration attributes
data MethodDecAttribute = MDc_Ready String | MDc_Enable String | MDc_Result String | MDc_Prefix String | MDc_Port String | MDc_Always_Ready | MDc_Always_Enabled | MDc_Doc String deriving (Eq, Show)
-- | Data type containing possible values of action attributes
data ActionAttribute =  Act_Doc String deriving (Eq, Show)
-- | Data type containing possible values of rule attributes
data RuleAttribute = Fire_When_Enabled | No_Implicit_Conditions | Rul_Descending_Urgency [String] | Rul_Execution_Order [String] | Rul_Mutually_Exclusive [String] | Rul_Conflict_Free [String] | Rul_Preempts [String] | Rul_Doc String  deriving (Eq, Show)
-- | Data type containing possible values of Method Body attributes
data MethodBodyAttribute =  Met_Doc String deriving (Eq, Show)
-- | Data type containing possible values of statement attributes
data StatementAttribute = Split | NoSplit | Sta_Descending_Urgency [String] | Sta_Execution_Order [String] | Sta_Mutually_Exclusive [String] | Sta_Conflict_Free [String] | Sta_Preempts [String] | Sta_Doc String  deriving (Eq, Show)


-- | a custom test for equality of method delcarations.  If a method has the exact same return type and arguments, it is the same.
mEq :: MethodDec -> MethodDec -> Bool
(mEq) (_, ret, args, _) (_, ret', args', _) = (ret == ret') && (args `aEq` args')
-- | a custom test for equality of lists of arguments.  Considers only the type of the argument, not the names.
aEq :: [Argument] -> [Argument] -> Bool
aEq [] [] = True
aEq [] ((_, t, _):ts) = False
aEq ((_, t, _):ts) [] = False
aEq ((_, t, _):ts) ((_, t', _):ts2) = (t == t') && (ts `aEq` ts2) 

--------PVS Side------------------------------------------------------------------------------------------------------------------------------------

data PVSType = PVS_Bool | PVS_Bit N | PVS_Int N | PVS_UInt N | PVS_Real | PVS_Custom Name | PVS_Maybe PVSType deriving (Eq, Show, Ord)

-- | Encodes a type definition declaration in BSV.  These can be either type synonyms, or relabelling of types, or enumerations.
data PVSTypeDef = PVS_Synonym Name PVSType | PVS_Enumeration Name [Enumerat] | PVS_Struct Name [PVS_Field] deriving (Eq, Show)
type PVS_Field = (Name, PVSType)
type DefaultValue = Expression

-- | Record structure for organizing PVS-centric generated data.
data PVSPackage = PVSPackage  	{ pvs_packageName :: PackageName -- ^ The name of the package that has been parsed.  
				, pvs_constants :: [PVSConstantDec] -- ^ a list of constants which have been declared by the current package
				, pvs_typedefs :: [PVSTypeDef] -- ^ a list of type definitions declared in the current package
				  , transitions :: [PVStransition]
				, pvs_state :: [PVSstateDec] 
				, pvs_instantiations :: [PVSInstDef]
				, pvs_functions :: [PVSFunction]
				} deriving (Show)

type PVSConstantDec = (Name, PVSType, Literal) 
type PVSstateDec = (Name, [PVSstate])
type PVSInstDef = (Name, [(Name, Literal)])
data PVSstate = PVS_Reg ID_Path PVSType Init 
              | PVS_Fifo FifoType ID_Path PVSType 
              | PVS_Vector ID_Path PVSType N VectorInit
              | PVS_SubModuleDec InterfaceName Name InstName 
              | PVS_DWire ID_Path PVSType Init 
                deriving (Eq, Show)
                
data VectorInit = Replicate Literal | Explicit [Literal] deriving (Show, Eq)


type PVStransition = (Integer, [(MethodName, [(MethodArg, PVSType)])], [ValueMethod], [TransitionTable])

-- | A State-centric organization of the hardware design, and an intermediate representation when translating from BSV to PVS.  Each Transition table corresponds to a module, and each transition tree handles how rules and methods effect one particular state element. 
type MethodArg = String
--type WireFunction = (ID_Path, ModuleName, PVSType, Expression)
type TransitionVar = (PVSType, [String])
type ValueMethod = (MethodName, ModuleName, String, Path, PVSType, Expression, [ID_Path])

type Path = [String]



data TransitionTable = TransMod Name [TransitionTable] 
  | TransReg ID_Path SpecificTree 
  | TransVect ID_Path Size [(Expression, SpecificTree)] 
  | TransStruct Name [TransitionTable] 
  | TransDWire ID_Path SpecificTree DefaultValue 
  | TransFIFO ID_Path EnqTree DeqTree ClearTree deriving (Show, Eq)

type EnqTree = SpecificTree
type DeqTree = SpecificTree
type ClearTree = SpecificTree

type InterfaceName = String

type TransitionGuard = (RuleName, ModuleName, Expression) -- (ruleName, )
type Size = Integer 

-- | An intermediate representation of the intermediate representation, with attached data about required method applications to the current module.  
data TransitionTableMethods = TransModMethods Name [TransitionTable] [MethodApplication] deriving (Show)

type MethodApplication = MethodBody

-- | Transition Tree: a custom data type for the PVS side. A binary tree showing representing the if-then-else structure of deciding which value to write into a particular state element.   
--data TransTree = Stem RuleName ModuleName TrueTree FalseTree | Leaf Expression deriving (Show, Eq)
--data VectTransTree = VectStem RuleName ModuleName TrueVectTree FalseVectTree | VectLeaf VectExps deriving (Show, Eq)
--type TrueTree = TransTree
--type FalseTree = TransTree
--type TrueVectTree = VectTransTree
--type FalseVectTree = VectTransTree
--type VectExps = [(IndexExp, Expression)]
--type IndexExp = Expression
type RuleName = String

-- | an intermediate data representation used to transition from BSV's rule-based organization to PVS's state-based organization.
data RuleUrgParams = RuleUrgParams 	{ rpName :: String
					, rpGuard :: Guard 
					, writesToP :: [ID_Path]
					, preEmpts :: [ActionPath]
					, doesntPreEmpt :: [ActionPath]
					, isPreEmptedByP :: [ActionPath]
					, isntPreEmptedBy :: [ActionPath]
					} deriving (Show)

data RuleSchedule = RuleSchedule{ rName :: ActionPath
				, rGuard :: Expression	
				, rStatements :: [Statement]
				, implicitConditions :: [Expression]
				, writesTo :: [ID_Path]
				, dWireWrites :: [ID_Path]
				, dWireReads :: [ID_Path]
				, fifoEnqs :: [ID_Path]
				, fifoDeqs :: [ID_Path]
				, fifoClears :: [ID_Path]
				, fifoFirsts :: [ID_Path]
                , actionMethodsCalled :: [Expression] 
				, conflictsWith :: [ActionPath]
				, noConflictsWith :: [ActionPath]
				, preempts :: [ActionPath]
				, isPreemptedBy :: [ActionPath]
				, executesAfter :: [ActionPath]
				, executesBefore :: [ActionPath]
				} deriving (Eq)

instance Show RuleSchedule where
    show x = "Rule Schedule for " ++ (show (rName x)) ++ "\n\t" ++  (intercalate "\n\t" (xs))
      where
        xs = [l1,l2,l3,l4]
        l1 = "Preempts : " ++ (show (preempts x))
        l2 = "Is Preempted By : " ++ (show (isPreemptedBy x))
        l3 = "Executes Before : " ++ (show (executesBefore x))
        l4 = "Executes After : " ++ (show (executesAfter x))
				
type RuleGraph = [RuleNode]
type RankedNodes = [[[RuleNode]]]
data RuleNode = RuleNode { rnName :: String
                         , rnPreempts :: [ActionPath]
	                 } deriving (Eq, Show)

data RuleTree = RuleStem (Maybe String) ModuleName RuleTree | RuleLeaf deriving (Eq, Show) 

--data WriteSchedule = RegSchedule ID_Path SpecificTree | VectSchedule ID_Path SpecificTree | SubMod ModuleInst SubmoduleName [WriteSchedule] | StructSchedule LocalName [WriteSchedule] | DWireSchedule ID_Path SpecificTree deriving (Show)
type LocalName = ID_Path

-- | An intermediate data type for clarity and organization
type MethodCallPass = (SubmoduleName, Guard, UrgIndex, MethodName, [Expression])
-- ^ 1 - submodule name directs the call to the correct instance
-- ^ 2 - The calling rule's guard must be inherited by the method call
-- ^ 3 - place in which the calling rule occurs in the module's descending urgency list.  This is also inherited by the method call
-- ^ 4 - The Method to be called
-- -- ^ 5 - the argument expressions recieved by the method (hopefully pre-flattened...?) 
type SubmoduleName = String
type UrgIndex = Int

type PVSstateRecord = (String, [(String, PVSType)])
type PVSstateInst = (String, [PVSstate])
-- | (identifier, argument list with types, return type, expression) This'll be a statement in BSV.  
type PVSFunction = (String, [PVSArgument], PVSType, Expression)
type PVSArgument = (String, PVSType)

data RuleHeap = RHeap String [RuleSchedule] [RuleHeap] deriving (Eq)

instance Show RuleHeap where
    show x = showRuleHeap "\n" x
    
showRuleHeap :: String -> RuleHeap -> String
showRuleHeap spc (RHeap x rs hs) = spc ++ x ++ " ==> " ++ (intercalate (spc ++ "    ") (map (show.rName) rs)) ++ (intercalate (spc ++ "  ") (map (showRuleHeap (spc ++ "  ")) hs)) 

data TreeHeap = THeap String TotalTree [TreeHeap] deriving (Eq, Show)

data TotalTree = TotalStem Guard [Statement] TrueTree FalseTree | TotalLeaf Guard [Statement] deriving (Eq)
type TrueTree = TotalTree
type FalseTree = TotalTree

instance Show TotalTree where
    show x = showTT x "" 
        
showTT :: TotalTree -> String -> String 
showTT (TotalStem gd sts tt ft) spc = intercalate ("\n"++spc) (( "--" ++ (show gd) ++ "--" ): ((map show sts) ++ (showTT tt (spc ++ "  ")):[] ++ (showTT ft (spc ++ "  ")):[]))
showTT (TotalLeaf gd sts) spc = intercalate ("\n"++spc) (("--"++(show gd)++"--") : (map show sts))

data SpecificTree = SpecStem Guard (Either Expression SpecTrueTree) SpecFalseTree | SpecLeaf Guard TrueExpression FalseExpression | SpecEx Expression deriving (Eq)
type SpecTrueTree = SpecificTree
type SpecFalseTree = SpecificTree
type TrueExpression = Expression
type FalseExpression = Expression

instance Show SpecificTree where
    show x = showST x "" 
        
showST :: SpecificTree -> String -> String 
showST (SpecStem gd (Left exp) ft) spc = intercalate ("\n"++spc) [( "--" ++ (show gd) ++ "--" ), ("  T = " ++ (show exp)), ("F = " ++  (showST ft (spc ++ "  ")))]
showST (SpecStem gd (Right tt) ft) spc = intercalate ("\n"++spc) [( "--" ++ (show gd) ++ "--" ), ("T = " ++ (showST tt (spc ++ "  "))), ("F = " ++ (showST ft (spc ++ "  ")))]
showST (SpecLeaf gd texp fexp) spc = intercalate ("\n"++spc) [( "--" ++ (show gd) ++ "--" ), ("  T = " ++ (show texp)), ("  F = " ++ (show fexp))] 
showST (SpecEx exp) spc = ("--"++(show exp)++"--")

showShortened :: SpecificTree -> String -> String
showShortened (SpecStem gd (Left exp) ft) spc = intercalate ("\n"++spc) [( "--" ++ (show gd) ++ "--" ), ("  T = some expression"), ("F = " ++  (showST ft (spc ++ "  ")))]
showShortened (SpecStem gd (Right tt) ft) spc = intercalate ("\n"++spc) [( "--" ++ (show gd) ++ "--" ), ("T = " ++ (showST tt (spc ++ "  "))), ("F = " ++ (showST ft (spc ++ "  ")))]
showShortened (SpecLeaf gd texp fexp) spc = intercalate ("\n"++spc) [( "--" ++ (show gd) ++ "--" ), ("  T = some expression"), ("  F = some expression")] 
showShortened (SpecEx exp) spc = ("-- some expression --")



data InvokationTree = Istem Name ModuleName [InvokationTree] | Ileaf Name ModuleName deriving (Eq, Show)

----------------------------------------------------------------TSP------------------------------------------------------------------------------

data TSPpackage = TSPpackage { tName :: String 
		  , typedefs :: [PVSTypeDef]
		  , defInsts :: [PVSInstDef]  
                  , varDecs :: [TVarDec]
                  , tsps :: [TSPTable]
                  , tsp_funcs :: [PVSFunction]
                  , macros :: [PVSMacro]
                  } deriving (Show, Eq) 

type TVarDec = ([String], PVSType)
-- ( Table name, output variable, init expression, variable synonyms, input variables, table lines)
type TSPTable = (TName, TSPOutVar, Expression, [Replacement], [String], [TSPLine]) 
type TSPLine = (Guard, Expression)
type TSPOutVar = (ID_Path, Temporal)
data Temporal = N_Time Int deriving (Eq, Show)

type TName = String
type Tperm = (Maybe TVarDec, Maybe TSPTable, Maybe PVSTypeDef, Maybe PVSInstDef, Maybe PVSFunction, Maybe PVSMacro)
type PVSMacro = (String, PVSType, Literal) -- (name, type, literal)

type Context = Maybe (String, String)
 ----------------------------------------------------------------Preprocessor---------------------------------------------------------------------
 
 ----------------------------------------------------------------Other---------------------------------------------------------------------
