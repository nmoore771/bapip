{-# OPTIONS_GHC -fno-warn-tabs #-}

module BAPIP where

import System.Environment
import System.IO
import System.Directory

import BSVLexer
import HEXLexer
import TSPLexer
import BSV2PVS
import PVS2BSV
import TSP2BSV
import BSVGenerator
import PVSGenerator
import LexerTypes
import MacroProcessor (runBSVMacros, parseMacros', removeIncludes, genDefineFile, findIncludes, findLocalDefines, removeDefines, dropAllComments)
import SourceFiles
import ConflictSolver (getSolvedSchedule)
--import FileGenerators
import Data.List (intercalate, nub)
import Data.List.Split (splitOn)
import Data.Char (toLower)

-- | This is the main function. It's only purpose is to pass any command line arguments to the dispatch function.
bapip = do
  argList <- getArgs
  putStrLn "><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><"
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn "Commencing Execution of BAPIP"
  dispatch $ desensitizeHead argList
  putStrLn "Terminating Excecution of BAPIP"
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
  putStrLn "><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><"
  
-- | Dispatch parses the command line arguments recieved by the program, and executes the corresponding functions.  The first argument specifies which mode to execute in, or to display the README.txt document.  The second argument is not optional, and specifies the file to be translated.  The third argument is mandatory for processes beginning with BSV, and specifies the module within the specified file which will be the root module of the translation process.  The fourth argument is optional, and specifies the output directory.  If unspecified, the translator will automatically create a new folder in the active directory, and place the translator output there.
dispatch :: [String] -> IO()
dispatch [] = noCommandMessage
dispatch ("help":_) = showHelp
dispatch ("bsv2pvs":[]) = putStr "BSV2PVS Error: you must specify an input file\n\n"
dispatch ("bsv2pvs":x:[]) = openBSVLexer (B2P) x (Nothing) ((takeWhile (/= '.') x) ++ "-pvs")
dispatch ("bsv2pvs":x:y:[]) = if ('/' `elem` y) 
  then openBSVLexer (B2P) x (Nothing) y
  else openBSVLexer (B2P) x (Just y) ((takeWhile (/= '.') x) ++ "-pvs")
dispatch ("bsv2pvs":x:y:z:_) = openBSVLexer (B2P) x (Just y) z
--dispatch ("pvs2bsv":[]) = putStr "Coming Soon!"
dispatch ("bsv2bsv":[]) = putStr "BSV2BSV Error: you must specify an input file\n\n"
dispatch ("bsv2bsv":x:[]) = runBSV2BSV x ((takeWhile (/= '.') x) ++ "-bsv2bsv")
dispatch ("bsv2bsv":x:y:_) = runBSV2BSV x y
dispatch ("pvs2pvs":[]) = putStr "PVS2PVS Error: you must specify an input directory\n\n"
dispatch ("pvs2pvs":x:[]) = openPVSLexer (P2P) x ((takeWhile (/= '.') x) ++ "-pvs")
dispatch ("pvs2pvs":x:y:[]) = openPVSLexer (P2P) x y
dispatch ("tsp2bsv":[]) = putStr "TSP2BSV Error: you must specify an input file\n\n"
dispatch ("tsp2bsv":x:[]) = openTSPLexer (T2B) x (takeWhile (/= '.') x)
dispatch ("tsp2bsv":x:y:[]) = openTSPLexer (T2B) x y
dispatch ("tsp2pvs":[]) = putStr "TSP2PVS Error: you must specify an input file\n\n"
dispatch ("tsp2pvs":x:[]) = openTSPLexer (T2P) x (takeWhile (/= '.') x)
dispatch ("tsp2pvs":x:y:[]) = openTSPLexer (T2P) x y
dispatch (x:_) = noCommandMessage

desensitizeHead :: [String] -> [String]
desensitizeHead [] = []
desensitizeHead (x:xs) = (map toLower x) : xs

-- | Reads the file "README.txt", and displays it.
showHelp :: IO ()
showHelp = do 
  putStr readMe

-- | Error message informing the user that they have selected an invalid first argument.  Also informs the user of the "help" command.
noCommandMessage :: IO ()
noCommandMessage = putStr "Command not recognized.  Use \"help\" to view available commands.\n\n\n"

-- | Top Level function for executing translation process.  The primary purpose of this function is to ensure that a valid module was selected as the top level module.  If no module was specified as the top level module, a list of available modules will be returned.  If a module was specified, it first checks to make sure the specified module exists in the selected file, and then invokes "processFiles," the second stage of the translation process. 
openBSVLexer :: Mode -> FilePath -> Maybe String -> FilePath -> IO() 
openBSVLexer (B2P) inputFile (Nothing) outputDir = do
  bsvFile <- readFile inputFile
  let bsvFile'' = dropAllComments bsvFile
  bsvFile' <- runMacroPreprocessing bsvFile'' inputFile
  putStrLn $ "Parsing " ++ inputFile 
  let result = runParser inputFile (getOmnia []) bsvFile'
  let mods = bsv_modules result
  let modnames = map mName mods
  if (not (null modnames))
    then do 
      putStrLn ("\nError! No module selected!")
      putStrLn ("\nPlease select a module to translate from the list of available modules, and try again!")
      putStrLn (show modnames)
      putStrLn ("")
    else do
      putStrLn "<!> Selected package does not contain any modules! Skipping schedule generation...\n"
      processBSVFiles (B2P) (Nothing) inputFile [] [] outputDir
      putStrLn (">> Translation was Successful! <<")
openBSVLexer (B2P) inputFile (Just mod) outputDir = do
  bsvFile <- readFile inputFile 
  let bsvFile'' = dropAllComments bsvFile
  bsvFile' <- runMacroPreprocessing bsvFile'' inputFile
  putStrLn $ "Parsing " ++ inputFile
  let result = runParser inputFile (getOmnia []) bsvFile'
  let mods = bsv_modules result
  let modnames = map mName mods
  if (mod `elem` modnames) 
    then do
      outDir <- createAvailableDir outputDir 1
--      let importList = map (\ x -> reverse (dropWhile (/='/') (reverse inputFile)) ++ x ++ ".bsv" ) $ imports result
      processBSVFiles (B2P) (Just mod) inputFile [] [] outDir
      setCurrentDirectory ".."
      putStrLn (">> Translation was Successful! <<")
    else do
      putStrLn ("\nError! Selected module was not found in specified file!")
      putStrLn ("\nPlease select a module to translate from the list of available modules, and try again!")
      putStrLn (show modnames)
      putStrLn ("")

runBSV2BSV :: FilePath -> FilePath -> IO()
runBSV2BSV input output = do
  bsvFile <- readFile input
  let bsvFile'' = dropAllComments bsvFile
  outDir <- createAvailableDir output 1
  processBSVFiles (B2B) (Nothing) input [] [] outDir
  setCurrentDirectory ".."
  putStrLn (">> Translation was Successful! <<")  
  
excludedImports :: [String]
excludedImports = ["FloatingPoint", "Vector", "DefaultValue", "RegFile", "SpecialFIFOs", "FIFO", "FIFOF"]

-- :: bsvFile name -> imports-to-be-checked-for-imports -> imports-checked-for-imports -> output-file-path
-- | invokes pre-processing, parsing, file generation, and post-processing.  Preprocessing consists of gathering all the files in the top-level BSV package's import hierarchy, so that they can be parsed and collected into one collection of all modules, declarations, and other sundry elements, This is encoded as a record type, under the name "PermDecl," which abbreviates "Permuted Declaration."  As an argument, this collection is referred to in-code as the "omnibus," and is drawn on frequently during the file generationg process.
-- | Parsing and file generation are executed once all the appropriate files have been gathered, please refer to the corresponding documentation for more information.  
-- | Post processing consists simply of writing the generated files to the disk.
processBSVFiles :: Mode
	      	-> Maybe String -- ^ name of top level module 
              	-> FilePath -- ^ name of top level bsv file (as passed to the translator by command line argument)
              	-> [FilePath] -- ^ List of filepaths leading to imported bsv packages that haven't been processed. 
              	-> [FilePath] -- ^ List of filepaths leading to imported bsv packages that have been processed.  
              	-> FilePath -- ^ Output file path
              	-> IO()
-- processBSVFiles (B2P) (Nothing) inputFile [] [] outputDir = do 
--   bsvFile <- readFile inputFile 
--   bsvFile' <- runMacroPreprocessing bsvFile inputFile
--   putStrLn $ "Parsing for Module Names : " ++ inputFile
--   let result = runParser inputFile (getOmnia []) bsvFile'
--   let mods = bsv_modules result
--   let modnames = map mName mods
--   putStrLn ("\nError! No module selected!")
--   putStrLn ("\nPlease select a module to translate from the list of available modules, and try again!")
--   putStrLn (show modnames)
--   putStrLn ("")
processBSVFiles (B2P) x inputFile [] [] outputDir = do 
  bsvFile <- readFile inputFile  
  let bsvFile'' = dropAllComments bsvFile
  bsvFile' <- runMacroPreprocessing bsvFile'' inputFile
  putStrLn $ "Acquiring list of imports... " ++ inputFile
  let (imports', hexFiles) = runParser inputFile getImports bsvFile'  
  let imports = filter (\x -> not (x `elem` excludedImports)) imports' 
  let importFilePaths = map (\ x -> reverse (dropWhile (/='/') (reverse inputFile)) ++ x ++ ".bsv" ) imports 
--  error $ inputFile
  processBSVFiles (B2P) x inputFile importFilePaths (inputFile:[]) outputDir 
processBSVFiles (B2P) x inputFile (i:is) toProc outputDir = do 
--  error $ show ((i:is) ++ toProc) 
  bsvFile <- readFile i
  let bsvFile'' = dropAllComments bsvFile
  bsvFile' <- runMacroPreprocessing bsvFile'' i
--   putStrLn $ "Parsing recursively for imports :" ++ i
  let (imports', hexFiles) = runParser i getImports bsvFile'  
  let imports = filter (\x -> not (x `elem` excludedImports)) imports' 
  if null imports
    then 
      processBSVFiles (B2P) x inputFile is (i:toProc) outputDir
    else do
      let importFilePaths = map (\ x -> reverse (dropWhile (/='/') (reverse inputFile)) ++ x ++ ".bsv" ) imports 
      processBSVFiles (B2P) x inputFile (is ++ importFilePaths) (i:toProc) outputDir 
processBSVFiles (B2P) topMod inputFile [] toProc outputDir = do
  fileTexts <- mapM (\ x -> readFile x ) (nub toProc)
  let fileTexts'' = map dropAllComments fileTexts
  fileTexts' <- mapM (\ x -> runMacroPreprocessing x inputFile) fileTexts''
  let toProc' = nub toProc
  putStrLn $ "Parsing for translation : \n\t\t\t" ++ (intercalate "\n\t\t\t" toProc')
  let allHexFiles = concat $ map snd $ map (\ (x,y) -> runParser x getImports y) (zip toProc' fileTexts')
  hexes <- procHexFiles inputFile allHexFiles 
  let allParsed = map (\ (x,y) -> runParser x (getOmnia hexes) y) (zip toProc' fileTexts')
  userSchedules <- getUserSchedules inputFile
  allParsed' <- getSolvedSchedule allParsed topMod 
  let pvsPackage = bsv2pvs allParsed' topMod userSchedules
  let files = showPVSPackage pvsPackage Nothing
  -- outputDir' <- createAvailableDir outputDir 0
  putStrLn $ "Outputting result to directory : " ++ outputDir
  exists <- doesDirectoryExist outputDir
  if (not exists) then createDirectory outputDir else return ()
  setCurrentDirectory outputDir
  --let files' = files ++ [("ClockTick.pvs", clockTick), ("monad.pvs", monadFile), ("arith_bitwise.pvs", arith_BWFile), ("defined_operators.pvs", definedOperators), ("Time.pvs", timeFile), ("FIFO.pvs", fifoFile)]
  let files' = files ++ [("ClockTick.pvs", clockTick), ("Maybe.pvs", maybeFile), ("arith_bitwise.pvs", arith_BWFile), ("defined_operators.pvs", definedOperators), ("Time.pvs", timeFile), ("FIFO.pvs", fifoFile)] 
  writeFileList files'
  setCurrentDirectory $ dottify outputDir
processBSVFiles (B2B) (Nothing) inputFile [] [] outputDir = do 
  bsvFile <- readFile inputFile  
  let bsvFile'' = dropAllComments bsvFile
  bsvFile' <- runMacroPreprocessing bsvFile'' inputFile
  putStrLn $ "Parsing for imports : " ++ inputFile
  let (imports', hexFiles) = runParser inputFile getImports bsvFile'  
  let imports = filter (\x -> not (x `elem` excludedImports)) imports' 
  let importFilePaths = map (\ x -> reverse (dropWhile (/='/') (reverse inputFile)) ++ x ++ ".bsv" ) imports 
  processBSVFiles (B2B) (Nothing) inputFile importFilePaths (inputFile:[]) outputDir 
processBSVFiles (B2B) (Nothing) inputFile (i:is) toProc outputDir = do 
  bsvFile <- readFile i  
  let bsvFile'' = dropAllComments bsvFile
  bsvFile' <- runMacroPreprocessing bsvFile'' inputFile
  putStrLn $ "Parsing recursively for imports : " ++ inputFile
  let (imports', hexFiles) = runParser inputFile getImports bsvFile'  
  let imports = filter (\x -> not (x `elem` excludedImports)) imports' 
  if null imports
    then 
      processBSVFiles (B2B) (Nothing) inputFile is (i:toProc) outputDir
    else do
      let importFilePaths = map (\ x -> reverse (dropWhile (/='/') (reverse inputFile)) ++ x ++ ".bsv" ) imports 
      processBSVFiles (B2B) (Nothing) inputFile (is ++ importFilePaths) (i:toProc) outputDir 
processBSVFiles (B2B) (Nothing) inputFile [] toProc outputDir = do
  fileTexts <- mapM (\ x -> readFile x ) toProc
  let fileTexts'' = map dropAllComments fileTexts
  putStrLn $ "Parsing for (non)translation : " ++ (show toProc)
  let allHexFiles = concat $ map snd $ map (\ (x,y) -> runParser x getImports y) (zip toProc fileTexts'')
  hexes <- procHexFiles inputFile allHexFiles 
  let omnibus = map (\ (x,y) -> runParser x (getOmnia hexes) y) (zip toProc fileTexts'')
  let files = showBSVPackages omnibus
  setCurrentDirectory outputDir
  writeFileList files
  setCurrentDirectory $ dottify outputDir

procHexFiles :: FilePath -> [FilePath] -> IO [HexFile]
procHexFiles inputFile toProc' = do 
  let prefix = reverse (dropWhile (/='/') (reverse inputFile))
  let toProc = map (\ x -> prefix ++ x) toProc'
  putStrLn $ "    Processing Hex Files : " ++ (show toProc)
  fileTexts <- mapM (\ x -> readFile x ) toProc
  let mid = zip toProc fileTexts 
  let hexes = map (\ (x, y) -> runParser x (hexParser x) y) mid
  return hexes
  
-- | ensures that each fresh translation is given its own unique directory.  If the requested directory already exists in the file structure, it will append the phrase "_V001" to the folder name, and recursively check to see if that folder has already been created.  It should be noted, that if the requested folder is already appended with a "_Vxxx" suffix, it will not append another, but increment the existing suffix.  In this manner.  
createAvailableDir :: FilePath -> Int -> IO String
createAvailableDir outputDir count = do
  exists <- doesDirectoryExist outputDir
  if not exists 
     then do createDirectory outputDir 
	     return outputDir
     else createAvailableDir nextDir (count + 1)
  where
    nextDir = if isVersioned outputDir
		then (take ((length outputDir) - 3) outputDir) ++ (zeroed count)
                else outputDir ++ "_V" ++ ( zeroed count )
    zeroed c 
     | c < 10 = "00" ++ (show c)
     | c < 100 = "0" ++ (show c)
     | otherwise = (show c)
    isVersioned name = and (zipWith (elem) (drop ((length name) - 5) name) (compareList))
    compareList = [ ['_'], ['V'], ['0'..'9'], ['0'..'9'], ['0'..'9']]

-- | helper function which takes a list of files created by the file generation back-end and simply writes them as files into the active directory.
writeFileList :: [File] -> IO()
writeFileList [] = do
  return () 
writeFileList ((name,contents):xs) = do
  writeFile name (swapUnderscores contents)
  writeFileList xs

swapUnderscores :: String -> String
swapUnderscores [] = []
swapUnderscores ('_':xs) = ('\x005F'):(swapUnderscores xs)
swapUnderscores (x:xs) = (x):(swapUnderscores xs)
  
-- | helper function that adds a sufficient number of "../" phrases to a change directory command to back out of a directory once entered.
dottify :: String -> String 
dottify xs = genDots $ pathDepth xs
  where 
    pathDepth ys = length (filter (=='/') ys) + 1 
    genDots n = intercalate "/" (replicate n "..")

getTopPackageName :: String -> [BSVPackage] -> PackageName
getTopPackageName nom [] = error "Error!  Module not found!"
getTopPackageName nom (x:xs) = if (tf) then (bsv_packageName x) else (getTopPackageName nom xs)
  where 
    mods = bsv_modules x 
    names = map (\ x -> mName x) mods
    tfList = map (\ x -> (x == nom)) names
    tf = foldl (\ x y -> x || y) False tfList 

copyFileName1 :: String
copyFileName1 = "ClockTick.pvs"

copyFileName2 :: String
copyFileName2 = "defined_operators.pvs"


-------------------------------------------PVS2PVS------------------------------------------------

openPVSLexer :: Mode -> FilePath -> FilePath -> IO() 
openPVSLexer (P2P) inputDir outputDir = do
  putStr "PVS2PVS mode not yet implemented!  Nor is it likely to be!"
--   typedefsFile <- readFile $ inputDir ++ "/Typedefinitions.pvs"
--   stateFile <- readFile $ inputDir ++ "/State.pvs"
--   transFile <- readFile $ inputDir ++ "/Transitions.pvs"
--   let packagePVS = parsePVSfiles (typedefsFile, stateFile, transFile) 
--   let files = showPVSPackage packagePVS Nothing
--   setCurrentDirectory outputDir
--   writeFileList files
--   setCurrentDirectory $ dottify outputDir
--   putStr "Translation was Successful!\n\n"
openPVSLexer (P2B) inputpath outputFile = do
  putStr "PVS2BSV mode not yet implemented!  Nor is it likely to be!"

------------------------------------------TSP2BSV-------------------------------------------------

openTSPLexer :: Mode -> FilePath -> FilePath -> IO ()
openTSPLexer (T2B) input outputDir = do
  tspFile <- readFile $ input
  let packageTSP = runTSPparser parseTSP tspFile
  let packageBSV = tsp2bsv packageTSP
  setCurrentDirectory outputDir
  --outputFileName <- setOutputFileName (takeWhile (/= '.') (reverse (takeWhile (/= '/') (reverse input)))) 0 
  let files = (showBSVPackage packageBSV):(if (null (bsv_macros packageBSV)) then [] else (genDefineFile ((bsv_packageName packageBSV) ++ ".defines") (bsv_macros packageBSV)) : [] )
  --putStr $ show files
  writeFileList files
  setCurrentDirectory $ dottify outputDir
  putStr "Generation was Successful!\n\n"
openTSPLexer (T2P) input outputDir = do
  tspFile <- readFile $ input
  let packageTSP = runTSPparser parseTSP tspFile
  let packageBSV = tsp2bsv packageTSP
  let uSched = [[],[(ID "set_Inputs")]]-- getUserSchedules input
  let packagePVS = bsv2pvs packageBSV (Just (getTopMod packageBSV)) uSched
  outDir <- createAvailableDir outputDir 1
  setCurrentDirectory outDir
  --outputFileName <- setOutputFileName (takeWhile (/= '.') (reverse (takeWhile (/= '/') (reverse input)))) 0 
  let files = [(showBSVPackage packageBSV)]
           ++ (if (null (bsv_macros packageBSV)) then [] else (genDefineFile ((bsv_packageName packageBSV) ++ ".defines") (bsv_macros packageBSV)):[])
           ++ (showPVSPackage packagePVS (Just packageTSP))
           ++ [("ClockTick.pvs", clockTick), ("defined_operators.pvs", definedOperators), ("Maybe.pvs", maybeFile), ("Time.pvs", timeFile), ("arith_bitwise.pvs", arith_BWFile), ("FIFO.pvs", fifoFile)]
  writeFileList files 
  setCurrentDirectory $ dottify outDir
  putStr "Generation was Successful!\n\n"
  
  
-- assumes there is only one module.  A safe assumption if package was generated by tsp2bsv.
getTopMod :: BSVPackage -> String 
getTopMod package = mName modo
  where
    mods = bsv_modules package
    modo = head mods

runMacroPreprocessing :: String -> String -> IO String 
runMacroPreprocessing file filePath = do 
  let includes = findIncludes file
  let localDefines = findLocalDefines file
  let defFileNames = map (\ z -> reverse (dropWhile (/='/') (reverse filePath)) ++ z) includes
  --if (null defFileNames) then return () else putStrLn $ "  Importing definition files : " ++ (intercalate "\n  --> " defFileNames)
  defFiles <- mapM (\x -> readFile x) defFileNames
  let macros = (++) localDefines $ concat $ map parseMacros' defFiles
  --if (filePath == "The Arena/First Colosseum/RapidIO_DTypes.bsv") then error (show defFileNames) else return ()
  let filez = (runBSVMacros (removeDefines (removeIncludes file)) macros) 
  return filez
    
getUserSchedules :: FilePath -> IO [[ID_Path]]
getUserSchedules filePath = do 
  let filePath' = (\ z -> reverse (dropWhile (/='/') (reverse filePath)) ++ z) "schedules.bapip"
  present <- doesFileExist filePath'
  if (present) 
    then do 
      putStrLn "User specified schedules detected!"
      usFile <- readFile filePath'
      let q = map (map strings2idpath) $ map (map (map scrubUnderscore)) $ map (map (splitOn ".")) $ map words $ lines usFile
      return ([] : q ) 
    else do 
      putStrLn "No user specified schedules found."
      return [] 

scrubUnderscore :: String -> String
--scrubUnderscore ('_':xs) = xs
scrubUnderscore xs = xs
      
