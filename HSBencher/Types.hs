{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns  #-}

module HSBencher.Types
       where

import Data.Maybe (catMaybes)
import Control.Monad (filterM)
import System.FilePath
import System.Directory
import HSBencher.MeasureProcess -- (CommandDescr(..))


type RunFlags     = [String]
type CompileFlags = [String]

#if 1
-- | A description of a set of files.  The description may take one of multiple
-- forms.
data FilePredicate = 
    WithExtension String -- ^ E.g. ".hs", WITH the dot.
  | IsExactly     String -- ^ E.g. "Makefile"
--   | SatisfiesPredicate (String -> Bool)

  | InDirectoryWithExactlyOne FilePredicate
    -- ^ A common pattern.  For example, we can build a file foo.c, if it lives in a
    -- directory with exactly one "Makefile".

  | PredOr FilePredicate FilePredicate -- ^ Logical or.
 deriving Show    
-- instance Show FilePredicate where
--   show (WithExtension s) = "<FilePredicate: *."++s++">"    

-- | This function gives meaning to the `FilePred` type.
--   It returns a filepath to signal "True" and Nothing otherwise.
filePredCheck :: FilePredicate -> FilePath -> IO (Maybe FilePath)
filePredCheck pred path =
  let filename = takeFileName path in 
  case pred of
    IsExactly str     -> return$ if str == filename
                                 then Just path else Nothing
    WithExtension ext -> return$ if takeExtension filename == ext
                                 then Just path else Nothing
    PredOr p1 p2 -> do
      x <- filePredCheck p1 path
      case x of
        Just _  -> return x
        Nothing -> filePredCheck p2 path
    InDirectoryWithExactlyOne p2 -> do
      ls  <- getDirectoryContents =<< getCurrentDirectory
      ls' <- fmap catMaybes $
             mapM (filePredCheck p2) ls
      case ls' of
        [x] -> return (Just$ takeDirectory path </> x)
        _   -> return Nothing

#else
-- Option two, opaque predicates:

type FilePredicate = FilePath -> IO Bool
instance Show FilePredicate where
  show _ = "<FilePredicate>"
#endif

data BuildResult =
    StandAloneBinary FilePath -- ^ This binary can be copied and executed whenever.
  | RunInPlace (RunFlags -> CommandDescr)
    -- ^ In this case the build return what you need to do the benchmark run, but the
    -- directory contents cannot be touched until after than run is finished.

#if 1
-- | A completely encapsulated method of building benchmarks.  Cabal and Makefiles
-- are two examples of this.  The user may extend it with their own methods.
data BuildMethod =
  BuildMethod
  { methodName :: String          -- ^ Identifies this build method for humans.
--  , buildsFiles :: FilePredicate
--  , canBuild    :: FilePath -> IO Bool
  , canBuild    :: FilePredicate  -- ^ Can this method build a given file/directory?
  , concurrentBuild :: Bool -- ^ More than one build can happen at once.  This
                            -- implies that compile always returns StandAloneBinary.
  , compile :: CompileFlags -> FilePath -> IO BuildResult
  }

instance Show BuildMethod where
  show BuildMethod{methodName, canBuild} = "<buildMethod "++methodName++" "++show canBuild ++">"

-- instance Show FilePredicate where
--   show (WithExtension s) = "<FilePredicate: *."++s++">"  
           
#else

class BuildMethodC b where
  buildsFiles :: b -> FilePredicate
  
#endif  

