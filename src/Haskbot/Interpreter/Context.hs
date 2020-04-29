module Haskbot.Interpreter.Context
  ( defaultModules
  , defaultQualifiedModules
  , defaultPackages
  ) where

-- | Modules which we should load by default.
-- These are of course whitelisted.
defaultModules :: [String]
defaultModules =
  [ "Prelude"
  , "Data.Function"
  , "Control.Applicative"
  , "Control.Monad"
  , "Control.Monad.Cont"
  , "Control.Monad.Except"
  , "Control.Monad.Fix"
  , "Control.Monad.Identity"
  , "Control.Monad.RWS"
  , "Control.Monad.Reader"
  , "Control.Monad.State"
  , "Control.Monad.Writer"
  , "Data.Array"
  , "Data.Bits"
  , "Data.Bool"
  , "Data.Char"
  , "Data.Complex"
  , "Data.Dynamic"
  , "Data.Either"
  , "Data.Eq"
  , "Data.Fixed"
  , "Data.Graph"
  , "Data.Int"
  , "Data.Ix"
  , "Data.List"
  , "Data.Maybe"
  , "Data.Monoid"
  , "Data.Ord"
  , "Data.Ratio"
  , "Data.Tree"
  , "Data.Tuple"
  , "Data.Typeable"
  , "Data.Word"
  , "System.Random"
  ]

-- | Whitelist of modules which should be safe to import
-- functions from, but which we don't want to import by default.
defaultQualifiedModules :: [(String, Maybe String)]
defaultQualifiedModules =
  [ ("Data.Text", Just "Text")
  , ("Data.ByteString", Just "ByteString")
  , ("Data.ByteString.Char8", Just "Char8")
  , ("Data.ByteString.Lazy", Just "ByteStringLazy")
  , ("Data.ByteString.Lazy.Char8", Just "ByteStringLazyChar8")

  , ("Data.Text", Just "T")
  , ("Data.ByteString", Just "BS")
  , ("Data.ByteString.Char8", Just "BSC")
  , ("Data.ByteString.Lazy", Just "BSL")
  , ("Data.ByteString.Lazy.Char8", Just "BSLC")

  , ("Data.Foldable", Just "Foldable")
  , ("Data.IntMap", Just "IntMap")
  , ("Data.IntSet", Just "IntSet")
  , ("Data.Map", Just "Map")
  , ("Data.Sequence", Just "Sequence")
  , ("Data.Set", Just "Set")
  , ("Data.Traversable", Just "Traversable")
  ]

defaultPackages :: [String]
defaultPackages =
  [ "array"
  , "vector"
  , "base"
  , "bytestring"
  , "utf8-string"
  , "text"
  , "exceptions"
  , "containers"
  , "transformers"
  , "mtl"
  ]
