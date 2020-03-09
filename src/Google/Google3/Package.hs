-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- For the Prelude import on ghc<8.4.1:
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Structure and syntax of Google3 BUILD files.
--
--   This isn't yet very complete; it has just enough functionality to be able
--   to generate BUILD files satisfying third_party requirements.
module Google.Google3.Package where

import Data.List (intersperse)
import Data.List.Extra (groupSort)
import Data.Text (Text)
import qualified Data.Text as Text
import Google.Google3.Name
import Text.PrettyPrint.HughesPJ
import Prelude hiding ((<>))

-- Enough abstract structure of packages to get by.

-- | Examples of RuleClass are "haskell_binary", "genrule"
type RuleClass = Text

type License = Text

data Glob = Glob {
  globIncludes :: [Text],
  globExcludes :: [Text]
}

-- | A thing that can be mentioned in a rule's srcs attribute. This
--   doesn't quite correspond to any real concept at this level of
--   abstraction, but is useful for constructing concrete expressions
--   that include globs.
data Source
  = FileSource Text      -- ^ A single file
  | LabelSource Label    -- ^ Some other label
  | GlobSource Glob      -- ^ A set of files

newtype Dependency
  = Labelled Label -- ^ Target name fully qualified by Google3 package.

-- | A single rule declaration in a BUILD file.
data Rule = Rule {
  ruleClass :: RuleClass,
  ruleName :: TargetName,
  ruleSrcs :: [Source],
  ruleDeps :: [Dependency],
  ruleOtherAttrs :: [(Symbol, Expr)]
}

data PackageAttributes = PackageAttributes {
  packageAttributesDefaultVisibility :: [Label]
}

data Package = Package {
  packageName :: PackageName,
  packageComment :: Text,
  packageAttributes :: PackageAttributes,
  packageLicenses :: [License],
  packageLicenseFile :: Text,
  packageRules :: [Rule],
  -- | Additional statements to add.
  -- TODO(judahjacobson): This is a little hacky due to cabal2buildv2
  -- which needs a non-standard "load".  We can try to clean this up
  -- after we remove the old way.
  packageStmts :: [Stmt]
}


-- Enough concrete syntax of build files to get by. In due course, this could
-- maybe be provided by the language-python Cabal package.

type Symbol = Text

data Expr
  = LitString Text
  | Var Text
  | LitList [Expr]
  | LitTuple [Expr]
  | Call Symbol [Expr] [(Symbol, Expr)]
  | BinOp Symbol Expr Expr

data Stmt
  = SAssign Symbol Expr
  | SExpr Expr
  | SWithComment Stmt Text  -- Comment attached to a statement
  | SComment Text           -- Free-standing comment

newtype BuildFile = BuildFile [Stmt]


-- Lowering from abstract syntax to concrete syntax

stringListSyntax = LitList . map LitString

globSyntax g = Call "glob"
  [stringListSyntax $ globIncludes g]
  [("exclude", stringListSyntax ex) | not $ null ex]
 where
  ex = globExcludes g

sourceSyntax :: [Source] -> Expr
sourceSyntax [] = LitList []
sourceSyntax srcs = foldr1 (BinOp "+") $
                    foldr (collectFiles . toExpr) [] srcs
 where
  toExpr (FileSource s) = LitString s
  toExpr (LabelSource s) = LitString $ showText s
  toExpr (GlobSource s) = globSyntax s
  -- Collect consecutive LitString entries into LitLists
  collectFiles :: Expr -> [Expr] -> [Expr]
  collectFiles e@(LitString _) (LitList ss : es) = LitList (e:ss) : es
  collectFiles e@(LitString _) es = LitList [e] : es
  collectFiles e es = e:es

dependencyExpr :: Dependency -> Expr
dependencyExpr (Labelled l) = LitString (Text.pack $ show l)

ruleSyntax r = SExpr $ Call (ruleClass r) []
    $ [("name", LitString . unTargetName . ruleName $ r)]
    ++ [("srcs", sourceSyntax ss) | let ss = ruleSrcs r, not $ null ss]
    ++ [("deps", LitList $ map dependencyExpr ds)
          | let ds = ruleDeps r, not $ null ds]
    ++ ruleOtherAttrs r

packageAttributesSyntax a = SExpr $ Call "package" [] [
    ("default_visibility",
     stringListSyntax . map showText .
     packageAttributesDefaultVisibility $ a)
  ]

commentSyntax s | Text.null s = []
                | otherwise   = [SComment s]

packageSyntax :: Package -> BuildFile
packageSyntax = BuildFile . stmts where
  -- Make sure to put load() statements at the top.
  -- Ideally we'd rely on buildifier to do that automatically, but it doesn't
  -- yet: https://github.com/bazelbuild/buildtools/issues/651
  stmts p = commentSyntax (packageComment p)
            ++ loadStmtsForRules (packageRules p)
            ++ [packageAttributesSyntax (packageAttributes p)]
            ++ licenseSyntax (packageLicenses p)
            ++ licenseFileSyntax (packageLicenseFile p)
            ++ map ruleSyntax (packageRules p)
            ++ packageStmts p
  licenseSyntax ls = [licenseAssignment ls]
  licenseAssignment ls = SExpr $ Call "licenses" [stringListSyntax ls] []
  licenseFileSyntax "" = []
  licenseFileSyntax lf = [SExpr $ Call "exports_files" [stringListSyntax [lf]] []]

-- Automatically adds load directives for Haskell rules.
loadStmtsForRules :: [Rule] -> [Stmt]
loadStmtsForRules rules = loadStmts
  where
    ruleTable =
        [ (hasClass "haskell_binary", "haskell_binary", mainDefFile)
        , (hasClass "haskell_library", "haskell_library", mainDefFile)
        , (hasClass "haskell_test", "haskell_test", mainDefFile)
        , (hasClass "cabal_haskell_package", "cabal_haskell_package",
               cabalDefFile)
        ]
    mainDefFile = "//tools/build_defs/haskell:def.bzl"
    cabalDefFile = "//tools/build_defs/haskell:cabal_package.bzl"
    requiredDefs = [ (defFile, def)
                   | (p, def, defFile) <- ruleTable, any p rules ]
    groupedByFile = groupSort requiredDefs
    loadStmts = map (uncurry addLoad) groupedByFile
    hasClass cls = (== cls) . ruleClass

addLoad :: Text -> [Text] -> Stmt
addLoad defFile defs = SExpr $ Call "load" (map LitString (defFile : defs)) []

-- | Minimal duplication of the prettyclass Cabal package, so as not to have to
--   depend on it. If pretty-printing becomes sufficiently involved, it might
--   in the future be worth adding dependency on that package.
class Pretty a where
  pPrint :: a -> Doc

-- How to pretty-print build files. As with the concrete syntax, this could
-- perhaps eventually be replaced by language-python.
instance Pretty Expr where
  pPrint = pPrintExpr empty

-- | Pretty-print an Expr, along with any previous contents of the same line.
--
-- Our goal is to format like:
--     foo = [
--         x,
--         y,
--         z,
--     ]
-- rather than
--     foo = [x,
--            y,
--            z,
--           ]
-- Otherwise, nested constructs become too unweildy (in particular, the
-- auto-generated package-description.bzl).
--
-- That format is surprisingly tricky to accomplish with Text.PrettyPrint.  To
-- help, the first argument of pPrintExpr holds anything that should be
-- prepended to the first line (e.g. "foo = " in the above example.
pPrintExpr :: Doc -> Expr -> Doc
pPrintExpr pre (LitString s) = pre <> doubleQuotes (text' $ Text.concatMap fixChar s)
  where
    fixChar '\n' = "\\n"
    fixChar '\\' = "\\\\"
    fixChar '"' = "\\\""
    fixChar c = Text.singleton c
pPrintExpr pre (Var s) = pre <> text' s
pPrintExpr pre (LitList es) = commaSep (pre <> "[") "]" $ map pPrint es
pPrintExpr pre (LitTuple es) = commaSep (pre <> "(") ")" $ map pPrint es
pPrintExpr pre (Call f args kwargs)
    = commaSep (pre <> text' f <> "(") ")" argList
  where
    argList = map pPrint args ++ map ppKwarg kwargs
    ppKwarg (k, v) = pPrintExpr (text' k <+> "=" <> space) v
-- TODO(judahjacobson): Figure out better formatting for BinOp.  (I'm not sure
-- if it's used anymore.)
pPrintExpr pre (BinOp o l r) = pre <> sep [pPrint l, text' o, pPrint r]

-- | Generate an expression separated by commas.
-- E.g., @commaSep "foobar(" ")" ["x", "y", "z"]@ renders to either
--     foobar(x, y, z)
-- or
--     foobar(
--         x,
--         y,
--         z
--      )
commaSep :: Doc -> Doc -> [Doc] -> Doc
commaSep start end xs = cat [start, nest 4 (sep $ punctuate comma xs), end]

commentLine l = char '#' <+> (if Text.null l then empty else text' l)

instance Pretty Stmt where
  pPrint (SAssign v e) = pPrintExpr (text' v <+> char '=' <> space) e
  pPrint (SExpr e) = pPrint e
  pPrint (SWithComment s c) = pPrint s <> text' "  " <> commentLine c
  pPrint (SComment c) = vcat [commentLine l | l <- Text.lines c]

instance Pretty BuildFile where
  pPrint (BuildFile rs) = vcat $ intersperse (text' "") $ map pPrint rs

showText :: (Show a) => a -> Text
showText = Text.pack . show

text' :: Text -> Doc
text' = text . Text.unpack
