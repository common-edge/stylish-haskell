--------------------------------------------------------------------------------
-- | This module provides helpers to enable ignoring blocks.
module Language.Haskell.Stylish.Ignore
    ( isIgnoreComment
    ) where

import           GHC.Parser.Annotation (EpaCommentTok (EpaBlockComment, EpaLineComment),
                                        LEpaComment, ac_tok)
import           GHC.Types.SrcLoc      (unLoc)

isIgnoreComment :: LEpaComment -> Bool
isIgnoreComment = isIgnoreTok . ac_tok . unLoc

isIgnoreTok :: EpaCommentTok -> Bool
isIgnoreTok (EpaBlockComment s) = isCommentString s
isIgnoreTok (EpaLineComment s)  = isCommentString s
isIgnoreTok _                   = False

isCommentString :: String -> Bool
isCommentString s = any ($ s)
  [ (==) "STYLISH ignore" . trimComment
  ]

trimComment :: String -> String
trimComment = reverse . trimCommLeft . reverse . trimCommLeft where
  trimCommLeft = dropWhile (`elem` "{-#} \n")
