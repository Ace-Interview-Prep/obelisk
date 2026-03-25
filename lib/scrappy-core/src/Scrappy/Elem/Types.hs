{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : Scrappy.Elem.Types
-- Description : Core types for HTML element representation, tree structures, and pattern matching
-- Copyright   : (c) Galen Sprout, 2024
-- License     : BSD-3-Clause
-- Maintainer  : galen.sprout@gmail.com

module Scrappy.Elem.Types
  ( -- * Type classes
    -- | @since 0.1.0.0
    ShowHTML(..)
    -- | @since 0.1.0.0
  , ElementRep(..)
    -- | @since 0.1.0.0
  , InnerHTMLRep(..)

    -- * Core element types
    -- | @since 0.1.0.0
  , TreeHTML(..)
    -- | @since 0.1.0.0
  , Elem'(..)
    -- | @since 0.1.0.0
  , InnerTextHTMLTree(..)
    -- | @since 0.1.0.0
  , InnerTextResult(..)

    -- * Matcher types
    -- | @since 0.1.0.0
  , HTMLMatcher(..)
    -- | @since 0.1.0.0
  , HTMLMatcherM
    -- | @since 0.1.0.0
  , Inner
    -- | @since 0.1.0.0
  , HTMLMatcherList
    -- | @since 0.1.0.0
  , HTMLBare(..)
    -- | @since 0.1.0.0
  , HMatcher'

    -- * Type aliases
    -- | @since 0.1.0.0
  , ElemHead
    -- | @since 0.1.0.0
  , Attrs
    -- | @since 0.1.0.0
  , Elem
    -- | @since 0.1.0.0
  , Tag
    -- | @since 0.1.0.0
  , Glength
    -- | @since 0.1.0.0
  , MaxLength
    -- | @since 0.1.0.0
  , TreeIndex

    -- * Error types
    -- | @since 0.1.0.0
  , AttrsError(..)

    -- * Grouping types
    -- | @since 0.1.0.0
  , GroupHtml(..)

    -- * Clickable and URL types
    -- | @since 0.1.0.0
  , Clickable(..)
    -- | @since 0.1.0.0
  , UrlPagination(..)

    -- * Experimental tree types
    -- | @since 0.1.0.0
  , MessyTree(..)
    -- | @since 0.1.0.0
  , MessyTreeMatch(..)

    -- * Functions
    -- | @since 0.1.0.0
  , noPat
    -- | @since 0.1.0.0
  , coerceAttrs
    -- | @since 0.1.0.0
  , f
    -- | @since 0.1.0.0
  , ungroup
    -- | @since 0.1.0.0
  , mkGH
    -- | @since 0.1.0.0
  , longestElem
    -- | @since 0.1.0.0
  , maxLength
    -- | @since 0.1.0.0
  , biggestHtmlGroup
    -- | @since 0.1.0.0
  , biggestGroup
    -- | @since 0.1.0.0
  , getHrefEl
    -- | @since 0.1.0.0
  , getHrefAttrs
    -- | @since 0.1.0.0
  , elemToStr
    -- | @since 0.1.0.0
  , treeElemToStr
    -- | @since 0.1.0.0
  , foldFuncMatchlist
    -- | @since 0.1.0.0
  , foldFuncTup
    -- | @since 0.1.0.0
  , foldFuncTrup
    -- | @since 0.1.0.0
  , foldFuncITR
    -- | @since 0.1.0.0
  , fHM_c
    -- | @since 0.1.0.0
  , makeBranch
    -- | @since 0.1.0.0
  , endTag
    -- | @since 0.1.0.0
  , enoughMatches
    -- | @since 0.1.0.0
  , enoughMatchesTree
    -- | @since 0.1.0.0
  , selfClosingTextful
    -- | @since 0.1.0.0
  , mkClickableEH
    -- | @since 0.1.0.0
  , mkClickable
    -- | @since 0.1.0.0
  , getLink
    -- | @since 0.1.0.0
  , getSrc

    -- * Tuple helpers
    -- | @since 0.1.0.0
  , fst'
    -- | @since 0.1.0.0
  , snd'
    -- | @since 0.1.0.0
  , thd'
  ) where

import Prelude
  ( Bool(..), Char, Eq, Int, Maybe(..), Ord, Show(..), String
  , Semigroup(..), Monoid(..)
  , ($), (.), (<$>), (==), (<=), (*), (>), (>>)
  , return, (=<<)
  , error, fmap, foldr, fst, length, pure, reverse, snd, undefined
  )
import Data.Kind (Type)

import Scrappy.Links (LastUrl, Link, parseLink)

import qualified Data.Text as T
import qualified Data.Map as Map

import Data.Graph (Tree (Node), Forest)
import Control.Applicative (some)
import Text.Parsec
  ( ParsecT, Stream, parserZero, string, (<|>), anyChar, char
  , optional, try, manyTill, alphaNum, parserFail
  )
import Data.Maybe (fromMaybe)

-- * Type classes

-- | Type class for values that can be shown as HTML strings.
--
-- Laws: for any parser @p@ and input @input@:
--
-- > x <- parse p input
-- > showH x == input
--
-- This is useful for extracting values while retaining structure for successive parsing.
--
-- @since 0.1.0.0
class ShowHTML a where
  showH :: a -> String

-- | @since 0.1.0.0
instance ShowHTML Char where
  showH = show

-- | @since 0.1.0.0
instance Show a => ShowHTML [a] where
  showH x = show x

-- | @since 0.1.0.0
instance ShowHTML T.Text where
  showH = T.unpack

-- | Shared accessors for HTML element datatypes.
--
-- @since 0.1.0.0
class ElementRep (a :: Type -> Type) where
  elTag :: a b -> Elem
  attrs :: a b -> Attrs
  innerText' :: a b -> String
  matches' :: a b -> [b]

-- | Summarises finds as a result of parsing then folding.
-- Recursive fold of multi-constructor structure.
--
-- @since 0.1.0.0
class (ShowHTML c, ElementRep a) => InnerHTMLRep (a :: Type -> Type)  (b :: Type -> Type) c | a c -> b c where
  foldHtmlMatcher :: [HTMLMatcher a c] -> b c
  innerText :: b c -> String
  matches :: b c -> [c]

-- * Core element types

-- | Tree-based HTML element representation.
--
-- @since 0.1.0.0
data TreeHTML a = TreeHTML
  { _topEl :: Elem
  , _topAttrs :: Map.Map String String
  , _matches' :: [a]
  , _innerText' :: String
  , _innerTree' :: Forest ElemHead
  } deriving Show

-- | Intermediate tree structure used when folding HTML matchers.
--
-- @since 0.1.0.0
data InnerTextHTMLTree a = InnerTextHTMLTree
  { _matches :: [a]
  , _innerText :: String
  , innerTree :: Forest ElemHead
  }

-- | Node-like HTML element representation.
--
-- @since 0.1.0.0
data Elem' a = Elem' { _el :: Elem
                      , _attrs :: Map.Map String String
                      , innerMatches :: [a]
                      , innerHtmlFull :: String
                      } deriving Show

-- | Result of folding inner HTML to a flat text plus matches.
--
-- @since 0.1.0.0
data InnerTextResult a = InnerTextResult
  { _matchesITR :: [a]
  , _fullInner :: String
  } deriving Show

-- * Matcher types

-- | The core HTML matching datatype: either inner text, a matched element, or a sub-element.
--
-- @since 0.1.0.0
data HTMLMatcher (a :: Type -> Type) b = IText String | Element (a b) | Match b deriving Show

-- | 'HTMLMatcher' specialised to 'TreeHTML'.
--
-- @since 0.1.0.0
type HTMLMatcherM a = HTMLMatcher TreeHTML a

-- | 'HTMLMatcher' specialised to 'Elem\''.
--
-- @since 0.1.0.0
type Inner a = HTMLMatcher Elem' a

-- | 'HTMLMatcher' specialised to lists.
--
-- @since 0.1.0.0
type HTMLMatcherList a = HTMLMatcher [] a

-- | Minimal testing structure for low-level parsing verification.
--
-- @since 0.1.0.0
data HTMLBare e a = HTMLBare
  { tag :: Elem
  , attrsss :: Attrs
  , htmlM :: [HTMLMatcher e a]
  }

-- | Type alias for a function that folds a list of 'HTMLMatcher's into a result.
--
-- @since 0.1.0.0
type HMatcher' a b c = [HTMLMatcher b c] -> a c

-- * Type aliases

-- | An element head is a pair of tag name and attributes.
--
-- @since 0.1.0.0
type ElemHead = (Elem, Attrs)

-- | Attributes are a map from attribute name to attribute value.
--
-- @since 0.1.0.0
type Attrs = Map.Map String String

-- | An element tag name.
--
-- @since 0.1.0.0
type Elem = String

-- | Alias for 'String' representing a tag name.
--
-- @since 0.1.0.0
type Tag = String

-- | Number of items in a group.
--
-- @since 0.1.0.0
type Glength = Int

-- | Length of the longest element in a group.
--
-- @since 0.1.0.0
type MaxLength = Int

-- | Index path into a tree structure.
--
-- @since 0.1.0.0
type TreeIndex = [Int]

-- * Error types

-- | Attribute parsing error.
--
-- @since 0.1.0.0
data AttrsError = IncorrectAttrs deriving Show

-- * Grouping types

-- | A group of HTML elements with metadata about group size and longest element.
--
-- @since 0.1.0.0
data GroupHtml element a = GroupHtml [element a] Glength MaxLength

-- * Clickable and URL types

-- | A clickable element: an element head paired with a link.
--
-- @since 0.1.0.0
data Clickable = Clickable ElemHead Link deriving (Eq, Show)

-- | Deprecated: use CurrentQuery instead.
--
-- @since 0.1.0.0
data UrlPagination = UrlPagination String String deriving (Eq, Show)

-- * Experimental tree types

-- | A tree that can contain noise (non-matching content) at the leaves.
--
-- @since 0.1.0.0
data MessyTree a b = Noise b | Nodee a [MessyTree a b]

-- | A tree that can contain noise, matches, or structured nodes.
--
-- @since 0.1.0.0
data MessyTreeMatch a b c = Noise' a | Match' b | Node' c [MessyTreeMatch a b c]

-- * Instances

-- | @since 0.1.0.0
instance Semigroup (InnerTextHTMLTree a) where
  InnerTextHTMLTree a b c <> InnerTextHTMLTree d e f' = InnerTextHTMLTree (a <> d) (b <> e) (c <> f')

-- | @since 0.1.0.0
instance Monoid (InnerTextHTMLTree a) where
  mempty = InnerTextHTMLTree { _matches = [], _innerText = "", innerTree = [] }

-- | @since 0.1.0.0
instance Semigroup (InnerTextResult a) where
  InnerTextResult a b <> InnerTextResult a' b' = InnerTextResult (a <> a') (b <> b')

-- | @since 0.1.0.0
instance Monoid (InnerTextResult a) where
  mempty = InnerTextResult { _matchesITR = [], _fullInner = "" }

-- | @since 0.1.0.0
instance ShowHTML c => InnerHTMLRep TreeHTML InnerTextHTMLTree c where
  foldHtmlMatcher = foldr fHM_c mempty
  matches = _matches
  innerText = _innerText

-- | @since 0.1.0.0
instance ShowHTML c => InnerHTMLRep Elem' InnerTextResult c where
  foldHtmlMatcher = foldr foldFuncITR mempty
  matches = _matchesITR
  innerText = _fullInner

-- | @since 0.1.0.0
instance ElementRep Elem' where
  elTag = _el
  attrs = _attrs
  innerText' = innerHtmlFull
  matches' = innerMatches

-- | @since 0.1.0.0
instance ElementRep TreeHTML where
  elTag = _topEl
  attrs = _topAttrs
  innerText' = _innerText'
  matches' = _matches'

-- | @since 0.1.0.0
instance ShowHTML a => ShowHTML (Elem' a) where
  showH = elemToStr

-- | @since 0.1.0.0
instance ShowHTML a => ShowHTML (TreeHTML a) where
  showH = treeElemToStr

-- | @since 0.1.0.0
instance Eq (GroupHtml e a) where
  GroupHtml _ gl1 ml1 == GroupHtml _ gl2 ml2 = (gl1 * ml1) == (gl2 * ml2)

-- | @since 0.1.0.0
instance Ord (GroupHtml e a) where
  (GroupHtml _ gl1 ml1) <= (GroupHtml _ gl2 ml2) = (gl1 * ml1) <= (gl2 * ml2)

-- | @since 0.1.0.0
instance (ElementRep e, Show (e a), Show a, ShowHTML a) => Show (GroupHtml (e :: Type -> Type) a) where
  show (GroupHtml [] count maxELen) =
    "GroupHtml { count =" <> (show count) <> ", longestElem= " <> show maxELen <> ", elemStructure=[]"
  show (GroupHtml (e:_) count maxELen) =
    "GroupHtml { count =" <> (show count) <> ", longestElem= " <> show maxELen <> ", elemStructure=" <> (show e)

-- * Functions

-- | Returns 'Nothing', used as a default for no pattern match.
--
-- @since 0.1.0.0
noPat :: Maybe (ParsecT s u m String)
noPat = Nothing

-- | Coerce attributes from a 'Map' to an association list, converting empty strings to 'Nothing'.
--
-- @since 0.1.0.0
coerceAttrs :: Attrs -> [(String, Maybe String)]
coerceAttrs as = (fmap.fmap) f $ Map.toList as

-- | Convert an empty string to 'Nothing', otherwise 'Just'.
--
-- @since 0.1.0.0
f :: String -> Maybe String
f "" = Nothing
f s = Just s

-- | Extract the elements from a 'GroupHtml'.
--
-- @since 0.1.0.0
ungroup :: ElementRep e => GroupHtml e a -> [e a]
ungroup (GroupHtml xs _ _) = xs

-- | Construct a 'GroupHtml' from a list of elements.
--
-- @since 0.1.0.0
mkGH :: ElementRep e => [e a] -> GroupHtml e a
mkGH [] = GroupHtml [] 0 0
mkGH result@(r:_) = GroupHtml result (length result) (length (innerText' r))

-- | Find the element with the longest inner text.
--
-- @since 0.1.0.0
longestElem :: [Elem' a] -> Maybe (Elem' a)
longestElem [] = Nothing
longestElem (a:[]) = Just a
longestElem (x:y:xs) = if length (innerText' x) > length (innerText' y)
                       then longestElem (x:xs)
                       else longestElem (y:xs)

-- | Return the longest list from a list of lists.
--
-- @since 0.1.0.0
maxLength :: [[a]] -> [a]
maxLength [] = []
maxLength (a:[]) = a
maxLength (x:y:xs) = if length x > length y then maxLength (x:xs) else maxLength (y:xs)

-- | Find the biggest group by the product of count and max element length.
--
-- @since 0.1.0.0
biggestHtmlGroup :: [GroupHtml e a] -> GroupHtml e a
biggestHtmlGroup ghs = foldr maxE (GroupHtml [] 0 0) ghs
  where
    maxE :: GroupHtml e a -> GroupHtml e a -> GroupHtml e a
    maxE (GroupHtml xs cnt lng) (GroupHtml ys cnt' lng') =
      if (cnt * lng) > (cnt' * lng')
      then (GroupHtml xs cnt lng)
      else (GroupHtml ys cnt' lng')

-- | Find the biggest group from a list, using pairwise comparison.
--
-- @since 0.1.0.0
biggestGroup :: ElementRep e => [GroupHtml e a] -> GroupHtml e a
biggestGroup [] = error "biggestGroup: empty list"
biggestGroup (gh:[]) = gh
biggestGroup (n0@(GroupHtml _a x1 y1) :n1@(GroupHtml _b x2 y2):ghs) = case (x1 * y1) > (x2 * y2) of
  True -> biggestGroup (n0:ghs)
  False -> biggestGroup (n1:ghs)

-- | Extract an href link from an element.
--
-- @since 0.1.0.0
getHrefEl :: ElementRep e => Bool -> LastUrl -> e a -> Maybe Link
getHrefEl b cUrl e = getHrefAttrs b cUrl $ attrs e

-- | Extract an href link from an attributes map.
--
-- @since 0.1.0.0
getHrefAttrs :: Bool -> LastUrl -> Map.Map String String -> Maybe Link
getHrefAttrs b cUrl atribs = parseLink b cUrl =<< Map.lookup "href" atribs

-- | Render an 'Elem\'' as an HTML string.
--
-- @since 0.1.0.0
elemToStr :: Elem' a -> String
elemToStr el = "<" <> elTag el <> buildAttrs (Map.toList (attrs el)) <> ">" <> innerHtmlFull el <> "</" <> elTag el <> ">"
  where
    buildAttrs [] = ""
    buildAttrs (attr:attrss) = " " <> fst attr <> "=" <> "\"" <> snd attr <> "\"" <> buildAttrs attrss

-- | Render a 'TreeHTML' as an HTML string.
--
-- @since 0.1.0.0
treeElemToStr :: (ShowHTML a) => TreeHTML a -> String
treeElemToStr (TreeHTML{..}) =
  "<" <> _topEl <> mdToStringPairs _topAttrs <> ">" <> _innerText'  <> "</" <>  _topEl <> ">"
  where mdToStringPairs attrsSet = go (Map.toList attrsSet)
        go [] = ""
        go (atr:[])        = (fst atr) <> "=" <> ('"' : snd atr) <> ('"' : [])
        go (atr: attrsSet) = (fst atr) <> "=" <> ('"' : snd atr) <> ('"' : []) <> go attrsSet

-- | Fold function for 'HTMLMatcherList' into 'InnerTextResult'. Currently undefined.
--
-- @since 0.1.0.0
foldFuncMatchlist :: (ShowHTML a, ElementRep e) => HTMLMatcher e a -> InnerTextResult a -> InnerTextResult a
foldFuncMatchlist _hMatcher _itr = undefined

-- | Fold function accumulating into a tuple of (inner text, matches).
--
-- @since 0.1.0.0
foldFuncTup :: (ShowHTML (e a), ShowHTML a, ElementRep e) => HTMLMatcher e a -> (String, [a]) -> (String, [a])
foldFuncTup hMatcher itr = case hMatcher of
   IText str ->
    (fst itr <> str, snd itr)
   Match mat ->
    (fst itr <> (reverse $ showH mat), snd itr <> [mat])
   Element el ->
    (fst itr <> (reverse $ showH el), snd itr <> matches' el)

-- | Fold function accumulating into a triple of (inner text, matches, forest).
--
-- @since 0.1.0.0
foldFuncTrup :: (ShowHTML a) => HTMLMatcher TreeHTML a -> (String, [a], Forest ElemHead) -> (String, [a], Forest ElemHead)
foldFuncTrup hMatcher itr = case hMatcher of
  IText str ->
    (fst' itr <> str, snd' itr, thd' itr)
  Match mat ->
    (fst' itr <> (showH mat), snd' itr <> [mat], thd' itr)
  Element el ->
    (fst' itr <> (reverse $ showH el), snd' itr <> matches' el, thd' itr <> ((makeBranch el):[]))

-- | Fold function for 'HTMLMatcher' into 'InnerTextResult'.
--
-- @since 0.1.0.0
foldFuncITR :: (ShowHTML a, ElementRep e) => HTMLMatcher e a -> InnerTextResult a -> InnerTextResult a
foldFuncITR hMatcher itr = case hMatcher of
  IText str ->
    InnerTextResult (_matchesITR itr) (_fullInner itr <> str)
  Match mat ->
    InnerTextResult (mat : _matchesITR itr) (_fullInner itr <> (showH mat))
  Element el ->
    InnerTextResult (matches' el <> _matchesITR itr) (_fullInner itr <> (innerText' el))

-- | Fold function for 'HTMLMatcher' over 'TreeHTML' into 'InnerTextHTMLTree'.
--
-- @since 0.1.0.0
fHM_c :: (InnerHTMLRep TreeHTML InnerTextHTMLTree a, ShowHTML a) =>
         HTMLMatcher TreeHTML a
      -> InnerTextHTMLTree a
      -> InnerTextHTMLTree a
fHM_c hMatcher ithT = case hMatcher of
  IText str ->
    InnerTextHTMLTree (_matches ithT) (_innerText ithT <> str) (innerTree ithT)
  Match mat ->
    InnerTextHTMLTree (mat : (_matches ithT)) (_innerText ithT <> (showH mat)) (innerTree ithT)
  Element htmlTree ->
    InnerTextHTMLTree (_matches ithT <> matches' htmlTree) ((_innerText ithT) <> showH htmlTree) ((makeBranch htmlTree) : innerTree ithT)

-- | Create a tree branch from a 'TreeHTML' node.
--
-- @since 0.1.0.0
makeBranch :: TreeHTML a -> Tree ElemHead
makeBranch treeH = Node (elTag treeH, attrs treeH) (_innerTree' treeH)

-- | Parse an end tag for the given element name.
--
-- @since 0.1.0.0
endTag :: Stream s m Char => String -> ParsecT s u m String
endTag el = try (string ("</" <> el <> ">"))

-- | Verify that the number of matches meets a required minimum for 'Elem\''.
--
-- @since 0.1.0.0
enoughMatches :: Int -> String -> Map.Map String String -> (String, [a]) -> ParsecT s u m (Elem' a)
enoughMatches required e a (asString, matchList) =
  if required <= (length matchList)
  then return $ Elem' e a matchList (reverse asString)
  else parserFail "not enough matches"

-- | Verify that the number of matches meets a required minimum for 'TreeHTML'.
--
-- @since 0.1.0.0
enoughMatchesTree :: Int -> String -> Map.Map String String -> (String, [a], Forest ElemHead) -> ParsecT s u m (TreeHTML a)
enoughMatchesTree required e a (asString, matchList, forest) =
  if required <= (length matchList)
  then return $ TreeHTML e a matchList (reverse asString) forest
  else parserFail "not enough matches"

-- | Parse inner content for self-closing tags that may contain text (e.g. @\<p\>@).
--
-- Self-closing tags do not allow embedded elements, only plaintext.
-- The text belonging to the tag is everything up until the next HTML control section.
--
-- @since 0.1.0.0
selfClosingTextful :: (ShowHTML a, Stream s m Char) =>
                      Maybe (ParsecT s u m a)
                   -> ParsecT s u m [HTMLMatcher e a]
selfClosingTextful innerP = do
  _ <- char '>'
  manyTill
    (
      (try (Match <$> innerP'))
      <|> (try ((IText . (:[])) <$> anyChar))
    )
    (
      (try anyEndTag) <|> (char '<' >> some alphaNum)
    )
  where anyEndTag = (try (char '<'
                       >> (optional (char '/'))
                       >> some anyChar
                       >> (string " " <|> string ">")))
        innerP' = fromMaybe parserZero innerP

-- | Construct a 'Clickable' from an 'ElemHead' by extracting the href attribute.
--
-- @since 0.1.0.0
mkClickableEH :: Bool -> LastUrl -> ElemHead -> Maybe Clickable
mkClickableEH booly cUrl (e, ats) = do
  h <- getHrefAttrs booly cUrl ats
  pure $ Clickable (e, ats) h

-- | Construct a 'Clickable' from any 'ElementRep' by extracting the href attribute.
--
-- @since 0.1.0.0
mkClickable :: ElementRep e => Bool -> LastUrl -> e a -> Maybe Clickable
mkClickable booly cUrl e = do
  let ats = attrs e
  h <- getHrefAttrs booly cUrl ats
  pure $ Clickable (elTag e,ats) h

-- | Extract the link from a 'Clickable'.
--
-- @since 0.1.0.0
getLink :: Clickable -> Link
getLink (Clickable _ link) = link

-- | Extract the src attribute. Currently undefined.
--
-- @since 0.1.0.0
getSrc :: a
getSrc = undefined

-- * Tuple helpers

-- | First element of a triple.
--
-- @since 0.1.0.0
fst' :: (a, b, c) -> a
fst' (a,_,_) = a

-- | Second element of a triple.
--
-- @since 0.1.0.0
snd' :: (a,b,c) -> b
snd' (_,b,_) = b

-- | Third element of a triple.
--
-- @since 0.1.0.0
thd' :: (a,b,c) -> c
thd' (_,_,c) = c
