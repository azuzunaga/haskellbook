module MadLibs where

import           Data.Monoid

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbing' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbing' e adv noun adj =
  e
    <> "! he said "
    <> adv
    <> " as he jumped into his car "
    <> noun
    <> " and drove off with his "
    <> adj
    <> " wife."

madlibbingBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbingBetter' e adv noun adj = mconcat
  [ e
  , "! he said "
  , adv
  , " as he jumped into his car "
  , noun
  , " and drove off with his "
  , adj
  , " wife."
  ]
