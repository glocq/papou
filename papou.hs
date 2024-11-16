#!/usr/bin/env runhaskell
{-|
Description : Procedural generation for the French verbal game "papou"
Copyright   : (c) Grégoire Locqueville, 2024
License     : GPL-3.0-or-later
Maintainer  : gregoireloc@gmail.com
Portability : POSIX

"Papou" is a French infinite verbal child game (mostly told to children) where
you make increasing nonsensical lists of descriptions of "papous" with lots of
"pa" and "pou" syllables. An example can be found
[in the French wiktionary](https://fr.wiktionary.org/w/index.php?title=Papou&oldid=34651672).
If you have GHC installed, you can run this file like you would a bash script
(just make sure you allow it to be run as an executable first, with
`chmod +x papou.hs`)
-}
import Data.List (intercalate)

-- | Remove the `take 1000000 $ ` part to get an infinite flow of text.
-- I limited it to 1000000 characters, because I don't want to have users
-- inadvertently run an infinite script. With 1000000 characters, the script
-- runs in less than 5 seconds on my machine, while still being visually
-- somewhat impressive.
main :: IO ()
main = putStrLn $ take 1000000 $ story ()

-- | Things that can be said of a papou or a pou
data Qualifier = Papou | Papa | APoux deriving (Eq, Show)

-- | The type variable `a` is here so we can basically define two types at once:
-- - A `Description ()` is a blueprint for descriptions of a papou, it is
--   basically a list of qualifiers
-- - A `Description Bool` is an instantiation of such a blueprint, that is, a
--   description of a class of papous. It is also a list of qualifiers,
--   augmented with a boolean for each qualifier (indicating whether the class
--   of papous being described has the given characteristic)
type Description a = [(Qualifier, a)]

-- | Members of this class can be converted to a chunk of the final papou story,
-- which can itself be obtained by applying `story` to the unit value `()`
class Story a where
  story :: a -> String

-- | French negation when `False`
instance Story Bool where
  story True  = ""
  story False = " pas"

instance Story Qualifier where
  story Papou = " papous"
  story Papa  = " papas"
  story APoux = " à poux"

-- | Concatenate all the qualifiers in the list, negating those that need to be
-- negated based on their associated boolean `flag`
instance Story (Description Bool) where
  story [] = ""
  story ((qualifier, flag) : descRest) = story flag <> story qualifier <> story descRest

-- | Make a list of descriptions into a text. We put a comma between each item,
-- except the last two ones, where we put an "et"
instance Story [Description Bool] where
  story descriptions = format $ fmap story descriptions
    where format :: [String] -> String
          format [          ] = error "This should not have happened"
          format [str       ] = error "This should not have happened"
          format [str1, str2] = "des papous" <> str1 <> " et des papous" <> str2
          format (str : strs) = "des papous" <> str <> ", " <> format strs

-- | Enumerate all descriptions that match the blueprint. We prepend this with
-- an intro explaining why we do that enumeration. The intro varies depending
-- on the situation, so we have quite a few cases to deal with
instance Story (Description ()) where
  -- This is the first enumeration in the story, so no "Mais" clause
  story desc@[(Papa, ())] = "Chez les papous, il y a " <> story (enumerate desc) <> "."
  -- No qualifier applied to "poux" yet, so we still have a special case here
  story desc@[(Papa, ()), (APoux, ())] =
    "Mais chez les papous, il y a des papous à poux et des papous pas à poux. \
    \Donc chez les papous, il y a " <> story (enumerate desc) <> "."
  -- The intro of longer descriptions is about "poux" now
  story desc =
    "Mais chez les poux, il y a des poux" <> story (fst . last $ desc) <>
    " et des poux pas"                    <> story (fst . last $ desc) <>
    ". Donc chez les papous, il y a " <> story (enumerate desc) <> "."

-- | Not much to say here. Each item in the list is a sentence that enumerates
-- all classes of papous corresponding to a given blueprint, and we separate
-- those sentences with a space.
instance Story [Description ()] where
  story descs = intercalate " " $ fmap story descs

-- | The final story!
instance Story () where
  story () = story allDescriptions

-- | Enumerate all papou descriptions corresponding to a blueprint
enumerate :: Description () -> [Description Bool]
enumerate [] = [[]]
enumerate ((q, ()) : descRest) = let enumRest = enumerate descRest in
  (((q, True ) :) <$> enumRest) <>
  (((q, False) :) <$> enumRest)

-- | An infinite list of all descriptions blueprints that we will need in the
-- story
allDescriptions :: [Description ()]
allDescriptions =
  [ [(Papa, ())]
  , [(Papa, ()), (APoux, ())]
  , [(Papa, ()), (APoux, ()), (Papou, ())]
  ] <> (
    ([(Papa, ()), (APoux, ()), (Papou, ())] <>) <$> allDescriptions
  )
