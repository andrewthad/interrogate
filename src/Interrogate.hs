{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module Interrogate where

import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Text (Text)
import Data.Functor.Identity (Identity(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.Vector as V
import qualified Data.Aeson as AE
import qualified Data.HashMap.Strict as HM

data Multiplicity = One | Many
data Atom = Object | Array | Value | Ground Type
data InterrogateError = InterrogateError

type family Multiplied (m :: Multiplicity) :: Type -> Type where
  Multiplied 'One = Identity
  Multiplied 'Many = Vector

data Interrogate :: Multiplicity -> Atom -> Atom -> Type where
  InterrogateKey :: Text -> Interrogate m 'Value a -> Interrogate m 'Object a
  InterrogateArray :: Interrogate m 'Array a -> Interrogate m 'Value a
  InterrogateObject :: Interrogate m 'Object a -> Interrogate m 'Value a
  InterrogateDive :: Interrogate 'One 'Value a -> Interrogate 'Many 'Array a

key :: Text -> Interrogate m 'Value a -> Interrogate m 'Object a
key = InterrogateKey

runOne :: Interrogate 'One 'Value ('Ground a) -> AE.Value -> Either InterrogateError a
runOne x0 v0 = fmap runIdentity (run x0 v0)

run :: Interrogate m 'Value ('Ground a) -> AE.Value -> Either InterrogateError (Multiplied m a)
run x0 v0 = case x0 of
  InterrogateObject x1 -> case v0 of
    AE.Object v1 -> runObject x1 v1
    _ -> Left InterrogateError
  InterrogateArray x1 -> case v0 of
    AE.Array v1 -> runArray x1 v1
    _ -> Left InterrogateError

runObject :: Interrogate m 'Object ('Ground a) -> HashMap Text AE.Value -> Either InterrogateError (Multiplied m a)
runObject (InterrogateKey k x1) hm = case HM.lookup k hm of
  Nothing -> Left InterrogateError
  Just v1 -> run x1 v1
  
runArray :: Interrogate m 'Array ('Ground a) -> Vector AE.Value -> Either InterrogateError (Multiplied m a)
runArray (InterrogateDive x1) v0 = V.mapM (\v1 -> fmap runIdentity (run x1 v1)) v0

