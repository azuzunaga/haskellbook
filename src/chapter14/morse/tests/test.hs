class Serializable a where

  toString :: a -> String
  fromString :: String -> Maybe a

newtype MyTrivial = MyTrivial String

-- instance Serializable MyTrivial where
--   toString This = "This"
--   toString That = "That"
--   fromString "This" = Just This
--   fromString "That" = Just That
--   fromString _      = Nothing

-- data List a = Just (a, (List a)) | Nothing
