module HLib where

import           Control.Monad.Fail as M

justOrFail :: MonadFail m => Maybe a -> String -> m a
justOrFail (Just value) _ = return value
justOrFail Nothing error = M.fail error

headOrNothing :: [a] -> Maybe a
headOrNothing (h : _) = Just h
headOrNothing _ = Nothing
