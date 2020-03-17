module HLib where

import           Control.Monad.Fail as M

justOrFail :: MonadFail m => Maybe a -> String -> m a
justOrFail (Just value) _ = return value
justOrFail Nothing error = M.fail error
