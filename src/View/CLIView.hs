module View.CLIView where

import Data.Text (Text)
import qualified Data.Text.IO as T
import UseCase.FeedGenerator (FeedViewer (view))

newtype CLIView a = CLIView (a -> Text)

instance FeedViewer CLIView where
  view (CLIView showText) = T.putStrLn . showText