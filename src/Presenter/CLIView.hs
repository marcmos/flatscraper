module Presenter.CLIView where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import UseCase.FeedGenerator (FeedViewer (view))

newtype CLIView a = CLIView (a -> Text)

htmlCliView :: CLIView H.Html
htmlCliView = CLIView (TL.toStrict . H.renderHtml)

instance FeedViewer CLIView where
  view (CLIView transform) inp = T.putStrLn $ transform inp