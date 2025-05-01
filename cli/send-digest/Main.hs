module Main where

import qualified Data.Text as T (pack)
import qualified Data.Text.Lazy as TL
import DataAccess.SQLite (SQLitePersistence (SQLitePersistence))
import Presenter.HTMLFeedPresenter
  ( BadgeColorMapper (cmPricePerMeter),
    HTMLFeedPresenter (HTMLFeedPresenter),
    cmArea,
    defaultColorMapper,
  )
import Presenter.UserPrefs (badgeColorMapper)
import System.Environment (getArgs)
import qualified Text.Blaze.Html.Renderer.Text as H
import UseCase.FeedGenerator (showNewSinceLastVisit)
import View.SMTPView (SMTPView (SMTPView), loadCredentialsFromFile)

main :: IO ()
main = do
  smtpCreds <- loadCredentialsFromFile "smtp-creds"
  args <- getArgs

  let recipient = case args of
        [r] -> T.pack r
        _ -> "example@gmail.com"

  case smtpCreds of
    Just creds -> do
      let viewer = SMTPView (TL.toStrict . H.renderHtml) creds recipient
      -- let viewer = htmlCliView
      showNewSinceLastVisit
        sqlite
        (HTMLFeedPresenter (Just badgeColorMapper))
        viewer
    Nothing -> return ()
  where
    sqlite = SQLitePersistence
    view = SMTPView (TL.toStrict . H.renderHtml)
    presenter = HTMLFeedPresenter (Just badgeColorMapper)