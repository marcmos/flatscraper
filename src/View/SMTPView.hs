module View.SMTPView (SMTPView (SMTPView), loadCredentialsFromFile) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import qualified Data.Text.Lazy as TL (fromStrict)
import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP
import Network.HaskellNet.SMTP.SSL (doSMTPSTARTTLS)
import Network.Mail.Mime (Address (Address), mailCc, simpleMail)
import System.IO (hPutStrLn, stderr)
import UseCase.FeedGenerator (FeedViewer (view))

data SMTPView a = SMTPView (a -> Text) (UserName, Password) Text

instance FeedViewer SMTPView where
  view = viewViaSMTP

loadCredentialsFromFile :: FilePath -> IO (Maybe (UserName, Password))
loadCredentialsFromFile path = do
  rawText <- T.readFile path
  let splitText = T.split (== '\n') rawText
  return $ case splitText of
    username : password : _ -> Just (T.unpack username, T.unpack password)
    _ -> Nothing

viewViaSMTP :: SMTPView a -> Text -> a -> IO ()
viewViaSMTP (SMTPView convert (username, password) recipient) subject input = do
  doSMTPSTARTTLS "smtp.gmail.com" $ \conn -> do
    -- (1)
    authSucceed <- authenticate PLAIN username password conn
    if authSucceed
      then do
        mail <-
          simpleMail
            (Address Nothing recipient)
            (Address Nothing $ T.pack username)
            subject
            "Hello! This is the mail body!"
            (TL.fromStrict $ convert input)
            []
        let mailWithCC = mail {mailCc = [Address Nothing $ T.pack username]}
        sendMail mailWithCC conn -- (3)
      else hPutStrLn stderr "Authentication failed."
