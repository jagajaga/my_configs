-- Love current song from moc player
-- Author: supki
-- Modified by JagaJaga

{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Lens
import           Data.Aeson.Lens
import           Data.Monoid
import           Data.Text            (Text, pack)
import qualified Data.Text            as T
import qualified Keys                 as K
import           Libnotify
import           Network.Lastfm
import qualified Network.Lastfm.Track as Track
import           System.Process


main :: IO ()
main = do
    let (ak, sk, s) = (K.aPIKey, K.sessionKey, K.secret)
    [a, t] <- getSong
    correct a t ak >>= love a ak sk s

{-getInfo :: MPD.Song -> [Text]-}
{-getInfo s = map getTag [MPD.Artist, MPD.Title]-}
 {-where-}
    {-getTag t = T.fromStrict . MPD.toText . head $ MPD.sgTags s M.! t-}

getSong :: IO [Text]
getSong = do
    artist <- readProcess "/home/jaga/myscripts/getmocpinfo.sh" ["-a"] []
    title <- readProcess "/home/jaga/myscripts/getmocpinfo.sh" ["-s"] []
    return [pack artist, pack title]


correct :: Text -> Text -> Text -> IO Text
correct a t ak = withConnection $ \conn -> do
    r <- lastfm conn $ Track.getCorrection <*> artist a <*> track t <*> apiKey ak <* json
    case r ^? folded . key "corrections" . key "correction" . key "track" . key "name"._String of
        Just t' -> return t'
        Nothing -> return t

love :: Text -> Text -> Text -> Text -> Text -> IO ()
love a ak sk s t = withConnection $ \conn -> do
    lastfm conn $ sign (Secret s) $ Track.love <*> artist a <*> track t <*> apiKey ak <*> sessionKey sk <* json
    display_ (summary "Last.fm" <> body ("Track loved:\n" ++ strip (T.unpack a) ++ "\n" ++ strip (T.unpack t)))
    return ()
 where
    strip (x:xs)
        | x == '&' = "&amp;" ++ strip xs
        | otherwise = x : strip xs
    strip [] = []
