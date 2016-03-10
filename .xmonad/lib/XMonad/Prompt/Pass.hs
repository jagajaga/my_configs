-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Pass
-- Copyright   :  (c) 2014 Igor Babuschkin, Antoine R. Dumont
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Antoine R. Dumont <eniotna.t@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides 3 <XMonad.Prompt> to ease passwords manipulation (generate, read, remove):
--
-- - one to lookup passwords in the password-storage.
--
-- - one to generate a password for a given password label that the user inputs.
--
-- - one to delete a stored password for a given password label that the user inputs.
--
-- All those prompts benefit from the completion system provided by the module <XMonad.Prompt>.
--
-- The password store is setuped through an environment variable PASSWORD_STORE_DIR.
-- If this is set, use the content of the variable.
-- Otherwise, the password store is located on user's home @$HOME\/.password-store@.
--
--
-- Source:
--
-- - The password storage implementation is <http://git.zx2c4.com/password-store the password-store cli>.
--
-- - Inspired from <http://babushk.in/posts/combining-xmonad-and-pass.html>
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Pass where

import System.Environment
import System.FilePath.Posix
import System.FilePath.Find
import XMonad
import XMonad.Prompt
import System.Directory

data Pass = Pass

instance XPrompt Pass where
  showXPrompt       Pass = "Pass: "
  commandToComplete _ c  = c
  nextCompletion      _  = getNextCompletion

passPrompt :: XPConfig -> X ()
passPrompt c = do
  li <- io getPasswords
  mkXPrompt Pass c (mkComplFunFromList li) selectPassword

selectPassword :: String -> X ()
selectPassword s = spawn $ "pass -c " ++ s

getPasswords :: IO [String]
getPasswords = do
  home <- getEnv "HOME"
  let passwordStore = home </> ".password-store"
  entries <- find System.FilePath.Find.always (fileName ~~? "*.gpg") $
    passwordStore
  return $ map ((makeRelative passwordStore) . dropExtension) entries 
