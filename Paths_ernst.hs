module Paths_ernst where
--This module will be overriden by cabal when using cabal-install
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return
