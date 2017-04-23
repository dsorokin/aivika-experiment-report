
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Base.FinalTableView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'FinalTableView' that saves the simulation
-- results in the final time points for all simulation runs in
-- the CSV file.
--

module Simulation.Aivika.Experiment.Report.Base.FinalTableView
       (FinalTableView(..), 
        defaultFinalTableView) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import qualified Data.Array as A
import Data.List

import System.IO
import System.FilePath

import Simulation.Aivika
import Simulation.Aivika.Experiment.Base.HtmlWriter
import Simulation.Aivika.Experiment.Base.ExperimentWriter
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Report.Types
import Simulation.Aivika.Experiment.Report.Base.WebReportRenderer
import Simulation.Aivika.Experiment.Utils (replace)

-- | Defines the 'View' that saves the simulation 
-- results in the final time points for all 
-- simulation runs in the CSV file.
data FinalTableView =
  FinalTableView { finalTableTitle       :: String,
                   -- ^ This is a title used in HTML.
                   finalTableDescription :: String,
                   -- ^ This is a description used in HTML.
                   finalTableRunText     :: String,
                   -- ^ Translated text \"Run\".
                   finalTableLinkText    :: String,
                   -- ^ It specifies the text for the link 
                   -- which is displayed in the HTML page. 
                   -- The link downloads the corresponded 
                   -- CSV file in the browser. 
                   --
                   -- An example is
                   --
                   -- @
                   --   finalTableLinkText = \"Download the CSV file\"
                   -- @
                   finalTableFileName    :: ExperimentFilePath,
                   -- ^ It defines the file name for the CSV file. 
                   -- It may include special variable @$TITLE@.
                   --
                   -- An example is
                   --
                   -- @
                   --   finalTableFileName = UniqueFilePath \"$TITLE.csv\"
                   -- @
                   finalTableSeparator   :: String,
                   -- ^ It defines the separator for the view. 
                   -- It delimits the cells in the rows of the CSV file.
                   finalTableFormatter   :: ShowS,
                   -- ^ It defines the formatter which is applied
                   -- to all values before they will be written
                   -- in the CSV file.
                   finalTableSourceKey   :: String,
                   -- ^ The source key.
                   finalTableSeries      :: [String] -> [String] 
                   -- ^ It defines the series to save in the CSV file.
                 }
  
-- | The default table view.  
defaultFinalTableView :: FinalTableView
defaultFinalTableView = 
  FinalTableView { finalTableTitle       = "Final Table",
                   finalTableDescription = "It refers to the CSV file with the results in the final time points.",
                   finalTableRunText     = "Run",
                   finalTableLinkText    = "Download the CSV file",
                   finalTableFileName    = UniqueFilePath "FinalTable.csv",
                   finalTableSeparator   = ",",
                   finalTableFormatter   = id,
                   finalTableSourceKey   = "Must supply with the finalTableSourceKey value.",
                   finalTableSeries      = id }

instance ReportView FinalTableView (WebReportRenderer a) where
  
  reportView view = 
    let newWriter agent exp renderer path =
          do file <- resolveFilePath path $
                     mapFilePath (flip replaceExtension ".csv") $
                     expandFilePath (finalTableFileName view) $
                     M.fromList [("$TITLE", finalTableTitle view)]
             return WebReportWriter { reportWriterInitialise = return (),
                                      reportWriterFinalise   = finaliseFinalTable view agent exp renderer file,
                                      reportWrite            = \runIndex -> return (),
                                      reportWriteTOCHtml     = finalTableTOCHtml view,
                                      reportWriteHtml        = finalTableHtml view path file
                                    }
    in WebReportGenerator { newReportWriter = newWriter }

-- | Save the results.
finaliseFinalTable :: FinalTableView
                      -> ExperimentAgent
                      -> ExperimentEntity
                      -> WebReportRenderer r
                      -> FilePath
                      -> IO ()
finaliseFinalTable view agent exp renderer file =
  do let runCount  = experimentEntityRunCount exp
         run       = finalTableRunText view
         formatter = finalTableFormatter view
         title     = finalTableTitle view
         separator = finalTableSeparator view
     src <- requireSourceEntityByKey agent (experimentEntityId exp) (finalTableSourceKey view)
     rs  <-
       forM [1..runCount] $ \runIndex ->
       do es' <- readLastValueEntities agent (experimentEntityId exp) (sourceEntityId src) runIndex
          fmap (\x -> (runIndex, x)) $
            fmap M.fromList $
            fmap mconcat $
            forM es' $ \e ->
            do let item = dataEntityItem e
                   name = varEntityName var
                   Just var = find (\x -> varEntityId x == dataEntityVarId e)
                              (sourceEntityVarEntities src)
               if null (finalTableSeries view [name])
                 then return []
                 else return [(name, dataItemValue item)]
     let names = if null rs then [] else M.keys (snd $ head rs)
     -- create a new file
     h <- openFile file WriteMode
     -- write a header
     hPutStr h $ show run
     forM_ names $ \name ->
       do hPutStr h separator
          hPutStr h $ show name
     hPutStrLn h ""
     -- write data
     forM_ rs $ \(runIndex, xs) ->
       do hPutStr h $ show runIndex
          forM_ names $ \name ->
            case M.lookup name xs of
              Nothing ->
                error $
                "Expected variable " ++ name ++
                " in the result set for run index = " ++ show runIndex ++
                ": finaliseFinalTable"
              Just x ->
                do hPutStr h separator
                   hPutStr h $ formatter (show x)
          hPutStrLn h ""
     -- close file
     hClose h 
     when (reportVerbose $ renderer) $
       putStr "Generated file " >> putStrLn file
     
-- | Get the HTML code.     
finalTableHtml :: FinalTableView -> FilePath -> FilePath -> Int -> HtmlWriter ()
finalTableHtml view path file index =
  do header view index
     writeHtmlParagraph $
       writeHtmlLink (makeRelative path file) $
       writeHtmlText (finalTableLinkText view)

header :: FinalTableView -> Int -> HtmlWriter ()
header view index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (finalTableTitle view)
     let description = finalTableDescription view
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
finalTableTOCHtml :: FinalTableView -> Int -> HtmlWriter ()
finalTableTOCHtml view index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (finalTableTitle view)
