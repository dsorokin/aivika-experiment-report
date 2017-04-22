
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Base.TableView
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines 'TableView' that saves the simulation
-- results in the CSV file(s).
--

module Simulation.Aivika.Experiment.Report.Base.TableView 
       (TableView(..), 
        defaultTableView) where

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

-- | Defines the 'View' that saves the simulation results
-- in the CSV file(s).
data TableView =
  TableView { tableTitle       :: String,
              -- ^ This is a title used in HTML.
              tableDescription :: String,
              -- ^ This is a description in the HTML.
              tableLinkText    :: String,
              -- ^ It specifies the text for the link 
              -- which is displayed in the HTML page
              -- if there is only one simulation run. 
              -- The link downloads the corresponded 
              -- CSV file in the browser. If there are
              -- more simulation runs, then this link 
              -- is not shown.
              --
              -- An example is
              --
              -- @
              --   tableLinkText = \"Download the CSV file\"
              -- @
              tableRunLinkText :: String,
              -- ^ It specifies the link text which is 
              -- displayed in the HTML page if there are 
              -- many simulation runs. Such a link downloads 
              -- the CSV file for the corresponded run. 
              -- To define the text, you can use special 
              -- variables @$LINK@, @$RUN_INDEX@ and @$RUN_COUNT@.
              --
              -- An example is 
              -- 
              -- @
              --   tableRunLinkText = \"$LINK / Run $RUN_INDEX of $RUN_COUNT\"
              -- @
              -- 
              -- If there is only one run, then the link of 
              -- this kind is not displayed. Instead, only one 
              -- link is shown, which text is defined by the 
              -- 'tableLinkText' field.
              tableFileName    :: ExperimentFilePath,
              -- ^ It defines the file name for each CSV file. 
              -- It may include special variables @$TITLE@, 
              -- @$RUN_INDEX@ and @$RUN_COUNT@.
              --
              -- An example is
              --
              -- @
              --   tableFileName = UniqueFilePath \"$TITLE - $RUN_INDEX.csv\"
              -- @
              tableSeparator   :: String,
              -- ^ It defines the separator for the view. 
              -- It delimits the cells in the rows of the CSV file.
              tableFormatter   :: ShowS,
              -- ^ It defines the formatter which is applied
              -- to all values before they will be written
              -- in the CSV file(s).
              tableTimeName :: String,
              -- ^ The time parameter name.
              tableSourceKey   :: String,
              -- ^ The source key.
              tableSeries      :: [String] -> [String] 
              -- ^ It defines the series to save in the CSV file(s).
            }
  
-- | The default table view.  
defaultTableView :: TableView
defaultTableView = 
  TableView { tableTitle       = "Table",
              tableDescription = "This section contains the CSV file(s) with the simulation results.",
              tableLinkText    = "Download the CSV file",
              tableRunLinkText = "$LINK / Run $RUN_INDEX of $RUN_COUNT",
              tableFileName    = UniqueFilePath "Table($RUN_INDEX).csv",
              tableSeparator   = ",",
              tableFormatter   = id,
              tableTimeName    = "time",
              tableSourceKey   = "Must supply with the tableSourceKey value.",
              tableSeries      = id }

instance ReportView TableView (WebReportRenderer a) where
  
  reportView view = 
    let newWriter agent exp renderer path =
          do let n = experimentEntityRunCount exp
             files <- forM [0..(n - 1)] $ \i ->
               resolveFilePath path $
               mapFilePath (flip replaceExtension ".csv") $
               expandFilePath (tableFileName view) $
               M.fromList [("$TITLE", tableTitle view),
                           ("$RUN_INDEX", show $ i + 1),
                           ("$RUN_COUNT", show n)]
             liftIO $ forM_ files $ flip writeFile []  -- reserve the file names
             let arr = A.listArray (1, n) files
             return WebReportWriter { reportWriterInitialise = return (),
                                      reportWriterFinalise   = return (),
                                      reportWrite            = \runIndex -> writeTable view agent exp renderer (arr A.! runIndex) runIndex,
                                      reportWriteTOCHtml     = tableTOCHtml view,
                                      reportWriteHtml        = tableHtml view path files
                                    }
    in WebReportGenerator { newReportWriter = newWriter }
  
-- | Write the tables.
writeTable :: TableView
              -> ExperimentAgent
              -> ExperimentEntity
              -> WebReportRenderer r
              -> FilePath
              -> Int
              -> IO ()
writeTable view agent exp renderer file runIndex =
  do let separator = tableSeparator view
         formatter = tableFormatter view
     src <- requireSourceEntityByKey agent (experimentEntityId exp) (tableSourceKey view)
     es <- readTimeSeriesEntities agent (experimentEntityId exp) (sourceEntityId src) runIndex
     rs <- fmap mconcat $
           forM es $ \e0 ->
           do e <- e0
              let items = dataEntityItem e
                  name  = varEntityName var
                  Just var = find (\x -> varEntityId x == dataEntityVarId e)
                             (sourceEntityVarEntities src)
              if null (tableSeries view [name])
                then return []
                else return [(name, items)]
     -- create a new file
     h <- openFile file WriteMode
     -- write a header
     hPutStr h $ show (tableTimeName view)
     forM_ (zip [0..] rs) $ \(column, (name, items)) ->
       do hPutStr h separator
          hPutStr h $ show name
     hPutStrLn h ""
     let itemss = concatDataItems $ map snd rs
     forM_ itemss $ \items ->
       do hPutStr h $ formatter $ show (dataItemTime items)
          forM_ (zip [0..] $ dataItemValue items) $ \(column, val) ->  -- write the row
            do hPutStr h separator
               hPutStr h $ formatter $ show val
          hPutStrLn h ""
     when (reportVerbose renderer) $
       putStr "Generated file " >> putStrLn file
     hClose h  -- close the file
     
-- | Get the HTML code.     
tableHtml :: TableView -> FilePath -> [FilePath] -> Int -> HtmlWriter ()
tableHtml view path0 [path] index = tableHtmlSingle view path0 [path] index
tableHtml view path0 paths index  = tableHtmlMultiple view path0 paths index
     
-- | Get the HTML code for a single run.
tableHtmlSingle :: TableView -> FilePath -> [FilePath] -> Int -> HtmlWriter ()
tableHtmlSingle view path0 [path] index =
  do header view index
     writeHtmlParagraph $
       writeHtmlLink (makeRelative path0 path) $
       writeHtmlText (tableLinkText view)

-- | Get the HTML code for multiple runs.
tableHtmlMultiple :: TableView -> FilePath -> [FilePath] -> Int -> HtmlWriter ()
tableHtmlMultiple view path0 paths index =
  do header view index
     let n = length paths
     forM_ (zip [0..] paths) $ \(i, path) ->
       do let sublink = 
                replace "$RUN_INDEX" (show $ i + 1) $
                replace "$RUN_COUNT" (show n) $
                replace "$LINK" (tableLinkText view)
                (tableRunLinkText view)
          writeHtmlParagraph $
            writeHtmlLink (makeRelative path0 path) $
            writeHtmlText sublink

header :: TableView -> Int -> HtmlWriter ()
header view index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (tableTitle view)
     let description = tableDescription view
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.    
tableTOCHtml :: TableView -> Int -> HtmlWriter ()
tableTOCHtml view index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (tableTitle view)
