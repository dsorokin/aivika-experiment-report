
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Base.TimingStatsView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'TimingStatsView' that shows the timing statistics
-- for the variables for every simulation run separately.
--

module Simulation.Aivika.Experiment.Report.Base.TimingStatsView 
       (TimingStatsView(..),
        defaultTimingStatsView) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.IORef

import Simulation.Aivika
import Simulation.Aivika.Experiment.Base.HtmlWriter
import Simulation.Aivika.Experiment.Base.ExperimentWriter
import Simulation.Aivika.Experiment.Base.TimingStatsWriter
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Report.Types
import Simulation.Aivika.Experiment.Report.Base.WebReportRenderer
import Simulation.Aivika.Experiment.Utils (replace)

-- | Defines the 'View' that shows the timing statistics
-- for variables for every simulation run separately.
data TimingStatsView =
  TimingStatsView { timingStatsTitle       :: String,
                    -- ^ The title for the view.
                    timingStatsRunTitle    :: String,
                    -- ^ The run title for the view. It may include
                    -- special variables @$RUN_INDEX@, @$RUN_COUNT@ and 
                    -- @$TITLE@.
                    --
                    -- An example is 
                    --
                    -- @
                    --   timingStatsRunTitle = \"$TITLE / Run $RUN_INDEX of $RUN_COUNT\"
                    -- @
                    timingStatsDescription :: String,
                    -- ^ The description for the view.
                    timingStatsWriter      :: TimingStatsWriter Double,
                    -- ^ It shows the timing statistics.
                    timingStatsSourceKey   :: String,
                    -- ^ The source key.
                    timingStatsSeries      :: [String] -> [String] 
                    -- ^ It defines the series for which the statistics to be collected.
                  }
  
-- | This is the default view.
defaultTimingStatsView :: TimingStatsView
defaultTimingStatsView =  
  TimingStatsView { timingStatsTitle       = "Statistics for Time-Persistent Variables",
                    timingStatsRunTitle    = "$TITLE / Run $RUN_INDEX of $RUN_COUNT",
                    timingStatsDescription = "The statistical data are gathered in the time points.",
                    timingStatsWriter      = defaultTimingStatsWriter,
                    timingStatsSourceKey   = "Must supply with the timingStatsSourceKey value.",
                    timingStatsSeries      = id }

instance ReportView TimingStatsView (WebReportRenderer a) where
  
  reportView view = 
    let newWriter agent exp renderer path =
          do let n = experimentEntityRunCount exp
             rs <- forM [1..n] $ \i -> liftIO $ newIORef []    
             let map = M.fromList $ zip [1..n] rs
             return WebReportWriter { reportWriterInitialise = return (),
                                      reportWriterFinalise   = return (),
                                      reportWrite            = \runIndex -> writeTimingStats view agent exp renderer map runIndex,
                                      reportWriteTOCHtml     = timingStatsTOCHtml view,
                                      reportWriteHtml        = timingStatsHtml view map
                                    }
    in WebReportGenerator { newReportWriter = newWriter }

-- | Get the time-dependent statistics.
writeTimingStats :: TimingStatsView
                    -> ExperimentAgent
                    -> ExperimentEntity
                    -> WebReportRenderer r
                    -> M.Map Int (IORef [(String, TimingStats Double)])
                    -> Int
                    -> IO ()
writeTimingStats view agent exp renderer map runIndex =
  do src <- requireSourceEntityByKey agent (experimentEntityId exp) (timingStatsSourceKey view)
     es  <- readFinalTimingStatsEntities agent (experimentEntityId exp) (sourceEntityId src) runIndex
     rs  <-
       fmap mconcat $
       forM es $ \e ->
       do let item = dataEntityItem e
              name = varEntityName var
              Just var = find (\x -> varEntityId x == dataEntityVarId e)
                         (sourceEntityVarEntities src)
          if null (timingStatsSeries view [name])
            then return []
            else return [(name, dataItemValue item)]
     let r = fromJust $ M.lookup runIndex map
     liftIO $ writeIORef r rs
     
-- | Get the HTML code.     
timingStatsHtml :: TimingStatsView -> M.Map Int (IORef [(String, TimingStats Double)]) -> Int -> HtmlWriter ()
timingStatsHtml view map index =
  let n = M.size map
  in if n == 1
     then timingStatsHtmlSingle view map index
     else timingStatsHtmlMultiple view map index
     
-- | Get the HTML code for a single run.
timingStatsHtmlSingle :: TimingStatsView -> M.Map Int (IORef [(String, TimingStats Double)]) -> Int -> HtmlWriter ()
timingStatsHtmlSingle view map index =
  do header view index
     let r = fromJust $ M.lookup 1 map
     pairs <- liftIO $ readIORef r
     forM_ pairs $ \(name, stats) ->
       do let writer = timingStatsWriter view
              write  = timingStatsWrite writer
          write writer name stats

-- | Get the HTML code for multiple runs
timingStatsHtmlMultiple :: TimingStatsView -> M.Map Int (IORef [(String, TimingStats Double)]) -> Int -> HtmlWriter ()
timingStatsHtmlMultiple view map index =
  do header view index
     let n = M.size map
     forM_ [1..n] $ \i ->
       do let subtitle = 
                replace "$RUN_INDEX" (show i) $
                replace "$RUN_COUNT" (show n) $
                replace "$TITLE" (timingStatsTitle view)
                (timingStatsRunTitle view)
          writeHtmlHeader4 $
            writeHtmlText subtitle
          let r = fromJust $ M.lookup i map
          pairs <- liftIO $ readIORef r
          forM_ pairs $ \(name, stats) ->
            do let writer = timingStatsWriter view
                   write  = timingStatsWrite writer
               write writer name stats

header :: TimingStatsView -> Int -> HtmlWriter ()
header view index =
  do writeHtmlHeader3WithId ("id" ++ show index) $
       writeHtmlText (timingStatsTitle view)
     let description = timingStatsDescription view
     unless (null description) $
       writeHtmlParagraph $
       writeHtmlText description

-- | Get the TOC item     
timingStatsTOCHtml :: TimingStatsView -> Int -> HtmlWriter ()
timingStatsTOCHtml view index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (timingStatsTitle view)
