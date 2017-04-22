
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Base.FinalStatsView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'FinalStatsView' that shows the statistics
-- in the final time points for different simulation runs.
--

module Simulation.Aivika.Experiment.Report.Base.FinalStatsView
       (FinalStatsView(..), 
        defaultFinalStatsView) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar

import Data.List

import Simulation.Aivika
import Simulation.Aivika.Experiment.Base.HtmlWriter
import Simulation.Aivika.Experiment.Base.ExperimentWriter
import Simulation.Aivika.Experiment.Base.SamplingStatsWriter
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Report.Types
import Simulation.Aivika.Experiment.Report.Base.WebReportRenderer
import Simulation.Aivika.Experiment.Concurrent.MVar

-- | Defines the 'View' that shows the statistics
-- in the final time points.
data FinalStatsView =
  FinalStatsView { finalStatsTitle       :: String,
                   -- ^ This is a title for the view.
                   finalStatsDescription :: String,
                   -- ^ This is a description used in HTML.
                   finalStatsWriter      :: SamplingStatsWriter Double,
                   -- ^ It shows the sampling statistics.
                   finalStatsSourceKey   :: String,
                   -- ^ The source key.
                   finalStatsSeries      :: [String] -> [String] 
                   -- ^ It defines the series for which the statistics to be collected.
                 }
  
-- | The default statistics view.  
defaultFinalStatsView :: FinalStatsView
defaultFinalStatsView = 
  FinalStatsView { finalStatsTitle       = "Final Statistics",
                   finalStatsDescription = "Statistics is gathered in final time points for all runs.",
                   finalStatsWriter      = defaultSamplingStatsWriter,
                   finalStatsSourceKey   = "Must supply with the finalStatsSourceKey value",
                   finalStatsSeries      = id }

instance ReportView FinalStatsView (WebReportRenderer a) where
  
  reportView view = 
    let newWriter agent exp renderer path =
          do results <- liftIO newEmptyMVar
             return WebReportWriter { reportWriterInitialise = return (),
                                      reportWriterFinalise   = finaliseFinalStats view agent exp renderer results,
                                      reportWrite            = \runIndex -> return (),
                                      reportWriteTOCHtml     = finalStatsTOCHtml view,
                                      reportWriteHtml        = finalStatsHtml view results
                                    }
    in WebReportGenerator { newReportWriter = newWriter }
       
-- | Get the statistics.
finaliseFinalStats :: FinalStatsView
                      -> ExperimentAgent
                      -> ExperimentEntity
                      -> WebReportRenderer r
                      -> MVar [(String, SamplingStats Double)]
                      -> IO ()
finaliseFinalStats view agent exp renderer results =
  do src <- requireSourceEntityByKey agent (experimentEntityId exp) (finalStatsSourceKey view)
     es <- readFinalDeviationEntities agent (experimentEntityId exp) (sourceEntityId src)
     ps <-
       fmap mconcat $
       forM es $ \e ->
       do let item  = multipleDataEntityItem e
              time  = dataItemTime item
              stats = dataItemValue item
              name  = varEntityName var
              Just var = find (\x -> varEntityId x == multipleDataEntityVarId e) (sourceEntityVarEntities src)
          if null (finalStatsSeries view [name])
            then return [] 
            else return [(name, stats)]
     liftIO $
       putMVar results ps

-- | Get the HTML code.     
finalStatsHtml :: FinalStatsView -> MVar [(String, SamplingStats Double)] -> Int -> HtmlWriter ()
finalStatsHtml view results index =
  do header view index
     pairs <- liftIO $ readMVar results
     let writer = finalStatsWriter view
         write  = samplingStatsWrite writer
     forM_ pairs $ \(name, stats) ->
       write writer name stats

header :: FinalStatsView -> Int -> HtmlWriter ()
header view index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (finalStatsTitle view)
     let description = finalStatsDescription view
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
finalStatsTOCHtml :: FinalStatsView -> Int -> HtmlWriter ()
finalStatsTOCHtml view index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (finalStatsTitle view)
