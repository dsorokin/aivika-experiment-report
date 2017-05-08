
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Base.LastValueView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'LastValueView' that shows the last values
-- for the simulation variables.
--

module Simulation.Aivika.Experiment.Report.Base.LastValueView 
       (LastValueView(..),
        defaultLastValueView) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import Data.List
import Data.Monoid
import Data.Maybe
import Data.IORef

import Simulation.Aivika
import Simulation.Aivika.Experiment.Base.HtmlWriter
import Simulation.Aivika.Experiment.Base.ExperimentWriter
import Simulation.Aivika.Experiment.Base.SamplingStatsWriter
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Report.Types
import Simulation.Aivika.Experiment.Report.Base.WebReportRenderer
import Simulation.Aivika.Experiment.Utils (replace)

-- | Defines the 'View' that shows the last values of the simulation
-- variables.
data LastValueView =
  LastValueView { lastValueTitle       :: String,
                  -- ^ The title for the view.
                  lastValueRunTitle    :: String,
                  -- ^ The run title for the view. It may include
                  -- special variables @$RUN_INDEX@, @$RUN_COUNT@ and 
                  -- @$TITLE@.
                  --
                  -- An example is 
                  --
                  -- @
                  --   lastValueRunTitle = \"$TITLE / Run $RUN_INDEX of $RUN_COUNT\"
                  -- @
                  lastValueDescription :: String,
                  -- ^ The description for the view.
                  lastValueFormatter   :: ShowS,
                  -- ^ It transforms data before they will be shown.
                  lastValueSourceKey   :: String,
                  -- ^ The source key.
                  lastValueSeries      :: [String] -> [String] 
                  -- ^ It defines the series for which the last values to be shown.
                }
  
-- | This is the default view.
defaultLastValueView :: LastValueView
defaultLastValueView =  
  LastValueView { lastValueTitle       = "Last Values",
                  lastValueRunTitle    = "$TITLE / Run $RUN_INDEX of $RUN_COUNT",
                  lastValueDescription = "It shows the values in the final time point(s).",
                  lastValueFormatter   = id,
                  lastValueSourceKey   = "Must supply with the lastValueSourceKey value.",
                  lastValueSeries      = id }

instance ReportView LastValueView (WebReportRenderer a) where
  
  reportView view = 
    let newWriter agent exp renderer path =
          do let n = experimentEntityRunCount exp
             rs <- forM [1..n] $ \i -> liftIO $ newIORef []    
             let map = M.fromList $ zip [1..n] rs
             return WebReportWriter { reportWriterInitialise = return (),
                                      reportWriterFinalise   = return (),
                                      reportWrite            = \runIndex -> writeLastValues view agent exp renderer map runIndex,
                                      reportWriteTOCHtml     = lastValueTOCHtml view,
                                      reportWriteHtml        = lastValueHtml view map
                                    }
    in WebReportGenerator { newReportWriter = newWriter }
       
-- | Get the last values.
writeLastValues :: LastValueView
                   -> ExperimentAgent
                   -> ExperimentEntity
                   -> WebReportRenderer r
                   -> M.Map Int (IORef [(String, String)])
                   -> Int
                   -> IO ()
writeLastValues view agent exp renderer map runIndex =
  do src <- requireSourceEntityByKey agent (experimentEntityId exp) (lastValueSourceKey view)
     es  <- readLastValueEntities agent (experimentEntityId exp) (sourceEntityId src) runIndex
     rs  <-
       fmap mconcat $
       forM es $ \e ->
       do let item = dataEntityItem e
              name = varEntityName var
              Just var = find (\x -> varEntityId x == dataEntityVarId e)
                         (sourceEntityVarEntities src)
          if null (lastValueSeries view [name])
            then return []
            else return [(name, show $ dataItemValue item)]
     let r = fromJust $ M.lookup runIndex map
     liftIO $ writeIORef r rs
     
-- | Get the HTML code.     
lastValueHtml :: LastValueView -> M.Map Int (IORef [(String, String)]) -> Int -> HtmlWriter ()
lastValueHtml view map index =
  let n = M.size map
  in if n == 1
     then lastValueHtmlSingle view map index
     else lastValueHtmlMultiple view map index
     
-- | Get the HTML code for a single run.
lastValueHtmlSingle :: LastValueView -> M.Map Int (IORef [(String, String)]) -> Int -> HtmlWriter ()
lastValueHtmlSingle view map index =
  do header view index
     let r = fromJust $ M.lookup 1 map
     pairs <- liftIO $ readIORef r
     forM_ pairs $ \pair ->
       formatPair pair (lastValueFormatter view)

-- | Get the HTML code for multiple runs
lastValueHtmlMultiple :: LastValueView -> M.Map Int (IORef [(String, String)]) -> Int -> HtmlWriter ()
lastValueHtmlMultiple view map index =
  do header view index
     let n = M.size map
     forM_ [1..n] $ \i ->
       do let subtitle = 
                replace "$RUN_INDEX" (show i) $
                replace "$RUN_COUNT" (show n) $
                replace "$TITLE" (lastValueTitle view)
                (lastValueRunTitle view)
          writeHtmlHeader4 $
            writeHtmlText subtitle
          let r = fromJust $ M.lookup i map
          pairs <- liftIO $ readIORef r
          forM_ pairs $ \pair ->
            formatPair pair (lastValueFormatter view)

header :: LastValueView -> Int -> HtmlWriter ()
header view index =
  do writeHtmlHeader3WithId ("id" ++ show index) $
       writeHtmlText (lastValueTitle view)
     let description = lastValueDescription view
     unless (null description) $
       writeHtmlParagraph $
       writeHtmlText description

formatPair :: (String, String) -> ShowS -> HtmlWriter ()
formatPair (name, value) formatter =
  writeHtmlParagraph $ 
  do writeHtmlText name
     writeHtmlText " = "
     writeHtmlText $ formatter value
          
-- | Get the TOC item     
lastValueTOCHtml :: LastValueView -> Int -> HtmlWriter ()
lastValueTOCHtml view index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (lastValueTitle view)
