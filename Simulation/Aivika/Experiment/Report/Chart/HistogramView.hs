
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Chart.HistogramView
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines 'HistogramView' that plots the histogram
-- collecting statistics in all integration time points and does
-- it for every simulation run separately.
--

module Simulation.Aivika.Experiment.Report.Chart.HistogramView
       (HistogramView(..), 
        defaultHistogramView) where

import Control.Monad
import Control.Monad.Trans
import Control.Lens

import Data.List
import qualified Data.Map as M
import qualified Data.Array as A
import Data.Either
import Data.Default.Class

import System.FilePath

import Graphics.Rendering.Chart

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Base.HtmlWriter
import Simulation.Aivika.Experiment.Base.ExperimentWriter
import Simulation.Aivika.Experiment.Chart.Types
import Simulation.Aivika.Experiment.Chart.Utils (colourisePlotBars)
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Report.Types
import Simulation.Aivika.Experiment.Report.Base.WebReportRenderer

-- | Defines the 'View' that plots the histogram collecting statistics
-- for all integration time points but for each simulation run separately.
data HistogramView =
  HistogramView { histogramTitle       :: String,
                  -- ^ This is a title used in HTML.
                  histogramDescription :: String,
                  -- ^ This is a description used in HTML.
                  histogramWidth       :: Int,
                  -- ^ The width of the histogram.
                  histogramHeight      :: Int,
                  -- ^ The height of the histogram.
                  histogramFileName    :: ExperimentFilePath,
                  -- ^ It defines the file name with optional extension for each image to be saved.
                  -- It may include special variables @$TITLE@, @$RUN_INDEX@ and @$RUN_COUNT@.
                  --
                  -- An example is
                  --
                  -- @
                  --   histogramFileName = UniqueFilePath \"$TITLE - $RUN_INDEX\"
                  -- @
                  histogramBuild       :: [[Double]] -> Histogram, 
                  -- ^ Builds a histogram by the specified list of 
                  -- data series.
                  histogramSourceKey   :: String,
                  -- ^ The source key.
                  histogramSeries      :: [String] -> [String], 
                  -- ^ It defines the series to be plotted on the histogram.
                  histogramPlotTitle   :: String,
                  -- ^ This is a title used in the histogram when
                  -- simulating a single run. It may include 
                  -- special variable @$TITLE@.
                  --
                  -- An example is
                  --
                  -- @
                  --   histogramPlotTitle = \"$TITLE\"
                  -- @
                  histogramRunPlotTitle :: String,
                  -- ^ The run title for the histogram. It is used 
                  -- when simulating multiple runs and it may 
                  -- include special variables @$RUN_INDEX@, 
                  -- @$RUN_COUNT@ and @$PLOT_TITLE@.
                  --
                  -- An example is 
                  --
                  -- @
                  --   histogramRunPlotTitle = \"$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT\"
                  -- @
                  histogramPlotBars :: PlotBars Double Double ->
                                       PlotBars Double Double,
                  -- ^ A transformation based on which the plot bar
                  -- is constructed for the series. 
                  --
                  -- Here you can define a colour or style of
                  -- the plot bars.
                  histogramLayout :: Layout Double Double ->
                                     Layout Double Double
                  -- ^ A transformation of the plot layout, 
                  -- where you can redefine the axes, for example.
                }
  
-- | The default histogram view.  
defaultHistogramView :: HistogramView
defaultHistogramView = 
  HistogramView { histogramTitle       = "Histogram",
                  histogramDescription = "It shows the histogram(s) by data gathered in the integration time points.",
                  histogramWidth       = 640,
                  histogramHeight      = 480,
                  histogramFileName    = UniqueFilePath "Histogram($RUN_INDEX)",
                  histogramBuild       = histogram binSturges,
                  histogramSourceKey   = "Must supply with the histogramSourceKey value",
                  histogramSeries      = mempty, 
                  histogramPlotTitle   = "$TITLE",
                  histogramRunPlotTitle = "$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT",
                  histogramPlotBars    = colourisePlotBars,
                  histogramLayout      = id }

instance ChartRendering r => ReportView HistogramView (WebReportRenderer r) where
  
  reportView view = 
    let newWriter agent exp renderer path =
          do let n = experimentEntityRunCount exp
             files <- forM [1..n] $ \i ->
               resolveFilePath path $
               mapFilePath (flip replaceExtension $ renderableChartExtension $ reportParameter renderer) $
               expandFilePath (histogramFileName view) $
               M.fromList [("$TITLE", histogramTitle view),
                           ("$RUN_INDEX", show i),
                           ("$RUN_COUNT", show n)]
             liftIO $ forM_ files $ flip writeFile []  -- reserve the file names
             let arr = A.array (1, n) $ zip [1..n] files
             return WebReportWriter { reportWriterInitialise = return (),
                                      reportWriterFinalise   = return (),
                                      reportWrite            = \runIndex -> writeHistogram view agent exp renderer (arr A.! runIndex) runIndex,
                                      reportWriteTOCHtml     = histogramTOCHtml view,
                                      reportWriteHtml        = histogramHtml view path files
                                    }
    in WebReportGenerator { newReportWriter = newWriter }
       
-- | Plot the histogram.
writeHistogram :: ChartRendering r
                  => HistogramView
                  -> ExperimentAgent
                  -> ExperimentEntity
                  -> WebReportRenderer r
                  -> FilePath
                  -> Int
                  -> IO ()
writeHistogram view agent exp renderer file runIndex =
  do let n = experimentEntityRunCount exp
         width   = histogramWidth view
         height  = histogramHeight view
         title   = histogramTitle view
         plotTitle  = histogramPlotTitle view
         runPlotTitle = histogramRunPlotTitle view
         build      = histogramBuild view
         bars       = histogramPlotBars view
         layout     = histogramLayout view
         plotTitle' = 
           replace "$TITLE" title
           plotTitle
         runPlotTitle' =
           if n == 1
           then plotTitle'
           else replace "$RUN_INDEX" (show runIndex) $
                replace "$RUN_COUNT" (show n) $
                replace "$PLOT_TITLE" plotTitle'
                runPlotTitle
     src <- requireSourceEntityByKey agent (experimentEntityId exp) (histogramSourceKey view)
     es  <- readValueListEntities agent (experimentEntityId exp) (sourceEntityId src) runIndex
     rs  <-
       liftIO $
       fmap mconcat $
       forM es $ \e0 ->
       do e <- e0
          let items = dataEntityItem e
              vals  = mconcat $ map dataItemValue items
              name  = varEntityName var
              Just var = find (\x -> varEntityId x == dataEntityVarId e) (sourceEntityVarEntities src)
          if null (histogramSeries view [name])
            then return []
            else return [(name, vals)]
     let zs = histogramToBars . filterHistogram . build $ 
              map (filterData . snd) rs
         names = map fst rs
         p  = plotBars $
              bars $
              plot_bars_values .~ zs $
              plot_bars_titles .~ names $
              def
         updateAxes =
           if null zs
           then let v = AxisVisibility True False False
                in \l -> layout_top_axis_visibility .~ v $
                         layout_bottom_axis_visibility .~ v $
                         layout_left_axis_visibility .~ v $
                         layout_right_axis_visibility .~ v $
                         l
           else id
         chart = layout . updateAxes $
                 layout_title .~ runPlotTitle' $
                 layout_plots .~ [p] $
                 def
     renderChart (reportParameter renderer) (width, height) file (toRenderable chart)
     when (reportVerbose renderer) $
       putStr "Generated file " >> putStrLn file
     
-- | Remove the NaN and inifity values.     
filterData :: [Double] -> [Double]
filterData = filter (\x -> not $ isNaN x || isInfinite x)
     
-- | Remove the NaN and inifity values.     
filterHistogram :: [(Double, a)] -> [(Double, a)]
filterHistogram = filter (\(x, _) -> not $ isNaN x || isInfinite x)
  
-- | Convert a histogram to the bars.
histogramToBars :: [(Double, [Int])] -> [(Double, [Double])]
histogramToBars = map $ \(x, ns) -> (x, map fromIntegral ns)

-- | Get the HTML code.     
histogramHtml :: HistogramView -> FilePath -> [FilePath] -> Int -> HtmlWriter ()     
histogramHtml view path [file] index = histogramHtmlSingle view path [file] index
histogramHtml view path files index  = histogramHtmlMultiple view path files index
     
-- | Get the HTML code for a single run.
histogramHtmlSingle :: HistogramView -> FilePath -> [FilePath] -> Int -> HtmlWriter ()
histogramHtmlSingle view path [file] index =
  do header view index
     writeHtmlParagraph $
       writeHtmlImage (makeRelative path file)

-- | Get the HTML code for multiple runs.
histogramHtmlMultiple :: HistogramView -> FilePath -> [FilePath] -> Int -> HtmlWriter ()
histogramHtmlMultiple view path files index =
  do header view index
     forM_ files $ \files ->
       writeHtmlParagraph $
       writeHtmlImage (makeRelative path files)

header :: HistogramView -> Int -> HtmlWriter ()
header view index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (histogramTitle view)
     let description = histogramDescription view
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
histogramTOCHtml :: HistogramView -> Int -> HtmlWriter ()
histogramTOCHtml view index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (histogramTitle view)
