
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Chart.FinalHistogramView
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines 'FinalHistogramView' that plots a histogram
-- by the specified series in final time points collected from different 
-- simulation runs.
--

module Simulation.Aivika.Experiment.Report.Chart.FinalHistogramView
       (FinalHistogramView(..), 
        defaultFinalHistogramView) where

import Control.Monad
import Control.Monad.Trans
import Control.Lens

import Data.List
import qualified Data.Map as M
import Data.Either
import Data.Monoid
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

-- | Defines the 'View' that plots the histogram
-- for the specified series in final time points
-- collected from different simulation runs.
data FinalHistogramView =
  FinalHistogramView { finalHistogramTitle       :: String,
                       -- ^ This is a title used in HTML.
                       finalHistogramDescription :: String,
                       -- ^ This is a description used in HTML.
                       finalHistogramWidth       :: Int,
                       -- ^ The width of the histogram.
                       finalHistogramHeight      :: Int,
                       -- ^ The height of the histogram.
                       finalHistogramFileName    :: ExperimentFilePath,
                       -- ^ It defines the file name with optional extension for each image to be saved.
                       -- It may include special variable @$TITLE@.
                       --
                       -- An example is
                       --
                       -- @
                       --   finalHistogramFileName = UniqueFilePath \"$TITLE\"
                       -- @
                       finalHistogramBuild       :: [[Double]] -> Histogram, 
                       -- ^ Builds a histogram by the specified list of 
                       -- data series.
                       finalHistogramSourceKey   :: String,
                       -- ^ The source key.
                       finalHistogramSeries      :: [String] -> [String], 
                       -- ^ It defines the series to be plotted on the histogram.
                       finalHistogramPlotTitle   :: String,
                       -- ^ This is a title used in the histogram. 
                       -- It may include special variable @$TITLE@.
                       --
                       -- An example is
                       --
                       -- @
                       --   finalHistogramPlotTitle = \"$TITLE\"
                       -- @
                       finalHistogramPlotBars :: PlotBars Double Double ->
                                                 PlotBars Double Double,
                       -- ^ A transformation based on which the plot bar
                       -- is constructed for the series. 
                       --
                       -- Here you can define a colour or style of
                       -- the plot bars.
                       finalHistogramLayout :: Layout Double Double ->
                                               Layout Double Double
                       -- ^ A transformation of the plot layout, 
                       -- where you can redefine the axes, for example.
                 }
  
-- | The default histogram view.  
defaultFinalHistogramView :: FinalHistogramView
defaultFinalHistogramView = 
  FinalHistogramView { finalHistogramTitle       = "Final Histogram",
                       finalHistogramDescription = "It shows a histogram by data gathered in the final time points.",
                       finalHistogramWidth       = 640,
                       finalHistogramHeight      = 480,
                       finalHistogramFileName    = UniqueFilePath "FinalHistogram",
                       finalHistogramBuild       = histogram binSturges,
                       finalHistogramSourceKey   = "Must supply with the finalHistogramSourceKey value.",
                       finalHistogramSeries      = id, 
                       finalHistogramPlotTitle   = "$TITLE",
                       finalHistogramPlotBars    = colourisePlotBars,
                       finalHistogramLayout      = id }

instance ChartRendering r => ReportView FinalHistogramView (WebReportRenderer r) where
  
  reportView view = 
    let newWriter agent exp renderer path =
          do file <- resolveFilePath path $
                     mapFilePath (flip replaceExtension $ renderableChartExtension $ reportParameter renderer) $
                     expandFilePath (finalHistogramFileName view) $
                     M.fromList [("$TITLE", finalHistogramTitle view)]
             return WebReportWriter { reportWriterInitialise = return (),
                                      reportWriterFinalise   = finaliseFinalHistogram view agent exp renderer file,
                                      reportWrite            = \runIndex -> return (),
                                      reportWriteTOCHtml     = finalHistogramTOCHtml view,
                                      reportWriteHtml        = finalHistogramHtml view path file
                                    }
    in WebReportGenerator { newReportWriter = newWriter }

-- | Plot the histogram.
finaliseFinalHistogram :: ChartRendering r
                          => FinalHistogramView
                          -> ExperimentAgent
                          -> ExperimentEntity
                          -> WebReportRenderer r
                          -> FilePath
                          -> IO ()
finaliseFinalHistogram view agent exp renderer file =
  do let title = finalHistogramTitle view
         plotTitle = finalHistogramPlotTitle view
         plotTitle' = 
           replace "$TITLE" title
           plotTitle
         width = finalHistogramWidth view
         height = finalHistogramHeight view
         histogram = finalHistogramBuild view
         bars = finalHistogramPlotBars view
         layout = finalHistogramLayout view
     src <- requireSourceEntityByKey agent (experimentEntityId exp) (finalHistogramSourceKey view)
     es <- readMultipleLastValueListEntities agent (experimentEntityId exp) (sourceEntityId src)
     vs <-
       fmap mconcat $
       forM es $ \e0 ->
       do e <- e0
          let item = multipleDataEntityItem e
              vals = dataItemValue item
              name = varEntityName var
              Just var = find (\x -> varEntityId x == multipleDataEntityVarId e) (sourceEntityVarEntities src)
          if null (finalHistogramSeries view [name])
            then return []
            else return [(name, vals)]
     let names = map fst vs
         xs = map snd vs
         zs = histogramToBars . filterHistogram . histogram $ 
              map filterData xs
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
                 layout_title .~ plotTitle' $
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
finalHistogramHtml :: FinalHistogramView -> FilePath -> FilePath -> Int -> HtmlWriter ()
finalHistogramHtml view path file index =
  do header view index
     writeHtmlParagraph $
       writeHtmlImage (makeRelative path file)

header :: FinalHistogramView -> Int -> HtmlWriter ()
header view index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (finalHistogramTitle view)
     let description = finalHistogramDescription view
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
finalHistogramTOCHtml :: FinalHistogramView -> Int -> HtmlWriter ()
finalHistogramTOCHtml view index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (finalHistogramTitle view)
