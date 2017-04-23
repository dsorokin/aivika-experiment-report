
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Chart.TimeSeriesView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'TimeSeriesView' that plots the time series charts.
--

module Simulation.Aivika.Experiment.Report.Chart.TimeSeriesView
       (TimeSeriesView(..), 
        defaultTimeSeriesView) where

import Control.Monad
import Control.Monad.Trans
import Control.Lens

import Data.List
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Either
import Data.Default.Class

import System.FilePath

import Graphics.Rendering.Chart

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Base.HtmlWriter
import Simulation.Aivika.Experiment.Base.ExperimentWriter
import Simulation.Aivika.Experiment.Chart.Types
import Simulation.Aivika.Experiment.Chart.Utils (colourisePlotLines)
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Report.Types
import Simulation.Aivika.Experiment.Report.Base.WebReportRenderer

-- | Defines the 'View' that plots the time series charts.
data TimeSeriesView =
  TimeSeriesView { timeSeriesTitle       :: String,
                   -- ^ This is a title used in HTML.
                   timeSeriesDescription :: String,
                   -- ^ This is a description used in HTML.
                   timeSeriesWidth       :: Int,
                   -- ^ The width of the chart.
                   timeSeriesHeight      :: Int,
                   -- ^ The height of the chart.
                   timeSeriesFileName    :: ExperimentFilePath,
                   -- ^ It defines the file name with optional extension for each image to be saved.
                   -- It may include special variables @$TITLE@, @$RUN_INDEX@ and @$RUN_COUNT@.
                   --
                   -- An example is
                   --
                   -- @
                   --   timeSeriesFileName = UniqueFilePath \"$TITLE - $RUN_INDEX\"
                   -- @
                   timeSeriesSourceKey    :: String,
                   -- ^ The source key.
                   timeSeriesLeftYSeries  :: [String] -> [String], 
                   -- ^ It defines the series plotted basing on the left Y axis.
                   timeSeriesRightYSeries :: [String] -> [String], 
                   -- ^ It defines the series plotted basing on the right Y axis.
                   timeSeriesPlotTitle    :: String,
                   -- ^ This is a title used in the chart when
                   -- simulating a single run. It may include 
                   -- special variable @$TITLE@.
                   --
                   -- An example is
                   --
                   -- @
                   --   timeSeriesPlotTitle = \"$TITLE\"
                   -- @
                   timeSeriesRunPlotTitle :: String,
                   -- ^ The run title for the chart. It is used 
                   -- when simulating multiple runs and it may 
                   -- include special variables @$RUN_INDEX@, 
                   -- @$RUN_COUNT@ and @$PLOT_TITLE@.
                   --
                   -- An example is 
                   --
                   -- @
                   --   timeSeriesRunPlotTitle = \"$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT\"
                   -- @
                   timeSeriesPlotLines :: [PlotLines Double Double ->
                                           PlotLines Double Double],
                   -- ^ Probably, an infinite sequence of plot 
                   -- transformations based on which the plot
                   -- is constructed for each series. Generally,
                   -- it must not coincide with a sequence of 
                   -- labels as one label may denote a whole list 
                   -- or an array of data providers.
                   --
                   -- Here you can define a colour or style of
                   -- the plot lines.
                   timeSeriesBottomAxis :: LayoutAxis Double ->
                                           LayoutAxis Double,
                   -- ^ A transformation of the bottom axis, 
                   -- after title @time@ is added.
                   timeSeriesLayout :: LayoutLR Double Double Double ->
                                       LayoutLR Double Double Double
                   -- ^ A transformation of the plot layout, 
                   -- where you can redefine the axes, for example.
                 }
  
-- | The default time series view.  
defaultTimeSeriesView :: TimeSeriesView
defaultTimeSeriesView = 
  TimeSeriesView { timeSeriesTitle       = "Time Series",
                   timeSeriesDescription = "It shows the Time Series chart(s).",
                   timeSeriesWidth       = 640,
                   timeSeriesHeight      = 480,
                   timeSeriesFileName    = UniqueFilePath "TimeSeries($RUN_INDEX)",
                   timeSeriesSourceKey   = "Must supply with the timeSeriesSourceKey value.",
                   timeSeriesLeftYSeries  = const mempty,
                   timeSeriesRightYSeries = const mempty,
                   timeSeriesPlotTitle    = "$TITLE",
                   timeSeriesRunPlotTitle = "$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT",
                   timeSeriesPlotLines   = colourisePlotLines,
                   timeSeriesBottomAxis  = id,
                   timeSeriesLayout      = id }

instance ChartRendering r => ReportView TimeSeriesView (WebReportRenderer r) where
  
  reportView view = 
    let newWriter agent exp renderer path =
          do let n = experimentEntityRunCount exp
             files <- forM [1..n] $ \i ->
               resolveFilePath path $
               mapFilePath (flip replaceExtension $ renderableChartExtension $ reportParameter renderer) $
               expandFilePath (timeSeriesFileName view) $
               M.fromList [("$TITLE", timeSeriesTitle view),
                           ("$RUN_INDEX", show i),
                           ("$RUN_COUNT", show n)]
             liftIO $ forM_ files $ flip writeFile []  -- reserve the file names
             let arr = A.array (1, n) $ zip [1..n] files
             return WebReportWriter { reportWriterInitialise = return (),
                                      reportWriterFinalise   = return (),
                                      reportWrite            = \runIndex -> writeTimeSeries view agent exp renderer (arr A.! runIndex) runIndex,
                                      reportWriteTOCHtml     = timeSeriesTOCHtml view,
                                      reportWriteHtml        = timeSeriesHtml view path files
                                    }
    in WebReportGenerator { newReportWriter = newWriter }
       
-- | Plot the time series chart.
writeTimeSeries :: ChartRendering r
                   => TimeSeriesView
                   -> ExperimentAgent
                   -> ExperimentEntity
                   -> WebReportRenderer r
                   -> FilePath
                   -> Int
                   -> IO ()
writeTimeSeries view agent exp renderer file runIndex =
  do let n = experimentEntityRunCount exp
         width   = timeSeriesWidth view
         height  = timeSeriesHeight view
         title   = timeSeriesTitle view
         plotTitle  = timeSeriesPlotTitle view
         runPlotTitle = timeSeriesRunPlotTitle view
         plotLines  = timeSeriesPlotLines view
         plotBottomAxis = timeSeriesBottomAxis view
         plotLayout = timeSeriesLayout view
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
     src <- requireSourceEntityByKey agent (experimentEntityId exp) (timeSeriesSourceKey view)
     es <- readTimeSeriesEntities agent (experimentEntityId exp) (sourceEntityId src) runIndex
     ps <-
       liftIO $
       fmap mconcat $
       forM (zip es plotLines) $ \(e0, plotLines) ->
       do e <- e0
          let items = dataEntityItem e
              times = map dataItemTime items
              vals  = map dataItemValue items
              name  = varEntityName var
              Just var = find (\x -> varEntityId x == dataEntityVarId e) (sourceEntityVarEntities src)
              isLeft  = not $ null $ timeSeriesLeftYSeries view [name]
              isRight = not $ null $ timeSeriesRightYSeries view [name]
              p = toPlot $
                  plotLines $
                  plot_lines_values .~ filterPlotLinesValues (zip times vals) $
                  plot_lines_title .~ name $
                  def
          if isLeft
            then return [Left p]
            else if isRight
                 then return [Right p]
                 else return []
     let ps1 = lefts ps
         ps2 = rights ps
         axis = plotBottomAxis $
                laxis_title .~ "time" $
                def
         updateLeftAxis =
           if null ps1
           then layoutlr_left_axis_visibility .~ AxisVisibility False False False
           else id
         updateRightAxis =
           if null ps2
           then layoutlr_right_axis_visibility .~ AxisVisibility False False False
           else id
         chart = plotLayout . updateLeftAxis . updateRightAxis $
                 layoutlr_x_axis .~ axis $
                 layoutlr_title .~ runPlotTitle' $
                 layoutlr_plots .~ ps $
                 def
     renderChart (reportParameter renderer) (width, height) file (toRenderable chart)
     when (reportVerbose renderer) $
       putStr "Generated file " >> putStrLn file
    
-- | Remove the NaN and inifity values.     
filterPlotLinesValues :: [(Double, Double)] -> [[(Double, Double)]]
filterPlotLinesValues = 
  filter (not . null) .
  divideBy (\(t, x) -> isNaN x || isInfinite x)

-- | Get the HTML code.     
timeSeriesHtml :: TimeSeriesView -> FilePath -> [FilePath] -> Int -> HtmlWriter ()
timeSeriesHtml view path [file] index = timeSeriesHtmlSingle view path [file] index
timeSeriesHtml view path files index  = timeSeriesHtmlMultiple view path files index
     
-- | Get the HTML code for a single run.
timeSeriesHtmlSingle :: TimeSeriesView -> FilePath -> [FilePath] -> Int -> HtmlWriter ()
timeSeriesHtmlSingle view path [file] index =
  do header view index
     writeHtmlParagraph $
       writeHtmlImage (makeRelative path file)

-- | Get the HTML code for multiple runs.
timeSeriesHtmlMultiple :: TimeSeriesView -> FilePath -> [FilePath] -> Int -> HtmlWriter ()
timeSeriesHtmlMultiple view path files index =
  do header view index
     forM_ files $ \file ->
       writeHtmlParagraph $
       writeHtmlImage (makeRelative path file)

header :: TimeSeriesView -> Int -> HtmlWriter ()
header view index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (timeSeriesTitle view)
     let description = timeSeriesDescription view
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
timeSeriesTOCHtml :: TimeSeriesView -> Int -> HtmlWriter ()
timeSeriesTOCHtml view index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (timeSeriesTitle view)
