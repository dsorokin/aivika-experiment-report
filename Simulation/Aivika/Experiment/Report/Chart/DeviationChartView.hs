
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Chart.DeviationChartView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'DeviationChartView' that plots the deviation chart by using rule 3-sigma.
--

module Simulation.Aivika.Experiment.Report.Chart.DeviationChartView
       (DeviationChartView(..), 
        defaultDeviationChartView) where

import Control.Monad
import Control.Monad.Trans
import Control.Lens

import Data.List
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
import Simulation.Aivika.Experiment.Chart.Utils (colourisePlotLines, colourisePlotFillBetween)
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Report.Types
import Simulation.Aivika.Experiment.Report.Base.WebReportRenderer

-- | Defines the 'View' that plots the deviation chart.
data DeviationChartView =
  DeviationChartView { deviationChartTitle       :: String,
                       -- ^ This is a title used in HTML.
                       deviationChartDescription :: String,
                       -- ^ This is a description used in HTML.
                       deviationChartWidth       :: Int,
                       -- ^ The width of the chart.
                       deviationChartHeight      :: Int,
                       -- ^ The height of the chart.
                       deviationChartFileName    :: ExperimentFilePath,
                       -- ^ It defines the file name with optional extension for each image to be saved.
                       -- It may include special variable @$TITLE@.
                       --
                       -- An example is
                       --
                       -- @
                       --   deviationChartFileName = UniqueFilePath \"$TITLE\"
                       -- @
                       deviationChartSourceKey    :: String,
                       -- ^ The source key.
                       deviationChartLeftYSeries  :: [String] -> [String], 
                       -- ^ It defines the series to be plotted basing on the left Y axis.
                       deviationChartRightYSeries :: [String] -> [String], 
                       -- ^ It defines the series to be plotted basing on the right Y axis.
                       deviationChartPlotTitle :: String,
                       -- ^ This is a title used in the chart. 
                       -- It may include special variable @$TITLE@.
                       --
                       -- An example is
                       --
                       -- @
                       --   deviationChartPlotTitle = \"$TITLE\"
                       -- @
                       deviationChartPlotLines :: [PlotLines Double Double ->
                                                   PlotLines Double Double],
                       -- ^ Probably, an infinite sequence of plot 
                       -- transformations based on which the plot
                       -- is constructed for each series. Generally,
                       -- it may not coincide with a sequence of 
                       -- labels as one label may denote a whole list 
                       -- or an array of data providers.
                       --
                       -- Here you can define a colour or style of
                       -- the plot lines.
                       deviationChartPlotFillBetween :: [PlotFillBetween Double Double ->
                                                         PlotFillBetween Double Double],
                       -- ^ Corresponds exactly to 'deviationChartPlotLines'
                       -- but used for plotting the deviation areas
                       -- by the rule of 3-sigma, while the former 
                       -- is used for plotting the trends of the 
                       -- random processes.
                       deviationChartBottomAxis :: LayoutAxis Double ->
                                                   LayoutAxis Double,
                       -- ^ A transformation of the bottom axis, 
                       -- after title @time@ is added.
                       deviationChartLayout :: LayoutLR Double Double Double ->
                                               LayoutLR Double Double Double
                       -- ^ A transformation of the plot layout, 
                       -- where you can redefine the axes, for example.
                 }
  
-- | The default deviation chart view.  
defaultDeviationChartView :: DeviationChartView
defaultDeviationChartView = 
  DeviationChartView { deviationChartTitle       = "Deviation Chart",
                       deviationChartDescription = "It shows the Deviation chart by rule 3-sigma.",
                       deviationChartWidth       = 640,
                       deviationChartHeight      = 480,
                       deviationChartFileName    = UniqueFilePath "DeviationChart",
                       deviationChartSourceKey   = "Must supply with the deviationChartSourceKey value.",
                       deviationChartLeftYSeries  = const [], 
                       deviationChartRightYSeries = id, 
                       deviationChartPlotTitle   = "$TITLE",
                       deviationChartPlotLines   = colourisePlotLines,
                       deviationChartPlotFillBetween = colourisePlotFillBetween,
                       deviationChartBottomAxis  = id,
                       deviationChartLayout      = id }

instance ChartRendering r => ReportView DeviationChartView (WebReportRenderer r) where
  
  reportView view = 
    let newWriter agent exp renderer path =
          do file <- resolveFilePath path $
                     mapFilePath (flip replaceExtension $ renderableChartExtension $ reportParameter renderer) $
                     expandFilePath (deviationChartFileName view) $
                     M.fromList [("$TITLE", deviationChartTitle view)]
             return WebReportWriter { reportWriterInitialise = return (),
                                      reportWriterFinalise   = finaliseDeviationChart view agent exp renderer file,
                                      reportWrite            = \runIndex -> return (),
                                      reportWriteTOCHtml     = deviationChartTOCHtml view,
                                      reportWriteHtml        = deviationChartHtml view path file
                                    }
    in WebReportGenerator { newReportWriter = newWriter }

-- | Plot the deviation chart.
finaliseDeviationChart :: ChartRendering r
                          => DeviationChartView
                          -> ExperimentAgent
                          -> ExperimentEntity
                          -> WebReportRenderer r
                          -> FilePath
                          -> IO ()
finaliseDeviationChart view agent exp renderer file =
  do let title = deviationChartTitle view
         plotTitle = deviationChartPlotTitle view
         plotTitle' = 
           replace "$TITLE" title
           plotTitle
         width = deviationChartWidth view
         height = deviationChartHeight view
         plotLines = deviationChartPlotLines view
         plotFillBetween = deviationChartPlotFillBetween view
         plotBottomAxis = deviationChartBottomAxis view
         plotLayout = deviationChartLayout view
     src <- requireSourceEntityByKey agent (experimentEntityId exp) (deviationChartSourceKey view)
     es <- readDeviationEntities agent (experimentEntityId exp) (sourceEntityId src)
     ps <-
       liftIO $
       fmap mconcat $
       forM (zip3 es plotLines plotFillBetween) $ \(e0, plotLines, plotFillBetween) ->
       do e <- e0
          let items = multipleDataEntityItem e
              times = map dataItemTime items
              stats = map dataItemValue items
              name  = varEntityName var
              Just var = find (\x -> varEntityId x == multipleDataEntityVarId e) (sourceEntityVarEntities src)
              isLeft  = not $ null $ deviationChartLeftYSeries view [name]
              isRight = not $ null $ deviationChartRightYSeries view [name]
              p1 = let zs = flip map (zip times stats) $ \(t, stats) ->
                         (t, samplingStatsMean stats)
                    in toPlot $
                       plotLines $
                       plot_lines_values .~ filterPlotLinesValues zs $
                       plot_lines_title .~ name $
                       def
              p2 = let zs = flip map (zip times stats) $ \(t, stats) ->
                         let mu    = samplingStatsMean stats
                             sigma = samplingStatsDeviation stats
                         in (t, (mu - 3 * sigma, mu + 3 * sigma))
                   in toPlot $
                      plotFillBetween $
                      plot_fillbetween_values .~ filterPlotFillBetweenValues zs $
                      plot_fillbetween_title .~ name $
                      def
          if isLeft
            then return [Left p1, Left p2]
            else if isRight
                 then return [Right p1, Right p2]
                 else return []
     let axis = plotBottomAxis $
                laxis_title .~ "time" $
                def
         updateLeftAxis =
           if null $ lefts ps
           then layoutlr_left_axis_visibility .~ AxisVisibility False False False
           else id
         updateRightAxis =
           if null $ rights ps
           then layoutlr_right_axis_visibility .~ AxisVisibility False False False
           else id
         chart = plotLayout . updateLeftAxis . updateRightAxis $
                 layoutlr_x_axis .~ axis $
                 layoutlr_title .~ plotTitle' $
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

-- | Remove the NaN and inifity values.     
filterPlotFillBetweenValues :: [(Double, (Double, Double))] -> [(Double, (Double, Double))]
filterPlotFillBetweenValues = 
  filter $ \(t, (x1, x2)) -> not $ isNaN x1 || isInfinite x1 || isNaN x2 || isInfinite x2

-- | Get the HTML code.     
deviationChartHtml :: DeviationChartView -> FilePath -> FilePath -> Int -> HtmlWriter ()
deviationChartHtml view path file index =
  do header view index
     writeHtmlParagraph $
       writeHtmlImage (makeRelative path file)

-- | Get the header.
header :: DeviationChartView -> Int -> HtmlWriter ()
header view index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (deviationChartTitle view)
     let description = deviationChartDescription view
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
deviationChartTOCHtml :: DeviationChartView -> Int -> HtmlWriter ()
deviationChartTOCHtml view index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (deviationChartTitle view)
