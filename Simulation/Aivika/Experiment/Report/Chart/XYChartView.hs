
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Chart.XYChartView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'XYChartView' that plots the XY charts.
--

module Simulation.Aivika.Experiment.Report.Chart.XYChartView
       (XYChartView(..), 
        defaultXYChartView) where

import Control.Monad
import Control.Monad.Trans
import Control.Lens

import Data.List
import qualified Data.Array as A
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
import Simulation.Aivika.Experiment.Chart.Utils (colourisePlotLines)
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Report.Types
import Simulation.Aivika.Experiment.Report.Base.WebReportRenderer

-- | Defines the 'View' that plots the XY charts.
data XYChartView =
  XYChartView { xyChartTitle       :: String,
                -- ^ This is a title used in HTML.
                xyChartDescription :: String,
                -- ^ This is a description used in HTML.
                xyChartWidth       :: Int,
                -- ^ The width of the chart.
                xyChartHeight      :: Int,
                -- ^ The height of the chart.
                xyChartFileName    :: ExperimentFilePath,
                -- ^ It defines the file name with optional extension for each image to be saved.
                -- It may include special variables @$TITLE@, @$RUN_INDEX@ and @$RUN_COUNT@.
                --
                -- An example is
                --
                -- @
                --   xyChartFileName = UniqueFilePath \"$TITLE - $RUN_INDEX\"
                -- @
                xyChartSourceKey   :: String,
                -- ^ The source key.
                xyChartXSeries     :: [String] -> [String],
                -- ^ This is the X series.
                --
                -- You must define it, because it is 'mempty' 
                -- by default. Also it must return exactly
                -- one 'ResultExtract' item when calling
                -- function 'extractDoubleResults' by the specified
                -- result set.
                xyChartLeftYSeries  :: [String] -> [String], 
                -- ^ It defines the series plotted basing on the left Y axis.
                xyChartRightYSeries :: [String] -> [String], 
                -- ^ It defines the series plotted basing on the right Y axis.
                xyChartPlotTitle   :: String,
                -- ^ This is a title used in the chart when
                -- simulating a single run. It may include 
                -- special variable @$TITLE@.
                --
                -- An example is
                --
                -- @
                --   xyChartPlotTitle = \"$TITLE\"
                -- @
                xyChartRunPlotTitle :: String,
                -- ^ The run title for the chart. It is used 
                -- when simulating multiple runs and it may 
                -- include special variables @$RUN_INDEX@, 
                -- @$RUN_COUNT@ and @$PLOT_TITLE@.
                --
                -- An example is 
                --
                -- @
                --   xyChartRunPlotTitle = \"$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT\"
                -- @
                xyChartPlotLines :: [PlotLines Double Double ->
                                     PlotLines Double Double],
                -- ^ Probably, an infinite sequence of plot 
                -- transformations based on which the plot
                -- is constructed for each Y series. Generally,
                -- it may not coincide with a sequence of 
                -- Y labels as one label may denote a whole list 
                -- or an array of data providers.
                --
                -- Here you can define a colour or style of
                -- the plot lines.
                xyChartBottomAxis :: LayoutAxis Double ->
                                     LayoutAxis Double,
                -- ^ A transformation of the bottom axis, 
                -- after the X title is added.
                xyChartLayout :: LayoutLR Double Double Double ->
                                 LayoutLR Double Double Double
                -- ^ A transformation of the plot layout, 
                -- where you can redefine the axes, for example.
              }
  
-- | The default time series view.  
defaultXYChartView :: XYChartView
defaultXYChartView = 
  XYChartView { xyChartTitle       = "XY Chart",
                xyChartDescription = "It shows the XY chart(s).",
                xyChartWidth       = 640,
                xyChartHeight      = 480,
                xyChartFileName    = UniqueFilePath "XYChart($RUN_INDEX)",
                xyChartSourceKey   = "Must supply with the xyChartSourceKey value.",
                xyChartXSeries      = mempty, 
                xyChartLeftYSeries  = mempty,
                xyChartRightYSeries = mempty,
                xyChartPlotTitle   = "$TITLE",
                xyChartRunPlotTitle  = "$PLOT_TITLE / Run $RUN_INDEX of $RUN_COUNT",
                xyChartPlotLines   = colourisePlotLines,
                xyChartBottomAxis  = id,
                xyChartLayout      = id }

instance ChartRendering r => ReportView XYChartView (WebReportRenderer r) where
  
  reportView view = 
    let newWriter agent exp renderer path =
          do let n = experimentEntityRunCount exp
             files <- forM [1..n] $ \i ->
               resolveFilePath path $
               mapFilePath (flip replaceExtension $ renderableChartExtension $ reportParameter renderer) $
               expandFilePath (xyChartFileName view) $
               M.fromList [("$TITLE", xyChartTitle view),
                           ("$RUN_INDEX", show i),
                           ("$RUN_COUNT", show n)]
             liftIO $ forM_ files $ flip writeFile []  -- reserve the file names
             let arr = A.array (1, n) $ zip [1..n] files
             return WebReportWriter { reportWriterInitialise = return (),
                                      reportWriterFinalise   = return (),
                                      reportWrite            = \runIndex -> writeXYChart view agent exp renderer (arr A.! runIndex) runIndex,
                                      reportWriteTOCHtml     = xyChartTOCHtml view,
                                      reportWriteHtml        = xyChartHtml view path files
                                    }
    in WebReportGenerator { newReportWriter = newWriter }
       
-- | Plot the XY chart.
writeXYChart :: ChartRendering r
                => XYChartView
                -> ExperimentAgent
                -> ExperimentEntity
                -> WebReportRenderer r
                -> FilePath
                -> Int
                -> IO ()
writeXYChart view agent exp renderer file runIndex =
  do let n = experimentEntityRunCount exp
         width   = xyChartWidth view
         height  = xyChartHeight view
         title   = xyChartTitle view
         plotTitle  = xyChartPlotTitle view
         runPlotTitle = xyChartRunPlotTitle view
         plotLines  = xyChartPlotLines view
         plotBottomAxis = xyChartBottomAxis view
         plotLayout = xyChartLayout view
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
     src <- requireSourceEntityByKey agent (experimentEntityId exp) (xyChartSourceKey view)
     es  <- readTimeSeriesEntities agent (experimentEntityId exp) (sourceEntityId src) runIndex
     rs  <-
       liftIO $
       fmap mconcat $
       forM (zip es plotLines) $ \(e0, plotLines) ->
       do e <- e0
          let items = dataEntityItem e
              times = map dataItemTime items
              vals  = map dataItemValue items
              name  = varEntityName var
              Just var = find (\x -> varEntityId x == dataEntityVarId e) (sourceEntityVarEntities src)
              isX      = not $ null $ xyChartXSeries view [name]
              isLeftY  = not $ null $ xyChartLeftYSeries view [name]
              isRightY = not $ null $ xyChartRightYSeries view [name]
              r1 | isX       = [Left (name, vals)]
                 | otherwise = []
              r2 | isLeftY   = [Right $ Left (name, vals)]
                 | isRightY  = [Right $ Right (name, vals)]
                 | otherwise = []
          return $ r1 ++ r2
     let xp =
           case (lefts rs) of
             [r] -> r
             _   -> error "Expected to see a single X series: writeXYChart"
         ps =
           flip map (zip (rights rs) plotLines) $ \(eitherYp, plotLines) ->
           do let yp = either id id eitherYp
                  xs = snd xp
                  ys = snd yp
                  name = fst yp
                  p =
                    toPlot $
                    plotLines $
                    plot_lines_values .~ filterPlotLinesValues (zip xs ys) $
                    plot_lines_title .~ name $
                    def
              case eitherYp of
                Left yp  -> Left p
                Right yp -> Right p
         ps1 = lefts ps
         ps2 = rights ps
         axis = plotBottomAxis $
                laxis_title .~ (fst xp) $
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
  divideBy (\(x, y) -> isNaN x || isInfinite x || isNaN y || isInfinite y)

-- | Get the HTML code.     
xyChartHtml :: XYChartView -> FilePath -> [FilePath] -> Int -> HtmlWriter ()
xyChartHtml view path [file] index = xyChartHtmlSingle view path [file] index
xyChartHtml view path files index  = xyChartHtmlMultiple view path files index
     
-- | Get the HTML code for a single run.
xyChartHtmlSingle :: XYChartView -> FilePath -> [FilePath] -> Int -> HtmlWriter ()
xyChartHtmlSingle view path [file] index =
  do header view index
     writeHtmlParagraph $
       writeHtmlImage (makeRelative path file)

-- | Get the HTML code for multiple runs.
xyChartHtmlMultiple :: XYChartView -> FilePath -> [FilePath] -> Int -> HtmlWriter ()
xyChartHtmlMultiple view path files index =
  do header view index
     forM_ files $ \file ->
       writeHtmlParagraph $
       writeHtmlImage (makeRelative path file)

header :: XYChartView -> Int -> HtmlWriter ()
header view index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (xyChartTitle view)
     let description = xyChartDescription view
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
xyChartTOCHtml :: XYChartView -> Int -> HtmlWriter ()
xyChartTOCHtml view index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (xyChartTitle view)
