
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Chart.FinalXYChartView
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'FinalXYChartView' that plots a single XY chart
-- in final time points for different simulation runs sequentially
-- by the run index.
--

module Simulation.Aivika.Experiment.Report.Chart.FinalXYChartView
       (FinalXYChartView(..), 
        defaultFinalXYChartView) where

import Control.Monad
import Control.Monad.Trans
import Control.Lens

import Data.List
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Either
import Data.Maybe
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

-- | Defines the 'View' that plots the XY chart
-- in final time points for different simulation runs
-- sequentially by the run index.
data FinalXYChartView =
  FinalXYChartView { finalXYChartTitle       :: String,
                     -- ^ This is a title used HTML.
                     finalXYChartDescription :: String,
                     -- ^ This is a description used in HTML.
                     finalXYChartWidth       :: Int,
                     -- ^ The width of the chart.
                     finalXYChartHeight      :: Int,
                     -- ^ The height of the chart.
                     finalXYChartFileName    :: ExperimentFilePath,
                     -- ^ It defines the file name with optional extension for each image to be saved.
                     -- It may include special variable @$TITLE@.
                     --
                     -- An example is
                     --
                     -- @
                     --   finalXYChartFileName = UniqueFilePath \"$TITLE\"
                     -- @
                     finalXYChartSourceKey   :: String,
                     -- ^ The source key.
                     finalXYChartXSeries     :: [String] -> [String],
                     -- ^ This is the X series.
                     --
                     -- You must define it, because it is 'mempty' 
                     -- by default. Also it must return exactly
                     -- one 'ResultExtract' item when calling
                     -- function 'extractDoubleResults' by the specified
                     -- result set.
                     finalXYChartLeftYSeries  :: [String] -> [String], 
                     -- ^ It defines the series to be plotted basing on the left Y axis.
                     finalXYChartRightYSeries :: [String] -> [String], 
                     -- ^ It defines the series to be plotted basing on the right Y axis.
                     finalXYChartPlotTitle   :: String,
                     -- ^ This is a title used in the chart. 
                     -- It may include special variable @$TITLE@.
                     --
                     -- An example is
                     --
                     -- @
                     --   finalXYChartPlotTitle = \"$TITLE\"
                     -- @
                     finalXYChartPlotLines :: [PlotLines Double Double ->
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
                     finalXYChartBottomAxis :: LayoutAxis Double ->
                                               LayoutAxis Double,
                     -- ^ A transformation of the bottom axis, 
                     -- after the X title is added.
                     finalXYChartLayout :: LayoutLR Double Double Double ->
                                           LayoutLR Double Double Double
                     -- ^ A transformation of the plot layout, 
                     -- where you can redefine the axes, for example.
                   }
  
-- | The default XY chart view.  
defaultFinalXYChartView :: FinalXYChartView
defaultFinalXYChartView = 
  FinalXYChartView { finalXYChartTitle       = "Final XY Chart",
                     finalXYChartDescription = "It shows the XY chart for the results in the final time points.",
                     finalXYChartWidth       = 640,
                     finalXYChartHeight      = 480,
                     finalXYChartFileName    = UniqueFilePath "FinalXYChart",
                     finalXYChartSourceKey   = "Must supply with the finalXYChartSourceKey value.",
                     finalXYChartXSeries     = mempty,
                     finalXYChartLeftYSeries  = mempty,
                     finalXYChartRightYSeries = mempty,
                     finalXYChartPlotTitle   = "$TITLE",
                     finalXYChartPlotLines   = colourisePlotLines,
                     finalXYChartBottomAxis  = id,
                     finalXYChartLayout      = id }

instance ChartRendering r => ReportView FinalXYChartView (WebReportRenderer r) where
  
  reportView view = 
    let newWriter agent exp renderer path =
          do file <- resolveFilePath path $
                     mapFilePath (flip replaceExtension $ renderableChartExtension $ reportParameter renderer) $
                     expandFilePath (finalXYChartFileName view) $
                     M.fromList [("$TITLE", finalXYChartTitle view)]
             liftIO $ writeFile file []  -- reserve the file name for efficiency
             return WebReportWriter { reportWriterInitialise = return (),
                                      reportWriterFinalise   = finaliseFinalXYChart view agent exp renderer file,
                                      reportWrite            = \runIndex -> return (),
                                      reportWriteTOCHtml     = finalXYChartTOCHtml view,
                                      reportWriteHtml        = finalXYChartHtml view path file
                                    }
    in WebReportGenerator { newReportWriter = newWriter }
       
-- | Plot the XY chart.
finaliseFinalXYChart :: ChartRendering r
                        => FinalXYChartView
                        -> ExperimentAgent
                        -> ExperimentEntity
                        -> WebReportRenderer r
                        -> FilePath
                        -> IO ()
finaliseFinalXYChart view agent exp renderer file =
  do let n = experimentEntityRunCount exp
         width   = finalXYChartWidth view
         height  = finalXYChartHeight view
         title   = finalXYChartTitle view
         plotTitle  = finalXYChartPlotTitle view
         plotLines  = finalXYChartPlotLines view
         plotBottomAxis = finalXYChartBottomAxis view
         plotLayout = finalXYChartLayout view
         plotTitle' = 
           replace "$TITLE" title
           plotTitle
     src <- requireSourceEntityByKey agent (experimentEntityId exp) (finalXYChartSourceKey view)
     let alterInner runIndex val Nothing   = Just [(runIndex, val)]
         alterInner runIndex val (Just xs) = Just $ (runIndex, val) : xs
         reduceInner runIndex (xs, ys1, ys2) e =
           let items = dataEntityItem e
               val   = dataItemValue items
               name  = varEntityName var
               Just var = find (\x -> varEntityId x == dataEntityVarId e) (sourceEntityVarEntities src)
               isX      = not $ null $ finalXYChartXSeries view [name]
               isLeftY  = not $ null $ finalXYChartLeftYSeries view [name]
               isRightY = not $ null $ finalXYChartRightYSeries view [name]
               fx  | isX       = M.alter (alterInner runIndex val) name
                   | otherwise = id
               fy1 | isLeftY   = M.alter (alterInner runIndex val) name
                   | otherwise = id
               fy2 | not isLeftY && isRightY = M.alter (alterInner runIndex val) name
                   | otherwise = id
           in (fx xs, fy1 ys1, fy2 ys2) 
         reduceOuter maps@(xs, ys1, ys2) runIndex =
           do es <- readLastValueEntities agent (experimentEntityId exp) (sourceEntityId src) runIndex
              return $ foldl (reduceInner runIndex) maps es
     (xs, ys1, ys2) <-
       foldM reduceOuter (M.empty, M.empty, M.empty) [1..n]
     let xp =
           case (M.keys xs) of
             [name] -> (name, A.array (1, n) $ fromJust (M.lookup name xs))
             _      -> error "Expected to see a single X series: finaliseFinalXYChart"
         yps1 = map Left $ M.toList $ flip fmap ys1 $ \ys -> A.array (1, n) ys
         yps2 = map Right $ M.toList $ flip fmap ys2 $ \ys -> A.array (1, n) ys
         yps  = yps1 ++ yps2
         ps   =
           flip map (zip yps plotLines) $ \(eitherYp, plotLines) ->
           do let yp = either id id eitherYp
                  xs = A.elems $ snd xp
                  ys = A.elems $ snd yp
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
         chart = plotLayout .
                 renderingLayoutLR (reportParameter renderer) .
                 updateLeftAxis . updateRightAxis $
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
  divideBy (\(x, y) -> isNaN x || isInfinite x || isNaN y || isInfinite y)

-- | Get the HTML code.     
finalXYChartHtml :: FinalXYChartView -> FilePath -> FilePath -> Int -> HtmlWriter ()
finalXYChartHtml view path file index =
  do header view index
     writeHtmlParagraph $
       writeHtmlImage (makeRelative path file)

header :: FinalXYChartView -> Int -> HtmlWriter ()
header view index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (finalXYChartTitle view)
     let description = finalXYChartDescription view
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
finalXYChartTOCHtml :: FinalXYChartView -> Int -> HtmlWriter ()
finalXYChartTOCHtml view index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (finalXYChartTitle view)
