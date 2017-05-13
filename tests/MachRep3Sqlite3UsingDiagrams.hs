
{-# LANGUAGE FlexibleContexts #-}

import Database.HDBC
import Database.HDBC.Sqlite3

import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Entity.HDBC
import Simulation.Aivika.Experiment.Report
import Simulation.Aivika.Experiment.Report.Base
import Simulation.Aivika.Experiment.Report.Chart
import Simulation.Aivika.Experiment.Chart.Backend.Diagrams

import Graphics.Rendering.Chart.Backend.Diagrams

renderer fonts =
  defaultWebReportRenderer { reportParameter = DiagramsRenderer SVG (return fonts) }

generators agent exp =
  return
  [reportView $ defaultExperimentSpecsView,
   reportView $ defaultDeviationChartView {
     deviationChartSourceKey = "deviation 1",
     deviationChartLeftYSeries = const [],
     deviationChartRightYSeries = filter (== "x") },
   reportView $ defaultFinalStatsView {
     finalStatsSourceKey = "final deviation 1",
     finalStatsSeries = filter (== "x") },
   reportView $ defaultTableView {
     tableSourceKey = "time series 1",
     tableSeries = filter (== "x") },
   reportView $ defaultFinalHistogramView {
     finalHistogramSourceKey = "multiple last value list 1",
     finalHistogramSeries = filter (== "x") },
   reportView $ defaultFinalTableView {
     finalTableSourceKey = "last value 1" },
   reportView $ defaultLastValueView {
     lastValueSourceKey = "last value 1",
     lastValueSeries = filter (== "x") },
   reportView $ defaultTimeSeriesView {
     timeSeriesSourceKey = "time series 1",
     timeSeriesLeftYSeries = filter (== "t"),
     timeSeriesRightYSeries = filter (== "x") },
   reportView $ defaultXYChartView {
     xyChartSourceKey = "time series 1",
     xyChartXSeries = filter (== "t"),
     xyChartLeftYSeries = filter (== "t"),
     xyChartRightYSeries = filter (== "x") },
   reportView $ defaultFinalXYChartView {
     finalXYChartSourceKey = "last value 1",
     finalXYChartXSeries = filter (== "n"),
     finalXYChartLeftYSeries = filter (== "t"),
     finalXYChartRightYSeries = filter (== "x") },
   reportView $ defaultHistogramView {
     histogramSourceKey = "time series 1",
     histogramSeries = filter (== "x") }]

main =
  do fonts <- loadCommonFonts
     conn  <- connectSqlite3 "test.db"
     agent <- newExperimentAgent conn
     runReportParallel agent generators (renderer fonts)
     disconnect conn
     