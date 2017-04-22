
{-# LANGUAGE FlexibleContexts #-}

import Database.HDBC
import Database.HDBC.Sqlite3

import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Entity.HDBC
import Simulation.Aivika.Experiment.Report
import Simulation.Aivika.Experiment.Report.Base
import Simulation.Aivika.Experiment.Report.Chart
import Simulation.Aivika.Experiment.Chart.Backend.Cairo

import Graphics.Rendering.Chart.Backend.Cairo

renderer =
  defaultWebReportRenderer { reportParameter = CairoRenderer PNG }

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
     finalHistogramSeries = filter (== "x") }]

main =
  do conn  <- connectSqlite3 "test.db"
     agent <- newExperimentAgent conn
     runReportParallel agent generators renderer
