
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
   reportView $ defaultTimingStatsView {
     timingStatsSourceKey = "final time-dependent statistics 1" }]

main =
  do conn  <- connectSqlite3 "test.db"
     agent <- newExperimentAgent conn
     runReportParallel agent generators renderer
