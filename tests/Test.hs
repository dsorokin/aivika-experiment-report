
import Database.HDBC
import Database.HDBC.Sqlite3

import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Entity.HDBC
import Simulation.Aivika.Experiment.Report
import Simulation.Aivika.Experiment.Report.Base

renderer = defaultWebReportRenderer

generators agent exp src = return Nothing

main =
  do conn  <- connectSqlite3 "test.db"
     agent <- newExperimentAgent conn
     runReportParallel agent generators renderer
