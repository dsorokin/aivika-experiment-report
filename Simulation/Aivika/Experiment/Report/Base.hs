
-- |
-- Module     : Simulation.Aivika.Report.Base
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- This module re-exports some part of the library functionality.
--

module Simulation.Aivika.Experiment.Report.Base
       (-- * Modules
        module Simulation.Aivika.Experiment.Report.Base.ExperimentSpecsView,
        module Simulation.Aivika.Experiment.Report.Base.ExperimentSpecsWriter,
        module Simulation.Aivika.Experiment.Report.Base.FinalStatsView,
        module Simulation.Aivika.Experiment.Report.Base.FinalTableView,
        module Simulation.Aivika.Experiment.Report.Base.LastValueView,
        module Simulation.Aivika.Experiment.Report.Base.TableView,
        module Simulation.Aivika.Experiment.Report.Base.TimingStatsView,
        module Simulation.Aivika.Experiment.Report.Base.WebReportRenderer) where

import Simulation.Aivika.Experiment.Report.Base.ExperimentSpecsView
import Simulation.Aivika.Experiment.Report.Base.ExperimentSpecsWriter
import Simulation.Aivika.Experiment.Report.Base.FinalStatsView
import Simulation.Aivika.Experiment.Report.Base.FinalTableView
import Simulation.Aivika.Experiment.Report.Base.LastValueView
import Simulation.Aivika.Experiment.Report.Base.TableView
import Simulation.Aivika.Experiment.Report.Base.TimingStatsView
import Simulation.Aivika.Experiment.Report.Base.WebReportRenderer
