
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Types
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines the simulation experiment reports.
--

module Simulation.Aivika.Experiment.Report.Types where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.ParallelIO.Local

import Data.Maybe
-- import Data.Monoid

import GHC.Conc (getNumCapabilities)

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Entity

-- | It allows rendering the simulation reports in an arbitrary way.
class ReportRendering r where

  -- | A report generator.                     
  data ReportGenerator r :: *

  -- | The report monad.
  type ReportMonad r :: * -> *

  -- | Create a report sequentially.
  runReport :: ExperimentAgent
               -- ^ the experiment agent
               -> ReportGeneratorMap r
               -- ^ generators used for rendering
               -> r
               -- ^ the rendering backend
               -> IO ()
  runReport = runReportWithExecutor sequence_

  -- | Create a report in parallel as possible.
  runReportParallel :: ExperimentAgent
                       -- ^ the experiment agent
                       -> ReportGeneratorMap r
                       -- ^ generators used for rendering
                       -> r
                       -- ^ the rendering backend
                       -> IO ()
  runReportParallel = runReportWithExecutor executor 
    where executor tasks =
            do n <- getNumCapabilities
               withPool n $ \pool ->
                 parallel_ pool tasks

  -- | Create a report using the specified executor.
  runReportWithExecutor :: ([IO ()] -> IO ())
                           -- ^ an executor that allows parallelizing the actions if required
                           -> ExperimentAgent
                           -- ^ the experiment agent
                           -> ReportGeneratorMap r
                           -- ^ generators used for rendering
                           -> r
                           -- ^ the rendering backend
                           -> IO ()

-- | Defines a view specifying how the report should be saved.
class ReportRendering r => ReportView v r where
  
  -- | Create a report generator.
  reportView :: v -> ReportGenerator r

-- | Defines the generators used for rendering.
type ReportGeneratorMap r = ExperimentAgent
                            -> ExperimentEntity
                            -> SourceEntity
                            -> ReportMonad r (Maybe (ReportGenerator r))
