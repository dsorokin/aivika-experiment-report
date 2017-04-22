
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Base.ExperimentSpecsView
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- The module defines 'ExperimentSpecsView' that shows the 
-- experiment specs.
--

module Simulation.Aivika.Experiment.Report.Base.ExperimentSpecsView 
       (ExperimentSpecsView(..),
        defaultExperimentSpecsView) where

import Control.Monad
import Control.Monad.Trans

import Data.Monoid

import Simulation.Aivika
import Simulation.Aivika.Experiment.Base.HtmlWriter
import Simulation.Aivika.Experiment.Base.ExperimentWriter
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Report.Types
import Simulation.Aivika.Experiment.Report.Base.WebReportRenderer
import Simulation.Aivika.Experiment.Report.Base.ExperimentSpecsWriter

-- | Defines the 'View' that shows the experiment specs.
data ExperimentSpecsView =
  ExperimentSpecsView { experimentSpecsTitle       :: String,
                        -- ^ The title for the view.
                        experimentSpecsDescription :: String,
                        -- ^ The description for the view.
                        experimentSpecsWriter      :: ExperimentSpecsWriter
                        -- ^ It shows the specs.
                      }
  
-- | This is the default view.
defaultExperimentSpecsView :: ExperimentSpecsView
defaultExperimentSpecsView =  
  ExperimentSpecsView { experimentSpecsTitle       = "Experiment Specs",
                        experimentSpecsDescription = "It shows the experiment specs.",
                        experimentSpecsWriter      = defaultExperimentSpecsWriter }

instance ReportView ExperimentSpecsView (WebReportRenderer a) where
  
  reportView view = 
    let newWriter agent exp renderer path =
          return WebReportWriter { reportWriterInitialise = return (),
                                   reportWriterFinalise   = return (),
                                   reportWrite            = \runIndex -> return (),
                                   reportWriteTOCHtml     = experimentSpecsTOCHtml view,
                                   reportWriteHtml        = experimentSpecsHtml view exp
                                 }
    in WebReportGenerator { newReportWriter = newWriter }

-- | Get the HTML code.     
experimentSpecsHtml :: ExperimentSpecsView -> ExperimentEntity -> Int -> HtmlWriter ()     
experimentSpecsHtml view exp index =
  do header view index
     let writer = experimentSpecsWriter view
         write  = experimentSpecsWrite writer
     write writer exp

header :: ExperimentSpecsView -> Int -> HtmlWriter ()
header view index =
  do writeHtmlHeader3WithId ("id" ++ show index) $
       writeHtmlText (experimentSpecsTitle view)
     let description = experimentSpecsDescription view
     unless (null description) $
       writeHtmlParagraph $
       writeHtmlText description

-- | Get the TOC item     
experimentSpecsTOCHtml :: ExperimentSpecsView -> Int -> HtmlWriter ()
experimentSpecsTOCHtml view index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (experimentSpecsTitle view)
