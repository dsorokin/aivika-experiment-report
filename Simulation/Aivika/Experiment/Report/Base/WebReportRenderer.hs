
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Report.Base.WebReportRenderer
-- Copyright  : Copyright (c) 2017, David Sorokin <david.sorokin@gmail.com>
-- License    : AllRightsReserved
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.2
--
-- It defines a renderer that creates a web page when generating the report..
--

module Simulation.Aivika.Experiment.Report.Base.WebReportRenderer
       (ReportGenerator(..),
        WebReportRenderer(..),
        defaultWebReportRenderer,
        WebReportGenerator(..),
        WebReportMonad(..),
        WebReportWriter(..)) where

import Control.Monad
import Control.Monad.Trans

import Data.Monoid

import System.IO
import System.Directory
import System.FilePath

import Simulation.Aivika
import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.Base
import Simulation.Aivika.Experiment.Entity
import Simulation.Aivika.Experiment.Report.Types

-- | It defines the web report renderer. 
data WebReportRenderer a =
  WebReportRenderer { reportParameter :: a,
                      -- ^ The parameter.
                      reportTitle :: String,
                      -- ^ The report title.
                      reportDescription :: String,
                      -- ^ The report description.
                      reportVerbose :: Bool,
                      -- ^ Whether the report rendering is verbose.
                      reportFilePath :: ExperimentFilePath
                      -- ^ A directory, where the report is to be saved in
                    }

-- | The default web report renderer.
defaultWebReportRenderer :: WebReportRenderer a
defaultWebReportRenderer =
  WebReportRenderer { reportParameter = error "Must supply with the reportParameter value",
                      reportTitle = "Simulation Report",
                      reportDescription = [],
                      reportVerbose = True,
                      reportFilePath = WritableFilePath "experiment"
                    }

-- | It replies to the requests made by the web report renderer.
data WebReportWriter a =
  WebReportWriter { reportWriterInitialise :: IO (),
                    -- ^ Initialise the report.
                    reportWriterFinalise :: IO (),
                    -- ^ Finalise the report.
                    reportWrite :: Int -> IO (),
                    -- ^ Write the report by the specified run index.
                    reportWriteTOCHtml :: Int -> HtmlWriter (),
                    -- ^ Return a TOC (Table of Contents) item for 
                    -- the HTML index file after the finalisation 
                    -- function is called, i.e. in the very end. 
                    -- The agument specifies the ordered number of 
                    -- the item.
                    --
                    -- You should wrap your HTML in 'writeHtmlListItem'.
                    reportWriteHtml :: Int -> HtmlWriter ()
                    -- ^ Return an HTML code for the index file
                    -- after the finalisation function is called,
                    -- i.e. in the very end. The agument specifies
                    -- the ordered number of the item.
                  }

-- | A convenient type synonym for describing a web report generator.
type WebReportGenerator a = ReportGenerator (WebReportRenderer a)

-- | A type synonym for the report monad.
type WebReportMonad a = ReportMonad (WebReportRenderer a)

-- | Rendering a web report.
instance ReportRendering (WebReportRenderer a) where

  -- | The report generator.
  data ReportGenerator (WebReportRenderer a) =
    WebReportGenerator { newReportWriter :: ExperimentAgent
                                            -> ExperimentEntity
                                            -> WebReportRenderer a
                                            -> FilePath
                                            -> WebReportMonad a (WebReportWriter a)
                         -- ^ Create a new web report writer.
                       }

  -- | The report monad.
  type ReportMonad (WebReportRenderer a) = ExperimentWriter

  runReportWithExecutor executor agent generators r =
    runExperimentWriter $
    do path <- prepareReport r
       exps <- prepareExperimentEntities agent generators r path
       liftIO $
         do initialiseExperimentEntities executor exps
            writeExperimentEntities executor exps
            finaliseExperimentEntities executor exps
       forM_ exps $ writeExperimentHtml r
       writeReportHtml r path exps

-- | The report experiment entity.
data WebReportExperimentEntity a =
  WebReportExperimentEntity { reportExperimentEntity :: ExperimentEntity,
                              -- ^ The experiment entity.
                              reportExperimentGenerated :: Bool,
                              -- ^ Whether the experiment report is to be generated.
                              reportExperimentFilePath :: FilePath,
                              -- ^ The experiment file path.
                              reportExperimentWriters :: [WebReportWriter a]
                              -- ^ The source entities.
                            }

-- | Prepare the report.
prepareReport :: WebReportRenderer a -> WebReportMonad a FilePath
prepareReport r =
  do path <- resolveFilePath "" (reportFilePath r)
     liftIO $ do
       when (reportVerbose r) $
         do putStr "Updating directory " 
            putStrLn path
       createDirectoryIfMissing True path
     return path

-- | Prepare the experiment entities.
prepareExperimentEntities :: ExperimentAgent
                             -> ReportGeneratorMap (WebReportRenderer a)
                             -> WebReportRenderer a
                             -> FilePath
                             -> WebReportMonad a [WebReportExperimentEntity a]
prepareExperimentEntities agent generators r path =
  do exps <- liftIO $ readExperimentEntities agent
     forM exps $ \exp ->
       do let expId = experimentEntityId exp
              path' = path </> show expId
          if not (experimentEntityCompleted exp)
            then return $ WebReportExperimentEntity exp False path' []
            else do f1 <- liftIO $ doesFileExist path'
                    f2 <- liftIO $ doesDirectoryExist path'
                    if f1 || f2
                      then return $ WebReportExperimentEntity exp False path' []
                      else do liftIO $ do
                                when (reportVerbose r) $
                                  do putStr "Creating directory " 
                                     putStrLn path'
                                createDirectoryIfMissing True path'
                              gens <- generators agent exp
                              wrts <-
                                forM gens $ \gen ->
                                newReportWriter gen agent exp r path'
                              return $ WebReportExperimentEntity exp True path' wrts

-- | Initialise the experiment entities.
initialiseExperimentEntities :: ([IO ()] -> IO ())
                                -- ^ the executor
                                -> [WebReportExperimentEntity a]
                                -- ^ the experiment entity
                                -> IO ()
initialiseExperimentEntities executor exps =
  executor $
  mconcat $
  let exps' = filter (experimentEntityCompleted . reportExperimentEntity) exps
  in flip map exps' $ \exp ->
  map reportWriterInitialise $ reportExperimentWriters exp

-- | Finalise the experiment entities.
finaliseExperimentEntities :: ([IO ()] -> IO ())
                              -- ^ the executor
                              -> [WebReportExperimentEntity a]
                              -- ^ the experiment entity
                              -> IO ()
finaliseExperimentEntities executor exps =
  executor $
  mconcat $
  let exps' = filter (experimentEntityCompleted . reportExperimentEntity) exps
  in flip map exps' $ \exp ->
  map reportWriterFinalise $ reportExperimentWriters exp

-- | Write the experiment entities.
writeExperimentEntities :: ([IO ()] -> IO ())
                           -- ^ the executor
                           -> [WebReportExperimentEntity a]
                           -- ^ the experiment entity
                           -> IO ()
writeExperimentEntities executor exps =
  executor $
  mconcat $
  let exps' = filter (experimentEntityCompleted . reportExperimentEntity) exps
  in flip map exps' $ \exp ->
  let n = experimentEntityRunCount $ reportExperimentEntity exp
  in mconcat $
     flip map (reportExperimentWriters exp) $ \wrt ->
     flip map [1..n] $ reportWrite wrt
     
-- | Write the experiment HTML file.
writeExperimentHtml :: WebReportRenderer a -> WebReportExperimentEntity a -> WebReportMonad a ()
writeExperimentHtml r exp =
  do let e = reportExperimentEntity exp
         wrts = reportExperimentWriters exp
         title = experimentEntityTitle e ++ " - " ++
                 experimentEntityRealStartTime e
         html :: HtmlWriter ()
         html = 
           writeHtmlDocumentWithTitle title $
           do writeHtmlList $
                forM_ (zip [1..] wrts) $ \(i, wrt) -> 
                reportWriteTOCHtml wrt i
              writeHtmlBreak
              unless (null $ experimentEntityDescription e) $
                writeHtmlParagraph $
                writeHtmlText $ experimentEntityDescription e
              forM_ (zip [1..] wrts) $ \(i, wrt) ->
                reportWriteHtml wrt i
         file = combine (reportExperimentFilePath exp) "index.html"
     when (reportExperimentGenerated exp) $
       do ((), contents) <- runHtmlWriter html id
          liftIO $
            withFile file WriteMode $ \h ->
            do hSetEncoding h utf8
               hPutStr h (contents [])
               when (reportVerbose r) $
                 do putStr "Generated file "
                    putStrLn file

-- | Write the report HTML file.
writeReportHtml :: WebReportRenderer a -> FilePath -> [WebReportExperimentEntity a] -> WebReportMonad a ()
writeReportHtml r path exps =
  do let html :: HtmlWriter ()
         html = 
           writeHtmlDocumentWithTitle (reportTitle r) $
           do unless (null $ reportDescription r) $
                writeHtmlParagraph $
                writeHtmlText $ reportDescription r
              writeHtmlList $
                forM_ (zip [1..] exps) $ \(i, exp) ->
                do let e = reportExperimentEntity exp
                   if experimentEntityCompleted e
                     then writeHtmlListItem $
                          writeHtmlLink (show (experimentEntityId e) ++ "/index.html") $
                          do writeHtmlText $ experimentEntityTitle e
                             writeHtmlText " - "
                             writeHtmlText $ experimentEntityRealStartTime e
                     else writeHtmlListItem $
                          do writeHtmlText $ experimentEntityTitle e
                             writeHtmlText " - "
                             writeHtmlText $ experimentEntityRealStartTime e
                             case experimentEntityErrorMessage e of
                               Nothing  -> return ()
                               Just msg ->
                                 do writeHtmlText " ("
                                    writeHtmlText msg
                                    writeHtmlText ")"
         file = combine path "index.html"
     ((), contents) <- runHtmlWriter html id
     liftIO $
       withFile file WriteMode $ \h ->
       do hSetEncoding h utf8
          hPutStr h (contents [])
          when (reportVerbose r) $
            do putStr "Generated file "
               putStrLn file
