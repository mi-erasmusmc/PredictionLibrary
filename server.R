# @file server.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(shiny)
library(plotly)
library(shinycssloaders)

source("helpers.R")
source("plots.R")

server <- shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)
  filterIndex <- shiny::reactive({getFilter(summaryTable,input)})

  # need to remove over columns:

  output$summaryTable <- DT::renderDataTable(DT::datatable(summaryTable %>% 
                                                             mutate("# of External Validations" = table(summaryTable$Analysis)[Analysis] - 1) %>%
                                                             filter(as.character(Dev) == as.character(Val)) %>%
                                                             select("Analysis","# of External Validations", "Dev",'Val',"T","O","Model","TAR start","TAR end", "AUC","T Size", "O Count", "O Incidence (%)" )
                                                           ,rownames= FALSE, selection = 'single', options=list(scrollX=TRUE)))
  
  
  
  selectedRow <- shiny::reactive({
    if(is.null(input$summaryTable_rows_selected[1])){
      return(1)
    }else{
      return(input$summaryTable_rows_selected[1])
    }
  })
  
  plpResult <- shiny::reactive({getPlpResult(result,validation,summaryTable, inputType,filterIndex(), selectedRow())})
  # covariate table
  output$modelView <- DT::renderDataTable(editCovariates(plpResult()$covariateSummary)$table,  
                                          colnames = editCovariates(plpResult()$covariateSummary)$colnames)
  
  
  output$modelCovariateInfo <- DT::renderDataTable(data.frame(covariates = nrow(plpResult()$covariateSummary),
                                                              nonZeroCount = sum(plpResult()$covariateSummary$covariateValue!=0)))
  
  # Download plpresult
  output$plpResult <- shiny::downloadHandler(
    filename = function(){
      "plpResult.rds"
      },
    content = function(file) {
      saveRDS(plpResult(), file)
    }
  )
  
  # Downloadable csv of model ----
  output$downloadData <- shiny::downloadHandler(
    filename = function(){'model.csv'},
    content = function(file) {
      write.csv(plpResult()$covariateSummary[plpResult()$covariateSummary$covariateValue!=0,c('covariateName','covariateValue','CovariateCount','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome' )]
                , file, row.names = FALSE)
    }
  )
  
  # input tables
  output$modelTable <- DT::renderDataTable(formatModSettings(plpResult()$model$modelSettings  ))
  output$covariateTable <- DT::renderDataTable(formatCovSettings(plpResult()$model$metaData$call$covariateSettings))
  output$populationTable <- DT::renderDataTable(formatPopSettings(plpResult()$model$populationSettings))
  
  
  
  
  # prediction text
  output$info <- shiny::renderText(paste0('Within ', summaryTable[filterIndex(),'T'][selectedRow()],
                                          ' predict who will develop ',  summaryTable[filterIndex(),'O'][selectedRow()],
                                          ' during ',summaryTable[filterIndex(),'TAR start'][selectedRow()], ' day/s',
                                          ' after ', ifelse(summaryTable[filterIndex(),'addExposureDaysToStart'][selectedRow()]==0, ' cohort start ', ' cohort end '),
                                          ' and ', summaryTable[filterIndex(),'TAR end'][selectedRow()], ' day/s',
                                          ' after ', ifelse(summaryTable[filterIndex(),'addExposureDaysToEnd'][selectedRow()]==0, ' cohort start ', ' cohort end '))
  )
  

  
  # validation table and selection
  validationTable <- shiny::reactive(dplyr::filter(summaryTable[filterIndex(),],
                     Analysis == summaryTable[filterIndex(),'Analysis'][selectedRow()]))
  
  output$validationTable <- DT::renderDataTable(dplyr::select(validationTable(),c(Analysis, Dev, Val, AUC)), rownames= FALSE)
  
  valFilterIndex <- shiny::reactive({getFilter(validationTable(), input)})
  valSelectedRow <- shiny::reactive({
    if(is.null(input$validationTable_rows_selected[1])){
      return(1)
    }else{
      # return(input$validationTable_rows_selected[1])
      return(input$validationTable_rows_selected)
    }
  })
  
  # PLOTTING FUNCTION
  plotters <- shiny::reactive({
    
    eval <- plpResult()$performanceEvaluation
    if(is.null(eval)){return(NULL)}
    
    calPlot <- NULL 
    rocPlot <- NULL
    prPlot <- NULL
    f1Plot <- NULL
    
    if(!is.null(eval)){
      #intPlot <- plotShiny(eval, input$slider1) -- RMS
      intPlot <- plotShiny(eval)
      rocPlot <- intPlot$roc
      prPlot <- intPlot$pr
      f1Plot <- intPlot$f1score
      
      list(rocPlot= rocPlot,
           prPlot=prPlot, f1Plot=f1Plot)
    }
  })
  
  
  performance <- shiny::reactive({
    
    eval <- plpResult()$performanceEvaluation
    
    if(is.null(eval)){
      return(NULL)
    } else {
      intPlot <- getORC(eval, input$slider1)
      threshold <- intPlot$threshold
      prefthreshold <- intPlot$prefthreshold
      TP <- intPlot$TP
      FP <- intPlot$FP
      TN <- intPlot$TN
      FN <- intPlot$FN
    }
    
    twobytwo <- as.data.frame(matrix(c(FP,TP,TN,FN), byrow=T, ncol=2))
    colnames(twobytwo) <- c('Ground Truth Negative','Ground Truth Positive')
    rownames(twobytwo) <- c('Predicted Positive','Predicted Negative')
    
    list(threshold = threshold, 
         prefthreshold = prefthreshold,
         twobytwo = twobytwo,
         Incidence = (TP+FN)/(TP+TN+FP+FN),
         Threshold = threshold,
         Sensitivity = TP/(TP+FN),
         Specificity = TN/(TN+FP),
         PPV = TP/(TP+FP),
         NPV = TN/(TN+FN) )
  })
  
  
  # preference plot
  output$prefdist <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotPreferencePDF(plpResult()$performanceEvaluation, 
                        type=plpResult()$type ) #+ 
      # ggplot2::geom_vline(xintercept=plotters()$prefthreshold) -- RMS
    }
  })
  
  output$preddist <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotPredictedPDF(plpResult()$performanceEvaluation, 
                       type=plpResult()$type ) # + 
      #ggplot2::geom_vline(xintercept=plotters()$threshold) -- RMS     
    }
  })
  
  output$box <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotPredictionDistribution(plpResult()$performanceEvaluation, type=plpResult()$type )
    }
  })
  
  output$cal <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotSparseCalibration2(plpResult()$performanceEvaluation, type=plpResult()$type )
    }
  })

  output$demo <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      tryCatch(plotDemographicSummary(plpResult()$performanceEvaluation, 
                                      type=plpResult()$type ),
               error= function(cond){return(NULL)})
    }
  })
# plots for the validation section. todo: add the development?
  
  valResult <- shiny::reactive({
    valtemplist <- list()
    valTable <- validationTable()
    valfindex <- valFilterIndex()
    #use sort so the order doesnt break the plotMultipleRoc/Cal
    rows <- sort(valSelectedRow())
    print(rows)
    names <- valTable[rows, "Val"]
    for (i in 1:length(rows)){
      valtemplist[[i]] <- getPlpResult(result,validation,valTable, inputType,valfindex, rows[i])
    }
    list(valtemplist, names)
  })
  
  valPlots <- shiny::reactive({
    results <- valResult()
    if(is.null(results[[1]][[1]]$performanceEvaluation)){
      return(NULL)
    } else{
      
      valCalPlot <- PredictionComparison::plotMultipleCal(results[[1]], names = results[[2]])
      valRocPlot <- PredictionComparison::plotMultipleRoc(results[[1]], names = results[[2]], grid = F)
      list(valRocPlot= valRocPlot, valCalPlot = valCalPlot)
      
    }
  })
  
  output$valRoc <- shiny::renderPlot({
    try(valPlots()$valRocPlot)
  })
  output$valCal <- shiny::renderPlot({
    try(valPlots()$valCalPlot)
  })
  # Do the tables and plots:
  
  output$performance <- shiny::renderTable(performance()$performance, 
                                           rownames = F, digits = 3)
  output$twobytwo <- shiny::renderTable(performance()$twobytwo, 
                                        rownames = T, digits = 0)
  
  
  output$threshold <- shiny::renderText(format(performance()$threshold,digits=5))
  
  output$roc <- plotly::renderPlotly({
    plotters()$rocPlot
  })
  
  output$pr <- plotly::renderPlotly({
    plotters()$prPlot
  })
  output$f1 <- plotly::renderPlotly({
    plotters()$f1Plot
  })

  # covariate model plots
  covs <- shiny::reactive({
    if(is.null(plpResult()$covariateSummary))
      return(NULL)
    plotCovariateSummary(formatCovariateTable(plpResult()$covariateSummary))
  })
  
  output$covariateSummaryBinary <- plotly::renderPlotly({ covs()$binary })
  output$covariateSummaryMeasure <- plotly::renderPlotly({ covs()$meas })
  
  # LOG
  output$log <- shiny::renderText( paste(plpResult()$log, collapse="\n") )
  
  # dashboard
  
  output$performanceBoxIncidence <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Incidence", paste0(round(performance()$Incidence*100, digits=3),'%'), icon = shiny::icon("ambulance"),
      color = "green"
    )
  })
  
  output$performanceBoxThreshold <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Threshold", format((performance()$Threshold), scientific = F, digits=3), icon = shiny::icon("edit"),
      color = "yellow"
    )
  })
  
  output$performanceBoxPPV <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "PPV", paste0(round(performance()$PPV*1000)/10, "%"), icon = shiny::icon("thumbs-up"),
      color = "orange"
    )
  })
  
  output$performanceBoxSpecificity <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Specificity", paste0(round(performance()$Specificity*1000)/10, "%"), icon = shiny::icon("bullseye"),
      color = "purple"
    )
  })
  
  output$performanceBoxSensitivity <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Sensitivity", paste0(round(performance()$Sensitivity*1000)/10, "%"), icon = shiny::icon("low-vision"),
      color = "blue"
    )
  })
  
  output$performanceBoxNPV <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "NPV", paste0(round(performance()$NPV*1000)/10, "%"), icon = shiny::icon("minus-square"),
      color = "black"
    )
  })
  
})