# this function finds the filter index
getFilter <- function(summaryTable,input){
  ind <- 1:nrow(summaryTable)
  if(input$devDatabase!='All'){
    ind <- intersect(ind,which(as.character(summaryTable$Dev)==input$devDatabase))
  }
  if(input$valDatabase!='All'){
    ind <- intersect(ind,which(as.character(summaryTable$Val)==input$valDatabase))
  }
  if(input$T!='All'){
    ind <- intersect(ind,which(summaryTable$T==input$T))
  }
  if(input$O!='All'){
    ind <- intersect(ind,which(summaryTable$O==input$O))
  }
  if(input$modelSettingName!='All'){
    ind <- intersect(ind,which(as.character(summaryTable$Model)==input$modelSettingName))
  }
  if(input$riskWindowStart!='All'){
    ind <- intersect(ind,which(summaryTable$`TAR start`==input$riskWindowStart))
  }
  if(input$riskWindowEnd!='All'){
    ind <- intersect(ind,which(summaryTable$`TAR end`==input$riskWindowEnd))
  }
  return(ind)
}


getPlpResult <- function(result,validation,summaryTable, inputType,filterIndex, selectedRow){
  # print(result)
  # print(validation)
  # print(head(summaryTable))
  # print(inputType)
  # print(filterIndex)
  # print(selectedRow)
  if(inputType == 'plpResult'){
    i <- filterIndex[selectedRow]
    if(i ==1){
      tempResult <- result
      tempResult$type <- 'test'
    }else{
      tempResult <- validation$validation[[i-1]]
      tempResult$type <- 'validation'
    }
    tempResult$log <- 'log not available'
  }else if(inputType == 'plpNoClass'){
    tempResult <- result
    tempResult$type <- 'validation'
    tempResult$log <- 'log not available'
  }else if( inputType == 'file') {
    tempResult <- NULL
    loc <- summaryTable[filterIndex,][selectedRow,]$plpResultLocation
    locLoaderFunc <- summaryTable[filterIndex,][selectedRow,]$plpResultLoad
    logLocation <- gsub('plpResult','plpLog.txt', gsub('validationResult.rds','plpLog.txt',gsub('plpResult.rds','plpLog.txt', as.character(loc))))
    if(file.exists(logLocation)){
      txt <- readLines(logLocation)
    } else{
      txt <- 'log not available'
    }
    if(file.exists(as.character(loc))){
      tempResult <- do.call(as.character(locLoaderFunc), list(as.character(loc)))
      tempResult$log <- txt
      tempResult$type <- ifelse(length(grep('/Validation',loc))>0,'validation','test')
    }
  }else {
    stop('Incorrect class')
  }
  return(tempResult)
}



# format modelSettings
formatModSettings <- function(modelSettings){
  modelset <- data.frame(Setting = c('Model',names(modelSettings[[2]])),
                         Value = c(modelSettings[[1]], unlist(lapply(modelSettings[[2]], 
                                                                     function(x) paste0(x, collapse='')))))
  row.names(modelset) <- NULL
  return(modelset)
}

# format covariateSettings
formatCovSettings <- function(covariateSettings){
  if(class(covariateSettings)=='list'){
    #code for when multiple covariateSettings
    covariates <- c() 
    covariates <- data.frame(covariateName = names(covariateSettings), 
                             SettingValue = unlist(lapply(covariateSettings, 
                                                          function(x) paste0(x, 
                                                                             collapse='-'))))
  }
  row.names(covariates) <- NULL
  return(covariates)
}

# format populationSettings
formatPopSettings <- function(populationSettings){
  population <- populationSettings
  population$attrition <- NULL # remove the attrition as result and not setting
  population <- data.frame(Setting = names(population), 
                           Value = unlist(lapply(population, 
                                                 function(x) paste0(x, 
                                                                    collapse='-')))
  ) 
  row.names(population) <- NULL
  return(population)
}


# format covariate summary table
formatCovariateTable <- function(covariateSummary){
  for(coln in c('covariateValue','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome','StandardizedMeanDiff')){
    if(sum(colnames(covariateSummary)==coln)>0){
      covariateSummary[,coln] <- format(round(covariateSummary[,coln], 4), nsmall = 4)
      class(covariateSummary[,coln]) <- "numeric"
    }
  }
  
  # edit the simple model names:
  covariateSummary$covariateName <- gsub('\\[COVID v1\\] Persons with ', '', covariateSummary$covariateName)
  covariateSummary$covariateName <- gsub('\\[Covid v1\\] Persons with ', '', covariateSummary$covariateName)
  covariateSummary$covariateName <- gsub('\\[covid v1\\] Persons with ', '', covariateSummary$covariateName)
  

  return(covariateSummary)
}



editCovariates <- function(covs){
  
  # remove Custom ATLAS variable during day -9999 through -1 days relative to index:
  covs$covariateName <- gsub('Custom ATLAS variable during day -9999 through -1 days relative to index: ','History of ', covs$covariateName)
  
  if(!is.null(covs$StandardizedMeanDiff)){
    return(list(table = formatCovariateTable(covs[,c('covariateName','covariateValue','CovariateCount','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome','StandardizedMeanDiff')]),
                colnames = c('Covariate Name', 'Value','Count', 'Outcome Mean', 'Non-outcome Mean','Std Mean Diff')
    ))
  } else{
    return(list(table = formatCovariateTable(covs[,c('covariateName','covariateValue','CovariateCount','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome')]),
                colnames = c('Covariate Name', 'Value','Count', 'Outcome Mean', 'Non-outcome Mean')
    ))
  }
}


