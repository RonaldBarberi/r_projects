# Created on Thu Nov 03 20:41:48 2024
# @author: Ronald.Barberi

#%% Imported Libraries

library(openxlsx)
library(dplyr)

#%% Create Class
AnalysisDataSet <- function(varPathData, varPathExport) {
  init <- list(
    varPathData = varPathData,
    varPathExport = varPathExport
  )
  class(init) <- 'EstructuredDF'
  return(init)
}

structured_data.EstructuredDF <- function(obj) {
  df <- read.csv(obj$varPathData, sep = '|')
  columnas <- c(
    'index',
    'X_id',
    'id',
    'time',
    'username',
    'name',
    'place',
    'likes_count',
    'hashtags',
    'link',
    'retweet',
    'near',
    'geo........'
  )
  df <- df[ , !(names(df) %in% columnas)]
  date_types <- sapply(df, class)
  print(date_types)
  df <- na.omit(df)
  
  return(df)
  # write.csv(df, obj$varPathExport, sep=';', row.names = FALSE)
  #write.xlsx(df, obj$varPathExport, rowNames = FALSE)
}

main <- function() {
  varPathData <- 'C:/Users/USER/OneDrive/Escritorio/github/r_projects/data/vacunasXFrase.csv'
  varPathExport <- 'C:/Users/USER/OneDrive/Escritorio/github/r_projects/data/dataset_export.xlsx'
  
  dataProcessor <- AnalysisDataSet(varPathData, varPathExport)
  structured_data.EstructuredDF(dataProcessor)
  print('Finish process.')
}

main()
