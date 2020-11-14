# Installation ------------------------------------------------------------
install.packages("openxlsx", dependencies = TRUE)
library(openxlsx)
install.packages("tidyverse", dependencies=TRUE, type="source")
library(tidyverse)
install.packages("dplyr", dependencies=TRUE, type="source")
library(dplyr)
install.packages("readr", dependencies=TRUE, type="source")
library(readr)
install.packages("tidyr", dependencies=TRUE, type="source")
library(tidyr)
install.packages("ggplot2", dependencies=TRUE, type="source")
library(ggplot2)
#Read in the data and specify the starting row so the question names are the column names
q_data <- read.xlsx("F:\\Doctorate\\Assessments\\Y3\\Thesis\\Results\\Questionnaire\\How EP work has changed to due COVID-19_14 November 2020_10.46.xlsx", startRow = 2, colNames = TRUE)



#Descriptive statistics
table(q_data$`How.have.you.found.the.changes.to.your.work?`)
