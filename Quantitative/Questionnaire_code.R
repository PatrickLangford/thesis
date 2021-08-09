# Installation ------------------------------------------------------------
install.packages("openxlsx")
library(openxlsx)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library("readxl")
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot", dependencies=TRUE, type="source")
library(corrplot)
#Read in the data and specify the starting row so the question names are the column names
q_data <- read_excel("D:\\Doctorate\\Assessments\\Y3\\Thesis\\Results\\Questionnaire\\How EP work has changed to due COVID-19_14 November 2020_10.46.xlsx", skip = 1, col_names = TRUE)



#Descriptive statistics

#Q8
pre_features <- read_excel("D:\\Doctorate\\Assessments\\Y3\\Thesis\\Results\\Questionnaire\\Q8_pure.xlsx", col_names = TRUE)
pre_features <- as.data.frame(pre_features)
#Spearman's rank correlation matrix of the pre-COVID features of consultation
m <- cor(pre_features, method = c("spearman"))
#Create a correlation plot with the correlation strength on the top half and pie on the bottom
corrplot.mixed(m, upper = "number", lower = "pie")
#Correlation test at 0.05 significance level
corrtest <- cor.mtest(m, conf.level = .95)
#Combine the correlation test with the correlation plot
corrplot(m, method = "number", p.mat = corrtest$p, sig.level = .05, type = "upper")



#"I.G.", "S.S.", "Str.", "D.A.R.", "E.C.", "P.P.", "S.d", "I.", "K.", "S.P.", "E.", "I.F.W.", "Em.", "Sc.S.", "E.E."), sep = ",")

#"Gathering information about things other than the presenting problem (including about the child/young person)", "Suggesting solutions", "Highlighting the strengths of the child/young person", "Discussing what is already working/current levels of support", "Everyone contributing and having their opinions valued", "Fully understanding the presenting problem(s)", "Summarising what has been said", "How to implement the interventions", "Using knowledge of child development, therapeutic approaches, and psychological theories", "Setting out a plan for the consultation", "Explaining what EPs do", "Suggesting ideas for future EP work", "Empowering those involved", "Using or gaining knowledge of the school system", "Exploring exceptions and limits to the presenting problem", "Other")

#For cor to work. I need to get the string of characters into a numeric vector, matrix or data frame. If I convert the character string of features into a binary array, I Can calculate a correlation (maybe, I'm not sure if I technically should but I'll come back to that)

#Q9
#I want this data as a data frame with each type of work as a column and the rating of difficulty as a level within those columns.
#Q10
replacements <- 
table(replacements)

#Q11
#Read in the data and specify the column name
Q11data <- as.data.frame(q_data$`How have you found the changes to your work?`)
colnames(Q11data) = "Rating"
#Add the missing factor (because no one said they found it not challenging at all)
levels(Q11data$Rating) <- c(levels(Q11data$Rating),"Not at all challenging", "Slightly challenging", "Moderately challenging", "Very challenging", "Extremely challenging")
#Reorder levels
Q11data$Rating <- factor(Q11data$Rating, levels = c('Not at all challenging', 'Slightly challenging', 'Moderately challenging', 'Very challenging', 'Extremely challenging'))
#I want the table format for the graph, so turn the table into a dataframe
Q11table <- table(Q11data)
df <- as.data.frame(Q11table)
#Plots
ggplot(df,
       aes(x = Q11data, 
           y = Freq,
           fill = Q11data)) +
  geom_col() +
  labs(y = "Frequency",
       x = "Rating",
       title = "How have you found the changes to your work as a result of COVID-19?") +
  theme(legend.title=element_blank())


#Whilst this was fun to make, it doesn't look as good as a standard bar graph
#ggplot(df,
#       aes(x = Freq, 
#           xend = 0,
#           y = Q11data,
#           yend = Q11data,
#           colour = Q11data)) +
#  geom_segment() +
#  geom_point() +
#  labs(y = "Frequency",
#       title = "How have you found the changes to your work as a result of COVID-19?") +
#  theme(legend.title=element_blank())


#Q14
post_features <- read_excel("D:\\Doctorate\\Assessments\\Y3\\Thesis\\Results\\Questionnaire\\Q14_pure.xlsx", col_names = TRUE)
post_features <- as.data.frame(post_features)
#Spearman's rank correlation matrix of the pre-COVID features of consultation
n <- cor(post_features, method = c("spearman"))
#Create a correlation plot with the correlation strength on the top half and pie on the bottom
corrplot.mixed(n, upper = "number", lower = "pie")
#Correlation test at 0.05 significance level
corrtest <- cor.mtest(n, conf.level = .95)
#Combine the correlation test with the correlation plot
corrplot(n, method = "number", p.mat = corrtest$p, sig.level = .05, type = "upper")


feats <- c("I.G.", "S.S.", "Str.", "D.A.R.", "E.C.", "P.P.", "S.d", "I.", "K.", "S.P.", "E.", "I.F.W.", "Em.", "Sc.S.", "E.E.", "C.D.")
prefreq <- c(95,	86,	103,	102,	101,	97,	101,	63,	99,	81,	88,	68,	98,	82,	95, 0)
postfreq <- c(74,	63,	78,	76,	63,	69,	73,	23,	70,	48,	61,	42,	70,	40,	72, 12)


compdf <- read_excel("D:\\Doctorate\\Assessments\\Y3\\Thesis\\Results\\Questionnaire\\Q8_&_14_comparison.xlsx", col_names = TRUE)

t.test(compdf$I.G., postfreq, paired = TRUE)
