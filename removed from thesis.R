output:
  word_document: null
html_document: null
pdf_document: 
  includes:
  in_header: header.tex




#I removed the 2 tables from the draft paper because the first code chunk wasn't loading because of ghost script. I therefore removed them from it so I could insert pictures copied from the thesis. The problem is the font is different and Word doesn't have the default latex font (computer modern).

**Table 1**
  
  ```{r Observation features, results='asis', message=FALSE, warning=FALSE, highlight=FALSE}

install.packages("tidyverse", repo="http://cran.rstudio.com/")
library(tidyverse)
first_obv     <- openxlsx::read.xlsx("C:\\Users\\paddy\\Documents\\Patrick\\Thesis\\Quantitative\\Child 1\\Child 1 JHS observation results.xlsx")
second_obv_p  <- openxlsx::read.xlsx("C:\\Users\\paddy\\Documents\\Patrick\\Thesis\\Quantitative\\Child 2\\Child 2 observation results parent.xlsx")
second_obv_t  <- openxlsx::read.xlsx("C:\\Users\\paddy\\Documents\\Patrick\\Thesis\\Quantitative\\Child 2\\Child 2 observation results teacher.xlsx")
third_obv_p   <- openxlsx::read.xlsx("C:\\Users\\paddy\\Documents\\Patrick\\Thesis\\Quantitative\\Child 3\\Child 3 observation results parent.xlsx")
fourth_obv_p  <- openxlsx::read.xlsx("C:\\Users\\paddy\\Documents\\Patrick\\Thesis\\Quantitative\\Child 4\\Family observation schedule.xlsx")
fourth_obv_t  <- openxlsx::read.xlsx("C:\\Users\\paddy\\Documents\\Patrick\\Thesis\\Quantitative\\Child 4\\Teacher observation schedule.xlsx")
#Data tidying
#Remove Total row at the end
first_obv <- select(first_obv, 1:107)
second_obv_t <- select(second_obv_t, 1:169)
second_obv_p <- select(second_obv_p, 1:57)
third_obv_p <- select(third_obv_p, 1:104)
fourth_obv_t <- select(fourth_obv_t, 1:93)
fourth_obv_p <- select(fourth_obv_p, 1:84)
#Convert first column to row names//// I don't want to do this just yet because I want to transpose the data to vertical but I want to keep the feature column as a name so I can do stuff with it.
#first_obv <- first_obv %>% remove_rownames %>% column_to_rownames(var="Features")
#Convert NAs to 0.
first_obv <- mutate_all(first_obv, ~replace(., is.na(.), 0))
second_obv_t <- mutate_all(second_obv_t, ~replace(., is.na(.), 0))
second_obv_p <- mutate_all(second_obv_p, ~replace(., is.na(.), 0))
third_obv_p  <- mutate_all(third_obv_p, ~replace(., is.na(.), 0))
fourth_obv_t <- mutate_all(fourth_obv_t, ~replace(., is.na(.), 0))
fourth_obv_p <- mutate_all(fourth_obv_p, ~replace(., is.na(.), 0))
#Transpose the data to normal
# first remember the names
n <- first_obv$Features
# transpose all but the first column (name)
first_obv <- as.data.frame(t(first_obv[,-1]))
colnames(first_obv) <- n
first_obv$myfactor <- factor(row.names(first_obv))

second_obv_t <- as.data.frame(t(second_obv_t[,-1]))
colnames(second_obv_t) <- n
second_obv_t$myfactor <- factor(row.names(second_obv_t))

second_obv_p <- as.data.frame(t(second_obv_p[,-1]))
colnames(second_obv_p) <- n
second_obv_p$myfactor <- factor(row.names(second_obv_p))

third_obv_p <- as.data.frame(t(third_obv_p[,-1]))
colnames(third_obv_p) <- n
third_obv_p$myfactor <- factor(row.names(third_obv_p))

fourth_obv_t <- as.data.frame(t(fourth_obv_t[,-1]))
colnames(fourth_obv_t) <- n
fourth_obv_t$myfactor <- factor(row.names(fourth_obv_t))

fourth_obv_p <- as.data.frame(t(fourth_obv_p[,-1]))
colnames(fourth_obv_p) <- n
fourth_obv_p$myfactor <- factor(row.names(fourth_obv_p))

#Feature sum for first consultation in its own data frame
first_obv_sums <- first_obv %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
first_obv_sums <- first_obv_sums[107,1:14]
row.names(first_obv_sums) <- "1st consultation"

second_obv_t_sums <- second_obv_t %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
second_obv_t_sums <- second_obv_t_sums[169,1:14]
row.names(second_obv_t_sums) <- "2nd consultation (teacher)"

second_obv_p_sums <- second_obv_p %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
second_obv_p_sums <- second_obv_p_sums[57,1:14]
row.names(second_obv_p_sums) <- "2nd consultation (parent)"

third_obv_sums <- third_obv_p %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
third_obv_sums <- third_obv_sums[104,1:14]
row.names(third_obv_sums) <- "3rd consultation (parent)"

fourth_obv_t_sums <- fourth_obv_t %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
fourth_obv_t_sums <- fourth_obv_t_sums[93,1:14]
row.names(fourth_obv_t_sums) <- "4th consultation (teacher)"

fourth_obv_p_sums <- fourth_obv_p %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
fourth_obv_p_sums <- fourth_obv_p_sums[84,1:14]
row.names(fourth_obv_p_sums) <- "4th consultation (parent)"

#Merge the data frames together into 1 table
#Add the means in at this point because it's a nightmare trying to add it later when it's a list of letters and numbers (for the means)
mean <-  c(0, 0.16, 2.66, 4, 0, 7, 0, 7.16, 35.5, 3.33, 8.16, 10.5, 4, 8.83)
roundmean <- round(mean, digits = 0)
features_sums <- rbind(first_obv_sums, second_obv_t_sums, second_obv_p_sums, third_obv_sums, fourth_obv_t_sums, fourth_obv_p_sums, roundmean)
#The column and row names are way too big so I need to shorten them
colnames(features_sums) <- c("Sch knw", "Empwr individ", "Ideas future EP work", "Set out plan", "EP explain role", "EP using exp knowl", "Plan/impl treatment", "Summ", "Unders presen problem", "Everyones contrib valued", "Discuss what alr working", "CYP strengths", "Suggest solutions", "Info gather")
#I want to have the table be part of the automatic numbering system for the contents. But I can't use kable to do that. Without kable, row names aren't presented. So I need to add the row names as the first column and then it will be fine.
features_sums <- cbind(c("1(j)", "2(t)", "2(p)", "3(p)", "4(t)", "4(p)", "Mean"), features_sums)
colnames(features_sums) <- c("Consultation", "Sch knw", "Empwr individ", "Ideas future EP work", "Set out plan", "EP explain role", "EP using exp knowl", "Plan/impl treatment", "Summ", "Unders presen problem", "Everyones contrib valued", "Discuss what alr working", "CYP strengths", "Suggest solutions", "Info gather")
```

```{r tab.cap = "Summary of features by consultation", tab.id = "Features"}
features_sums
```

**Table 2**
  
  ```{r TME results}
#I create the data and then I turn them into factors.
total_TME <- data.frame(EP = c(1,1,1,2,2,2,2,2,2,2),
                        Adult = c(1,1,2,1,2,2,2,2,1,2), 
                        Child = c(1,1,1,2,2,2,2,3,4,4),
                        Goal = c(1.1,1.2,1.2,2.1,2.1,2.2,2.3,3.1,4.1,4.2), 
                        Baseline = c(3,3,3,3,5,5,3,3,2,3), 
                        Expected = c(6,5,5,7,8,8,5,4,4,5), 
                        Actual =   c(4,4,3,3,6,7,4,3,3,4), 
                        Change =   c(1,1,0,0,1,2,1,0,1,1))
#total_TME$Child <- factor(total_TME$EP, levels = c("1","2","3","4"), labels = c("1", "2","3","4"))
total_TME$EP <- factor(total_TME$EP, levels = c("1","2"), labels = c("1", "2"))
total_TME$Adult <- factor(total_TME$Adult, levels = c("1","2"), labels = c("Teacher", "Parent"))
total_TME$Goal <- factor(total_TME$Goal, levels = c(1.1,1.2,2.1,2.2,2.3,3.1,4.1,4.2), labels = c("Solving maths problems up to 10", "Accept play requests", "Not pausing when naming emotions", "Joining sounds up and reading unfamiliar words", "Using phoneme knowledge for unfamiliar words", "Maintaining a conversation", "Learning self-esteem", "Managing frustration when instructed"))

```

```{r tab.cap = "TME goals with ratings for baseline, expected, and actual", tab.id = "TME"}
total_TME
```