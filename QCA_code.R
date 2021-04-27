#QCA
# Installation ---------------------------------------------------------
install.packages("openxlsx")
library(openxlsx)
install.packages("tidyverse")
library(tidyverse)
install.packages("QCA", dependencies = TRUE)
library(QCA)
#Read in the data frame of the results
first_obv     <- read.xlsx("D:\\Doctorate\\Assessments\\Y3\\Thesis\\Results\\Observations\\20.11.20\\20.11.20 observation results.xlsx")
second_obv_p  <- read.xlsx("D:\\Doctorate\\Assessments\\Y3\\Thesis\\Results\\Observations\\20.12.04\\20.12.04 observation results parent.xlsx")
second_obv_t  <- read.xlsx("D:\\Doctorate\\Assessments\\Y3\\Thesis\\Results\\Observations\\20.12.04\\20.12.04 observation results teacher.xlsx")
third_obv_p   <- read.xlsx("D:\\Doctorate\\Assessments\\Y3\\Thesis\\Results\\Observations\\20.12.16\\20.12.16 Observation results parent.xlsx")
fourth_obv_p  <- read.xlsx("D:\\Doctorate\\Assessments\\Y3\\Thesis\\Results\\Observations\\21.01.14\\Family observation schedule.xlsx")
fourth_obv_t  <- read.xlsx("D:\\Doctorate\\Assessments\\Y3\\Thesis\\Results\\Observations\\21.01.14\\Teacher observation schedule.xlsx")
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

#Graphs and data of TME
#To create the TME data table, need to create factors for the adult doing the rating and which goal they're talking about (I can then check with the TME forms to see which goal corresponds to which letter)

#At the moment, the data frame is creating a set of vectors etc. that are independent of the above factors created. Do I need to create the entire table and then convert the relevant aspects to factors?

#Table for the TME data. This first one is for the first child
first_obv_TME <- data.frame(Adult = c(1,1,2), Goal = c(1.1,1.2,1.1), Baseline = c(3,3,3), Expected = c(6,5,5), Actual = c(4,4,3))

first_obv_TME$Adult <- factor(first_obv_TME$Adult, levels = c("1","2"), labels = c("Teacher", "Parent"))
first_obv_TME$Goal <- factor(first_obv_TME$Goal, levels = c(1.1,1.2,2.1,2.2,2.3,3.1,4.1,4.2), labels = c("Maths problems up to 10", "Accept play requests", "Not pausing when naming emotions", "Joining sounds up and reading unfamiliar words", "Using phoneme knowledge for unfamiliar words", "Maintaining a conversation", "Learning self-esteem", "Managing frustration when instructed"))

#This is for all the data I currently have (missing the last child). I create the data and then I turn them into factors.
total_TME <- data.frame(EP = c(1,1,1,2,2,2,2,2,2,2),
                        Adult = c(1,1,2,1,2,2,2,2,1,2), 
                        Child = c(1,1,1,2,2,2,2,3,4,4),
                        Goal = c(1.1,1.2,1.1,2.1,2.2,2.3,2.1,3.1,4.1,4.2), 
                        Baseline = c(3,3,3,3,5,3,5,3,2,3), 
                        Expected = c(6,5,5,7,8,5,8,4,4,5), 
                        Actual =   c(4,4,3,3,7,4,6,3,3,4), 
                        Change =   c(1,1,0,0,2,1,1,0,1,1))
#total_TME$Child <- factor(total_TME$EP, levels = c("1","2","3","4"), labels = c("1", "2","3","4"))
total_TME$EP <- factor(total_TME$EP, levels = c("1","2"), labels = c("1", "2"))
total_TME$Adult <- factor(total_TME$Adult, levels = c("1","2"), labels = c("Teacher", "Parent"))
total_TME$Goal <- factor(total_TME$Goal, levels = c(1.1,1.2,2.1,2.1,2.2,2.3,3.1,4.1,4.2), labels = c("Maths problems up to 10", "Accept play requests", "Not pausing when naming emotions", "Joining sounds up and reading unfamiliar words", "Using phoneme knowledge for unfamiliar words", "Maintaining a conversation", "Learning self-esteem", "Learning self-esteem", "Managing frustration when instructed"))


#To explore the degree of change in consultations with the number of features present, I'd need to create a data frame with columns of child (teacher or parent for different column?), then feature sums, then change
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
all_obs_sums <- rbind(first_obv_sums, second_obv_t_sums, second_obv_p_sums, third_obv_sums, fourth_obv_t_sums, fourth_obv_p_sums)

#I want to add the change for each consultation on to the end of the data frame so I need to create a data frame with the consultation name and change
Consultation <- c("1st consultation", "2nd consultation (parent)", "2nd consultation (teacher)", "3rd consultation", "4th consultation (teacher)", "4th consultation (parent")
Change = c(1,1,0,0,2,1,1,0,1,1)

#Creating a data frame for the change for each consultation
Goal_Adult <- c("t1.1", "t1.2", "p1.1", "t2.1", "p2.2", "p2.3", "p2.1", "p3.1", "t4.1", "p4.2")
Goal_&adult_change <- data.frame(Goal_Adult, Change)
#I need to load in the features data for the other consultations and then do the transposing as above + sum the features, make it into its own data frame, then create a full data frame where each row is a consultation and the columns are the features and the change

#QCA
#For QCA, you can only work out how the outcomes relate to different combinations of the presence or absence of different features. I therefore lose the data about the relative frequency of each feature. I don't quite lose them: I can work out the thresholds for whether a feature should be classified as present or absent. Currently, I've just read it as either fully absent or fully present. But I can run some tests to see where the reasonable threshold for each feature is.
#Create a truth table where the presence or complete absence of a feature for each consultation is calculated.
QCAtable <- all_obs_sums
#For the QCA later, I need to have single word column names so I'm going to have to rename all of the columns
colnames(QCAtable) = c("SK", "EI", "IFEPW", "SOP", "EPER", "EPUEK", "PIT", "Summ", "UPP", "ECV", "DWAW", "CYPS", "SS", "IG")
#I need to add change to my QCA table. But there are 6 unique observations but 10 goals because some consultations have more than 1 goal. So I need to work out which rows to duplicate.
#Run the below code twice because there are 3 goals for the first consultation.
QCAtable <-  QCAtable %>% 
  filter(row_number() <= 1) %>% 
  bind_rows(QCAtable)
QCAtable <-  QCAtable %>% 
  filter(row_number() <= 1) %>% 
  bind_rows(QCAtable)
#Add two more copies of the 5th row as this consultation has 2 more goals attached to it.
QCAtable <- rbind(QCAtable, QCAtable[rep(5, 2), ])
#The duplicated rows were added to the bottom so I need to reorder
row.names(QCAtable) <- c("1st consultation t1.1",
                         "1st consultation t1.2",
                         "1st consultation p1.1",
                         "2nd consultation (teacher)",
                         "2nd consultation p1.1",
                         "3rd consultation (parent)",
                         "4th consultation (teacher)",
                         "4th consultation (parent)",
                         "2nd consultation p1.2",
                         "2nd consultation p1.3"
)

#Easiest thing to do is to convert the row names into a column, order the data frame using that, then put the column back to the row name
QCAtable <- tibble::rownames_to_column(QCAtable, "row_names")
order <- c("1st consultation t1.1",
           "1st consultation t1.2",
           "1st consultation p1.1",
           "2nd consultation (teacher)",
           "2nd consultation p1.1",
           "2nd consultation p1.2",
           "2nd consultation p1.3",
           "3rd consultation (parent)",
           "4th consultation (teacher)",
           "4th consultation (parent)")
QCAtable <- QCAtable %>%
  slice(match(order, QCAtable$row_names))
rownames(QCAtable) <- QCAtable[,1]
QCAtable[,1] <- NULL
QCAtable$Change <- Change
#I don't actually want to do this because this classes the feature as present if it was observed just once.
#QCAtable$SK     <- ifelse(QCAtable$SK > 0, 1, 0)
#QCAtable$EI     <- ifelse(QCAtable$EI > 0, 1, 0)
#QCAtable$IFEPW  <- ifelse(QCAtable$IFEPW > 0, 1, 0)
#QCAtable$SOP    <- ifelse(QCAtable$SOP > 0, 1, 0)
#QCAtable$EPER   <- ifelse(QCAtable$EPER > 0, 1, 0)
#QCAtable$EPUEK  <- ifelse(QCAtable$EPUEK > 0, 1, 0)
#QCAtable$PIT    <- ifelse(QCAtable$PIT > 0, 1, 0)
#QCAtable$Summ   <- ifelse(QCAtable$Summ > 0, 1, 0)
#QCAtable$UPP    <- ifelse(QCAtable$UPP > 0, 1, 0)
#QCAtable$ECV    <- ifelse(QCAtable$ECV > 0, 1, 0)
#QCAtable$DWAW   <- ifelse(QCAtable$DWAW > 0, 1, 0)
#QCAtable$CYPS   <- ifelse(QCAtable$CYPS > 0, 1, 0)
#QCAtable$SS     <- ifelse(QCAtable$SS > 0, 1, 0)
#QCAtable$IG     <- ifelse(QCAtable$IG > 0, 1, 0)


#The code below uses the built in function to perform hierarchical cluster analysis, which gives a threshold as to which the number above can be classed as 'present' and if the feature appears less than that threshold it's classed as 'absent'. hclustm = "complete" produces an extremely high threshold so I'm going to play around with it. All except average give 56.5 (presumably because average is based on the mean). distm = canberra gives 23.5 (all others give 56.5)
#I think it makes sense to choose the mean as the threshold: the mean is a meaningful number as they're counts of features so it's ratio data + there are so few cases (6) that there probably isn't enough for the fancy maths to really do anything.
#findTh(all_obs_sums$`Understanding presenting problem`, groups = 2, hclustm = "average", distm = "euclidean")

#Convert the table into a truth table but instead of the feature being classed as present for each consultation if at least one is observed, it's only classed as present if the frequency is greater than the mean.
mean(QCAtable$SK) # 0                 
mean(QCAtable$EI) # 0.16
mean(QCAtable$IFEPW) #2.66
mean(QCAtable$SOP)  #4
mean(QCAtable$EPER) #0                
mean(QCAtable$EPUEK)  #7        
mean(QCAtable$PIT) #0  
mean(QCAtable$Summ)  #7.16                        
mean(QCAtable$UPP) #35.5 
mean(QCAtable$ECV)  # 13.33 
mean(QCAtable$DWAW) # 8.16
mean(QCAtable$CYPS)  # 10.5                   
mean(QCAtable$SS) # 4             
mean(QCAtable$IG)  # 8.83
mean(QCAtable$Change) # 0.8

QCAtable$SK <- calibrate(QCAtable$SK, type = "crisp", thresholds = 1)
QCAtable$EI <- calibrate(QCAtable$EI, type = "crisp", thresholds = 0.16)    
QCAtable$IFEPW <- calibrate(QCAtable$IFEPW, type = "crisp", thresholds = 2.66)
QCAtable$SOP <- calibrate(QCAtable$SOP, type = "crisp", thresholds = 4)
QCAtable$EPER <- calibrate(QCAtable$EPER, type = "crisp", thresholds = 1) 
QCAtable$EPUEK <- calibrate(QCAtable$EPUEK, type = "crisp", thresholds = 7)    
QCAtable$PIT <- calibrate(QCAtable$PIT, type = "crisp", thresholds = 1) 
QCAtable$Summ <- calibrate(QCAtable$Summ, type = "crisp", thresholds = 7.16)
QCAtable$UPP <- calibrate(QCAtable$UPP, type = "crisp", thresholds = 35.5)
QCAtable$ECV <- calibrate(QCAtable$ECV, type = "crisp", thresholds = 13.33)  
QCAtable$DWAW <- calibrate(QCAtable$DWAW, type = "crisp", thresholds = 8.16)
QCAtable$CYPS <- calibrate(QCAtable$CYPS, type = "crisp", thresholds = 10.5)
QCAtable$SS <- calibrate(QCAtable$SS, type = "crisp", thresholds = 4)         
QCAtable$IG <- calibrate(QCAtable$IG, type = "crisp", thresholds = 8.83)
QCAtable$Change <- calibrate(QCAtable$Change, type = "crisp", thresholds = 0.8)
#You need to specify the outcome for superSubset to work. The outcome I want to explain is the change, which I currently don't have in my table. I therefore need to add that. I will need to do the same threshold thing as above. Eyeballing it, I think the cut off point should be just below 1: so if any change is seen, it's classed as present. Which makes sense, given that some of the consultations saw no change. So the massive load of superSubset code for different outcomes is irrelevant to my desired analysis, because the outcome I want to explain is change.
#ssSK <- superSubset(QCAtable, outcome = "SK", incl.cut = 0.9, cov.cut = 0.52)
#ssEI <- superSubset(QCAtable, outcome = "EI", incl.cut = 0.9, cov.cut = 0.52)
#ssIFEPW <- superSubset(QCAtable, outcome = "IFEPW", incl.cut = 0.9, cov.cut = 0.52)
#ssSOP <- superSubset(QCAtable, outcome = "SOP", incl.cut = 0.9, cov.cut = 0.52)
#ssEPER <- superSubset(QCAtable, outcome = "EPER", incl.cut = 0.9, cov.cut = 0.52)
#ssEPUEK <- superSubset(QCAtable, outcome = "EPUEK", incl.cut = 0.9, cov.cut = 0.52)
#ssPIT <- superSubset(QCAtable, outcome = "PIT", incl.cut = 0.9, cov.cut = 0.52)
#ssSumm <- superSubset(QCAtable, outcome = "Summ", incl.cut = 0.9, cov.cut = 0.52)
#ssUPP <- superSubset(QCAtable, outcome = "UPP", incl.cut = 0.9, cov.cut = 0.52)
#ssECV <- superSubset(QCAtable, outcome = "ECV", incl.cut = 0.9, cov.cut = 0.52)
#ssDWAW <- superSubset(QCAtable, outcome = "DWAW", incl.cut = 0.9, cov.cut = 0.52)
#ssCYPS <- superSubset(QCAtable, outcome = "CYPS", incl.cut = 0.9, cov.cut = 0.52)
#ssSS <- superSubset(QCAtable, outcome = "SS", incl.cut = 0.9, cov.cut = 0.52)
#ssIG <- superSubset(QCAtable, outcome = "IG", incl.cut = 0.9, cov.cut = 0.52)
#Should I remove the columns with 0 counts? No because the absence of this feature is meaningful, even if it is literally never seen.
#There are 196 possible logical causal combinations (14 features x 14)
#Sufficiency gives far fewer results than necessity but the results are all the same.
ssChange <- superSubset(QCAtable, outcome = "Change", relation = "necessity", incl.cut = 0.9, cov.cut = 0.52)
#My QCAtable has two sets of the same observation which both lead to change. Should I combine them so that each row is a unique combination of
#Remove rows without recorded change so I can eyeball the results
slimQCAtable <- QCAtable
slimQCAtable <- slimQCAtable[-c(3,4,8),]




#ggplot graph. I need to make a vector for each of the 3 variables I want to plot (stage on the x, rating on the y, and then grouped by which goal for which person). I need Stage to be a factor to get the proper x axis label.
GoalXAdult <- c("t1.1", "t1.1", "t1.1", "t1.2", "t1.2", "t1.2", "p1.1", "p1.1", "p1.1", "t2.1", "t2.1", "t2.1", "p2.2", "p2.2", "p2.2", "p2.3", "p2.3", "p2.3", "p2.1", "p2.1", "p2.1", "p3.1", "p3.1", "p3.1", "t4.1", "t4.1", "t4.1", "p4.2", "p4.2", "p4.2")
GoalXAdult <- factor(GoalXAdult, levels = c("t1.1", "t1.2", "p1.1", "t2.1", "p2.2", "p2.3", "p2.1", "p3.1", "t4.1", "p4.2"))
Stage <- c(1,2,3,
           1,2,3,
           1,2,3,
           1,2,3,
           1,2,3,
           1,2,3,
           1,2,3,
           1,2,3,
           1,2,3,
           1,2,3)
Stage <- factor(Stage, levels = c(1,2,3), labels = c("Baseline", "Expected", "Actual"))
Rating <- c(3,6,4,
            3,5,4,
            3,5,3,
            3,7,3,
            5,8,7,
            3,5,4,
            5,8,6,
            3,4,3,
            2,4,3,
            3,5,4)
tibble <- data.frame(GoalXAdult, Stage, Rating)

ggplot(tibble, aes(x=Stage, y=Rating, group = GoalXAdult)) +
  geom_line(aes(color = GoalXAdult), size = 1.2) +
  geom_point(size=3)