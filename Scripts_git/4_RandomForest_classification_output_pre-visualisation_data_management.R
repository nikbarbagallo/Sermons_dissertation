options(max.print = 100000000)
##install.packages("reshape2")
library(reshape2)
library("car")
library(readr)
library(MASS)
library(nnet)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
setwd("~/Desktop/Text as data/MSc_dissertation/Data")

##Read in the dataset 
Final_sermons_labeled <- read_csv("Final_sermons_labeled.csv", 
                                  col_types = cols(`0` = col_skip(), `1` = col_skip(), 
                                                   `3` = col_skip(), `4` = col_skip(), 
                                                   X1 = col_skip()))

names(Final_sermons_labeled) <- c("ID_HTML","Sentiment")

##Translate from long to wide
Wide_Final_sermons_labeled <- dcast(Final_sermons_labeled, ID_HTML~Sentiment, value.var='Sentiment')
Wide_Final_sermons_labeled <- na.omit(Wide_Final_sermons_labeled)
Wide_Final_sermons_labeled$Sentiment <- ifelse(Wide_Final_sermons_labeled$negative >0, "negative",
                                                       ifelse(Wide_Final_sermons_labeled$positive >0, "positive",
                                                              ifelse(Wide_Final_sermons_labeled$neutral >0, "neutral",
                                                                            NA  )))
##Compute sentiment frequency
count(Wide_Final_sermons_labeled, vars=Wide_Final_sermons_labeled$Sentiment)

##Pull in whole dataset
sermons_for_merged <- read_csv("sermons_for_merged.csv", 
                               col_types = cols(X1 = col_skip(), abortion = col_skip(), 
                                                agg_tags = col_skip(), american_values = col_skip(), 
                                                antichrist = col_skip(), christian_elections = col_skip(), 
                                                church_name = col_skip(), holiness = col_skip(), 
                                                pastor_name = col_skip(), service_type = col_skip(), 
                                                tenure = col_skip()))

Final_sermons <- sermons_for_merged[c(1:22,50)]
remove(sermons_for_merged)

##Combine final sermons with sermons' sentiment and drop the rest of the dataset
Final_sermons <- as.data.table(Final_sermons)
Final_sermons_labeled <- as.data.table(Wide_Final_sermons_labeled)
Final_sermons_labeled <- merge(Final_sermons, Final_sermons_labeled,
                                   by.x =  "url", 
                                   by.y = "ID_HTML", 
                                   all = FALSE)
##Drop unsued dataset
remove(Wide_Final_sermons_labeled)

##Compute sentiment frequency
count(Final_sermons_labeled, vars=Final_sermons_labeled$Sentiment)

##Save two dataset to CSV for future comparison
write.csv(Final_sermons_labeled, file = "Final_sermons_labeled.csv")
write.csv(Final_sermons, file = "Final_sermons.csv")

##Bring in the dataset again
Final_sermons_labeled <- read_csv("Final_sermons_labeled.csv")
count(Final_sermons_labeled, vars=Final_sermons_labeled$lat,Final_sermons_labeled$long)
Address=count(Final_sermons_labeled, vars=Final_sermons_labeled$address)
Dat=count(Final_sermons_labeled, vars=Final_sermons_labeled$date_posted)

##Bring in the labellled and unlabelled dataset and attach pastors names

Final_sermons_labeled <- read_csv("Final_sermons_labeled.csv")
Final_sermons <- read_csv("Final_sermons.csv")
##Extracting pastor_names
One_Sermons_merged_CSV <- Sermons_merged_CSV[c("url", "pastor_name")]
One_Sermons_merged_CSV <- as.data.table(One_Sermons_merged_CSV)
Final_sermons <- as.data.table(Final_sermons)
Final_sermons_labeled <- as.data.table(Final_sermons_labeled)
##Combine final sermons and final sermons labelled with pastors' names

Pastor_Final_sermons_labeled <- merge(One_Sermons_merged_CSV, Final_sermons_labeled,
                               by.x =  "url", 
                               by.y = "url", 
                               all = FALSE)
Pastor_Final_sermons_labeled <- Pastor_Final_sermons_labeled[!duplicated(Pastor_Final_sermons_labeled$pastor_name),]
Pastor_Final_sermons <- merge(One_Sermons_merged_CSV, Final_sermons,
                              by.x =  "url", 
                              by.y = "url", 
                              all = FALSE)
Pastor_Final_sermons <- Pastor_Final_sermons[!duplicated(Pastor_Final_sermons$pastor_name),]
##Save datsets
write.csv(Pastor_Final_sermons_labeled, file = "Pastor_Final_sermons_labeled.csv")
write.csv(Pastor_Final_sermons, file = "Pastor_Final_sermons.csv")



