##Increase max print
options(max.print = 100000000)
##Set Working Directory
setwd("~/Desktop/Text as data/MSc_dissertation/Data")
##Load necessary packages
library(reshape2)
library("car")
library(readr)
library(MASS)
library(nnet)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)

##Open paragpraphed dataset
gay_para_filtered <- read_csv("gay_para_filtered_NOSODOM.csv", 
                              col_types = cols(X1 = col_skip()))
names(gay_para_filtered) <- c("ID_HTML")
colnames(gay_para_filtered)[which(is.na(names(gay_para_filtered)))] <- "PARAG"
names(gay_para_filtered)[names(gay_para_filtered)=="PARAG"]<-sprintf("PARAG%d",1:sum(names(gay_para_filtered)=="PARAG"))

##randomely select 1000 sermons
gay_para_1000 <- gay_para_filtered[sample(nrow(gay_para_filtered), 1000), ]
##Go from wide to london format
###data_long <- gather(gay_para_1000, key = ID_0NE, value = ID, -ID, na.rm = TRUE, factor_key = TRUE)
data_long <- melt(gay_para_1000, "ID_HTML", na.rm = TRUE)

##Tidy up console
gay_para_1000 <- data_long
remove(data_long)

##Re-order dataset according to HTML so that paragraphs follow logically
gay_para_1000 <- gay_para_1000[order(gay_para_1000$ID_HTML),]

##write to CSV 
write.csv(gay_para_1000, file = "gay_para_1000_NOSODOM.csv")

##Open 1000 csv + attach labels + clean up labels dataset
label_sermon <- read_csv("label_sermon.csv", 
                    col_types = cols(Agreement = col_skip(), 
                                     `Created At` = col_skip(), `Created By` = col_skip(), 
                                     `Dataset Name` = col_skip(), `Project Name` = col_skip(), 
                                     Reviews = col_skip(), `Seconds to Label` = col_skip()))
#reorder by column index
sermon_labelled <- label_sermon[,c(1,4,3,2)]
write.csv(sermon_labelled, file="sermon_labelled.csv")
##reload whole paragraphed dataset
gay_para_filtered <- read_csv("gay_para_filtered_NOSODOM.csv", 
                              col_types = cols(X1 = col_skip()))
names(gay_para_filtered) <- c("ID_HTML")

colnames(gay_para_filtered)[which(is.na(names(gay_para_filtered)))] <- "PARAG"

names(gay_para_filtered)[names(gay_para_filtered)=="PARAG"]<-sprintf("PARAG%d",1:sum(names(gay_para_filtered)=="PARAG"))

gay_para_long <- melt(gay_para_filtered, "ID_HTML", na.rm = TRUE)

##Re-order dataset according to HTML so that paragraphs follow logically
gay_para_long <- gay_para_long[order(gay_para_long$ID_HTML),]

##write to CSV 
write.csv(gay_para_long, file = "gay_para_long.csv")


