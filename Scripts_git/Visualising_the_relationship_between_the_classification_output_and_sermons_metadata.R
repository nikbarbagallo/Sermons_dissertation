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
library("wesanderson")

path <- "~/Desktop/Text as data/MSc_dissertation/Images and data"
setwd("~/Desktop/Text as data/MSc_dissertation/Data")

###Divergent colours
library(RColorBrewer)
n <- 37
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))

##Pull in dataset for plots of training dataset 

##Standard training dataset
y_for_freq_plot <- read_csv("y_for_freq_plot.csv")

##Balanced (Oversampled) trasining dataset
y_res_for_freq_plot <- read_csv("y_res_for_freq_plot.csv")

##Standard training dataset bar chart
ggplot(y_for_freq_plot, aes(y_for_freq_plot$`0`, fill=y_for_freq_plot$`0`)) +
  guides(fill=guide_legend(title="Sentiment"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = c("Negative","Neutral","Positive"), breaks = c("negative","neutral","positive"), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Training Dataset Sentiment Imbalance")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  scale_fill_discrete(name="Sentiment",
                      breaks=c("negative","neutral","positive"), 
                      labels= c("Negative", "Neutral", "Positive"))+
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "Training_proportions.jpeg"), width = 8, height = 6)

##Oversampled training dataset bar chart
ggplot(y_res_for_freq_plot, aes(y_res_for_freq_plot$`0`, fill=y_res_for_freq_plot$`0`)) +
  guides(fill=guide_legend(title="Sentiment"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = c("Negative","Neutral","Positive"), breaks = c("negative","neutral","positive"), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Oversampled Training Dataset")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  scale_fill_discrete(name="Sentiment",
                      breaks=c("negative","neutral","positive"), 
                      labels= c("Negative", "Neutral", "Positive"))+
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "Training_oversampled_proportions.jpeg"), width = 8, height = 6)

##Descriptive stats

##Sentiment frequency final paragraphs
##Pull long datset
Long <- read_csv("long_Final_sermons_labeled.csv")

ggplot(Long, aes(Long$`5`, fill=Long$`5`)) +
  guides(fill=guide_legend(title="Sentiment"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = c("Negative","Neutral","Positive"), breaks = c("negative","neutral","positive"), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Labelled paragraphs Negative, Neutral and Positive proportions")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  scale_fill_discrete(name="Sentiment",
                      breaks=c("negative","neutral","positive"), 
                      labels= c("Negative", "Neutral", "Positive"))+
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "Para_Overall_Sentiment_on_Homosexuality.jpeg"), width = 8, height = 6)
count(Long, vars=Long$`5`)
##Sentiment frequency final sermons
ggplot(Final_sermons_labeled, aes(Final_sermons_labeled$Sentiment, fill=Final_sermons_labeled$Sentiment)) +
  guides(fill=guide_legend(title="Sentiment"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = c("Negative","Neutral","Positive"), breaks = c("negative","neutral","positive"), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Overall Sentiment on Homosexuality")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  scale_fill_discrete(name="Sentiment",
                      breaks=c("negative","neutral","positive"), 
                      labels= c("Negative", "Neutral", "Positive"))+
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "Overall_Sentiment_on_Homosexuality.jpeg"), width = 8, height = 6)

##Sentiment frequency by pastors
count(Pastor_Final_sermons_labeled, vars=Pastor_Final_sermons_labeled$Sentiment)
ggplot(as.data.frame(na.omit(Pastor_Final_sermons_labeled$Sentiment)), aes(na.omit(Pastor_Final_sermons_labeled$Sentiment), fill=na.omit(Pastor_Final_sermons_labeled$Sentiment))) +
  guides(fill=guide_legend(title="Sentiment"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = c("Negative","Neutral","Positive"), breaks = c("negative","neutral","positive"), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Share of pastors expressing a Negative, Neutral and Positive sentiment on homosexuality")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  scale_fill_discrete(name="Sentiment",
                      breaks=c("negative","neutral","positive"), 
                      labels= c("Negative", "Neutral", "Positive"))+
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "pastors_Sentiment_on_Homosexuality.jpeg"), width = 8, height = 6)

##Church Type

Final_sermons_labeled$denomination = car::recode(Final_sermons_labeled$denomination,
                                  "'Evangelical/Non-Denominational'='Evangelical';
                                  'Evangelical/Non-denominational'='Evangelical';
                                  'Evangelical Free'='Evangelical';
                                  'Independent/Bible'='Independent Bible';
                                  'Independent Bible'='Independent Bible'")
Final_sermons$denomination = car::recode(Final_sermons$denomination,
                            "'Evangelical/Non-Denominational'='Evangelical';
                            'Evangelical/Non-denominational'='Evangelical';
                            'Evangelical Free'='Evangelical';
                            'Independent/Bible'='Independent Bible';
                            'Independent Bible'='Independent Bible'")
##Church denomination, homosexuality
ggplot(Final_sermons_labeled, aes(Final_sermons_labeled$denomination, fill=Final_sermons_labeled$denomination)) +
  guides(fill=guide_legend(title="Church\nDenomination"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Sermons on Homosexuality by Church Denomination")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
 ## scale_fill_discrete(name="Sentiment",
       ##               labels= c("Negative", "Neutral", "Positive"))+
   ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  scale_fill_manual(values = (c(col_vector[1:28], col_vector[46:54])))
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "Homosexuality_denomination.jpeg"), width = 15, height = 10)
##Church denomination whole
ggplot(Final_sermons, aes(Final_sermons$denomination, fill=Final_sermons$denomination)) +
  guides(fill=guide_legend(title="Church\nDenomination"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Sermons by Church Denomination")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  ## scale_fill_discrete(name="Sentiment",
  ##               labels= c("Negative", "Neutral", "Positive"))+
  ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  scale_fill_manual(values = (c(col_vector[1:28], col_vector[46:54])))
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "Sermons_denomination.jpeg"), width = 15, height = 10)

 

##Extract denominations 
Hello= count(Final_sermons_labeled, vars=Final_sermons_labeled$denomination)
count(Final_sermons, vars=Final_sermons$denomination)

Hello_two= Final_sermons$denomination
Hello_two= as.character(Hello_two)
Hello_two= as.data.frame(table(Hello_two))

##Save as CSV for data management in Excell
write.csv(Hello, file = "denomination.csv")
write.csv(Hello_two, file = "denomination_homo.csv")

##Bring fixed up dataset back in
denomination_homo_whole_prop <- read_csv("denomination_homo_whole_prop.csv")

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

denomination_homo_whole_prop <- round_df(denomination_homo_whole_prop, 2)

##Plot the difference between the two

ggplot(denomination_homo_whole_prop, aes(vars, Proportion_difference)) +   
  geom_bar(aes(fill = vars), stat="identity")+
  geom_text(position = position_dodge(width=0.9), 
            aes(y=ifelse(denomination_homo_whole_prop$Proportion_difference>0, Proportion_difference+0.1, Proportion_difference-0.1), label=paste(Proportion_difference, "%"), hjust=0.45), 
            angle=0,
            size=2.25)+
  labs(title= "Difference in Church Denomination Frequency",
       subtitle= "Percental difference between the frequency of a certain church denomination among sermons expressing an opinion on homosexuality and all sermons in the dataset")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position = c(0.7,0.2),
        title = element_text(face = "bold"),
        legend.key.size = unit(0.7, "line"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        plot.subtitle=element_text(face="italic", color="black"))+
  ylab("Percental difference")+
  expand_limits(y= c(-4,+4))+
  geom_hline(yintercept = 0, colour="black", size=0.8)+
  scale_fill_manual(values = (c(col_vector[1:28], col_vector[46:54])),guide_legend(title="Church Denomination"))+
  scale_color_discrete(guide=FALSE)+
  scale_x_discrete(labels=c("AAA Total GHG emissions" = "Overall GHG emissions"))
ggsave(file.path(path, "denomination_homo_whole.jpeg"), width = 12, height = 7.41)

##Share of sermons expressing an opinion on homosexuality


hello <- c(seq(1,1,length.out=4016),seq(0,0,length.out=106407))
hello <- as.character(hello)
Final_sermons$homo_count <- hello

##Bar plot
ggplot(Final_sermons, aes(Final_sermons$homo_count, fill=Final_sermons$homo_count)) +
  guides(fill=guide_legend(title="Sermon Topic"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = element_blank(), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Proportion of Sermons on Homosexuality")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  scale_fill_discrete(name="Sermon Topic", labels= c("Does not mention\nhomosexuality", "Mentions\nhomosexuality"), breaks= c("0","1")) +
  ## scale_fill_discrete(name="Sentiment",
  ##               labels= c("Negative", "Neutral", "Positive"))+
  ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "homo_share_sermons.jpeg"), width = 8, height = 6)

##Etnicity proportion homosexuality

##Extract ethnicity 

##Hello= count(Final_sermons_labeled, vars=Final_sermons_labeled$ethnicity_cf)

"""Hello_two= Final_sermons$ethnicity_cf
Hello_two= as.character(Hello_two)
Hello_two= as.data.frame(table(Hello_two))
Hello_two$one = na.omit(Hello)
count(Hello_two, vars=Hello_two)
library(reshape2)
data.m <- melt(Projections, id.vars='Scenario')"""

Final_sermons$ethnicity_cf = car::recode(Final_sermons$ethnicity_cf,"'NA'= NA")

ggplot(as.data.frame(na.omit(Pastor_Final_sermons$ethnicity_cf)), aes(na.omit(Pastor_Final_sermons$ethnicity_cf), fill=na.omit(Pastor_Final_sermons$ethnicity_cf))) +
  guides(fill=guide_legend(title="Ethnicity"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = element_blank(), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Pastor Ethnicity (whole dataset)")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  scale_fill_discrete(name="Ethnicity", labels= c("Asian", "Latino", "White", "Black", "Other"), breaks= c("asian", "latino", "white", "black", "other")) +
  ## scale_fill_discrete(name="Sentiment",
  ##               labels= c("Negative", "Neutral", "Positive"))+
  ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "whole_ethnicity.jpeg"), width = 8, height = 6)
##Ethcniticity of pastors expressing a negative sentiment on homosexuality

Pastor_Final_sermons_labeled$new_ethnicity <- ifelse(Pastor_Final_sermons_labeled$Sentiment=="negative", Pastor_Final_sermons_labeled$ethnicity_cf, NA)

ggplot(as.data.frame(na.omit(Pastor_Final_sermons_labeled$new_ethnicity)), aes(na.omit(Pastor_Final_sermons_labeled$new_ethnicity), fill=na.omit(Pastor_Final_sermons_labeled$new_ethnicity))) +
  guides(fill=guide_legend(title="Ethnicity"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = element_blank(), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Pastor Ethnicity\n(share of sermons expressing negative sentiment on homosexuality)")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  scale_fill_discrete(name="Ethnicity", labels= c("Asian", "Latino", "White", "Black", "Other"), breaks= c("asian", "latino", "white", "black", "other")) +
  ## scale_fill_discrete(name="Sentiment",
  ##               labels= c("Negative", "Neutral", "Positive"))+
  ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "homo_neg_ethnicity.jpeg"), width = 8, height = 6)

##Gender whole

ggplot(as.data.frame(na.omit(Pastor_Final_sermons$gender_cf)), aes(na.omit(Pastor_Final_sermons$gender_cf), fill=na.omit(Pastor_Final_sermons$gender_cf))) +
  guides(fill=guide_legend(title="Gender"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = element_blank(), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Pastor Gender (whole dataset)")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  scale_fill_discrete(name="Gender", labels= c("Female", "Male", NA), breaks= c("female", "male", NA)) +
  ## scale_fill_discrete(name="Sentiment",
  ##               labels= c("Negative", "Neutral", "Positive"))+
  ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "whole_gender.jpeg"), width = 8, height = 6)
##Gender of pastors expressing a negative sentiment on homosexuality

Pastor_Final_sermons_labeled$new_gender <- ifelse(Pastor_Final_sermons_labeled$Sentiment=="negative", Pastor_Final_sermons_labeled$gender_cf, NA)

ggplot(as.data.frame(na.omit(Pastor_Final_sermons_labeled$new_gender)), aes(na.omit(Pastor_Final_sermons_labeled$new_gender), fill=na.omit(Pastor_Final_sermons_labeled$new_gender))) +
  guides(fill=guide_legend(title="Gender"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = element_blank(), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Pastor Gender\n(share of sermons expressing negative sentiment on homosexuality)")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  scale_fill_discrete(name="Gender", labels= c("Female", "Male"), breaks= c("female", "male")) +
  ## scale_fill_discrete(name="Sentiment",
  ##               labels= c("Negative", "Neutral", "Positive"))+
  ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "homo_neg_gender.jpeg"), width = 8, height = 6)



##Recode bible passage variable

Cited_bible <- data.frame(Cited_books = c("chronicles", 
  "corinthians",
  "paul",
  "john",
  "romans",
  "ruth",
  "luke",
  "matthew",
  "malachi",
  "isaiah",
  "mark",
  "song of solomon",
  "job",
  "revelation",
  "ephesians",
  "thessalonians",
  "genesis",
  "leviticus",
  "daniel",
  "kings",
  "hebrews",
  "salms",
  "hosea",
  "timothy",
  "peter",
  "proverbs",
  "micah",
  "nahum",
  "nehemiah",
  "numbers",
  "philemon",
  "philippians",
  "james",
  "jeremiah",
  "revelation",
  "galatians",
  "titus",
  "zechariah",
  "ezekiel",
  "esther",
  "exodus",
  "ezra",
  "habakkuk",
  "haggai",
  "joshua",
  "joel",
  "jonah",
  "jude",
  "judges",
  "lamentations",
  "samuel",
  "acts",
  "amos",
  "colossians",
  "deuteronomy",
  "ecclesiastes"),
Cited_books_labels = c("Chronicles", 
                        "Corinthians",
                        "Paul",
                        "John",
                        "Romans",
                        "Ruth",
                        "Luke",
                        "Matthew",
                        "Malachi",
                        "Isaiah",
                        "Mark",
                        "Song of solomon",
                        "Job",
                        "Revelation",
                        "Ephesians",
                        "Thessalonians",
                        "Genesis",
                        "Leviticus",
                        "Daniel",
                        "Kings",
                        "Hebrews",
                        "Salms",
                        "Hosea",
                        "Timothy",
                        "Peter",
                        "Proverbs",
                        "Micah",
                        "Nahum",
                        "Nehemiah",
                        "Numbers",
                        "Philemon",
                        "Philippians",
                        "James",
                        "Jeremiah",
                        "Revelation",
                        "Galatians",
                        "Titus",
                        "Zechariah",
                        "Ezekiel",
                        "Esther",
                        "Exodus",
                        "Ezra",
                        "Habakkuk",
                        "Haggai",
                        "Joshua",
                        "Joel",
                        "Jonah",
                        "Jude",
                        "Judges",
                        "Lamentations",
                        "Samuel",
                        "Acts",
                        "Amos",
                        "Colossians",
                        "Deuteronomy",
                        "Ecclesiastes"),
stringsAsFactors = FALSE)

Final_sermons_labeled$cited_books <- 0


for ( i in 1:nrow(Cited_bible) ) {
  
  pattern     <- Cited_bible$Cited_books[i]
  replacement <- Cited_bible$Cited_books_labels[i]
  
  Final_sermons_labeled$cited_books <- ifelse(grepl(pattern, Final_sermons_labeled$bible_passage, ignore.case = TRUE), replacement, Final_sermons_labeled$cited_books)
}

Final_sermons$cited_books <- 0
for ( i in 1:nrow(Cited_bible) ) {
  
  pattern     <- Cited_bible$Cited_books[i]
  replacement <- Cited_bible$Cited_books_labels[i]
  
  Final_sermons$cited_books <- ifelse(grepl(pattern, Final_sermons$bible_passage, ignore.case = TRUE), replacement, Final_sermons$cited_books)
}

Final_sermons <- ungroup(Final_sermons)

##Transform 0 to NA
Final_sermons_labeled$cited_books <- ifelse(Final_sermons_labeled$cited_books==0,
                                            NA,
                                            Final_sermons_labeled$cited_books)
Final_sermons$cited_books <- ifelse(Final_sermons$cited_books==0,
                                    NA,
                                    Final_sermons$cited_books)
AAA = count(Final_sermons_labeled, vars=cited_books)                                                                                                        
BBB = count(Final_sermons, vars=cited_books) 

##Plot books_cited and export data to create table in excell
                                                       
##Whole dataset
ggplot(Final_sermons, aes(Final_sermons$cited_books, fill=Final_sermons$cited_books)) +
  guides(fill=guide_legend(title="Biblical books"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = element_blank(), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Cited Religious Text (whole dataset)")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count",
            angle = 270,
            hjust = +1,
            vjust = +0.50)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  ##scale_fill_discrete(name="Gender", labels= c("Female", "Male", NA), breaks= c("female", "male", NA)) +
  ## scale_fill_discrete(name="Sentiment",
  ##               labels= c("Negative", "Neutral", "Positive"))+
  ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = c(0.85,0.85), 
        legend.direction = "vertical",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        title = element_text(face = "bold"),
        legend.key.size = unit(0.7, "line"))+
  expand_limits(y= c(0,+0.11))+
  scale_fill_manual(values = (c(col_vector[1:28], col_vector[46:71])))
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "whole_cited_book.jpeg"), width = 15, height = 10)

##Homo
ggplot(Final_sermons_labeled, aes(Final_sermons_labeled$cited_books, fill=Final_sermons_labeled$cited_books)) +
  guides(fill=guide_legend(title="Biblical books"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = element_blank(), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Cited Religious Text\n(sermons expressing sentiment on homosexuality)")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count",
            angle = 270,
            hjust = +1,
            vjust = +0.50)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  ##scale_fill_discrete(name="Gender", labels= c("Female", "Male", NA), breaks= c("female", "male", NA)) +
  ## scale_fill_discrete(name="Sentiment",
  ##               labels= c("Negative", "Neutral", "Positive"))+
  ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = c(0.25,0.75), 
        legend.direction = "vertical",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        title = element_text(face = "bold"),
        legend.key.size = unit(0.7, "line"))+
  expand_limits(y= c(0,+0.11))+
  scale_fill_manual(values = (c(col_vector[1:28], col_vector[46:71])))
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "homo_cited_book.jpeg"), width = 15, height = 10)

##Extract cited book 
book_labelled <- count(Final_sermons_labeled, vars=Final_sermons_labeled$cited_books)
book <- count(Final_sermons, vars=Final_sermons$cited_books)

##Save as CSV for data management in Excell
write.csv(book, file = "book.csv")
write.csv(book_labelled, file = "book_labelled.csv")

##Bring fixed up dataset back in
cited_books_homo_whole_prop <- read_csv("book_proportions.csv")

##Plot difference between cited books
ggplot(cited_books_homo_whole_prop, aes(cited_books_homo_whole_prop$vars, cited_books_homo_whole_prop$difference)) +   
  geom_bar(aes(fill = vars), stat="identity")+
  geom_text(position = position_dodge(width=0.9), 
            aes(y=ifelse(cited_books_homo_whole_prop$difference>0, difference+0.1, difference-0.1), label=paste(difference, "%"), hjust=0.45), 
            angle=0,
            size=2.25)+
  labs(title= "Difference in the proportion of bibilical books cited",
       subtitle= "Percental difference between the propotion of biblical books cited in the whole Sermon-Central dataset and that of books cited in the homosexuality subset")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position = c(0.2,0.2),
        title = element_text(face = "bold"),
        legend.key.size = unit(0.7, "line"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        plot.subtitle=element_text(face="italic", color="black"))+
  ylab("Percental difference")+
  expand_limits(y= c(-4,+4))+
  geom_hline(yintercept = 0, colour="black", size=0.8)+
  scale_fill_manual(values = (c(col_vector[1:28], col_vector[46:71])),guide_legend(title="Biblical books"))+
  scale_color_discrete(guide=FALSE)
ggsave(file.path(path, "cited_book_homo_whole.jpeg"), width = 15, height = 9.41)

##Church rating, homosexuality
ggplot(Final_sermons_labeled, aes(Final_sermons_labeled$stars, fill=Final_sermons_labeled$stars)) +
  guides(fill=guide_legend(title="Sermon\nstar rating"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), position = "dodge")+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels= c("Five stars", "Four stars", "Four and a\nhalf stars", "One and a\nHalf stars", "One star", "Three and a\nhalf stars", "Three stars", "Two and a\nhalf stars", "Two stars"),
                   breaks= c("fivestar","fourhalfstar","fourstar", "onehalfstar", "onestar", "threehalfstar", "threestar", "twohalfstar", "twostar"), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Sermons on Homosexuality by star rating")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  scale_fill_discrete(name="Sermon\nstar rating",
                      labels= c("Five stars", "Four stars", "Four and a\nhalf stars", "One and a\nHalf stars", "One star", "Three and a\nhalf stars", "Three stars", "Two and a\nhalf stars", "Two stars"),
                      breaks= c("fivestar","fourhalfstar","fourstar", "onehalfstar", "onestar", "threehalfstar", "threestar", "twohalfstar", "twostar"))+
  ## scale_fill_discrete(name="Sentiment",
  ##               labels= c("Negative", "Neutral", "Positive"))+
  ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "Homosexuality_star_rating.jpeg"), width = 8, height = 6)

##Church rating whole
ggplot(Final_sermons, aes(Final_sermons$stars, fill=Final_sermons$stars)) +
  guides(fill=guide_legend(title="Sermon\nstar rating"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), position = "dodge")+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels= c("Five stars", "Four stars", "Four and a\nhalf stars", "One and a\nHalf stars", "One star", "Three and a\nhalf stars", "Three stars", "Two and a\nhalf stars", "Two stars"),
                   breaks= c("fivestar","fourhalfstar","fourstar", "onehalfstar", "onestar", "threehalfstar", "threestar", "twohalfstar", "twostar"), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1), 
        legend.position = "right", 
        legend.direction = "vertical")+
  ggtitle("Sermons by star rating\n(whole dataset)")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count", 
            vjust = -0.25)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  scale_fill_discrete(name="Sermon\nstar rating",
                      labels= c("Five stars", "Four stars", "Four and a\nhalf stars", "One and a\nHalf stars", "One star", "Three and a\nhalf stars", "Three stars", "Two and a\nhalf stars", "Two stars"),
                      breaks= c("fivestar","fourhalfstar","fourstar", "onehalfstar", "onestar", "threehalfstar", "threestar", "twohalfstar", "twostar"))+
  ## scale_fill_discrete(name="Sentiment",
  ##               labels= c("Negative", "Neutral", "Positive"))+
  ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = "bottom", legend.direction = "horizontal")
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "whole_star_rating.jpeg"), width = 8, height = 6)

##Bible book if negative sentiment homosexuality

Final_sermons_labeled$new <- ifelse(Final_sermons_labeled$Sentiment=="negative", Final_sermons_labeled$cited_books, NA)

ggplot(as.data.frame(na.omit(Final_sermons_labeled$new)), aes(na.omit(Final_sermons_labeled$new), fill=na.omit(Final_sermons_labeled$new))) +
  guides(fill=guide_legend(title="Cited book"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels = element_blank(), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  ggtitle("Cited Religious Text\n(share of sermons expressing negative sentiment on homosexuality)")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count",
            angle = 270,
            hjust = +1,
            vjust = +0.50)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  ##scale_fill_discrete(name="Gender", labels= c("Female", "Male", NA), breaks= c("female", "male", NA)) +
  ## scale_fill_discrete(name="Sentiment",
  ##               labels= c("Negative", "Neutral", "Positive"))+
  ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = c(0.3,0.8), 
        legend.direction = "horizontal",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        title = element_text(face = "bold"),
        legend.key.size = unit(0.7, "line"))+
  expand_limits(y= c(0,0.11))+
  scale_fill_manual(values = sample(col_vector,55))
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "homo_cited_book_negative.jpeg"), width = 15, height = 10)

##Negative star rating

Final_sermons_labeled$new_rating <- ifelse(Final_sermons_labeled$Sentiment=="negative", Final_sermons_labeled$stars, NA)

ggplot(as.data.frame(na.omit(Final_sermons_labeled$new_rating)), aes(na.omit(Final_sermons_labeled$new_rating), fill=na.omit(Final_sermons_labeled$new_rating))) +
  guides(fill=guide_legend(title="Star Rating"))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]))+
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  scale_x_discrete(labels= c("Five stars", "Four stars", "Four and a\nhalf stars", "One and a\nHalf stars", "One star", "Three and a\nhalf stars", "Three stars", "Two and a\nhalf stars", "Two stars"),
                   breaks= c("fivestar","fourhalfstar","fourstar", "onehalfstar", "onestar", "threehalfstar", "threestar", "twohalfstar", "twostar"), name = NULL)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  ggtitle("Sermons' star rating\n(share of sermons expressing negative sentiment on homosexuality)")+
  geom_text(aes(y = ((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                label = scales::percent((..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])), 
            stat = "count",
            angle = 270,
            hjust = +1,
            vjust = +0.50)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "darkgrey"))+
  ##scale_fill_discrete(name="Gender", labels= c("Female", "Male", NA), breaks= c("female", "male", NA)) +
  ## scale_fill_discrete(name="Sentiment",
  ##               labels= c("Negative", "Neutral", "Positive"))+
  ####                   breaks=c("negative","neutral","positive"), 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        title = element_text(face = "bold"),
        legend.key.size = unit(0.7, "line"))+
  expand_limits(y= c(0,0.45))+
  scale_fill_manual(values = sample(col_vector,9),
                      name="Sermon\nstar rating",
                      labels= c("Five stars", "Four stars", "Four and a half stars", "One and a Half stars", "One star", "Three and a half stars", "Three stars", "Two and a half stars", "Two stars"),
                      breaks= c("fivestar","fourhalfstar","fourstar", "onehalfstar", "onestar", "threehalfstar", "threestar", "twohalfstar", "twostar"))
ggsave(file.path("~/Desktop/Text as data/MSc_dissertation/Images and data",
                 "homo_star_rating_negative.jpeg"), width = 8, height = 6)

##Save out datasets
write.csv(Final_sermons_labeled, file = "Final_sermons_labeled.csv")
write.csv(Final_sermons, file = "Final_sermons.csv")
write.csv(AAA, file = "book_cited_homo_sermons.csv")
write.csv(BBB, file = "book_cited_whole_sermons.csv")


