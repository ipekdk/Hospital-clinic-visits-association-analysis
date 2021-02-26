# This is the main code for Chapter: Using Data Mining Techniques for Designing Patient-Friendly Hospitals

# It takes hospital visit data (hospitaldata_small.xlsx), preprocesses it and makes association mining
# Since the dataset is only a very small portion of the data in the chapter, the results of the analyses will be completely different from the chapter.
# Please cite as: 

#library loading, please install them before loading if they are not installed.
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(readr)
library(magrittr)
library(writexl)
library(readxl)
library(rio)

# The data set should have at least these columns: submission date, patient ID and visited clinic
# A small dataset is given in Github files 
# As excel file: (https://github.com/ipekdk/Hospital-clinic-visits-association-analysis/blob/main/hospitaldata_small.xlsx)
# As Rdata file: (https://github.com/ipekdk/Hospital-clinic-visits-association-analysis/blob/main/hospitaldata_small.Rdata)
# You should download them first in your computer to import.

# Importing the data
data <- rio::import("C:/.../hospitaldata_small.xlsx")
# or use this to import
data <- read_excel("C:/.../hospitaldata_small.xlsx")

### Save your data in Rdata format if you want
# save.image("C:/.../hospitaldata_small.RData")

### Load Dataset in Rdata format
# load("C:/.../hospitaldata_small.RData")

################################################################################

# Data preprocessing begins from here
# data set is in "data" 

# Sort data by patientID and submission_date

data<-arrange(data, patientID,submission_date)

# convert submission_date to date format

data$submission_date<-as.Date(data$submission_date)


# Assign a visitID by grouping visits (rows) by patientID and submission_date
DT <- data.table(data, key="patientID,submission_date")
DT[, visitID:=.GRP, by=key(DT)]

# arrange data by clinic 

DT<-arrange(DT, clinic)
  
# if you want to use IDs instead of clinic names, you can use the code below, otherwise omit it. (optional) 

#DT <- data.table(DT, key="clinic")
#DT[, clinicID:=.GRP, by=key(DT)]

# Delete single time visits (we need multiple visits to associate clinics)

DT <-  DT %>% group_by(visitID) %>% filter(n()>1) #

# You may examine demographic data (optional)
  
#freq(DT$gender)
#freq(DT$insurance)

################################################################################
# Association analysis begins from here
# data set is in "DT" 

# list visited clinics next to visitID (for arules transaction format) 

DT2 <- as.data.table(DT)
DT2 <- DT2[, list(clinic=list(clinic)), by=visitID]

### Save your formatted data in Rdata format if you want
# save.image("C:/.../hospitaldata_mining_ready.RData")

### Load Dataset in Rdata format
# load("C:/.../hospitaldata_mining_ready.RData")

# set up "transaction" object for arules library

library("arules")
trans<-as.list(DT2$clinic)
trans <- as(trans, "transactions")

# Frequent itemset mining

# visit the link below for determining limits for conf and supp 
# https://stackoverflow.com/questions/43588163/how-can-we-find-support-and-confident-in-apriori-for-rules
  
# Finds and lists the top 10 frequently visited clinics, plots them, and saves to an excel file.
dev.new()
itemFrequencyPlot(trans, topN=10, type="absolute", main="Item Frequency") # plot frequent items  

# mine freq items and association rules with eclat algorithm
frequentItems <- eclat (trans, parameter = list(supp = 0.001, maxlen = 15)) # calculates support for frequent items
fr_items<-DATAFRAME(frequentItems)
write_xlsx(x = fr_items, "freq_items.xlsx")

rules_eclat <- ruleInduction(frequentItems, trans, confidence = 0.5)
rules_eclat <- sort (rules_eclat, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
length(rules_eclat)
inspect((rules_eclat)) # show the support, lift and confidence for all rules
write_xlsx(as(rules_eclat, "data.frame"), "rules_eclat.xlsx")
  
# mine association rules with apriori algorithm

rules_apriori <- apriori (trans, parameter = list(supp = 0.001, conf = 0.5,maxlen = 5)) # Min Support as 0.001, confidence as 0.8.
rules_conf <- sort(rules_apriori, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
length(rules_conf)
inspect((rules_conf)) # show the support, lift and confidence for all rules
write_xlsx(as(rules_conf, "data.frame"), "rules_conf.xlsx")

# clinics with high "lift":
rules_lift <- sort(rules_apriori, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift)) # show the support, lift and confidence for all rules  
write_xlsx(as(rules_lift, "data.frame"), "rules_lift.xlsx")

# finding overlapping rules   
overlapping_Rules <- which(colSums(is.subset(rules_apriori, rules_apriori)) > 1) # get subset rules in vector

#Plotting and graphing rules

library(arulesViz)
rules<-rules_apriori #you may take rules_eclat as well.

## The following techniques work better with fewer rules. "subset" takes a smaller subset of the rule set.
subrules <- subset(rules, lift>25)
rules<-subrules

#Figure1
dev.new()
plot(rules, method = "two-key plot")
#Figure2
dev.new()
plot(rules, method = "graph")
#Figure3
dev.new()
plot(rules, method = "grouped")
#Figure4
dev.new()
plot(rules, method = "paracoord", control = list(reorder = TRUE))

#### Below this line, there are other plot and graph options, which are not used in the Chapter.

## igraph layout generators can be used (see ? igraph::layout_)
plot(rules, method="graph", control=list(layout=igraph::in_circle()))
plot(rules, method="graph", control=list(layout=igraph::with_graphopt(spring.const=5, mass=50)))

## 2D matrix with shading
plot(subrules, method="matrix", measure="lift")
plot(subrules, method="matrix", measure="lift", control=list(reorder="measure"))

## matrix with two measures
plot(subrules, method="matrix", measure=c("lift", "confidence"))
plot(subrules, method="matrix", measure=c("lift", "confidence"), 
     control=list(reorder="similarity"))

# export rules to examine with Gephi
saveAsGraph(head(rules, n = 13, by = "lift"), file = "rules.graphml")