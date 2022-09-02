#this is so oyu can run fread and fwrite which is much better at reading large files
library(data.table)
library(dplyr)
library(plyr)
#chose your working directory containing your CTL data
setwd("~/GRAM/CTL")
#your CTL data name
CTL <- fread(file.choose())
#Changes -1 values with NA
CTL[CTL == -1] <- NA
#change Si to Yes
CTL$quality_val[CTL$quality_val=="Si"]<-"Yes"
#Makes a new column called Hispanic based on if rows in column race_agg have a H
CTL$Hispanic <- fifelse(grepl("H", CTL$race_agg), 1, 0)
#Makes a new column removing the H from the race_agg
CTL$race_agg_no_H <- gsub('H', '', CTL$race_agg)
#renaming and grouping different groups
CTL$race_agg_no_H <- fifelse(CTL$race_agg_no_H == "W", "White", 
                          fifelse(CTL$race_agg_no_H == "B", "Black",
                               fifelse(CTL$race_agg_no_H == "A", "Asian",
                                   fifelse(CTL$race_agg_no_H == "C", "Indigenous_American",
                                       fifelse(CTL$race_agg_no_H == "M", "Middle_Eastern",
                                           fifelse(CTL$race_agg_no_H == "WC", "Indigenous_American",
                                                fifelse(CTL$race_agg_no_H == "WA", "Asian",
                                                   fifelse(CTL$race_agg_no_H =="N", "Pacific_Islander",
                                                        "Other_Mixed_Race"))))))))


#gender
gender_occur <- count(CTL, "gender_agg")
#Renaming the different gender identities
CTL$gender_agg<- fifelse(CTL$gender_agg == "M", "Male", 
         ifelse(CTL$gender_agg == "F", "Female",
                "Nonconforming"))

fwrite(CTL, "CTL_Race_and_Gender.csv")
