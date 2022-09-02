#this is so oyu can run fread and fwrite which is much better at reading large files
library(data.table)
library(dplyr)
library(plyr)
#chose your working directory containing your CTL data
setwd("~/GRAM/CTL")
#your CTL data name
CTL <- fread(file.choose())

#==============================================================================#
# Minor cleaning of data
#==============================================================================#

# Changes -1 values with NA
CTL[CTL == -1] <- NA

# Change Si to Yes
CTL$quality_val[CTL$quality_val=="Si"]<-"Yes"

#==============================================================================#
# Race and Ethnicity cleaning/combining
#==============================================================================#

# Create a new column named Hispanic
CTL$Hispanic <- fifelse(grepl("H", CTL$race_agg), 1, 
                  fifelse(CTL$race_agg == "", 2,
                    0))

CTL$Hispanic[CTL$Hispanic == 1] <- "Hispanic"
CTL$Hispanic[CTL$Hispanic == 0] <- ""
CTL$Hispanic[CTL$Hispanic == 2] <- "No Answer"

# Makes a new column removing the H from the race_agg
CTL$race_agg_no_H <- gsub('H', '', CTL$race_agg)

# Renaming and grouping different groups
CTL$race_agg_no_H <- fifelse(CTL$race_agg_no_H == "W", "White", 
                        fifelse(CTL$race_agg_no_H == "B", "Black",
                          fifelse(CTL$race_agg_no_H == "A", "Asian",
                            fifelse(CTL$race_agg_no_H == "C", "Indigenous_American",
                              fifelse(CTL$race_agg_no_H == "M", "Middle_Eastern",
                                fifelse(CTL$race_agg_no_H == "WC", "Indigenous_American",
                                  fifelse(CTL$race_agg_no_H == "WA", "Asian",
                                    fifelse(CTL$race_agg_no_H =="N", "Pacific_Islander",
                                      fifelse(CTL$race_agg_no_H == "", "No Answer",
                                        "Other_Mixed_Race")))))))))

CTL$Race_and_ethnicity <- paste(CTL$Hispanic,CTL$race_agg_no_H)

CTL$Race_and_ethnicity_agg <- ifelse(grepl("Hispanic", CTL$Race_and_ethnicity), "Hispanic", CTL$Race_and_ethnicity)

#==============================================================================#
# Gender cleaning
#==============================================================================#


gender_occur <- count(CTL, "gender_agg")

#Renaming the different gender identities
CTL$gender_agg<- fifelse(CTL$gender_agg == "M", "Male", 
                  fifelse(CTL$gender_agg == "F", "Female",
                    fifelse(CTL$gender_agg == "", "No Answer",
                      "Nonconforming")))

#fwrite(CTL, "CTL_Race_and_Gender.csv")

#==============================================================================#
# Combining CTL with Area Code 
#==============================================================================#

Area_Code <- fread(file.choose())

CTL_Area_Code <- left_join(CTL, Area_Code, by = c("npanxx" = "NPANXX"))
CTL <- 0

#fwrite(CTL_Area_Code, "CTL_with_Area_Codes.csv")

#==============================================================================#
# Removing duplicate entries
#==============================================================================#

#CTL_Area_Code <- fread(file.choose())

#Reorders the dataframe to have the largest county populations at the top, smallest at the bottom
CTL_reordered <- CTL_Area_Code[order(desc(as.numeric(as.character(CTL_Area_Code$CountyPop)))),]

CTL_Area_Code <- 0

#adds a column that numbers all the rows
CTL_reordered$row_num <- seq.int(nrow(CTL_reordered)) 

#creating a dataframe of the repeated fake calls
duplicate_df <- subset(CTL_reordered, duplicated(CTL_reordered[,3:5]))

#Takes CTL_reordered and removes from it any rows in duplicate so now only the largest county is left
CTL_with_Area_Code <- CTL_reordered[!(CTL_reordered$row_num %in% duplicate_df$row_num),]

fwrite(CTL_with_Area_Code, "CTL_Race_and_Gender_by_Area_Code.csv")


