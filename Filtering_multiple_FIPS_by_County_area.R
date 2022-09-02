library(data.table)
library(dplyr)

Area_Code <- fread(file.choose())
CTL <- fread(file.choose())

CTL_Area_Code <- left_join(CTL, Area_Code, by = c("npanxx" = "NPANXX"))
CTL <- 0

#fwrite(CTL_Area_Code, "CTL_with_Area_Codes.csv")

#CTL_Area_Code <- fread(file.choose())
#Reorders the dataframe to have the largest county area at the top, smallest at the bottom
CTL_reordered <- CTL_Area_Code[order(desc(as.numeric(as.character(CTL_Area_Code$Area)))),]
CTL_Area_Code <- 0
#adds a column that numbers all the rows
CTL_reordered$row_num <- seq.int(nrow(CTL_reordered)) 

#creating a dataframe of the repeated fake calls
duplicate_df <- subset(CTL_reordered, duplicated(CTL_reordered[,3:5]))
#Takes CTL_reordered and removes from it any rows in duplicate so now only the largest county is left
CTL_with_Area_Code <- CTL_reordered[!(CTL_reordered$row_num %in% duplicate_df$row_num),]

fwrite(CTL_with_Area_Code, "CTL_ranked_by_Area.csv")