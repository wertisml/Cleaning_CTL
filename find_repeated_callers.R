#this is so oyu can run fread and fwrite which is much better at reading large files
library(data.table)
library(dplyr)
#chose your working directory containing your CTL data
setwd("~/GRAM/CTL")
#your CTL data name
CTL <- fread("CTL_Cleaned.csv")
#the number of times each actor_id called into the CTL
n_occur <- data.frame(table(CTL$actor_id))
#from n_occur repeated looks for each row of Val1 for a total of times as it is stated in Freq,
#so it will look for Bob 3 times in the origional dataframe, df and put the whole row into a new 
#dataframe but only of those who called more than once
duplicates <- CTL[CTL$actor_id %in% n_occur$Var1[n_occur$Freq > 1],]

#if you are going to create a csv of the repeated callers
fwrite(duplicates, "duplicated_users.csv")




################################################################################



#to understand what is being done above but on a smaller scale
df <- data.frame(
  Name = c("Bob", "Bob", "Bob", "Tom", "Jim", "Jim", "Tim", "Tim", "Tim", "Luc"),
  Number = c(10, 10, 11, 8, 8, 6, 6, 6, 4, 9),
  Hi = c(1, 4, 2, 5, 6, 2, 4, 6, 1, 0),
  poop = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1)
)
#counts the number of times that a name is repeated
n_occur <- data.frame(table(df$Name))
#from n_occur repeated looks for each row of Val1 for a total of times as it is stated in Freq,
#so it will look for Bob 3 times in the origional dataframe, df and put the whole row into a new 
#dataframe
repeated <- df[df$Name %in% n_occur$Var1[n_occur$Freq > 1],]

df1 <- df[order(desc(as.numeric(as.character(df$Number)))),]

duplicate_column1_column2 <- subset(df1, duplicated(df1[,1:2]))

df2 <- df1[!(df1$poop %in% duplicate_column1_column2$poop),]

df[df=="Bob"]<-"bob"

n = 1
for (i in df$Name){
  print(i)
  if (i == "Bob"){
   df$Name[n] <- "bob"
   n=n+1
  }
}

n = 1
for (i in df$Name){
  fifelse(i == "Bob", df$Name[n] <- "Robert", 
         fifelse(i == "Tom", df$Name[n] <- "Thomas",
                fifelse(i == "Jim", df$Name[n] <- "James",
                       fifelse(i == "Tim", df$Name[n] <- "Timothy", df$Name[n] <- i))))
  n=n+1
}

n = 1
for (i in df$Name){
  fifelse(i == "Bob", df$Name, df$Name[n] <- "Robert")
  n = n+1
}



df$Hi[df$Number < 9] <- 99


fifelse(df$Name == "Bob", "Robert", 
        fifelse(df$Name == "Tom", "Thomas", "James"))
