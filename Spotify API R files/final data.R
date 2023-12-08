library(tidyverse)
library(dplyr)

Martin_Garrix <- read.csv("Martin_Garrix_v2.csv")
#view(Martin_Garrix)

David_Guetta <- read.csv("David_Guetta_final.csv")
#view(David_Guetta)

DVLM <- read.csv("Dimitri_Vegas_Like_Mike_final.csv")
#view(DVLM)

Alok <- read.csv("Alok_final.csv")
#view(Alok)

Armin_van_Buuren <- read.csv("Armin_van_Buuren_v11New.csv")
#view(Armin_van_Buuren)

# combine the data from the DJs
DJs <- rbind(Martin_Garrix, David_Guetta, DVLM, Alok, Armin_van_Buuren)
#view(DJs)
nrow(DJs)


# remove the track id and track popularity
New <- select(DJs, Ego, Alter1)
df_artist_order <- DJs[order(unlist(DJs$Alter1), decreasing = FALSE), ]
#view(df_artist_order)

lst_ego <- list()
lst_alter1 <- list()
lst_check <- list('test')



# get the alter 2 and alter 1 of each DJ and turn them into ego and alter 1
for (i in 1:nrow(DJs)) {
  if (df_artist_order$Alter1[i] != last(lst_check)) {
    collabs <- as.list(strsplit(df_artist_order$Alter2[i], ",")[[1]])
    
    if (length(collabs) >= 1) {
      for (j in 1:length(collabs)) {
        lst_alter1 <-append(lst_alter1, collabs[[j]])
        lst_ego <- append(lst_ego, df_artist_order$Alter1[i])
      }
    }
    lst_check <- append(lst_check, df_artist_order$Alter1[i])
    
  } 
}

lst_ego
lst_alter1
lst_check

total_ego <- append(lst_ego_old, lst_ego)
length(total_ego)
total_alter1 <- append(lst_alter1_old, lst_alter1)
length(total_alter1)

# create network data frame with only alter 1 and ego
df_network <- data.frame(unlist(total_ego),unlist(total_alter1))
names(df_network) = c("Ego","Alter1")
df_network
nrow(df_network)


# remove whitespaces
for (i in 1:nrow(df_network)){
  
  str_ego <- df_network$Ego[i]
  #new_str <- str_trim(str_ego)
  df_network$Ego[i] <- str_trim(str_ego)
  
  str_alter1 <- df_network$Alter1[i]
  #new_str <- str_trim(str_ego)
  df_network$Alter1[i] <- str_trim(str_alter1)
  
}

view(df_network)

# get the data with id and popularity of artist
df_artist <-read.csv("C:/Users/20193702/OneDrive - TU Eindhoven/JADS/Jaar 1/Semester 1/Social Network Analysis for DS/Spotify/All_artist_done.csv")
df_artist

# make sure that the top 5 DJs are not in there twice with the exact same collab
df2<-df_network[!(df_network$Ego=="Martin Garrix" | df_network$Ego=="David Guetta" | df_network$Ego=="Dimitri Vegas & Like Mike" | df_network$Ego=="Alok"| df_network$Ego=="Armin van Buuren"),]
view(df2)

sample <- df_network[1:1002,]
class(sample)
view(sample)

df_network <- rbind(sample, df2)
class(df_network)
view(df_network)


# create for every column 0
lst <- list()
for (x in 1:nrow(df_network)) {
  lst <- append(lst, 0)
}
lst
df_network$Ego_id <- unlist(lst)
df_network$Ego_popularity <- unlist(lst)
df_network$Alter1_id <- unlist(lst)
df_network$Alter1_popularity <- unlist(lst)



df2<-df_artist[!(df_artist$artist_id == 0),]
df2

# add id and popularity for ego
for (i in 1:nrow(df_network)) {
      for (j in 1:nrow(df2)) {
        
        if (df_network$Ego[i] == df2$Artist[j]){
          print(c('j:', j))
          id <- df2$artist_id[j]
          popularity <- df2$popularity_artist[j]
          
          df_network$Ego_id[i] <- id
          df_network$Ego_popularity[i] <- popularity
            
        }

      } 
}

# add id and popularity for alter 1
for (i in 1:nrow(df_network)) {
  for (j in 1:nrow(df2)) {
    
    if (df_network$Alter1[i] == df2$Artist[j]){
      print(c('j:', j))
      id <- df2$artist_id[j]
      popularity <- df2$popularity_artist[j]
      
      df_network$Alter1_id[i] <- id
      df_network$Alter1_popularity[i] <- popularity
      
    }
    
  } 
}



write.csv(df_network, "network_complete_no_dupl.csv", row.names=FALSE)

# select the data with collabs of 10 or more times
df_network_count <- data.table::setDT(df_network)[, .N, by=.(Ego, Alter1, Ego_id, Ego_popularity, Alter1_id, Alter1_popularity)]
df <- data.frame(df_network_count)

df_10 <- subset(df, N >= 10)
view(df_10)
nrow(df_10)

#write.csv(df_10, "df_10_no_dupl.csv", row.names=FALSE)
