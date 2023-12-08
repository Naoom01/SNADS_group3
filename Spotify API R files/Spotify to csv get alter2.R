library(tidyverse)
library(spotifyr)

#get this from the SPOTIFY API Dashboard 
Sys.setenv(SPOTIFY_CLIENT_ID = "Client_ID")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "Client_Secret")

token <- spotifyr::get_spotify_access_token()
#Check if a token is created, if not check/renew your client id and client secret
token

#This is used in the big for loop
main_artist = 'Alok'

#get the csv file created in 'Spotify to csv EGo and alter 1' for the correct artist
David_Guetta <- read.csv("Alok_main.csv")

view(David_Guetta)

#putting the dataframe in alphabetic order
David_Guetta_order <- David_Guetta[order(David_Guetta$Alter1, decreasing = FALSE), ]

#remove the duplicates that are created with ordering
D_G <- David_Guetta_order[!duplicated(David_Guetta_order$track_id), ]

#reset index
row.names(D_G) <- NULL

#This is so I did not have to adjust all the names in the rest from the code
David_Guetta <- D_G

#add 0 for all the columns that need to be filled with the Alter 1 collabs (Alter2)
#do not run this again after starting the for loop!
lst <- list()
for (x in 1:nrow(David_Guetta)) {
  lst <- append(lst, 0)
}
lst
David_Guetta$Alter2 <- lst
David_Guetta$Alter2_track_id <- lst
David_Guetta$Alter2_popularity <- lst


#create a list with all colabs of the artist (David Guetta)
collab_list <- list()
for (x in 1:nrow(David_Guetta)) {
      collab_list <- append(collab_list, David_Guetta$Alter1[x])
}

#Remove dupicate so we don't have to run an artist multiple times (and save time and requests)
collab_list_no_dupl <- collab_list[!duplicated(collab_list)]
collab_list_no_dupl


# creating empty lists
collaboration_list <- list()
name_list <- list()
track_id_list <- list()
popularity_list <- list()


#This is where we find the collabs of the collabs (alter 2)
#Change the number before the : if you run into an error, we will just skip the artists
#That given an error, we have too many anyway
for (y in 106:length(collab_list_no_dupl)) {
  
  #This print statement is to chech where your alogirhtm is if runs into an error
  #so next time you run it adjust the above number to start from y or y+1
  print(paste0("print y:", y))
  
  #This gets the name of the artist
  alter1 = unlist(collab_list_no_dupl)[y]
  alter1

  artist_name = alter1
  artist <- get_artist_audio_features(artist_name, c("album", "single"))
  artist


  #remove duplicates based on track name & remix and mixes
  artist <- artist[!duplicated(artist$track_name), ]
  
  artist <- artist[!grepl("Remix", artist$track_name),]
  
  artist <- artist[!grepl("Mix", artist$track_name),]
  
  #This gets the list of data frames which contain all the artists on tracks with chosen artist
  artists <- artist$artists
  
  print('Start first For-loop')
#below we get the artists with who the artist colaborated with and gives a list with them also for track_id and popularity
  for (x in 1:length(artists)) {
    #This print statement is too see at which x it gets stuck
    #print(paste0('print x:', x))
    for (i in 1:length(artists[[x]]$name)) {
      #Important to also put the main artist in here, because they are already linked any way
      if (artists[[x]]$name[i] != artist_name & artists[[x]]$name[i] != main_artist) {
        #This print statement is too see at which x it gets stuck
        #print(paste0('print i:', i))
        collaboration_list <- append(collaboration_list, artists[[x]]$name[i])
        name_list <- append(name_list, artist_name)
        track_id <- artist$track_id[x]
        track_id_list <- append(track_id_list, track_id)
        track <- get_track(track_id)
        popularity <- track$popularity
        popularity_list <- append(popularity_list, popularity)
      }
    }
  }
  
  print('First For-loop completed & start second')
  #This adds the list created above to the right cell in the dataframe
  for (x in 1:nrow(David_Guetta)) {
    #print(paste0('second x:', x))
    if (David_Guetta$Alter1[x] == alter1) {
      if (David_Guetta$Alter2[x] == 0) {
        #it adds the lists as strings
        David_Guetta$Alter2[x] <- toString(collaboration_list)
        David_Guetta$Alter2_track_id[x] <- toString(track_id_list)
        David_Guetta$Alter2_popularity[x] <- toString(popularity_list)
        
      }
      
      
    }
  }
  
  # creating empty lists
  collaboration_list <- list()
  name_list <- list()
  track_id_list <- list()
  popularity_list <- list()
  
}  

#check if the code ran nicely
view(David_Guetta)

#to be able to make an csv we need to turn the data frame into this (matrix, array)
df <- apply(David_Guetta,2,as.character)

#create csv make sure to adjust it to your folder & the name of the csv file you want
write.csv(df, "Alok_final.csv", row.names=FALSE)

#This I use to put in the y value that gives an error to find the artist name
alter1 = unlist(collab_list_no_dupl)[15]
alter1



#######################################

#This is code to access the df again (for if we are going to use the csv)
view(df)
df$Alter2
class(df)

#This I used to check if the list had the same length
data = as.data.frame(df)
collabs <- as.list(strsplit(data$Alter2[1], ",")[[1]])
collabs
length(collabs)

collabs <- as.list(strsplit(data$Alter2_track_id[1], ",")[[1]])
collabs
length(collabs)

collabs <- as.list(strsplit(data$Alter2_popularity[1], ",")[[1]])
collabs
length(collabs)
