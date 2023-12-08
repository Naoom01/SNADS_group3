library(tidyverse)
library(spotifyr)

#replace "your_client_id" & "your_client_secret" with yours
Sys.setenv(SPOTIFY_CLIENT_ID = "Client_ID")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "Client_Secret")

token <- spotifyr::get_spotify_access_token()
#Check if a token is created, if not check/renew your client id and client secret
token


alter2_list <- list()
colab_name_list <- list()

# Pick on of the DJs below
#artist_name = 'David_Guetta'
#artist_name = 'Martin Garrix'
#artist_name = 'Armin van Buuren'
#artist_name = 'Alok'
#artist_name = 'Dimitri Vegas & Like Mike'

artist <- get_artist_audio_features(artist_name, c("album", "single"))
artist
#length(artist)

view(artist)

#remove duplicates based on track name
artist <- artist[!duplicated(artist$track_name), ]
#artist

artist <- artist[!grepl("Remix", artist$track_name),]
#view(artist)

artist <- artist[!grepl("Mix", artist$track_name),]
#view(artist)

#This gets the list of data frames which contain all the artists on tracks with chosen artist
artists <- artist$artists
class(artists)

# creating an empty list
collaboration_list <- list()
name_list <- list()
track_id_list <- list()
popularity_list <- list()

#below we get the artists with the artist colaborated with and gives a list with them
for (x in 1:length(artists)) {
  for (i in 1:length(artists[[x]]$name)) {
    if (artists[[x]]$name[i] != artist_name) {
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



df_artist <- data.frame(unlist(name_list),unlist(collaboration_list), 
                           unlist(track_id_list), unlist(popularity_list))
names(df_artist) = c("Ego","Alter1", "track_id", "popularity")
df_artist


#write.csv(df_artist, "David_Guetta3.csv", row.names=FALSE)
#write.csv(df_artist, "Martin_Garrix_Main.csv", row.names=FALSE)
#write.csv(df_artist, "Armin_van_Buuren_Main2.csv", row.names=FALSE)
#write.csv(df_artist, "Alok_Main.csv", row.names=FALSE)
#write.csv(df_artist, "Dimitri_Vegas_Like_Mike_Main.csv", row.names=FALSE)

