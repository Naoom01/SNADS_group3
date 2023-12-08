library(tidyverse)
library(spotifyr)

#get this from the SPOTIFY API Dashboard 
Sys.setenv(SPOTIFY_CLIENT_ID = "Client_ID")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "Client_Secret")



token <- spotifyr::get_spotify_access_token()
#Check if a token is created, if not check/renew your client id and client secret
token



############    Martin Garrix

Martin_Garrix <- read.csv("Martin_Garrix_v2.csv")
#view(Martin_Garrix)
class(Martin_Garrix)

# get all the alter 2 rows that are left empty (Error: that Spotify could not retrieve this artist)
lst <- list()
for (x in 1:nrow(Martin_Garrix)) {
  if (Martin_Garrix$Alter2[x] == 0 | Martin_Garrix$Alter2[x] == "") {
    lst <- append(lst, x)
  }
}

lst

#remove the above colleted rows and take them out of the data
Martin_Garrix_del_err <- Martin_Garrix %>%  filter(!row_number() %in% lst)
#view(Martin_Garrix_del_err)

MG_artist_lst <- list()
MG_artist_lst <- append(MG_artist_lst, Martin_Garrix_del_err$Ego[1])
MG_artist_lst <- append(MG_artist_lst, Martin_Garrix_del_err$Alter1)
MG_artist_lst

lst2 <- list()

# get a list with all the artist in the data
for (i in 1:nrow(Martin_Garrix_del_err)) {
  collabs <- as.list(strsplit(Martin_Garrix_del_err$Alter2[i], ", ")[[1]])
  lst2 <- append(lst2, collabs)

}

MG_artist_lst_alter2 <- append(MG_artist_lst, lst2)
MG_artist_lst_alter2

length(MG_artist_lst_alter2)

MG_artist_lst_no_dupl <- MG_artist_lst[!duplicated(MG_artist_lst)]
MG_artist_lst_no_dupl

length(MG_artist_lst_no_dupl)

MG_artist_lst_alter2_no_dupl <- MG_artist_lst_alter2[!duplicated(MG_artist_lst_alter2)]
MG_artist_lst_alter2_no_dupl

length(MG_artist_lst_alter2_no_dupl)



############    David Guetta

David_Guetta <- read.csv("David_Guetta_final.csv")
#view(David_Guetta)
class(David_Guetta)


lst <- list()
for (x in 1:nrow(David_Guetta)) {
  if (David_Guetta$Alter2[x] == 0 | David_Guetta$Alter2[x] == "") {
    lst <- append(lst, x)
  } 
}

lst


David_Guetta_del_err <- David_Guetta %>%  filter(!row_number() %in% lst)
#view(David_Guetta_del_err)

DG_artist_lst <- list()
DG_artist_lst <- append(DG_artist_lst, David_Guetta_del_err$Ego[1])
DG_artist_lst <- append(DG_artist_lst, David_Guetta_del_err$Alter1)
DG_artist_lst

lst2 <- list()

for (i in 1:nrow(David_Guetta_del_err)) {
  collabs <- as.list(strsplit(David_Guetta_del_err$Alter2[i], ", ")[[1]])
  lst2 <- append(lst2, collabs)               
  
}               

DG_artist_lst_alter2 <- append(DG_artist_lst, lst2)
DG_artist_lst_alter2

length(DG_artist_lst_alter2)

DG_artist_lst_no_dupl <- DG_artist_lst[!duplicated(DG_artist_lst)]
DG_artist_lst_no_dupl

length(DG_artist_lst_no_dupl)

DG_artist_lst_alter2_no_dupl <- DG_artist_lst_alter2[!duplicated(DG_artist_lst_alter2)]
DG_artist_lst_alter2_no_dupl

length(DG_artist_lst_alter2_no_dupl)


############    Dimitri Vegas & Like Mike

DVLM <- read.csv("Dimitri_Vegas_Like_Mike_final.csv")
#view(DVLM)
class(DVLM)



lst <- list()
for (x in 1:nrow(DVLM)) {
  if (DVLM$Alter2[x] == 0 | DVLM$Alter2[x] == "") {
    lst <- append(lst, x)
  } 
}

lst


DVLM_del_err <- DVLM %>%  filter(!row_number() %in% lst)
#view(DVLM_del_err)

DVLM_artist_lst <- list()
DVLM_artist_lst <- append(DVLM_artist_lst, DVLM_del_err$Ego[1])
DVLM_artist_lst <- append(DVLM_artist_lst, DVLM_del_err$Alter1)
DVLM_artist_lst

lst2 <- list()

for (i in 1:nrow(DVLM_del_err)) {
  collabs <- as.list(strsplit(DVLM_del_err$Alter2[i], ", ")[[1]])
  lst2 <- append(lst2, collabs)               
  
}               

DVLM_artist_lst_alter2 <- append(DVLM_artist_lst, lst2)
DVLM_artist_lst_alter2

length(DVLM_artist_lst_alter2)

DVLM_artist_lst_no_dupl <- DVLM_artist_lst[!duplicated(DVLM_artist_lst)]
DVLM_artist_lst_no_dupl

length(DVLM_artist_lst_no_dupl)

DVLM_artist_lst_alter2_no_dupl <- DVLM_artist_lst_alter2[!duplicated(DVLM_artist_lst_alter2)]
DVLM_artist_lst_alter2_no_dupl

length(DVLM_artist_lst_alter2_no_dupl)

############    Alok

Alok <- read.csv("Alok_final.csv")
#view(Alok)
class(Alok)


lst <- list()
for (x in 1:nrow(Alok)) {
  if (Alok$Alter2[x] == 0 | Alok$Alter2[x] == "") {
    lst <- append(lst, x)
  } 
}

lst


Alok_del_err <- Alok %>%  filter(!row_number() %in% lst)
#view(Alok_del_err)

Alok_artist_lst <- list()
Alok_artist_lst <- append(Alok_artist_lst, Alok_del_err$Ego[1])
Alok_artist_lst <- append(Alok_artist_lst, Alok_del_err$Alter1)
Alok_artist_lst

lst2 <- list()

for (i in 1:nrow(Alok_del_err)) {
  collabs <- as.list(strsplit(Alok_del_err$Alter2[i], ", ")[[1]])
  lst2 <- append(lst2, collabs)               
  
}               

Alok_artist_lst_alter2 <- append(Alok_artist_lst, lst2)
Alok_artist_lst_alter2

length(Alok_artist_lst_alter2)

Alok_artist_lst_no_dupl <- Alok_artist_lst[!duplicated(Alok_artist_lst)]
Alok_artist_lst_no_dupl

length(Alok_artist_lst_no_dupl)

Alok_artist_lst_alter2_no_dupl <- Alok_artist_lst_alter2[!duplicated(Alok_artist_lst_alter2)]
Alok_artist_lst_alter2_no_dupl

length(Alok_artist_lst_alter2_no_dupl)


############    Armin van Buuren

Armin_van_Buuren <- read.csv("Armin_van_Buuren_v11New.csv")
#view(Armin_van_Buuren)
class(Armin_van_Buuren)

lst <- list()
for (x in 1:nrow(Armin_van_Buuren)) {
  if (Armin_van_Buuren$Alter2[x] == 0 | Armin_van_Buuren$Alter2[x] == "") {
    lst <- append(lst, x)
  } 
}

lst


Armin_van_Buuren_del_err <- Armin_van_Buuren %>%  filter(!row_number() %in% lst)
#view(Armin_van_Buuren_del_err)

AvB_artist_lst <- list()
AvB_artist_lst <- append(AvB_artist_lst, Armin_van_Buuren_del_err$Ego[1])
AvB_artist_lst <- append(AvB_artist_lst, Armin_van_Buuren_del_err$Alter1)
AvB_artist_lst

lst2 <- list()

for (i in 1:nrow(Armin_van_Buuren_del_err)) {
  collabs <- as.list(strsplit(Armin_van_Buuren_del_err$Alter2[i], ", ")[[1]])
  lst2 <- append(lst2, collabs)               
  
}               

AvB_artist_lst_alter2 <- append(AvB_artist_lst, lst2)
AvB_artist_lst_alter2

length(AvB_artist_lst_alter2)

AvB_artist_lst_no_dupl <- AvB_artist_lst[!duplicated(AvB_artist_lst)]
AvB_artist_lst_no_dupl

length(AvB_artist_lst_no_dupl)

AvB_artist_lst_alter2_no_dupl <- AvB_artist_lst_alter2[!duplicated(AvB_artist_lst_alter2)]
AvB_artist_lst_alter2_no_dupl

length(AvB_artist_lst_alter2_no_dupl)

########### Combining artist_lst_no_dupl

artist_lst <- append(MG_artist_lst_no_dupl, DG_artist_lst_no_dupl)
artist_lst <- append(artist_lst, DVLM_artist_lst_no_dupl)
artist_lst <- append(artist_lst, Alok_artist_lst_no_dupl)
artist_lst <- append(artist_lst, AvB_artist_lst_no_dupl)
artist_lst
length(artist_lst)

artist_lst_no_dupl <- artist_lst[!duplicated(artist_lst)]
artist_lst_no_dupl
length(artist_lst_no_dupl)


length(MG_artist_lst_no_dupl)
length(DG_artist_lst_no_dupl)
length(DVLM_artist_lst_no_dupl)
length(Alok_artist_lst_no_dupl)
length(AvB_artist_lst_no_dupl)

########### Combining artist_lst_no_dupl Alter 2

artist_lst <- list()
artist_lst <- append(MG_artist_lst_alter2_no_dupl, DG_artist_lst_alter2_no_dupl)
artist_lst <- append(artist_lst, DVLM_artist_lst_alter2_no_dupl)
artist_lst <- append(artist_lst, Alok_artist_lst_alter2_no_dupl)
artist_lst <- append(artist_lst, AvB_artist_lst_alter2_no_dupl)
artist_lst

artist_lst_no_dupl <- artist_lst[!duplicated(artist_lst)]
artist_lst_no_dupl
length(artist_lst_no_dupl)


length(MG_artist_lst_alter2_no_dupl)
length(DG_artist_lst_alter2_no_dupl)
length(DVLM_artist_lst_alter2_no_dupl)
length(Alok_artist_lst_alter2_no_dupl)
length(AvB_artist_lst_alter2_no_dupl)

########### Artist ID

# create a list with all artist including alter 2 artists
artist_lst3 <- append(DVLM_artist_lst_alter2_no_dupl, Alok_artist_lst_alter2_no_dupl)
artist_lst3 <- append(artist_lst3, AvB_artist_lst_alter2_no_dupl)
artist_lst3 <- append(artist_lst3, MG_artist_lst_alter2_no_dupl)
artist_lst3 <- append(artist_lst3, DG_artist_lst_alter2_no_dupl)

length(artist_lst3)

df_artist <- data.frame(unlist(artist_lst3))
names(df_artist) = c("Artist")
view(df_artist)
nrow(df_artist)

# add 0 for the artist id and popularity
lst <- list()
for (x in 1:nrow(df_artist)) {
  lst <- append(lst, 0)
}
lst

df_artist$artist_id <- lst
df_artist$popularity_artist <- lst
df_artist

# # Import here the previously gathered artist with artist id and popularity if necessary
# df_artist_done <-read.csv("All_artist_DG_all.csv")
# nrow(df_artist_done)
# 
# # check the list with the df_artist_done to gather the artist that are already processed
# lst <- list()
# for (x in 1:nrow(df_artist_done)) {
#   if (df_artist_done$artist_id[x] == 0 | df_artist_done$artist_id[x] == "") {
#     lst <- append(lst, x)
#   }
# }
# 
# #filter out the artists that are already done but have value 0
# lst
# length(lst)
# df_artist_done <- df_artist_done %>%  filter(!row_number() %in% lst)
# nrow(df_artist_done)
# view(df_artist_done)
# 
# # check the done artist and append them to the dataframe
# for (i in 1:nrow(df_artist)) {
#   for (j in 1:nrow(df_artist_done)) {
#     if (df_artist$Artist[i] == df_artist_done$Artist[j]) {
#       
#       print(df_artist$Artist[i])
#       print(df_artist_done$Artist[j])
#       
#       df_artist$artist_id[i] <- df_artist_done$artist_id[j]
#       df_artist$popularity_artist[i] <- df_artist_done$popularity_artist[j]
#       
#       
#     }
#   }
# }
# 
# #order the list so that artist that are done are in the front
# df_artist_order <- df_artist[order(unlist(df_artist$artist_id), decreasing = TRUE), ]
# df_artist_order
# row.names(df_artist_order) <- NULL
# df_artist_order
# df_artist <- df_artist_order


# change the index to where the code stopt running
# this gets the id of the artist and popularity
for (i in 3869:nrow(df_artist)) {
  
  tryCatch({
  if (df_artist$artist_id[i] == 0) {

    
    artist_name = df_artist$Artist[i]
    print(i)
    print(artist_name)
    artist <- get_artist_audio_features(artist_name, c("album", "single"))
    artist_id <- unique(artist$artist_id)
    popularity <- get_artist(artist_id)$popularity
  
    df_artist$artist_id[i] <- artist_id
    df_artist$popularity_artist[i] <- popularity
    
  } 
    
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
    
  
} 


view(df_artist)

# save dataframe
df <- apply(df_artist,2,as.character)

write.csv(df, "All_artist_id1.csv", row.names=FALSE)


