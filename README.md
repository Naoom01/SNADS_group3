# SNADS_group3
This repository consists of all Data files and Code files used for the SNA report

The Data is uploaded to the Data folder. Different csv files were used but the most important data files for the CUG and ERGM model are the Nodelist_Final3_adjusted.csv and Edgelist_Final3.csv. The other data files were used to get to these final files.

The preprocessing R files needed to retrieve data from the Spotify API can be found under Spotify API R files.

  - Spotify to csv Ego and alter 1, get the main files from the top 5 DJs with their collaboration (alter 1) and the track id of the track they collaborated on and the popularity of that track
  - Spotify to csv get alter 2, gets the collabs from the above gathered alter 1 and the track id of the track they collaborated on and the popularity of that track
  - Get all artists, gets the artist id and popularity score for every artist in the full network
  - final data, created the data used with only collabs of 10 or more times
  - 

The preprocessing Python file and API code retrieving from the musicbranz API can be found under Pre-processing and API Python files. These files are also available as HTML in the HTML Python folder within that directory.

The R files for the CUG and ERGM can be found under Model R files.
