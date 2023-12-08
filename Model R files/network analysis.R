#Tutorials on our data

# df_10 <-read.csv("C:/Users/20193702/OneDrive - TU Eindhoven/JADS/Jaar 1/Semester 1/Social Network Analysis for DS/Spotify/df_10_collabs_no_dupl.csv")
# df_10

library(tidyverse)
library(dplyr)

artist <- readr::read_csv("H:/My Drive/Semester 1 JADS/Social Network Analysis/Data/Node and edge list/Nodelist_Final3 (1).csv")
collabs <- readr::read_csv("H:/My Drive/Semester 1 JADS/Social Network Analysis/Data/Node and edge list/Edgelist_Final3 (1).csv")

g_dj <- igraph::graph_from_data_frame(collabs, artist, directed = FALSE)
net_dj <- snafun::to_network(g_dj)

net_dj
snafun::g_summary(net_dj)



igraph::degree(g_dj)
degree <- igraph::degree(g_dj)
hist(degree)

#artist <- readr::read_csv("H:/My Drive/Semester 1 JADS/Social Network Analysis/Data/Node and edge list/All_artist_done.csv")
#collabs <- readr::read_csv("H:/My Drive/Semester 1 JADS/Social Network Analysis/Data/Node and edge list/network_complete_no_dupl.csv")

popularity <- snafun::extract_vertex_attribute(net_dj, "Popularity")
popularity

############# T2

nrow(df_10)

ego_alter1 <- dplyr::select(df_10, Ego, Alter1)
all_djs_matrix <- data.matrix(ego_alter1)
e_10 <- as.matrix(ego_alter1)
g_10 <- igraph::graph_from_edgelist(all_djs_matrix)
g_10

snafun::g_summary(g_10)

# make undirected network
g_10_undir <- igraph::as.undirected(g_10)
g_10_undir

snafun::g_summary(g_10_undir)

#############  T3

######### directed network

g_10 <- g_dj

snafun::g_mean_distance(g_10)

snafun::g_diameter(g_10)

#For the enwiki network every vertex can reach every other vertex within at most 12 steps. That is actually a lot.

snafun::g_radius(g_10)
#It is impossible that all vertices in the graph reach all other vertices in less than 4 steps (this 11 steps)


snafun::count_dyads(g_10)

snafun::g_reciprocity(g_10)



####### undirected network

g_10_undir <- g_dj

snafun::g_mean_distance(g_10_undir)

snafun::g_diameter(g_10_undir)

#For the enwiki network every vertex can reach every other vertex within at most 12 steps. That is actually a lot.

snafun::g_radius(g_10_undir)
#It is impossible that all vertices in the graph reach all other vertices in less than 4 steps (this 11 steps)


snafun::count_dyads(g_10_undir)

snafun::g_reciprocity(g_10_undir)

snafun::g_transitivity(g_10_undir)

snafun::g_density(g_10_undir)

info <- snafun::g_summary(g_10_undir)
names(info)
info$number_of_isolates
info$triad_census
info$density


## subgroups

snafun::print(net_dj)

snafun::plot(net_dj)


#### We've got high modularity which is good. I don't understand it fully though


# Girvan
girvan <- snafun::extract_comm_girvan(g_dj)
girvan

# check the modularity
igraph::modularity(girvan)

# check which community each vertex belongs to
membership_girvan <- igraph::membership(girvan)
membership_girvan[1] == membership_girvan[2]

membership_girvan[4] == membership_girvan[370]

snafun::plot(girvan, g_dj)


# Louvain

louvain <- snafun::extract_comm_louvain(g_dj)
louvain
# check the modularity
igraph::modularity(louvain)

# check which community each vertex belongs to
membership_louvain <- igraph::membership(louvain)
membership_louvain

snafun::plot(louvain, g_dj)

membership_girvan[1] == membership_louvain[1]
membership_girvan[109]

class(membership_girvan)


# walktrap

walktrap <- snafun::extract_comm_walktrap(g_dj)
walktrap

# check the modularity
igraph::modularity(walktrap)

# check which community each vertex belongs to
membership_walktrap <- igraph::membership(walktrap)
membership_walktrap

snafun::plot(walktrap, g_dj)


#order on group
order_gir <- list()
order_lau <- list()
order_wal <- list()

for (j in 1:92) {
  for (i in 1:length(membership_girvan)) {
    
    if (membership_girvan[j] == membership_girvan[i]) {
      order_gir <- append(order_gir, membership_girvan[i])
    }
    
  }
  
}

order_gir

for (j in 1:92) {
  for (i in 1:length(membership_louvain)) {
    
    if (membership_louvain[j] == membership_louvain[i]) {
      order_lau <- append(order_lau, membership_louvain[i])
    }
    
  }
  
}

order_lau


for (j in 1:92) {
  for (i in 1:length(membership_walktrap)) {
    
    if (membership_walktrap[j] == membership_walktrap[i]) {
      order_wal <- append(order_wal, membership_walktrap[i])
    }
    
  }
  
}

order_wal



df <- data.frame(popularity, membership_girvan, membership_louvain, membership_walktrap)
view(df)




degree <- snafun::v_degree(g_dj, vids = NULL, mode = c("all", "out", "in"), loops = FALSE, rescaled = FALSE)
degree_all <- snafun::v_degree(g_dj, vids = NULL, mode = c("all"), loops = FALSE, rescaled = FALSE)
degree_out <- snafun::v_degree(g_dj, vids = NULL, mode = c("out"), loops = FALSE, rescaled = FALSE)
degree_in <- snafun::v_degree(g_dj, vids = NULL, mode = c("in"), loops = FALSE, rescaled = FALSE)

df_degree <- data.frame(popularity, degree, degree_all, degree_out, degree_in)
view(df_degree) 

plot(density(snafun::v_betweenness(net_dj)), main = "Betweenness distribution")
  

?plot()
##### Bipartite networks



####  Vertex centrality

eccentricity <- snafun::v_eccentricity(g_dj)
eccentricity

snafun::v_stress(g_dj)

stress <- snafun::v_stress(g_dj)

closeness <- snafun::v_closeness(g_dj, vids = NULL, mode = c("all", "out", "in"), rescaled = FALSE)

betweenness <- snafun::v_betweenness(g_dj, vids = NULL, directed = FALSE, rescaled = FALSE)
betweenness

df <- data.frame(popularity, eccentricity, stress, closeness, betweenness, degree, membership_girvan, membership_louvain, membership_walktrap)
view(df)

#write.csv(df, "C:/Users/20193702/OneDrive - TU Eindhoven/JADS/Jaar 1/Semester 1/Social Network Analysis for DS/df_collabs_10_measures.csv", row.names=TRUE)


color <- rep("green", snafun::count_vertices(g_dj))
stressful_judges <- which(stress > 0)
color[stressful_judges] <- "red"

snafun::plot(g_dj, vertex.color = color, vertex.size = sqrt(stress))

# or, somewhat clearer in terms of what is going on
snafun::plot(g_dj, vertex.color = color, vertex.size = sqrt(stress),
             layout = igraph::layout.reingold.tilford(g_dj),
             displaylabels = FALSE)


#### Centrality on the internet

snafun::v_pagerank(g_dj, damping = .999)



########### T4

#### marriage

snafun::plot_centralities(g_dj)

snafun::g_centralize(g_dj, measure = "betweenness")

snafun::g_centralize(g_dj, measure = "closeness")

snafun::g_centralize(g_dj, measure = "degree")

snafun::g_centralize(g_dj, measure = "eigenvector")



##### automated CUG test
cug_dj_tran <- sna::cug.test(net_dj, mode = "graph", 
                                 FUN = sna::gtrans, 
                                 cmode = "edges", reps = 1000, 
                                 FUN.args = list(mode = "graph"))
cug_dj_tran



trans_f <- function(x, directed = FALSE) {  # note: directed = FALSE!
  x <- snafun::fix_cug_input(x, directed = directed)
  snafun::g_transitivity(x)
}

cug_dj_tran <- sna::cug.test(net_dj, mode = "graph", 
                                 FUN = trans_f, 
                                 cmode = "edges", reps = 1000)
cug_dj_tran

sna::plot.cug.test(cug_dj_tran)


#betweenness centrality
betw_f <- function(x, directed = FALSE) {  # note: directed = FALSE!
  x <- snafun::fix_cug_input(x, directed = directed)
  snafun::g_centralize(x, measure = "betweenness", directed = directed)$centralization
}

cug_dj_betw <- sna::cug.test(net_dj, mode = "graph", FUN = betw_f, 
                                 cmode = "edges", 
                                 reps = 1000)
cug_dj_betw

sna::plot.cug.test(cug_dj_betw)

#closeness centrality
betw_f <- function(x, directed = FALSE) {  # note: directed = FALSE!
  x <- snafun::fix_cug_input(x, directed = directed)
  snafun::g_centralize(x, measure = "closeness", directed = directed)$centralization
}

cug_dj_clos <- sna::cug.test(net_dj, mode = "graph", FUN = betw_f, 
                             cmode = "edges", 
                             reps = 1000)
cug_dj_clos

sna::plot.cug.test(cug_dj_clos)
sna::plot.cugtest(cug_dj_clos)

#degree centrality
betw_f <- function(x, directed = FALSE) {  # note: directed = FALSE!
  x <- snafun::fix_cug_input(x, directed = directed)
  snafun::g_centralize(x, measure = "degree", directed = directed)$centralization
}

cug_dj_deg <- sna::cug.test(net_dj, mode = "graph", FUN = betw_f, 
                             cmode = "edges", 
                             reps = 1000)
cug_dj_deg

sna::plot.cug.test(cug_dj_deg)


?sna::plot.cug.test()

##### logistic regression
net_dj

popularity <- snafun::extract_vertex_attribute(net_dj, "Popularity")
popularity

snafun::make_matrix_from_vertex_attribute(net_dj, 
                                          name = "Popularity", measure = "absdiff")



distances <- snafun::d_structural_equivalence(net_dj)
distances[4]




##### CUG by hand

net_dj


snafun::create_random_graph(n_vertices = 384, 
                            strategy = "gnm",
                            m = 296,
                            directed = FALSE,
                            graph = "network")

replicate(n = 10,
          snafun::create_random_graph(n_vertices = 384, 
                                      strategy = "gnm",
                                      m = 296,
                                      directed = FALSE,
                                      graph = "network") |> 
            snafun::g_transitivity(),
          simplify = TRUE
)

trans <- replicate(n = 2000,
                   snafun::create_random_graph(n_vertices = 384, 
                                               strategy = "gnm",
                                               m = 296,
                                               directed = FALSE,
                                               graph = "network") |>
                     snafun::g_transitivity(),
                   simplify = TRUE
)
trans


plot(density(trans), main = "Empirical transitivity distribution", 
xlab = "transitivity")
abline(v = snafun::g_transitivity(net_dj), lty = "dashed")

mean(trans > snafun::g_transitivity(net_dj))




########## T5

#### plotting networks in igraph

plot(net_dj) 

snafun::plot(net_dj, 
     vertex.size = 5,
     # edge.arrow.size = .4, 
     vertex.label = NA, 
     edge.curved = .1
     )

?plot

snafun::plot(net_dj, 
     vertex.size = 7,
     vertex.label = igraph::V(net)$name, 
     edge.curved = .1,
     vertex.label.cex = 0.6,  # vertex label size
     vertex.label.color = "black", 
     edge.color = "blue",
     vertex.color = "orange", 
     vertex.frame.color = "green") # node perimeter


net_dj

plot(net_dj,
     edge.arrow.size = .2, 
     edge.color = "gray80",
     vertex.frame.color = "#ffffff",#node perimeter
     vertex.label = igraph::V(g_dj)$Gender, 
     vertex.label.cex = 0.6,  # vertex label size
     vertex.label.color = "black",
     vertex.color = igraph::V(g_dj)$Country
)
rlang::last_trace()
warnings()

plot(net_dj,
     layout = igraph::layout_nicely)

lst <- list()
for (i in 1:nrow(df)) {
  lst <- append(lst, 1)
}
class(lst)

df1 <-df

df1$Count <- unlist(lst)

df1$Count[1]

view(df1)

df_group_girvan = df1 %>% group_by(membership_girvan)  %>%
  summarise(mean_popularity = mean(popularity), 
            mean_eccentricity = mean(eccentricity), 
            mean_stress = mean(stress), 
            mean_closeness = mean(closeness),
            mean_betweenness= mean(betweenness), 
            mean_degree = mean(degree),
            sum_count = sum(Count),
             .groups = 'drop')
view(df_group_girvan)

