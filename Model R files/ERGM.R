artist <- readr::read_csv("Nodelist_Final3_adjusted.csv")
collabs <- readr::read_csv("Edgelist_Final3.csv")

g_dj <- igraph::graph_from_data_frame(collabs, artist, directed = FALSE)
net_dj <- snafun::to_network(g_dj)
net_dj <- snafun::remove_edge_weight(net_dj)

#Baseline model
mod_1 <- ergm::ergm(net_dj ~ edges,
                    control = ergm::control.ergm(MCMC.burnin = 6500,
                                                 MCMC.samplesize = 20000,
                                                 seed = 347,
                                                 parallel = 4,
                                                 parallel.type = "PSOCK"))
summary(mod_1)
goof_1 <- ergm::gof(mod_1)
plot(goof_1)

#Adding isolates into the ERGM
mod_2 <- ergm::ergm(net_dj ~ edges + isolates,
                    control = ergm::control.ergm(MCMC.burnin = 6500,
                                                 MCMC.samplesize = 20000,
                                                 seed = 347,
                                                 parallel = 4,
                                                 parallel.type = "PSOCK"))
summary(mod_2)
ergm::mcmc.diagnostics(mod_2)
goof_2 <- ergm::gof(mod_2)
plot(goof_2)

#Adding degree(2) into the ERGM
mod_3 <- ergm::ergm(net_dj ~ edges + isolates + degree(2),
                    control = ergm::control.ergm(MCMC.burnin = 6500,
                                                 MCMC.samplesize = 20000,
                                                 seed = 347,
                                                 parallel = 4,
                                                 parallel.type = "PSOCK"))
summary(mod_3)
ergm::mcmc.diagnostics(mod_3)
goof_3 <- ergm::gof(mod_3)
plot(goof_3)

#Adding degree(3) into ERGM based on previous GOF results
mod_4 <- ergm::ergm(net_dj ~ edges + isolates + degree(2) + degree(3),
                    control = ergm::control.ergm(MCMC.burnin = 6500,
                                                 MCMC.samplesize = 20000,
                                                 seed = 347,
                                                 parallel = 4,
                                                 parallel.type = "PSOCK"))
summary(mod_4)
ergm::mcmc.diagnostics(mod_4)
goof_4 <- ergm::gof(mod_4)
plot(goof_4)

#Adding degree(4) into ERGM based on previous GOF results
mod_5 <- ergm::ergm(net_dj ~ edges + isolates + degree(2) + degree(3) + degree(4),
                    control = ergm::control.ergm(MCMC.burnin = 6500,
                                                 MCMC.samplesize = 20000,
                                                 seed = 347,
                                                 parallel = 4,
                                                 parallel.type = "PSOCK"))
summary(mod_5)
ergm::mcmc.diagnostics(mod_5)
goof_5 <- ergm::gof(mod_5)
plot(goof_5)

#Adding absdiff Popularity to see if artists with comparable Popularity score collaborate with one another
mod_6 <- ergm::ergm(net_dj ~ edges + isolates + degree(2) + degree(3) + degree(4) + 
                      absdiff("Popularity"),
                    control = ergm::control.ergm(MCMC.burnin = 6500,
                                                 MCMC.samplesize = 20000,
                                                 seed = 347,
                                                 parallel = 4,
                                                 parallel.type = "PSOCK"))
summary(mod_6)
ergm::mcmc.diagnostics(mod_6)
goof_6 <- ergm::gof(mod_6)
plot(goof_6)

#Adding nodematch continent, diff = FALSE, diff = TRUE doesn't work
#Nodefactor doesn't converge
mod_7 <- ergm::ergm(net_dj ~ edges + isolates + degree(2) + degree(3) + degree(4) + 
                    absdiff("Popularity") + 
                    nodematch("Continent", diff = FALSE),
                    control = ergm::control.ergm(MCMC.burnin = 6500,
                                                 MCMC.samplesize = 20000,
                                                 seed = 347,
                                                 parallel = 4,
                                                 parallel.type = "PSOCK"))
summary(mod_7)
ergm::mcmc.diagnostics(mod_7)
goof_7 <- ergm::gof(mod_7)
plot(goof_7)

#Adding nodematch Gender, diff = FALSE, diff = TRUE doesn't work
#Nodefactor doesn't converge
mod_8 <- ergm::ergm(net_dj ~ edges + isolates + degree(2) + degree(3) + degree(4) + 
                    absdiff("Popularity") + 
                    nodematch("Continent", diff = FALSE) + 
                    nodematch("Gender", diff = FALSE),
                    control = ergm::control.ergm(MCMC.burnin = 6500,
                                                 MCMC.samplesize = 20000,
                                                 seed = 347,
                                                 parallel = 4,
                                                 parallel.type = "PSOCK"))
summary(mod_8)
ergm::mcmc.diagnostics(mod_8)
goof_8 <- ergm::gof(mod_8)
plot(goof_8)

#Adding nodecov Popularity * Gender for robustness
mod_9 <- ergm::ergm(net_dj ~ edges + isolates + degree(2) + degree(3) + degree(4) + 
                      absdiff("Popularity") + 
                      nodematch("Continent", diff = FALSE) + 
                      nodematch("Gender", diff = FALSE) + 
                      nodecov("Popularity_gender"),
                    control = ergm::control.ergm(MCMC.burnin = 6500,
                                                 MCMC.samplesize = 20000,
                                                 seed = 347,
                                                 parallel = 4,
                                                 parallel.type = "PSOCK"))
summary(mod_9)
ergm::mcmc.diagnostics(mod_9)
goof_9 <- ergm::gof(mod_9)
plot(goof_9)

#Changing burnin and sample size to make the MCMC trace plot fuzzier
mod_10 <- ergm::ergm(net_dj ~ edges + isolates + degree(2) + degree(3) + degree(4) + 
                       absdiff("Popularity") + 
                       nodematch("Continent", diff = FALSE) + 
                       nodematch("Gender", diff = FALSE) + 
                       nodecov("Popularity_gender"),
                     control = ergm::control.ergm(MCMC.burnin = 25000,
                                                  MCMC.samplesize = 90000,
                                                  seed = 347,
                                                  parallel = 4,
                                                  parallel.type = "PSOCK"))

summary(mod_10)
ergm::mcmc.diagnostics(mod_10)
goof_10 <- ergm::gof(mod_10)
plot(goof_10)

texreg::screenreg(list(mod_1, mod_2, mod_3, mod_4, mod_5, mod_6, mod_7, mod_8, mod_9, mod_10))
