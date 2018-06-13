#-----------------------------------------------------------------------------
# Load Globals 
#-----------------------------------------------------------------------------

library(dplyr)
library(data.table)
library(stringr)  
library(ggplot2)
library(purrr)
library(mongolite)
library(phytools)
library(igraph)
library(data.tree)
library(mnormt)

#-----------------------------------------------------------------------------
# Load Datasets
#-----------------------------------------------------------------------------

setwd("~/Development/Last.fm-dataset/data")

nodes <- as.matrix(fread("nodes.csv"))

edges <- as.matrix(fread("edges.csv"))

initial_tree <- make_tree(nodes, edges)

# Dataset via http://socialcomputing.asu.edu/datasets/Last.fm

#-----------------------------------------------------------------------------
# Generate a Pseudo-Random Number to Start Probility Distribution Calculation 
#-----------------------------------------------------------------------------

set.seed(100)

#-----------------------------------------------------------------------------
# Generate Tree of Real Data using igraph Package 
#-----------------------------------------------------------------------------

er <- as.matrix(sample_gnm(nrow(nodes), nrow(edges)))

er <- set_graph_attr(er, "name", "Last_FM")

e4s <- simplify(er, 
                remove.multiple = T, 
                remove.loops = F, 
                edge.attr.comb=c(weight="sum", type="ignore"))

#-----------------------------------------------------------------------------
# Simplify 
#-----------------------------------------------------------------------------

e4_s_one <- (e4s[1] == 1) 
t_e4_s_one <- which(e4_s_one == TRUE)
t_in_e1 <- t_e4_s_one[e4s[1]] 

e4_s_two <- (e4s[2] == 1)
t_e4_s_two <- which(e4_s_two == TRUE)
t_in_e2 <- t_e4_s_two[e4s[2]]

e4_s_three <- (e4s[3] == 1)
t_e4_s_three <- which(e4_s_three == TRUE)
t_in_e3 <- t_e4_s_three[e4s[3]]

e4_s_four <- (e4s[4] == 1)
t_e4_s_four <- which(e4_s_four == TRUE)
t_in_e4 <- t_e4_s_four[e4s[4]]

e4_s_five <- (e4s[5] == 1)
t_e4_s_five <- which(e4_s_five == TRUE)
t_in_e5 <- t_e4_s_five[e4s[5]]

e4_s_six <- (e4s[6] == 1)
t_e4_s_six <- which(e4_s_six == TRUE)
t_in_e6 <- t_e4_s_six[e4s[6]]

e4_s_seven <- (e4s[7] == 1)
t_e4_s_seven <- which(e4_s_seven == TRUE)
t_in_e7 <- t_e4_s_seven[e4s[7]]

e4_s_eight <- (e4s[8] == 1)
t_e4_s_eight <- which(e4_s_eight == TRUE)
t_in_e8 <- t_e4_s_eight[e4s[8]]

e4_s_nine <- (e4s[9] == 1)
t_e4_s_nine <- which(e4_s_nine == TRUE)
t_in_e9 <- t_e4_s_nine[e4s[9]]

e4_s_ten <- (e4s[10] == 1)
t_e4_s_ten <- which(e4_s_ten == TRUE)
t_in_e10 <- t_e4_s_ten[e4s[10]]

my_matrix <- as.matrix(t_in_e1[1],
                       t_in_e2[1], 
                       t_in_e3[1], 
                       t_in_e4[1],
                       t_in_e5[1],
                       t_in_e6[1],
                       t_in_e7[1],
                       t_in_e8[1],
                       t_in_e9[1],
                       t_in_e10[1])

adjMat <- make_tree(my_matrix)


plot(adjMat)

myRing <- make_ring(my_matrix)

plot(myRing, directed = TRUE, mutual = TRUE, circular = TRUE)

#-----------------------------------------------------------------------------
# Find the Number of Associations -- Not Quite!! 
#-----------------------------------------------------------------------------

myRing.neigh = connect.neighborhood(myRing, 12)

plot(myRing.neigh, vertex.size=32, vertex.label=NA) 

#-----------------------------------------------------------------------------
# Find the Number of Associations -- Flip to Circle  
#-----------------------------------------------------------------------------

myRing.neigh = connect.neighborhood(myRing, 13)

plot(myRing.neigh, vertex.size=33, vertex.label=NA) 



