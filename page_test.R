library(tidyverse)
library(igraph) # for graph_from_data_frame and get.adjacency
library(Gmisc) # for pathJoin function

source("C:/Users/zheji/OneDrive/brown/PHP 2560 Programming with R/Final project/code/readdata.R", echo=TRUE)

df <- yeardf(2001,2010,type = "ATP",sur = "all")

adjacencyMatrix<-function(df){
  # given dataframe of tennis, return adjacencyMatrix
  
  pl.name <- unique(c(df$winner_name, df$loser_name))
  
  n <- n_distinct(pl.name)
  weight <- matrix(0, nrow = n,ncol = n)
  pl.name <-as.matrix(pl.name)
  weight <-as.data.frame(weight)
  rownames(weight) <- pl.name
  colnames(weight) <- pl.name
 
  plays <- df %>% select(loser_name,winner_name) 
  
  G <- graph_from_data_frame( d= plays, vertices = pl.name, directed = T )
  adjM <- get.adjacency(G,sparse = FALSE) # sparse = false is needed ,otherwise return sparse matrix
  #adjM's row is j, col is i
  # return transposed , then row is i, col is j
  return(t(adjM))
}

adjM <- adjacencyMatrix(df)


itera_fun <- function(M, d = 0.85, iter = 100){
  # M is adjacencymatrix, d is (1-q) is paper, iter is iteration times
  # return a list with preScore and all results in iteration (pre.matrix)
  n <- nrow(M)
  cs <- colSums(M)
  cs[cs==0] <- 1

  r <- rep(1/n,n) # iteration start
  pr <- matrix(0,nrow = n, ncol = iter) # pre.matrix, save all iteration results in each column,
  pr[,1] <- r  
  A <- matrix(1/cs,n,n,byrow = TRUE) # A_ij is 1/L(Pj), w_ji/s_jout in paper
  signM <- M/ifelse(M == 0, 1, M) # signal matrix of M, is j links to i , signM_ij is 1, otherwise 0
  A <- A * M # if j links to i, A_ij is 1/L(Pj), otherwise 0. this is original Pagerank, a litter different from paper, but the results is same. 
  
  for( j in 2:iter){
  
    pr[,j] <- (1-d)/n + d*A %*% pr[,j-1] # matrix form of equation (1) in paper
    
  }
  
  pre <- data.frame("preScore" = pr[,iter], "players.name" = rownames(M))
  results <- list("preScore" =pre, "pre.matrix" = pr,"iteration.times" = iter)
}


pre <- itera_fun(adjM)

