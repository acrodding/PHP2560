df<-read.csv("C:/Users/zheji/OneDrive/brown/PHP 2560 Programming with R/Final project/tennis_atp-master/tennis_atp-master/atp_matches_2014.csv")
library(tidyverse)
library(igraph)

adjacencyMatrix<-function(df, sur = "all"){
  pl.id1 <- df%>% distinct(winner_id) %>% rename(id = winner_id)
  pl.id2 <- df%>% distinct(loser_id) %>% rename(id = loser_id)
  pl.id <- full_join(pl.id1,pl.id2)

  n <- as.numeric(count(pl.id))
  weight <- matrix(0, nrow = n,ncol = n)
  pl.id <-as.matrix(pl.id)
  weight <-as.data.frame(weight)
  rownames(weight) <- pl.id
  colnames(weight) <- pl.id
  if(sur == "all"){
    plays <- df %>% select(loser_id,winner_id) 
  }else{
    plays <- df %>% group_by(loser_id,winner_id, surface)%>% filter(surface == sur) %>% select(-surface)
  }
  
  G <- graph_from_data_frame( d= plays, vertices = pl.id, directed = T )
  V(G)$name <- as.character(pl.id)
  adjM <- get.adjacency(G)
  
  return(as.matrix(adjM))

}
G <- adjacencyMatrix(df)

dProbabilityMatrix<-function(G,d=0.85){
  cs <- colSums(G)
  cs[cs==0] <- 1
  n <- nrow(G)
  delta <- (1-d)/n
  A <- matrix(delta,nrow(G),ncol(G))
  for (i in 1:n) A[i,] <- A[i,] + d*G[i,]/cs
  A
}

W <- dProbabilityMatrix(G)

calcEigenMatrix<-function(G){
  x <- Re(eigen(G)$vectors[,1])
  x/sum(x)
}

calcEigenMatrix(W)

sort(calcEigenMatrix(W),decreasing = TRUE)
