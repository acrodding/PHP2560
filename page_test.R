df<-read.csv("C:/Users/zheji/OneDrive/brown/PHP 2560 Programming with R/Final project/tennis_atp-master/tennis_atp-master/atp_matches_2014.csv")
library(tidyverse)
library(igraph)

adjacencyMatrix<-function(df, sur = "all"){
  # sur is a defult for surface 
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

adjM <- adjacencyMatrix(df)

dProbabilityMatrix<-function(G,d=0.85){
  # 1 step trans probability matrix, d is 1-q is paper
  cs <- colSums(G)
  cs[cs==0] <- 1
  n <- nrow(G)
  delta <- (1-d)/n
  A <- matrix(delta,nrow(G),ncol(G))
  rownames(A) <- rownames(G)
  colnames(A) <- colnames(G)
  for (i in 1:n) A[i,] <- A[i,] + d*G[i,]/cs
  A
}

W <- dProbabilityMatrix(adjM)

calcEigenMatrix<-function(W){
  x <- Re(eigen(W)$vectors[,1])
  ei<-x/sum(x)
  ei<-as.data.frame(ei)
  ei$players.id <- rownames(W)
  return(ei)
}


prescore <- calcEigenMatrix(W) %>% arrange(desc(ei)) %>% rename("preScroe" = ei)

winner <- df %>% select(winner_id,winner_name) %>% rename("players.id" = winner_id, 
                                                          "players.name" = winner_name)
loser <- df %>% select(loser_id, loser_name) %>% rename("players.id" = loser_id, 
                                                        "players.name" = loser_name)
players <- unique(rbind(winner,loser)) 
players$players.id <-as.character(players$players.id)


PresRusults <- left_join(prescore,players, by = "players.id" )
