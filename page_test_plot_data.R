library(tidyverse)
library(igraph) # for graph_from_data_frame and get.adjacency
library(Gmisc) # for pathJoin function
#library(gganimate)
#library(gapminder)

source("C:/Users/Anthony_Laptop/Documents/Brown University/PHP2560/Final/code/readdata.R", echo = TRUE)

#df <- yeardf(1968,1968,type = "ATP",sur = "all")

adjacencyMatrix<-function(df){
  # given dataframe of tennis, return adjacencyMatrix
  
  pl.name <- unique(c(as.vector(df$winner_name), as.vector(df$loser_name)))
  
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

# where row names are winner names: sum of row is win record, sum of col is loss record
#adjM <- adjacencyMatrix(df)


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

#pre <- itera_fun(adjM)


#win_names = numeric(length = dim(adjM)[1])
#for(name in rownames(adjM)){
#  win_names[name] = sum(adjM[name,])/(sum(adjM[,name]) + sum(adjM[name,]))
#}

#hist(win_names, breaks = 50)
#hist(win_names[win_names > 0], breaks = 50)


create_plot_data <- function(years, type = "ATP", sur = "all"){
  # creates data.frame(cols = preScore, name, year)
  # where each row represents a players preScore in a given year
  # calculates prestige scores between years min(years)
  # and each other year in years input
  # Ex) years = (1968, 1970, 1972)
  # gets bind(p(1968,1968),p(1968,1970),p(1968,1972))
  
  get_rows <- function(year_end, type = type, sur = sur){
    # Gets one new iteration between 2 years (min year and year_end)
    # p (min(years), year_end)
    df_l <- yeardf(min(years),year_end,type = "ATP",sur = "all")
    adjM_l <- adjacencyMatrix(df_l)
    pre_l <- itera_fun(adjM_l)
    pre_l[[1]]$year = year_end
    return(pre_l$preScore)
  }
  
  # bind(p(1968,1968),p(1968,1970),p(1968,1972))
  # using helper function
  d = do.call(rbind, lapply(X = years, FUN = get_rows))
  return(d)
}

y = c(1968, 1970, 1972)
#y = c(1970, 1980, 1990, 2000, 2010, 2020) - takes longer
#y = seq(1968,2021,5) - this one will crash your computer lol

test = create_plot_data(y)
