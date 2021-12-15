library(tidyverse)
library(igraph) # for graph_from_data_frame and get.adjacency
library(Gmisc) # for pathJoin function
library(plotly)

source("C:/Users/Anthony_Laptop/Documents/Brown University/PHP2560/Final/code/readdata.R", echo = TRUE)


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

create_plot_data <- function(yrs, type = "ATP", sur = "all"){
  # creates data.frame(cols = preScore, name, year)
  # where each row represents a players preScore in a given year
  # calculates prestige scores between years min(years)
  # and each other year in years input
  # Ex) years = (1968, 1970, 1972)
  # gets bind(p(1968,1968),p(1968,1970),p(1968,1972))
  
  # Appropriate spacing between years
  range = length(yrs)
  by = case_when(range < 6 ~ 1,
                 range < 17 ~ 2,
                 range < 25 ~ 3,
                 range < 33 ~ 4,
                 range > 33 ~ 5)

  years = seq(min(yrs), max(yrs), by = by)
  
  # Check to include last year of inputted year 
  if (tail(years, n=1) != max(yrs)){
    years = append(years, max(yrs))
  }
  
  
  
  get_win_pct <- function(name, mat){
    # gets win percentage of player in given year
    return(sum(mat[name,])/(sum(mat[,name]) + sum(mat[name,])))
  }
  
  get_rows <- function(year_end, type = type, sur = sur){
    # Gets one new iteration between 2 years (min year and year_end)
    # p (min(years), year_end)
    df_l <- yeardf(min(years),year_end,type = "ATP",sur = "all")
    adjM_l <- adjacencyMatrix(df_l)
    pre_l <- itera_fun(adjM_l)
    
    win_pcts = sapply(X = rownames(adjM_l), get_win_pct, mat = adjM_l)
    # Add year, win_pct and rank cols
    pre_l[[1]]$win_pct = win_pcts
    pre_l[[1]]$year = year_end
    pre_l[[1]] = pre_l[[1]] %>% 
          arrange(desc(preScore)) %>%
          mutate(rank = 1:nrow(pre_l[[1]]))
    
    return(pre_l$preScore)
  }
  
  # using helper function
  plot_data = do.call(rbind, lapply(X = years, FUN = get_rows))
  return(plot_data)
}

create_plot <- function(plot_data, players){
  
  plotted_players = plot_data %>% 
        filter(players.name %in% players) %>%
        mutate(text = paste0(players.name," (Rank: ", as.character(rank), ")"))
  
  fig <- plotted_players %>%
    plot_ly(
      x = ~year, 
      y = ~rank, 
      frame = ~year, 
      text = ~text, 
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers',
      showlegend = F
    )
  
  low_rank = max(plotted_players$rank)
  
  fig <- fig %>% layout(
    yaxis = list(
      range = c(round(low_rank + 10, -1) + 10,1),
      tickvals = c(round(low_rank + 10, -1), round(low_rank + 10, -1)/2, 1),
      title = "Player Rank"
    ),
    xaxis = list(title = "Year")
  )
  
    return(fig)
}

y = seq(1980, 1989)
plot_data = create_plot_data(y)
animation = create_plot(plot_data, c("Arthur Ashe", "Rod Laver", "Tom Gorman", "John McEnroe"))
animation

