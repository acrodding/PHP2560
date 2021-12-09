library(tidyverse)
library(cowplot)


yeardf <-function(year.st,year.end, type = "ATP",sur = "all"){
  # given year range, and tournaments type in c("ATP","WTA"), and surface type
  #return the dataframe conditional on input
  
  yearchar <- as.character(year.st:year.end)
  
  ATP_WTA <- function(type,sur){
    
    path <- pathJoin("C:/Users/zheji/OneDrive/brown/PHP 2560 Programming with R/Final project/",type)
    setwd(path)
    files <- list.files()  #load the file names into the workspace
    #initialize n_players and n_tournaments
    n_players<- vector()
    n_tournaments<- vector()
    
    all.df <- read.csv(files[1])
    if(sur == "all"){
      all.df <- all.df %>% select(tourney_id,tourney_name,winner_name,winner_id,loser_name,loser_id)
    }else{
      all.df <- all.df %>% filter(surface == sur)%>% select(tourney_id,tourney_name,winner_name,winner_id,loser_name,loser_id)
    }

    for(j in 2:length(files)){
      df <-read.csv(files[j])
      if(sur =="all"){
        df.temp <- df  %>% select(tourney_id,tourney_name,winner_name,winner_id,loser_name,loser_id)
      }else{
        df.temp <- df %>% filter(surface == sur) %>% select(tourney_id,tourney_name,winner_name,winner_id,loser_name,loser_id)
        
      }
      all.df <- rbind(all.df,df.temp)
    }
    
    all.df <- all.df %>% mutate(year = as.numeric(str_sub(tourney_id,1,4)))
    return(all.df)
    
  }
  
  df <-ATP_WTA(type,sur) %>% filter(year %in% yearchar)
  return(df)
}


figure1 <- function(df){
  # given dataframe
  # return figure 1 in paper
  numbers <- df %>% group_by(year) %>% 
    summarise(n_tournaments = n_distinct(tourney_id),n_players = n_distinct(c(winner_name,loser_name)))
  numbers$year<-as.character(numbers$year)
  
  players<- ggplot(dat=numbers) +
    geom_col(aes(x=year, y=n_players))
  tournaments<- ggplot(dat=numbers) +
    geom_col(aes(x=year, y=n_tournaments))
  
  cowplot::plot_grid(players, 
                     tournaments, 
                     ncol = 1,
                     labels = "auto")
}

#df80s <-yeardf(1980,1989,type = "ATP",sur = "Clay")
#figure1(df80s)
