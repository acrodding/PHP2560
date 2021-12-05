library(tidyverse)
library(cowplot)
setwd("/Users/tianyecui/Desktop/2560/final/data")  # set to your directory
files <- list.files()  #load the file names into the workspace
#initialize n_players and n_tournaments
n_players<- vector()
n_tournaments<- vector()

for(i in 1:length(files)){
  df <- read.csv(files[i])
  n_players[i]<- n_distinct(c(df$winner_name, df$loser_name))
  n_tournaments[i]<- n_distinct(df$tourney_name)
}
num<- data.frame(year=1968:2021, players=n_players, tournaments=n_tournaments)

# Figure 1 panel A
players<- ggplot(dat=num) +
  geom_col(aes(x=year, y=players))
tournaments<- ggplot(dat=num) +
  geom_col(aes(x=year, y=tournaments))

cowplot::plot_grid(players, 
                   tournaments, 
                   ncol = 1,
                   labels = "auto")

# Figure 1 panel B

