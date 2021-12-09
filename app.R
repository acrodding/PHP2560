library(tidyverse)
library(cowplot)
library(Gmisc)
library(plotly)

yeardf <-function(year.st,year.end, type = "ATP",sur = "all"){
    # given year range, and tournaments type in c("ATP","WTA"), and surface type
    #return the dataframe conditional on input
    
    yearchar <- as.character(year.st:year.end)
    
    ATP_WTA <- function(type,sur){
        
        path <- pathJoin("/Users/tianyecui/Desktop/2560/final/data", type)
        setwd(path)
        files <- list.files()  #load the file names into the workspace
        #initialize n_players and n_tournaments
        n_players<- vector()
        n_tournaments<- vector()
        
        all.df <- read.csv(files[1])
        if("all" %in% sur){
            all.df <- all.df %>% select(tourney_id,tourney_name,winner_name,winner_id,loser_name,loser_id)
        }else{
            all.df <- all.df %>% filter(surface %in% sur)%>% select(tourney_id,tourney_name,winner_name,winner_id,loser_name,loser_id)
        }
        
        for(j in 2:length(files)){
            df <-read.csv(files[j])
            if("all" %in% sur){
                df.temp <- df  %>% select(tourney_id,tourney_name,winner_name,winner_id,loser_name,loser_id)
            }else{
                df.temp <- df %>% filter(surface %in% sur) %>% select(tourney_id,tourney_name,winner_name,winner_id,loser_name,loser_id)
                
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


ui <- fluidPage(
    titlePanel("PHP2560 Final Project"), 
    # collect tournament type choice from the user
    radioButtons("TournamentType", "Tournament Type:",
                 c("ATP"="ATP",
                   "WTA"="WTA")),
    # collect surface type choice from the user
    checkboxGroupInput("SurfaceType", "Surface Type:",
                       c("Hard"="Hard",
                         "Clay"="Clay",
                         "Grass"="Grass",
                         "Carpet"="Carpet")),
    # collect year range from the user
    # start year
    numericInput("startYear", "Start Year:", 1968, min=1968, max=2021),
    # end year
    numericInput("endYear", "End Year:", 2021, min=1968, max=2021),
    
    plotOutput("Figure1")
)


server <- function(input, output) {
    
    output$Figure1<- renderPlot({
        figure1(yeardf(input$startYear, input$endYear,
                       type = input$TournamentType, sur = input$SurfaceType))
    })
}

shinyApp(ui = ui, server = server)

