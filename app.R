library(tidyverse)
library(cowplot)
library(Gmisc)
library(plotly)
library(ggpubr)
library(igraph)
library(knitr)
library(tableone) # for prettier tables to display
library(kableExtra)

yeardf <-function(year.st,year.end, type = "ATP",sur = "all"){
    # given year range, and tournaments type in c("ATP","WTA"), and surface type
    #return the dataframe conditional on input
    
    yearchar <- as.character(year.st:year.end)
    
    ATP_WTA <- function(type,sur){
        
        # path <- pathJoin("./Data", type)
        # setwd(path)
        # files <- list.files()  #load the file names into the workspace
        
        if(type=="ATP") {files<- paste("data/",tolower(type),"_matches_",1968:2021,".csv", sep="")}
        else {files<- paste("data/",tolower(type),"_matches_",1927:2021,".csv", sep="")}
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
            df <- read.csv(files[j])
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
    
    players<- ggplotly(ggplot(dat=numbers) +
        geom_col(aes(x=year, y=n_players)) )
    
    tournaments<- ggplotly(ggplot(dat=numbers) +
        geom_col(aes(x=year, y=n_tournaments)) ) 
    
    subplot(players, tournaments, nrows=2, margin=0.1) %>%
        layout(showlegend = FALSE,
               annotations = list(
                   list(x = 0.5 , y = 1.08, text = "Number of Players in Each Year", showarrow = F, xref='paper', yref='paper'),
                   list(x = 0.5 , y = 0.45, text = "Number of Tournaments in Each Year", showarrow = F, xref='paper', yref='paper')))

}

# prestige score
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



ui <- fluidPage(
    titlePanel("PHP2560 Final Project"), 
    
    sidebarPanel(
        #-------------------------
        # Figure 1 in the article
        #-------------------------
        # collect tournament type choice from the user
        radioButtons("TournamentType", "Tournament Type:",
                     c("ATP"="ATP",
                       "WTA"="WTA")),
        # collect surface type choice from the user
        checkboxGroupInput("SurfaceType", "Surface Type:",
                           c("Hard"="Hard",
                             "Clay"="Clay",
                             "Grass"="Grass",
                             "Carpet"="Carpet"),
                           selected="Hard"),
        # collect year range from the user
        # start year
        sliderInput("startYear", "Start Year:", min=1968, max=2021, value=1980),
        # end year
        sliderInput("endYear", "End Year:", min=1968, max=2021, value=1989),
        
        #-------------------------
        # Prestige score
        #-------------------------
        # itera_fun(adjacencyMatrix(df))
    ),
    
    mainPanel(
        # Figure 1 
        plotlyOutput("Figure1"),
        # Prestige score
        tableOutput("best10")
    )
)


server <- function(input, output) {
    
    df_filtered <- reactive({
        # Creates a list of the studies checked and then filters the data 
        # based on this information
        return(yeardf(input$startYear, input$endYear,
                      type = input$TournamentType, sur = input$SurfaceType))
    }) 

    output$Figure1<- renderPlotly({
        figure1(df_filtered())
    })


    output$best10<- function(){
        adjM <- adjacencyMatrix(df_filtered())
        prestige<- itera_fun(adjM)$preScore
        data.frame(prestige %>% arrange(desc(preScore)) %>% head(5) %>% select(players.name)) %>%
            knitr::kable("html", caption="Top 5 Players", col.names="") 
    }
    
}

shinyApp(ui = ui, server = server)

