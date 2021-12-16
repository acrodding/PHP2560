library(tidyverse)
library(cowplot)
library(Gmisc)
library(plotly)
library(ggpubr)
library(igraph)
library(knitr)
library(tableone) # for prettier tables to display
library(kableExtra)
library(shinycssloaders)
library(shinythemes)

# -----------------------------------------
# Create data set for use and reproduce Figure 1
# -----------------------------------------
yeardf <-function(year.st, year.end, type = "ATP", sur = "all"){
    # given year range, and tournaments type in c("ATP","WTA"), and surface type
    # return the data frame conditional on input
    
    yearchar <- as.character(year.st:year.end)
    
    ATP_WTA <- function(tp,sur){
        
        if(type=="ATP") {files<- paste("data/",tolower(tp),"_matches_",1968:2021,".csv", sep="")}
        else {files<- paste("data/",tolower(tp),"_matches_",1927:2021,".csv", sep="")}
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
    
    df <- ATP_WTA(type,sur) %>% filter(year %in% yearchar)
    return(df)
}

# Recreate Figure 1 in the original article
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

    
# -----------------------------------------
# Prestige score
# -----------------------------------------

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

# -----------------------------------------
# Animated rank plot
# -----------------------------------------
create_plot_data <- function(yrs, type2 = "ATP", sur = "all"){
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
                   range < 33 ~ 5,
                   range > 33 ~ 8)
    
    years = seq(min(yrs), max(yrs), by = by)
    
    # Check to include last year of inputted year 
    if (tail(years, n=1) != max(yrs)){
        years = append(years, max(yrs))
    }
    
    get_win_pct <- function(name, mat){
        # gets win percentage of player in given year
        return(sum(mat[name,])/(sum(mat[,name]) + sum(mat[name,])))
    }
    
    get_rows <- function(year_end, type3 = type2, sur = sur){
        # Gets one new iteration between 2 years (min year and year_end)
        # p (min(years), year_end)
        df_l <- yeardf(min(years),year_end,type = type3,sur = "all")
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


# -----------------------------------------
# Shiny app 
# -----------------------------------------
ui <- fluidPage(
    
    navbarPage("The Top Players in the History of Professional Tennis",
               
               tabPanel("Results", fluidPage(theme = shinytheme("flatly")),
    
    sidebarPanel(
        
        p("The user may specify year range, court surface, and the choice of players in either the WTA or ATP"), 
        
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
        sliderInput("startYear", "Start Year:", min=1968, max=2021, value=1980, sep=""),
        # end year
        sliderInput("endYear", "End Year:", min=1968, max=2021, value=1989, sep=""),
        
        actionButton("update","Update")
    
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Descriptive Plot", plotlyOutput("Figure1") %>% withSpinner()), 
            tabPanel("Top 10 Players", tableOutput("best10") %>% withSpinner()), 
            tabPanel("Rank Plot", plotlyOutput("rank") %>% withSpinner())
        )
        
      
    )
),

tabPanel("Description", 
         h2("PHP2560 Final Project"),  #header (size 2)
         
         # Link to the original article
         a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0017249", 
           "Article: Who Is the Best Player Ever? A Complex Network Analysis of the History of Professional Tennis",
           target="_blank", style = "font-size:20px"),
         
         p("This app explores using a network graph to rank tennis players in a given range of years. 
       The metric generated from the network, Prestige Score, assigns a value to each player 
       similar to how the PageRank algorithm assigns rank to web pages. 
       The value is reflective of each player's match record, but importantly, also considers the quality of opponent for each match. 
       Being time independent, this approach allows for analysis between players who never played each other.",
           style = "font-size:25px")
),

tabPanel("Developers", 
         p("Anthony Girard",style = "font-size:25px"),
         p("e-mail: anthony_girard1@brown.edu",style = "font-size:20px"),
         p("Tianye Cui",style = "font-size:25px"),
         p("e-mail: tianye_cui@brown.edu",style = "font-size:20px"),
         p("Zhejia Dong",style = "font-size:25px"),
         p("e-mail: zhejia_dong@brown.edu",style = "font-size:20px"),
         
         
)
))
         


server <- function(input, output) {

    df<- eventReactive(input$update, {
        yeardf(input$startYear, input$endYear,
               type = input$TournamentType, sur = input$SurfaceType)
    })
        
    output$Figure1<- renderPlotly({
        withProgress(message = 'Making plot',
                     detail = 'This may take a while...', style="old", {
                         figure1(df())
                     })
        })

    output$best10<- function(){
        adjM <- adjacencyMatrix(df())
        prestige<- itera_fun(adjM)$preScore
        as.data.frame(cbind(1:10, prestige %>% arrange(desc(preScore)) %>% head(10) %>% select(players.name))) %>%
            knitr::kable(format="html", caption="Top 10 Players", col.names=c("Rank", "Players")) %>%
            kable_styling("striped", full_width = F) 
    }
    
    plot_data<- eventReactive(input$update, {
        create_plot_data(seq(input$startYear,input$endYear),
                         type = input$TournamentType, sur = input$SurfaceType)
    })
    
    top_players<- function(){
        adjM <- adjacencyMatrix(df())
        prestige<- itera_fun(adjM)$preScore
        return(unlist(prestige %>% arrange(desc(preScore)) %>% head(10) %>% select(players.name)))
    }
    
    output$rank<- renderPlotly({
        withProgress(message="Making plot",
                     detail = 'This may take a while...', style="old",{
                         create_plot(plot_data(), top_players())
                     })
    })
    
}

shinyApp(ui = ui, server = server)

