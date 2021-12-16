# PHP2560 Final Project
### Relevant article
Who Is the Best Player Ever? A Complex Network Analysis of the History of Professional Tennis


- https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0017249


- Data source: https://github.com/JeffSackmann/tennis_atp


- Shiny App Link: https://tcui6.shinyapps.io/tennis_app/

### Shiny app contents:
1. Reproduced Figure 1 in the original article to give an overall description of the datasets. To visualize the total number of tournaments in each year, and the fraction of players having played, won and lost a certain number of matches.


2. An interactive plot of the contact network of the players in ATP/WTA tournaments. Users can choose to restrict the visualization to certain year(s), type of court surface, or top 10 WTA/ATP players.


3. Derivation of the prestige score in the original article, with updated scores across user inputted year range. <br />

      
      
      *NOTE: Due to computational restrictions of Shiny app server, choosing a large enough year range in the app will crash the app - this works on personal machine. 
      
      
### Main functions:
##### yeardf(year.st, year.end, type = "ATP", sur = "all")
Given the year range, selected tournaments—type in c("ATP","WTA")— and surface type, return the data frame specified by the input. The tournament type is set to be "ATP" by default and the surface type is set to include all types by default. They are subject to change by user specification.

##### figure1(df)
Using the data frame we get from yeardf() function, plot the histograms of the total number of players and the total number of tournaments for each year.

##### adjacencyMatrix(df)
Given the data frame obtained from df() function, adjacencyMatrix is generated. adjM_{ij}, the weight in paper, is the number of times player j loses against player i.

##### itera_fun(M, d = 0.85, iter = 100)
Given adjacency matrix M, the damping parameter d is set by default as 0.85 as d is indicated to be (1-q) in the paper. iter is the number of iterations and is set by default as 100.\
This function uses adjacency matrix and damping parameter to compute prestige score by iteration, and obtain the converged prestige score. It returns a list with prestige score and all iteration results and iteration times.

##### create_plot_data(yrs, type2 = "ATP", sur = "all")
Create_plot_data prepares the data needed for plotting prestige score rank changes across years. The function takes as arguments, the year range of interest, ATP or WTA player specification, and surface type specification. Depending on the length of the year range, the prestige scores for players are calculated some number of times throughout the year range. Helper function get_rows calculates prestige score between two years, and returns a dataframe with player name, prestige score, the end year, and the rank of that player in terms of prestige score. This helper function is applied to incremented year ranges such that the prestige scores are calculated at various points in the user-specified year range as desired. This is computationally expensive, since in order to obtain the prestige score in a given year, the network has to be recreated with all year up to and including that year, but excluding years after.


##### create_plot(plot_data, players)
Create_plot plots an animated graph of prestige score rank changes given a player list. It takes in as arguments the data needed for the plot, prepared by create_plot_data, and the list of players to be graphed.
