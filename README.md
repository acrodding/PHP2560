# PHP2560
Relevant article
Who Is the Best Player Ever? A Complex Network Analysis of the History of Professional Tennis
https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0017249
Data source: https://github.com/JeffSackmann/tennis_atp

Description in Shiny App:
This app explores using a network graph to rank tennis players in a given range of years. The metric generated from the network, Prestige Score, assigns a value to each player similar to how the PageRank algorithm assigns rank to web pages. The value is reflective of each player's match record, but importantly, also considers the quality of opponent for each match. Being time independent, this approach allows for analysis between players who never played each other. 

The user may specify year range, court surface, and the choice of players in either the WTA or ATP

Description in Plot Tab:
The plot below shows how the rank of the top 10 players in the specified year range changes between years. 


Proposed Shiny app contents:
1. Reproduce Figure 1 in the original article to give an overall description of the datasets. To visualize the total number of tournaments in each year, and the fraction of players having played, won and lost a certain number of matches.
2. An interactive plot of the contact network of the players in ATP tournaments. (It should look like panel A of Figure 2 in the original article.) Users can choose to restrict the visualization to certain year(s), type of court surface, or numbers of top players to display.
3. Simulation of the prestige score deriving process in the original article, and check for their match. 
