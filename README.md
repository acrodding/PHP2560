# PHP2560
Relevant article
Who Is the Best Player Ever? A Complex Network Analysis of the History of Professional Tennis
https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0017249
Data source: https://github.com/JeffSackmann/tennis_atp


Shiny App Link: https://tcui6.shinyapps.io/tennis_app/

Shiny app contents:
1. Reproduced Figure 1 in the original article to give an overall description of the datasets. To visualize the total number of tournaments in each year, and the fraction of players having played, won and lost a certain number of matches.
2. An interactive plot of the contact network of the players in ATP/WTA tournaments. Users can choose to restrict the visualization to certain year(s), type of court surface, or top 10 WTA/ATP players.
3. Derivation of the prestige score in the original article, with updated scores across user inputted year range. *
      *NOTE: Due to computational restrictions of Shiny app server, choosing a large enough year range in the app will crash the app - this works on personal machine.
