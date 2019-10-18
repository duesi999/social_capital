library(plotly)
library(processx)

# create graphing function
council.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of councils in data to loop over 
  council_list <- unique(df$council)
  
  # create for loop to produce plotly graphs 
  for (i in seq_along(council_list)) { 
    
    # create plot for each council in df 
    plot <- 
      plot_ly(subset(df, df$council==council_list[i]), labels = ~sentiment, values = ~freq, type = 'pie') %>%
      layout(title = council_list[i],
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    print(plot)
    #  paste0("plot_",council_list[i]) <- plot
    # assign(paste0("plotly_",council_list[i]), plot, envir = .GlobalEnv)
  }
}

council.graph(council_sentiment)

             
             
             
# #Stephen's suggestion for the function and loop
# plot_one_council <- function(df) {
#   #code to plot a single council's data
# }
# 
# council_plots <- council_sentiment %>%
#   split(.$council) %>% #is now a list of separate data frames per council
#   map(plot_one_council) #runs the plot_one_council function on each data frame in the list
# 
# #council_plots is now a list containing a plot for each council separately



p<- plot_ly(council_sentiment, labels = ~sentiment, values = ~freq, type = 'pie') %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p

orca(p, "test-plot.png")
