library(plotly)
library(cowplot)

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
  }
}

council.graph(council_sentiment)

             
             
             
  
  