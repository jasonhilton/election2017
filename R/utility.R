transparent_theme <- theme(
    panel.background = element_rect(fill = "transparent",
                                    colour="transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent",
                                     colour="transparent") # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent",
                                       colour="transparent",
                                       linetype = 0) # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent", 
                                           colour="transparent") # get rid of legend panel bg
  )
