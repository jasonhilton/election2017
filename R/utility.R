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



commonwealth_countries <- c(
"Antigua and Barbuda",
"Australia",
"Bangladesh",
"Barbados",
"Belize",
"Botswana",
"Brunei",
"Cameroon",
"Canada",
"Dominica",
"Fiji",
"The Gambia",
"Ghana",
"Grenada",
"Grenadines",
"Guyana",
"India",
"Jamaica",
"Kenya",
"Kiribati",
"Lesotho",
"Malawi",
"Malaysia",
"Malta",
"Mauritius",
"Mozambique",
"Namibia",
"Nauru",
"New Zealand",
"Nigeria",
"Pakistan",
"Papua New Guinea",
"Republic of Cyprus*",
"Rwanda",
"Samoa",
"Seychelles",
"Sierra Leone",
"Singapore",
"Solomon Islands",
"South Africa",
"Sri Lanka",
"St Christopher and Nevis",
"St Lucia",
"St Vincent and the",
"Swaziland",
"The Bahamas",
"Tonga",
"Trinidad and Tobago",
"Tuvalu",
"Uganda",
"United Kingdom",
"United Republic of Tanzania",
"Vanuatu",
"Zambia",
"Zimbabwe"
) 
# Source:
# https://www.tendringdc.gov.uk/council/elections-voting/list-commonwealth-countries-voting-rights


plot_matrix <- function(sample_mat, plot_dim =c("columns","rows"), add=F, ...){
  plot_dim <- match.arg(plot_dim)
  if (plot_dim == "rows"){
    sample_mat <- t(sample_mat)
  }
  
  iters <- 1:ncol(sample_mat)
  if (!add){
    plot(sample_mat[,1], ylim=range(sample_mat), type="l", ...)  
  }
  rubbish<-lapply(iters, function(j) points(sample_mat[,j], type="l", ...))
}
