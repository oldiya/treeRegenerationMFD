################################################################################
# Title: Figures plotting stile for the manuscript
# Aim: Have a common plotting strategy for figures
# Author: Olalla Díaz-Yáñez | @olalla | olalladiaz.net 
################################################################################


# Order of models with names as in the dataset----

    modelsOrder <- c("Empirical" ,"4C", 
                     "ForCEEPS", "ForCEEPS(f)",
                     "FORMIND", "ForClim 1", "ForClim 11", 
                     "SIBYLA", "xComp", "PICUS",
                     "iLand", "LandClim", "Landis II", "TreeMig",
                     "LPJ-GUESS", "aDGVM2")

    selSpecies <- c("Abies alba","Acer pseudoplatanus","Alnus glutinosa",
                    "Betula spp.","Carpinus betulus","Fagus sylvatica",
                    "Fraxinus excelsior","Picea abies","Pinus sylvestris",
                    "Quercus spp.","Tilia cordata")

    aDVGMSpecies <- c("low SLA Dec. Rain",    "low SLA Dec. Light" ,
                      "low SLA Ever. Rain",   "low SLA Ever. Light",
                      "high SLA Dec. Light",  "high SLA Ever. Rain",
                      "high SLA Ever. Light")

# Colors per model -----

    values_color <- c("Empirical" = "#FF0000",   #red
                      "4C" = "#FF7F00",          #orange
                      "ForCEEPS" = "#A6CEE3",    #blue light 
                      "ForCEEPS(f)" = "#1F78B4", #blue darker
                      "FORMIND" = "#FDDBC7",     #salmon 
                      "ForClim 1" = "#B2DF8A",   #green light  
                      "ForClim 11" =  "#33A02C", #green dark
                      "SIBYLA" = "#E6AB02",      #brown light  
                      "PICUS" = "#660000",       #darkred
                      "xComp" = "#8B8B00",       #oliva
                      "iLand" = "#A6761D",       #brown dark
                      "LandClim" = "#00AFBB",    #blueish
                      "Landis II" = "#666666",   #grey
                      "TreeMig" = "#FDBF6F",     #light brown
                      "aDGVM2" = "#666600",      #green dark
                      "LPJ-GUESS" = "#CAB2D6")   #purple light
          

# Labels per model -----

    labels <- c("Empirical" = "Empirical", 
                "4C" = "4C", 
                "Forceeps" = "ForCEEPS",
                "Forceeps_f" = "ForCEEPS(f)",
                "Formind" = "FORMIND", 
                "ForClim1" = "ForClim1",  
                "ForClim11" = "ForClim11",  
                "Sibyla" = "SIBYLA", 
                "Picus" = "Picus",
                "xcomp" = "xComp",
                "iLand" = "iLand", 
                "LandClim" = "LandClim",  
                "Landis II" = "LANDIS-II",  
                "Treemig" = "TreeMig",
                "aDGVM2" = "aDGVM2", 
                "LPJ-GUESS" = "LPJ-GUESS")

# Labels figures  ----

    labEnvVarDDS <- "Seasonal degree-day sum (d°C)"
    labEnvVarWB <- "Climatic water balance (cm)"
    labEnvVartotBA <- expression("Total basal area (m"^2*"ha"^-1*")")
    labRecruitment <- expression("R (trees ha"^-1*"10yr"^-1*")")
    labMeanRecruitment <- expression(bar(R) * " (trees ha"^-1*"10yr"^-1*")")
    labShannonR <- c(bquote(bar(H) * " recruitment"))
