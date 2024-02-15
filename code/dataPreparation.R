################################################################################
# Title: Data preparation from model simulations and observations
# Aim: Prepare and merge data from simulations and observations
#      produces dataOutputs.csv file needed in code/figures.R
# Authors: Olalla Díaz-Yáñez | @olalla | olalladiaz.net
#          and Yannek Käber 
################################################################################ 

# Download the data 
# IMPORTANT! this data is needed to run this script

# this section requires to download the study data
# the data is available in: 
# https://doi.org/10.3929/ethz-b-000643426
# via this link you can download the observations and simulation_results data
# these two folders should be placed under the folder data 
# data/observations/
# data/simulations_results/

# Load observations -----

    observationsData <- data.table::fread("data/observations/observations.csv")

# Load model simulations ----
    
    # Species asked to be simulated in the protocol
    # Assign other species to other category
    simSppAsked <- c("fasy", "Fagus sylvatica",
                     "pabi", "Picea abies", "piab",
                     "abal", "Abies alba",
                     "cabe", "Carpinus betulus",
                     "tico", "Tilia cordata",
                     "acps", "Acer pseudoplatanus",
                     "betu", "Betula spp.",
                     "frex", "Fraxinus excelsior",
                     "quer", "Quercus spp.",
                     "algl", "Alnus glutinosa",
                     "pisy", "Pinus sylvestris",
                     "othr" , "Other")

## Stand ----     

    ### ForClim 1 ----
    forclim1 <- data.table::fread("data/simulation_results/forclim/forclim_variant1.csv")
    forclim1$variant <- NULL 
    forclim1$trees <- NULL 
    forclim1$repID <- NULL 
    forclim1$year <- NULL 
    forclim1$species_long <- NULL 
    colnames(forclim1)[colnames(forclim1) == "plot_id"] <- "site"
    forclim1$model <- "ForClim 1"

    # Assign species simulated outside the ones asked in the protocol to category others
    forclim1$species[!forclim1$species %in% simSppAsked] <- "othr"
    
    # Joint values from other category 
    forclim12 <- forclim1 |>
      dplyr::group_by(sample, site, species, dbh, model) |>  # for each row
      dplyr::summarise(r.trees = sum(r.trees),
                       r.ba = sum(r.ba),
                       ba = sum(ba)) # calculate the sum of those columns
    
    forclim1 <- forclim12

    ### ForClim 11 ----
    forclim11 <- data.table::fread("data/simulation_results/forclim/forclim_variant11.csv")
    forclim11$variant <- NULL 
    forclim11$trees <- NULL 
    forclim11$repID <- NULL 
    forclim11$year <- NULL 
    forclim11$species_long <- NULL 
    colnames(forclim11)[colnames(forclim11) == "plot_id"] <- "site"
    forclim11$model <- "ForClim 11"

    # Assign species simulated outside the ones asked in the protocol to category others
    forclim11$species[!forclim11$species %in% simSppAsked] <- "othr"
    # Joint values from other category 
    forclim112 <- forclim11 |>
      dplyr::group_by(sample, site, species, dbh, model) |>  # for each row
      dplyr::summarise(r.trees = sum(r.trees),
                       r.ba = sum(r.ba),
                       ba = sum(ba)) # calculate the sum of those columns
    
    forclim11 <- forclim112

    ### ForCEEPS ----
    forceeps_7cm <- data.table::fread("data/simulation_results/forceeps/Results_Forceeps_7cm.csv")
    forceeps_10cm <- data.table::fread("data/simulation_results/forceeps/Results_Forceeps_10cm.csv")
    forceeps <- rbind(forceeps_7cm, forceeps_10cm)
    forceeps$model <- "ForCEEPS"

    ### ForCEEPS(f)----
    forceepsfeedback_7cm <- data.table::fread("data/simulation_results/forceeps/Results_Forceeps_feedback_7cm.csv")
    forceepsfeedback_10cm <- data.table::fread("data/simulation_results/forceeps/Results_Forceeps_feedback_10cm.csv")
    forceepsfeedback <- rbind(forceepsfeedback_7cm, forceepsfeedback_10cm)
    forceepsfeedback$model <- "ForCEEPS(f)"

    ### 4C ----
    fourC <- data.table::fread("data/simulation_results/4C/recruitment_data_4c_all_221213.csv")
    fourC$model <- "4C"
    
    ### SIBYLA ----
    sibyla <- data.table::fread("data/simulation_results/sibyla/TabTreesSibyla0_KJMerganic.csv")
    sibyla$model <- "SIBYLA"
    
    # Some sites have more than 200 samples, 
    # remove the extra samples reported to have 200 samples per site
    sibyla <- sibyla[!sibyla$sample > 200,]
    

    ### xComp----
    xcomp7 <- data.table::fread("data/simulation_results/xcomp/xcomp2022_simulation_v01_summary07cm.csv")
    xcomp7$V1 <- NULL 
    
    xcomp10 <- data.table::fread("data/simulation_results/xcomp/xcomp2022_simulation_v01_summary10cm.csv")
    xcomp10$V1 <- NULL 
    
    xcomp <- rbind(xcomp7, xcomp10)
    xcomp$model <- "xComp"

    # Add 0 values to those sites where there is 0 recruitment and 0 BA for a spp
    # that were simulated by the model
    sppSimxcomp <- unique(xcomp$species)

    missingxcomp <- xcomp |> 
                     dplyr::group_by(site, sample, dbh, model) |>
                     dplyr::summarize(species = sppSimxcomp[!sppSimxcomp %in% unique(species)],
                                      sample = unique(sample),
                                      site = unique(site),
                                      dbh  = unique(dbh), 
                                      model = unique(model),
                                      r.ba = 0,
                                      r.trees = 0,
                                      ba = 0)
    xcomp <- rbind(xcomp, missingxcomp)                                                    


    ### FORMIND ----
    formind <- data.table::fread("data/simulation_results/formind/formind_results_corrected.csv")
    formind$model <- "FORMIND"
    formind$dbh <- formind$dbh * 100


    ### PICUS ----
    picus <- data.table::fread("data/simulation_results/picus/PICUS_OutputData_2022-05-27.csv")
    picus$model <- "PICUS"
    
    # It has 201 sample per site, remove one sample

    picus <- picus[!picus$sample > 200,]
 
       
 ## Landscape ----

    ### iLand ----
    iland <- data.table::fread("data/simulation_results/iland/iland_data-2.csv")
    iland$model <- "iLand"

    ### landclim ----
    landclim <- data.table::fread("data/simulation_results/landclim/recruitmentAll.csv")
    landclim$model <- "LandClim"
    landclim$V1 <- NULL

    # less than 1 tree becomes 0 trees and 0 BA in recruitment
    landclim$r.ba[landclim$r.trees < 1] <- 0
    landclim$r.trees[landclim$r.trees < 1] <- 0

    # Change the species names
    landclim$species[landclim$species == "abiealba"] <- "abal"
    landclim$species[landclim$species == "piceabie"] <- "pabi"
    landclim$species[landclim$species == "pinusilv"] <- "pisy"
    landclim$species[landclim$species == "acerpseu"] <- "acps"
    landclim$species[landclim$species == "alnuglut"] <- "algl"
    landclim$species[landclim$species == "betuspp" ] <- "betu"
    landclim$species[landclim$species == "carpbetu"] <- "cabe"
    landclim$species[landclim$species == "fagusilv"] <- "fasy"
    landclim$species[landclim$species == "fraxexce"] <- "frex" 
    landclim$species[landclim$species == "querspp"] <- "quer"
    landclim$species[landclim$species == "tilicord"] <- "tico"

  # Add 0 values to those sites where there is 0 recruitment and 0 BA for a spp

    sppSimLandclim <- c("fasy","pabi","abal",  "cabe", "tico",  "acps", "betu",  
                        "frex", "quer", "algl", "pisy")

    missing <- landclim |> 
                 dplyr::group_by(site, sample, dbh, model) |>
                 dplyr::summarize(species = sppSimLandclim[!sppSimLandclim %in% unique(species)],
                                  sample = unique(sample),
                                  site = unique(site),
                                  dbh  = unique(dbh), 
                                  model = unique(model),
                                  r.ba = 0,
                                  r.trees = 0,
                                  ba = 0)
    landclim <- rbind(landclim, missing)                                                    


    ### Landis II ----
    
    landis <- data.table::fread("data/simulation_results/landisII/LANDIS-II_data_version_04.csv")
    
    landis$ID <- NULL 
    landis$V1 <- NULL 
    
    
    ### Treemig ----

    treemig2 <- read.delim("data/simulation_results/treemig/TreeMig_corr_220720/TreeMig_Data_corr_220720_2aOrigSeq_1Corr_4DecMort.txt",
                       header = TRUE, sep = ",", dec = ".")
    treemig2$model <- "TreeMig"  

    # Change species categories
    treemig2$species[treemig2$species == "quro"] <-  "Quercus spp." #Quercus robur
    treemig2$species[treemig2$species == "qupe"] <-  "Quercus spp." #Quercus petraea
    treemig2$species[treemig2$species == "bepe"] <-  "Betula spp."  #“Betula pendula", B. pubescens no simulated!

    # Assign to 0 recruitment to r.trees between 0 and 1
    treemig2$r.ba[treemig2$r.trees < 1 & treemig2$r.trees > 0] <- 0
    treemig2$r.trees[treemig2$r.trees < 1 & treemig2$r.trees > 0] <- 0

    # Assign other species to other category
    simSppAsked <- c("fasy", "Fagus sylvatica",
                     "pabi", "Picea abies", "piab",
                     "abal", "Abies alba",
                     "cabe", "Carpinus betulus",
                     "tico", "Tilia cordata",
                     "acps", "Acer pseudoplatanus",
                     "betu", "Betula spp.",
                     "frex", "Fraxinus excelsior",
                     "quer", "Quercus spp.",
                     "algl", "Alnus glutinosa",
                     "pisy", "Pinus sylvestris",
                     "othr" , "Other")
    treemig2$species[!treemig2$species %in% simSppAsked ] <- "othr"

    # Joint values from other category 
    treemig2 <- treemig2 |>
                  dplyr::group_by(sample, site, species,dbh, model) |>  # for each row
                  dplyr::summarise( r.trees = sum(r.trees),
                                    r.ba = sum(r.ba),
                                    ba = sum(ba)) # calculate the sum of those columns

     treemig <- treemig2

  ## Global ----

    ### aDGVM2 ----
    adgvm2 <- data.table::fread("data/simulation_results/adgvm2/results_aDGVM2.dat")
    adgvm2$model <- "aDGVM2"

    # Changes in species levels
    adgvm2$species[adgvm2$species == 1] <- "low SLA Dec. Rain"    # 1 - low SLA deciduous rain-triggered
    adgvm2$species[adgvm2$species == 2] <- "low SLA Dec. Light"   # 2 - low SLA deciduous light-triggered
    adgvm2$species[adgvm2$species == 3] <- "low SLA Ever. Rain"   # 3 - low SLA evergreen rain-triggered
    adgvm2$species[adgvm2$species == 4] <- "low SLA Ever. Light"  # 4 - low SLA evergreen light-triggered
    adgvm2$species[adgvm2$species == 5] <- "low SLA Dec. Rain"    # 5 - high SLA deciduous rain-triggered
    adgvm2$species[adgvm2$species == 6] <- "high SLA Dec. Light"  # 6 - high SLA deciduous light-triggered
    adgvm2$species[adgvm2$species == 7] <- "high SLA Ever. Rain"  # 7 - high SLA evergreen rain-triggered
    adgvm2$species[adgvm2$species == 8] <- "high SLA Ever. Light" # 8 - high SLA evergreen light-triggered 


    ### LPJ-GUESS ----

    lpjguess2 <- read.delim("data/simulation_results/lpjguess/Output_SGN_LPJGUESS_updated_patchdestroy_withoutBES.txt",
                            header = TRUE, sep = "\t", dec = ".")

    lpjguess2$model <- "LPJ-GUESS"
    lpjguess2$n <- NULL


    lpjguess27 <- lpjguess2[, c("site", "sample", "species", "r_7cm.trees", 
                                "r_7cm.ba", "ba", "model")]
    colnames(lpjguess27)[colnames(lpjguess27) == "r_7cm.trees"] <- "r.trees"
    colnames(lpjguess27)[colnames(lpjguess27) == "r_7cm.ba"] <- "r.ba"
    lpjguess27$dbh <- 7

    lpjguess210 <- lpjguess2[, c("site", "sample", "species", "r_10cm.trees", 
                             "r_10cm.ba", "ba",  "model")]
    colnames(lpjguess210)[colnames(lpjguess210) == "r_10cm.trees"] <- "r.trees"
    colnames(lpjguess210)[colnames(lpjguess210) == "r_10cm.ba"] <- "r.ba"
    lpjguess210$dbh <- 10

    lpjguess2 <- rbind(lpjguess27, lpjguess210)

    # Change species names 
    lpjguess2$species[lpjguess2$species == "quer_pub"] <- "quer"
    lpjguess2$species[lpjguess2$species == "quer_rob"] <- "quer"
    lpjguess2$species[lpjguess2$species == "quer_ile"] <- "othr"
    lpjguess2$species[lpjguess2$species == "MRS" ] <- "othr" # ???
    lpjguess2$species[lpjguess2$species == "betu_pen"] <- "Betula spp"
    lpjguess2$species[lpjguess2$species == "betu_pub"] <- "Betula spp"


    # Remove simulated shrubs ("BES", and "quer_coc") 
    lpjguess2 <- lpjguess2[!lpjguess2$species == "quer_coc", ] #"Quercus coccifera"
    #lpjguess2 <- lpjguess2[!lpjguess2$species == "BES", ] #"Boreal shrub" #Boreal evergreen shrub
    lpjguess2 <- lpjguess2[!lpjguess2$species == "jun_oxy", ]#"Juniperus oxycedrus"
    lpjguess2 <- lpjguess2[!lpjguess2$species == "cor_ave", ] # Corylus avellana
                     # there are only three tree observations,
                     # they are removed because it is impossible to model the category "others" 
                     # for this model with only three observations


    # Adding species not present but simulated in LPJ-GUESS 2
    sppSimLpjGuess2 <- c("pabi", "fasy", "abal", "pisy", "quer", "Betula spp",
                         "othr", "cabe", "frex", "tico")
    missingLpjGuess2 <- lpjguess2 |> 
                            dplyr::group_by(site, sample, dbh, model) |>
                            dplyr::summarize(species = sppSimLpjGuess2[!sppSimLpjGuess2 %in% unique(species)],
                                             sample = unique(sample),
                                             site = unique(site),
                                             dbh  = unique(dbh), 
                                             model = unique(model),
                                             r.ba = 0,
                                             r.trees = 0,
                                             ba = 0)
    lpjguess2 <- rbind(lpjguess2, missingLpjGuess2) 
   
    # Joint values from other category 
    lpjguess2 <- lpjguess2 |>
      dplyr::group_by(sample, site, species, dbh, model) |>  # for each row
      dplyr::summarise(r.trees = sum(r.trees),
                       r.ba = sum(r.ba),
                       ba = sum(ba)) # calculate the sum of those columns
    
    lpjguess <- lpjguess2 
    
    
    
  # Missing samples in some sites
  # IMPORTANT! Missing samples were grasslands (authors personal com.)
  # and will be added here as no recruitment 
    
    # 7cm
    nosamples7 <- as.data.frame(table(lpjguess$site[lpjguess$dbh == 7], 
                                      lpjguess$sample[lpjguess$dbh == 7]))
    missingSamples7 <- nosamples7[nosamples7$Freq == 0, ]
    colnames(missingSamples7) <- c("site", "sample", "freq")
    missingSamples7$sample <- as.numeric(levels(missingSamples7$sample))[missingSamples7$sample] 
    missingSamples7$site <- as.numeric(levels(missingSamples7$site))[missingSamples7$site] 
    missingSamples7$freq <- NULL
    missingSamples7$model <- "LPJ-GUESS" 
    missingSamples7$dbh <- 7
    missingSamples7$r.trees  <- 0  
    missingSamples7$r.ba <- 0  
    missingSamples7$ba <- 0
    missingSamples7$species <- "pabi"
    
    missingLpjGuess_nosamples7 <- missingSamples7 |> 
      dplyr::group_by(site, sample, dbh, model) |>
      dplyr::summarize(species = sppSimLpjGuess2[!sppSimLpjGuess2 %in% unique(species)],
                       sample = unique(sample),
                       site = unique(site),
                       dbh  = unique(dbh), 
                       model = unique(model),
                       r.ba = 0,
                       r.trees = 0,
                       ba = 0)
    missingSamples7 <- rbind(missingSamples7, missingLpjGuess_nosamples7)
    
    
    # Total number of sites with missing samples 
    table(missingSamples7$site) 
    sum(table(missingSamples7$site))
    
    #10cm
    nosamples10 <- as.data.frame(table(lpjguess$site[lpjguess$dbh == 10], 
                                       lpjguess$sample[lpjguess$dbh == 10]))
    missingSamples10 <- nosamples10[nosamples10$Freq == 0,]
    colnames(missingSamples10) <- c("site", "sample", "freq")
    missingSamples10$sample <- as.numeric(levels(missingSamples10$sample))[missingSamples10$sample] 
    missingSamples10$site <- as.numeric(levels(missingSamples10$site))[missingSamples10$site] 
    missingSamples10$freq <- NULL
    missingSamples10$model <- "LPJ-GUESS" 
    missingSamples10$dbh <- 10
    missingSamples10$r.trees  <- 0  
    missingSamples10$r.ba <- 0  
    missingSamples10$ba <- 0
    missingSamples10$species <- "pabi"
    
    missingLpjGuess_nosamples10 <- missingSamples10 |> 
      dplyr::group_by(site, sample, dbh, model) |>
      dplyr::summarize(species = sppSimLpjGuess2[!sppSimLpjGuess2 %in% unique(species)],
                       sample = unique(sample),
                       site = unique(site),
                       dbh  = unique(dbh), 
                       model = unique(model),
                       r.ba = 0,
                       r.trees = 0,
                       ba = 0)
    missingSamples10 <- rbind(missingSamples10, missingLpjGuess_nosamples10)

    #Total number of sites with missing samples 
    table(missingSamples10$site) 
    sum(table(missingSamples10$site))
    
    
    lpjguess <- rbind(lpjguess, missingSamples7)
    lpjguess <- rbind(lpjguess, missingSamples10)
    
    #One site is missing 
    unique(observationsData$site)[ !unique(observationsData$site)  %in% unique(lpjguess$site)]
    
    

# Combined data for all models ----     
    simulationData <- dplyr::bind_rows(forclim1, forclim11, 
                                       forceeps, forceepsfeedback,
                                       picus, xcomp, sibyla, fourC, formind,
                                       iland, landis, landclim,
                                       treemig, lpjguess, adgvm2)
    
    simulationData$end_year <- NULL 

    # Add environmental variables to simulated data 
    
    envData <- observationsData[, c("site", "dds", "wb")] |> 
                    dplyr::group_by(site) |> 
                    dplyr::filter(dplyr::row_number() == 1) 
    
    simulationResults <- merge(simulationData, envData, 
                               by = "site", all.x = TRUE)
    
    # Add observations and simulation results data 
    dataOutputs <- rbind(observationsData, simulationResults)

### Change species names 

    dataOutputs$species[dataOutputs$species == "abal"] <- "Abies alba"
    dataOutputs$species[dataOutputs$species == "acps"] <- "Acer pseudoplatanus"
    dataOutputs$species[dataOutputs$species == "algl"] <- "Alnus glutinosa"
    dataOutputs$species[dataOutputs$species == "betu"] <- "Betula spp."
    dataOutputs$species[dataOutputs$species == "Betula spp"] <- "Betula spp."
    dataOutputs$species[dataOutputs$species == "cabe"] <-  "Carpinus betulus"
    dataOutputs$species[dataOutputs$species == "fasy"] <-  "Fagus sylvatica"
    dataOutputs$species[dataOutputs$species == "frex"] <-  "Fraxinus excelsior"
    dataOutputs$species[dataOutputs$species == "piab"] <-  "Picea abies" 
    dataOutputs$species[dataOutputs$species == "pabi"] <-  "Picea abies" 
    dataOutputs$species[dataOutputs$species == "pisy"] <-  "Pinus sylvestris" 
    dataOutputs$species[dataOutputs$species == "quer"] <-  "Quercus spp." 
    dataOutputs$species[dataOutputs$species == "tico"] <-  "Tilia cordata" 
    dataOutputs$species[dataOutputs$species == "othr"] <-  "Other spp." 
    dataOutputs$species[dataOutputs$species == "other"] <- "Other spp."


# Calculate r.Totba and Totba per site and sample-----
    # Calculate the total basal area and total recruited basal area 
    simAgg <- dataOutputs |>
                 dplyr::group_by(site, sample, model, dbh) |> 
                 dplyr::summarise(r.trees = round(sum(r.trees), 2), 
                                  r.ba = round(sum(r.ba), 4),
                                  ba = round(sum(ba), 2), # This is total BA in the site, value per ha!
                                  dds = unique(dds), 
                                  wb = unique(wb))

    # Add the total ba for all the species 
    BATot <- simAgg[, c("site", "sample", "model", "ba", "r.ba", "dbh")]
    colnames(BATot)[4] <- "Totba"
    colnames(BATot)[5] <- "Totr.ba"
    dataOutputs <- merge(dataOutputs, BATot, 
                         by = c("site", "sample", "dbh", "model"))

    
# Save the data -----

    data.table::fwrite(dataOutputs, file = "data/dataOutputs.csv")
    


