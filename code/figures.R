################################################################################
# Title: Figures
# Aim: Create figures and analysis for the manuscript
# Author: Olalla Díaz-Yáñez | @olalla | olalladiaz.net &
#         some part of the figures code were based in the work of the 
#         regeneration workshop attendees
################################################################################

# Load plotting styles 

    source("code/plottingManuscript.R")

# Load data ----

    outputsDF <- data.table::fread("data/dataOutputs.csv")
    outputsDF$model[outputsDF$model == "Empirical"] <- "Observed"

    # Number of trees in the observed data 
    obsNum <- outputsDF[outputsDF$model == "Observed", ]
    round(sum(obsNum$r.trees, na.rm = TRUE), 0)
    
    obsNumAgg <- obsNum |> 
                   dplyr::group_by(site, sample) |>
                   dplyr::summarise(r.ha = sum(r.trees))
    
    sum(obsNumAgg$r.ha, na.rm =  T)
    range(obsNumAgg$r.ha, na.rm = TRUE)
    mean(obsNumAgg$r.ha, na.rm = TRUE)
    mean(obsNumAgg$r.ha[!obsNumAgg$r.ha == 0], na.rm = TRUE)
    median(obsNumAgg$r.ha, na.rm = TRUE)
    
    # Assign models and dbh to levels 

    outputsDF$model <- factor(outputsDF$model, levels = modelsOrder)
    outputsDF$dbh <- factor(outputsDF$dbh, levels = c("7", "10"))
    
# Remove the 35 sites from the models data for threshold 7cm 
# as they do not have observations for those sites

    sitesWoObs <- outputsDF[is.na(outputsDF$r.trees), ]
    outputsDF <- outputsDF[!(outputsDF$dbh == 7 & outputsDF$site %in% unique(sitesWoObs$site)), ]

# Data preparation ----    
    
    ## Recruitment Shannon index  by basal area ----
    ShannonDF <- outputsDF 
    ShannonDF$giDIVG <- ShannonDF$r.ba / ShannonDF$Totr.ba
    ShannonDF$lngiDIVG <- log(ShannonDF$r.ba / ShannonDF$Totr.ba)
    ShannonDF$mult <- round(ShannonDF$giDIVG * ShannonDF$lngiDIVG, 2)
    
    # Sum across species in each observation
    ShannonIndex <- data.frame(cbind(aggregate(mult ~ site + sample + model + dbh,
                                               sum, data = ShannonDF)))
    
    ShannonIndex$mult <- (-1) * (ShannonIndex$mult)
    colnames(ShannonIndex) <- c("site", "sample", "model", "dbh",
                                "ShannonIndexRecruit")
    
    ## Stand Shannon index ----
    ShannonDFAll <- outputsDF 
    ShannonDFAll$giDIVG <- ShannonDFAll$ba / ShannonDFAll$Totba
    ShannonDFAll$lngiDIVG <- log(ShannonDFAll$ba / ShannonDFAll$Totba)
    ShannonDFAll$mult <- round(ShannonDFAll$giDIVG * ShannonDFAll$lngiDIVG, 2)
    
    # Sum across species per site and sample
    ShannonIndexAll <- data.frame(cbind(aggregate(mult ~ site + sample + model + dbh,
                                                  sum, data = ShannonDFAll)))
    
    ShannonIndexAll$mult <- (-1) * (ShannonIndexAll$mult)
    colnames(ShannonIndexAll) <- c("site", "sample", "model",
                                   "dbh", "ShannonIndexAllAges")
    
    ShannonIndex <- merge(ShannonIndex, ShannonIndexAll, by = c("site", "sample",
                                                                "model","dbh"))
    
    ## Relative Shannon index ----
    
    ShannonIndex$relShannon <- ShannonIndex$ShannonIndexRecruit / ShannonIndex$ShannonIndexAllAges
    
# Zero inflated data in recruitment ----
      
    zeroInflSim <- outputsDF[!outputsDF$model == "Observed",]
    zeroInflEmp <- outputsDF[outputsDF$model == "Observed",]
    
    zeroInflSim <-  zeroInflSim[, c("site", "sample", "model", "Totr.ba")]
    zeroInflSim <- zeroInflSim[!duplicated(zeroInflSim)]
    zeroInflEmp <- zeroInflEmp[, c("site", "sample", "Totr.ba")]
    zeroInflEmp <- zeroInflEmp[!duplicated(zeroInflEmp)]
    
    #Number of sites and samples with recruitment equal to 0 in observations
    zeroInflatedAll <- outputsDF[outputsDF$Totr.ba == 0, ]
    zeroInflatedAll <- zeroInflatedAll[, c("site", "sample",  "model", "Totr.ba", "dbh")]
    zeroInflatedAll <-  zeroInflatedAll[!duplicated(zeroInflatedAll)]
    
    # Zero data
    zerroAll <- as.array(table(zeroInflatedAll$model, zeroInflatedAll$dbh))
    colnames(zerroAll) <- c("zero7", "zero10")
    #Total observations
    allObs <- as.array(table(outputsDF$model, outputsDF$dbh ))
    colnames(allObs) <- c("allObs7", "allObs10")
    
    inflatedZeroDF <- as.data.frame(cbind(allObs, zerroAll))
    
    inflatedZeroDF$share10 <- round((inflatedZeroDF$zero10 / inflatedZeroDF$allObs10), 4) * 100
    inflatedZeroDF$share7 <- round((inflatedZeroDF$zero7 / inflatedZeroDF$allObs7), 4) * 100
    
    colnames(inflatedZeroDF) <- c("Total (7 cm)", "Total (10cm)",
                                  "No recruitment (7 cm)", 
                                  "No recruitment (10cm)", 
                                  "Percentage of no recruitment (7 cm)", 
                                  "Percentage of no recruitment (10 cm)")
    inflatedZeroDF <- tibble::rownames_to_column(inflatedZeroDF, "Model")
    
    data.table::fwrite(inflatedZeroDF, file = "figures/noRecruitment.csv")
    
# Ingrowth levels ----

    overDTsim <- outputsDF |> 
        dplyr::group_by(site, sample, dbh, model) |>
        dplyr::summarise(r.trees = sum(r.trees),
                         r.ba = sum(r.ba),
                         Totba = unique(Totba)) |>
        dplyr::group_by(site, dbh, model) |>
        dplyr::summarise(r.trees = mean(r.trees),
                         r.ba = mean(r.ba),
                         Totba = mean(Totba))
    
  ### Fig. overestimation 7&10 ----

    recOverAll <- ggplot2::ggplot() +
        ggplot2::geom_boxplot(ggplot2::aes(y = r.trees, x =  model, 
                                           fill = model, alpha = dbh), 
                              data = overDTsim, 
                              outlier.alpha = 0.1) +
        ggplot2::theme(legend.position = "right", 
                       panel.background = ggplot2::element_blank(), 
                       axis.line = ggplot2::element_line(colour = "black"),
                       legend.key =  ggplot2::element_blank(), 
                       legend.title = ggplot2::element_blank(),
                       strip.background = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(angle = 90),
                       strip.text.y = ggplot2::element_text(angle = 0),
                       panel.border = ggplot2::element_blank()) +
        ggplot2::xlab(label = "") +
        ggplot2::ylab(label = labMeanRecruitment) +
        ggplot2::scale_fill_manual(values = values_color[names(values_color) %in% modelsSel],
                                  guide = "none") +
        ggplot2::scale_alpha_manual(values = c(0.3, 1),
                                    guide = ggplot2::guide_legend(override.aes = list(alpha = c(0.4, 1),
                                                                             fill = "red")),
                                    name = "", #"Diameter threshold",
                                    labels = c('7 cm', '10 cm')) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = quantile(overDTsim$r.trees[overDTsim$dbh == "7" & overDTsim$model == "Observed"],
                                                               c(0.25),
                                                               na.rm = TRUE)), 
                            colour = "#990000", linetype = "dashed") +
        ggplot2::geom_hline(ggplot2::aes(yintercept = quantile(overDTsim$r.trees[overDTsim$dbh == "7" & overDTsim$model == "Observed"],
                                                               c(0.75),
                                                               na.rm = TRUE)),
                            colour = "#990000", linetype = "dashed") 
    
    ggplot2::ggsave("figures/Figure2.jpg", 
                    plot =  recOverAll,
                    width = 21, height = 12, scale = 0.9,
                    dpi = 300, units = "cm", 
                    device = 'jpg') 
    
# Diversity in recruitment ----

    ### Fig. H7 & H10-----
  
    # Data preparation 
    # Mean Shannon index per site (across samples)
    dat710 <- ShannonIndex |>  
        dplyr::group_by(model, site, dbh) |> 
        dplyr::summarise(ShannonIndexRecruit = mean(ShannonIndexRecruit))
    dat710$dbh <- ordered(dat710$dbh, levels = c("7", "10"))
    modelsSel <-  modelsOrder[!modelsOrder  == "aDGVM2"]
    
    #### Plot
    H7_10 <- ggplot2::ggplot(dat710[!dat710$model == "aDGVM2", ],
                             ggplot2::aes(y = ShannonIndexRecruit, 
                                          x = model,
                                          fill = model,
                                          alpha = dbh)) + 
        ggplot2::scale_x_discrete(labels = labels) +
        ggplot2::geom_boxplot(notch = F) +
        #ggplot2::ylim(c(0, 0.3)) +
        ggplot2::xlab(label = "") +
        ggplot2::ylab(label = bquote(bar(H) * " recruitment")) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                       strip.text.y = ggplot2::element_text(angle = 0),
                       legend.position = "right",
                       panel.background = ggplot2::element_blank(), 
                       axis.line = ggplot2::element_line(colour = "black"),
                       legend.key =  ggplot2::element_blank(), 
                       strip.background =  ggplot2::element_blank()) +
        ggplot2::scale_fill_manual(#labels[names(labels) %in%  unique(dat710[!dat710$model == "aDGVM2", ])],
            values = values_color[names(values_color) %in% modelsSel],
            guide = "none") +
        ggplot2::scale_alpha_manual(values = c(0.3, 1),
                                    guide = ggplot2::guide_legend(override.aes = list(alpha = c(0.4, 1),
                                                                             fill = "red")),
                                    name = "", #"Diameter threshold",
                                    labels = c('7 cm', '10 cm')) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = quantile(dat710$ShannonIndexRecruit[dat710$dbh == "7" & dat710$model == "Observed"],
                                                               c(0.25))), 
                            colour = "#990000", linetype = "dashed") +
        ggplot2::geom_hline(ggplot2::aes(yintercept = quantile(dat710$ShannonIndexRecruit[dat710$dbh == "7" & dat710$model == "Observed"],
                                                               c(0.75))),
                            colour = "#990000", linetype = "dashed") 
    
    ggplot2::ggsave("figures/Figure3.jpg",
                    plot =   H7_10,
                    width = 21, height = 12, scale = 0.9,
                    dpi = 300, units = "cm", device = 'jpg')
    
    
    # Significant difference per model between 7 and 10 diameter threshold
    
    sigH7_10 <- ggpubr::compare_means(ShannonIndexRecruit ~ dbh, 
                                      data = dat710[!dat710$model == "aDGVM2", ],
                                      group.by = "model", method = "t.test")
    
    write.csv(sigH7_10,"figures/sigH7_10.csv", row.names = FALSE)
    
    
    # GLME 
    
   
    ### Fig. H obs. VS sim. in stand and recruitment  ----
    
    ### Data preparation
    ### Prepare data to have a set of Observed and simulated values per model
    empShannon <- ShannonIndex[ShannonIndex$model == "Observed",]
    plotShannonDF <- ShannonIndex[!ShannonIndex$model == "Observed", ]

    # Repeat All Rows (Multiple)
    empShannon <- empShannon |>
        dplyr::slice(rep(1:dplyr::n(), length(unique(plotShannonDF$model))))
    empShannon$model <- rep(unique(plotShannonDF$model), each = 579)
    empShannon$type <- "Observed"
    plotShannonDF$type <- "model"

    dataPlot <- rbind(empShannon, plotShannonDF)
    dataPlot$type <- factor(dataPlot$type, levels = c("Observed",  "model"))
    dataPlotS <- rbind(empShannon, plotShannonDF)
    dataPlotS$type <- factor(dataPlotS$type, levels = c("Observed",  "model"))
    dataPlot7 <- dataPlot[dataPlot$dbh == "7", ]
    dataPlot10 <- dataPlot[dataPlot$dbh == "10", ]
    
    plotShannonDF <- ShannonIndex[!ShannonIndex$model == "Observed", ]
    empShannon <- ShannonIndex[ShannonIndex$model == "Observed",]
    colnames(empShannon) <- c("site", "sample","model", "dbh",
                              "Emp_ShannonIndexRecruit", "Emp_ShannonIndexAllAges",
                              "Emp_relShannon")
    empShannon <- empShannon[, c("site", "sample", "dbh",
                                 "Emp_ShannonIndexRecruit", "Emp_ShannonIndexAllAges",
                                 "Emp_relShannon")]
    plotShannonDFmean <-  plotShannonDF |>
        dplyr::group_by(site, model, dbh) |>
        dplyr::summarise(ShannonIndexRecruitSD = sd(ShannonIndexRecruit),
                         ShannonIndexRecruitMEAN = mean(ShannonIndexRecruit),
                         ShannonIndexAllAgesSD = sd(ShannonIndexAllAges),
                         ShannonIndexAllAgesMEAN = mean(ShannonIndexAllAges))
    
    empShannonmean <- empShannon |>
        dplyr::group_by(site, dbh) |>
        dplyr::summarise(Emp_ShannonIndexRecruitSD = sd(Emp_ShannonIndexRecruit),
                         Emp_ShannonIndexRecruitMEAN = mean(Emp_ShannonIndexRecruit),
                         Emp_ShannonIndexAllAgesSD = sd(Emp_ShannonIndexAllAges),
                         Emp_ShannonIndexAllAgesMEAN = mean(Emp_ShannonIndexAllAges))
    
    plotShannonDF2 <- merge(plotShannonDFmean, empShannonmean, 
                            by = c("site", "dbh"), all.x = TRUE)
    
    # Plot stand observed VS. simulated
    selModels2 <-  c("ForClim 11", "iLand", "4C")
    plotData3 <- plotShannonDF2[plotShannonDF2$model %in% selModels2 ,]
    plotData3 <- plotData3[plotData3$dbh == "7", ]
    
    labels2 <- c(paste0("Overpredict", "\n(n=4)"),
                 paste0("Intermediate", "\n(n=7)"),
                 paste0("Underpredict", "\n(n=3)"))
    
    plotData3$model <- factor(plotData3$model, levels = selModels2, labels = labels2)
    
    sm <- ggplot2::ggplot(plotData3,
                          ggplot2::aes(x = Emp_ShannonIndexAllAgesMEAN,
                                       y = ShannonIndexAllAgesMEAN,
                                       color = "black"),
                          fill = "white") + 
        ggplot2::guides(color = "none") +
        ggplot2::scale_color_manual(values = "black") + 
        ggplot2::geom_point(size = 0.6, alpha = 0.7, position = "jitter") + 
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(), 
                       strip.background =  ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black"),
                       legend.position = "right") +
        ggplot2::coord_cartesian(xlim = c(0, 2), ylim = c(0, 2)) +  
        ggplot2::labs(y = bquote(bar(H) * " stand"),
                      x = bquote(bar(H) * " stand")) +
        ggplot2::facet_wrap(~model) +
        ggplot2::geom_abline(linetype = "dashed")
    
    
    # Plot recruited observed VS. simulated
    selModels <- c("PICUS","FORMIND", "Landis II")
    
    plotData2 <- plotShannonDF2[plotShannonDF2$model %in% selModels, ]
    plotData2 <- plotData2[plotData2$dbh == "7", ]
    
    labelsMod <- c(paste0("Overpredict", "\n(n=7)"),
                   paste0("Intermediate", "\n(n=5)"),
                   paste0("Underpredict", "\n(n=2)"))
    
    plotData2$model <- factor(plotData2$model, levels = selModels, labels = labelsMod)
    
    
    sl <- ggplot2::ggplot(plotData2,
                          ggplot2::aes(x = Emp_ShannonIndexRecruitMEAN,
                                       y = ShannonIndexRecruitMEAN,
                                       color = "black"),
                          fill = "white") + 
        ggplot2::guides(color = "none") +
        ggplot2::scale_color_manual(values = "black") + 
        ggplot2::geom_point(size = 0.6, alpha = 0.7, position = "jitter") + #color = c("#69b3a2", rgb(0.3,0.5,1,0.4))
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(), 
                       strip.background =  ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black"),
                       legend.position = "right") +
        ggplot2::coord_cartesian(xlim = c(0, 2), ylim = c(0, 2)) +  
        ggplot2::labs(y = bquote(bar(H) * " recruitment"),
                      x = bquote(bar(H) * " recruitment")) +
        ggplot2::facet_wrap(~model) +
        ggplot2::geom_abline(linetype = "dashed")
    
    
    xx <- ggpubr::ggarrange(sm, sl,
                            labels = c("A", "B"),
                            ncol = 1, nrow = 2, heights = c(3, 3), align = "v")
    
    xx <- ggpubr::annotate_figure(xx, 
                                  left = grid::grid.text("Simulated", 
                                                         rot = 90, vjust = 1,
                                                         gp = grid::gpar(cex = 1.3)),
                                  bottom = grid::grid.text("Observed", 
                                                           gp = grid::gpar(cex = 1.3)))
    
    ggplot2::ggsave("figures/Figure4.jpg",
                    plot = xx,
                    dpi = 300, 
                    width = 20, height = 18, scale = 0.9,
                    units = "cm", device = 'jpg')
    
    
    #### FigS. rec vs all 7cm ----
    
    p <- ggplot2::ggplot(dataPlot7[!dataPlot7$model == "aDGVM2",],
                         ggplot2::aes(x = ShannonIndexAllAges,
                                      y = ShannonIndexRecruit,
                                      color = type,
                                      alpha = type),
                         fill = "white") + 
        ggplot2::coord_cartesian(xlim = c(0, 2), ylim = c(0, 2)) +  
        ggplot2::geom_point(size = 0.1, alpha = 0.3 ) + 
        ggplot2::theme_classic() +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::labs(y = bquote(bar(H) * " recruitment"),
                      x = bquote(bar(H) * " stand")) +
        ggplot2::ggtitle("7 cm diameter threshold") +
        ggplot2::facet_wrap(~model) +
        ggplot2::geom_abline() +
        ggplot2::scale_color_manual(labels = c("Observed" = "Observed", 
                                               "model" = "Simulated"),
                                    values = c("#FDAE61", "grey")) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 10)))
    
    
    ggplot2::ggsave("figures/recruitmentAdultRichness7.png",
                    plot = p,
                    width = 20, height = 20, scale = 0.9,
                    dpi = 300, units = "cm", device = 'png')
    
    
    #### FigS. rec vs all 10cm ----
    r <- ggplot2::ggplot(dataPlot7[!dataPlot7$model == "aDGVM2",],
                         ggplot2::aes(x = ShannonIndexAllAges,
                                      y = ShannonIndexRecruit,
                                      color = type,
                                      alpha = type),
                         fill = "white") + 
        ggplot2::coord_cartesian(xlim = c(0, 2), ylim = c(0, 2)) +  
        ggplot2::geom_point(size = 0.1, alpha = 0.3 ) + 
        ggplot2::theme_classic() +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::labs(y = bquote(bar(H) * " recruitment"),
                      x = bquote(bar(H) * " stand")) +
        ggplot2::ggtitle("10 cm diameter threshold") +
        ggplot2::facet_wrap(~model) +
        ggplot2::geom_abline() +
        ggplot2::scale_color_manual(labels = c("Observed" = "Observed", 
                                               "model" = "Simulated"),
                                    values = c("#FDAE61", "grey")) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 10)))
    
    
    ggplot2::ggsave("figures/recruitmentAdultRichness10.png",
                    plot = r,
                    width = 20, height = 20, scale = 0.9,
                    dpi = 300, units = "cm", device = 'png')
    
    #### FigS. stand sim.vs obs.  species richness -----
    
    category <- c("A3",  # "4C" 
                  "A2",  # "ForCEEPS" 
                  "A3",  # "ForCEEPS(f)"
                  "A1",  # "FORMIND"
                  "A1",  # "ForClim 1"
                  "A1",  #"ForClim 11"
                  "A2",  # "SIBYLA"  
                  "A2",  # "xComp"
                  "A2",  # "PICUS"
                  "A2",  # "iLand"
                  "A2",  # "LandClim" 
                  "A2",  # "Landis II"
                  "A1",  # "TreeMig"
                  "A3")  # "LPJ-GUESS"
    
    ann_text2 <- data.frame(Emp_ShannonIndexRecruitMEAN = 1.7,
                            ShannonIndexRecruitMEAN = 0.1,
                            label = category,
                            dbh = "7",
                            model = factor(modelsOrder[!modelsOrder %in% c("Observed", "aDGVM2")],
                                           modelsOrder[!modelsOrder %in% c("Observed", "aDGVM2")]))
    
    e <- ggplot2::ggplot(plotShannonDF2[!plotShannonDF2$model == "aDGVM2",],
                         ggplot2::aes(x = Emp_ShannonIndexAllAgesMEAN,
                                      y = ShannonIndexAllAgesMEAN,
                                      color = dbh),
                         fill = "white") + 
        ggplot2::theme_classic() +
        ggplot2::coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 2.5)) +  
        ggplot2::geom_point(size = 0.6, alpha = 0.7 ) + 
        ggplot2::labs(y = bquote(bar(H) * " simulated stand"),
                      x = bquote(bar(H) * " observed stand")) +
        ggplot2::facet_wrap(~model) +
        ggplot2::geom_abline() +
        ggplot2::geom_abline() +
        ggplot2::geom_text(data = ann_text2, color = "black",
                           mapping = ggplot2::aes(x = ShannonIndexRecruitMEAN, 
                                                  y = Emp_ShannonIndexRecruitMEAN,
                                                  label = label)) +
        ggplot2::scale_color_manual(values = c("7" = "#FDAE61", "10" = "grey"),
                                    name = "Diameter threshold",
                                    labels = c("7" = '7 cm', "10" = '10 cm')) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 10)))
    
    ggplot2::ggsave("figures/allAgesRichness.png",
                    plot = e,
                    width = 22, height = 18, scale = 0.9,
                    dpi = 300, units = "cm", device = 'png')
    
    #### FigS. recr. Sim.vs obs. recr. species richness -----
    
    category <- c( "B3", # "4C" 
                   "B1", # "ForCEEPS" 
                   "B2", # "ForCEEPS(f)"
                   "B2", # "FORMIND"
                   "B1", # "ForClim 1"
                   "B1", #"ForClim 11"
                   "B2", # "SIBYLA"  
                   "B1", # "xComp"
                   "B1", # "PICUS"
                   "B2", # "iLand"
                   "B1", # "LandClim" 
                   "B3", # "Landis II"
                   "B1", # "TreeMig"
                   "B2") # "LPJ-GUESS"

    ann_text <- data.frame(Emp_ShannonIndexRecruitMEAN = 0.7,
                           ShannonIndexRecruitMEAN = 0.1,
                           label = category,
                           dbh = "7",
                           model = factor(modelsOrder[!modelsOrder %in% c("Observed", "aDGVM2")],
                                          modelsOrder[!modelsOrder %in% c("Observed", "aDGVM2")]))
    
    d <- ggplot2::ggplot(plotShannonDF2[!plotShannonDF2$model == "aDGVM2",],
                         ggplot2::aes(x = Emp_ShannonIndexRecruitMEAN,
                                      y = ShannonIndexRecruitMEAN,
                                      color = dbh),
                         fill = "white") + 
        ggplot2::coord_cartesian(xlim = c(0, 2), ylim = c(0, 2)) +  
        ggplot2::geom_point(size = 0.6, alpha = 0.7 ) + 
        ggplot2::theme_classic() +
        ggplot2::labs(y = bquote(bar(H) * " simulated recruitment"),
                      x = bquote(bar(H) * " observed recruitment")) +
        ggplot2::facet_wrap(~model) +
        ggplot2::geom_abline() +
        ggplot2::geom_text(data = ann_text, color = "black",
                           mapping = ggplot2::aes(x = ShannonIndexRecruitMEAN, 
                                                  y = Emp_ShannonIndexRecruitMEAN,
                                                  label = label),
                           show.legend = FALSE) +
        ggplot2::scale_color_manual(values = c("7" = "#FDAE61", "10" = "grey"),
                                    name = "Diameter threshold",
                                    labels = c("7" = '7 cm', "10" = '10 cm')) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 10)))

    
    ggplot2::ggsave("figures/recruitmentRichness.png",
                    plot = d, width = 22, height = 18, scale = 0.9,
                    dpi = 300, units = "cm", device = 'png')
    
# Mortality 7-10 -----
    
  ### Fig. R ratio 7/10 ----
    
    # Data preparation    
    dataMort <- outputsDF[outputsDF$species %in% c(selSpecies, aDVGMSpecies), ]
    
    treesTot <- dataMort |>
      dplyr::group_by(model, site, sample, dbh) |>
      dplyr::summarise(Tot.rtrees = sum(r.trees),
                       dds = unique(dds),      
                       wb = unique(wb))
    meanTrees <- treesTot |>
      dplyr::group_by(model, site, dbh) |>
      dplyr::summarise(mean.rtrees = mean(Tot.rtrees),
                       dds = unique(dds),  
                       wb = unique(wb))
    
    meanTrees7 <- meanTrees[meanTrees$dbh == 7,]
    colnames(meanTrees7) <- c("model", "site", "dbh", "nn7", "dds", "wb")
    meanTrees7 <- meanTrees7[, c("model", "site", "nn7")]
    meanTrees10 <- meanTrees[meanTrees$dbh == 10,]
    colnames(meanTrees10) <- c("model", "site", "dbh", "nn10", "dds", "wb")
    meanTrees10 <- meanTrees10[, c("model", "site", "nn10",  "dds", "wb")]
    
    mortAll <- merge(meanTrees7, meanTrees10, by = c("model","site"))
    mortAll$nn710 <- mortAll$nn7 / mortAll$nn10
    
    # the plot 
    ratio7_10 <- ggplot2::ggplot(mortAll, ggplot2::aes(y = nn710, x = model, 
                                                     fill = model)) + 
      ggplot2::geom_boxplot() +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 1), colour = "darkblue", 
                          linetype = "dashed") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 1.77), colour = "darkblue",
                          linetype = "dashed") +
      ggplot2::ylim(c(0,4)) +
      ggplot2::xlab(label = "") +
      ggplot2::ylab(label = "Ratio of recruitment (7 and 10 cm)") +
                    # bquote(bar(R) * "(7cm) / " * bar(R) * "(10cm)")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                     strip.text.y = ggplot2::element_text(angle = 0),
                     legend.position = "none",
                     panel.background = ggplot2::element_blank(), 
                     axis.line = ggplot2::element_line(colour = "black"),
                     legend.key =  ggplot2::element_blank(), 
                     legend.title = ggplot2::element_blank(),
                     strip.background =  ggplot2::element_blank()) +
      ggplot2::scale_fill_manual(labels = labels[names(labels) %in%  unique(mortAll$model)],
                                 values =  values_color[names(values_color) %in% unique(mortAll$model)],
                                 guide = "none")
    
    ggplot2::ggsave("figures/Figure5.jpg",
                    plot = ratio7_10,
                    width = 21, height = 12, scale = 0.9,
                    dpi = 300, units = "cm", device = 'jpg') 
    
    
    mortAll$nn710 <-  as.numeric(as.character(mortAll$nn710))
    
    mortRatio7_10 <- ggpubr::compare_means(nn710 ~ model,  
                                        data = mortAll[!is.na(mortAll$nn710) & !is.infinite(mortAll$nn710), ],
                                        ref.group = "Observed", method = "t.test")
    write.csv(mortRatio7_10 ,"figures/mortRatio7_10.csv", row.names = FALSE)
    
    
    
    
  ### Fig. median vs overestimate per model ----
    ## Prepare the data
    ## calculated as the difference between the 3rd quartile and the 1st quartile
    ## in rate of ntrees between 10 and 7 cm
    iqrDT <- mortAll[is.finite(mortAll$nn710),] |>
                dplyr::group_by(model) |> 
                dplyr::summarise(iqr7_10 = IQR(nn710, na.rm = TRUE),
                                 median = median(nn710, na.rm = TRUE),
                                 mean = mean(nn710, na.rm = TRUE),
                                 sd = sd(nn710, na.rm = TRUE) )
    
    # Overestimation at 7cm   
    dbhSel <- "7"
    simResdbh2 <- outputsDF |> dplyr::filter(dbh %in% dbhSel) # this is only for dbh 7cm
    aggNrecr <- simResdbh2 |>  
      dplyr::group_by(model, site, sample) |> 
      dplyr::summarise(Totr.trees = sum(r.trees, na.rm = TRUE),
                       dds = unique( dds),    
                       wb = unique(wb))
    
    meanNrecr <- aggNrecr |> 
      dplyr::group_by(model, site) |> 
      dplyr::summarise(meanTotr.trees = mean(Totr.trees, na.rm = TRUE),
                       dds = unique( dds),    
                       wb = unique(wb))
    modelMean <- meanNrecr |> 
      dplyr::group_by(model) |> 
      dplyr::summarise(meanModTotr.trees = mean(meanTotr.trees, na.rm = TRUE),
                       sdModTotr.trees = sd(meanTotr.trees, na.rm = TRUE))
    
    modelMean$diff <- (modelMean$meanModTotr.trees - modelMean$meanModTotr.trees[modelMean$model == "Observed"]) / modelMean$meanModTotr.trees[modelMean$model == "Observed"]
    modelMean2 <- merge(modelMean, iqrDT, by = "model")
    
    # Plot
    overMortmedian <- ggplot2::ggplot(modelMean2,
                                      ggplot2::aes(x = diff, y = median, 
                                                   fill = model,  alpha = 0.8)) +
      ggplot2::geom_point(alpha = .8) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 1), colour = "#990000", linetype = "dashed") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 1.77), colour = "#990000", linetype = "dashed") +
      ggplot2::xlim(c(-5, 10)) +
      ggplot2::geom_vline(xintercept = 0, colour = "#990000", linetype = "dashed") +
      ggplot2::scale_colour_manual(values = values_color) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0),
                     strip.text.y = ggplot2::element_text(angle = 0),
                     panel.background = ggplot2::element_blank(), 
                     axis.line = ggplot2::element_line(colour = "black"),
                     legend.key =  ggplot2::element_blank(), 
                     legend.title = ggplot2::element_blank(),
                     strip.background =  ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::xlab(label = "Proportion of overestimation (7 cm)") +
      ggplot2::ylab(label = "Ratio of recruitment (7 and 10 cm)") +
       ggrepel::geom_label_repel(ggplot2::aes(label = model, size = NULL, 
                                             color = NULL),
                                nudge_y = 0.05) +
      ggplot2::scale_fill_manual(values = values_color) +
      ggplot2::annotate("text", x = 10, y = 2.6, label = "A") + 
      ggplot2::annotate("text", x = 10, y = 1.6, label = "B") + 
      ggplot2::annotate("text", x = -4.6, y = 1.6, label = "C") + 
      ggplot2::annotate("text", x = -4.6, y = 2.6, label = "D") + 
    
      ggplot2::annotate("text", x = 8.5, y = 1.81, label = "Reineke self-thinning") + 
      ggplot2::annotate("text", x = 7.9, y = 1.03, label = "Ratio equal to 1")
    
    ggplot2::ggsave("figures/Figure6.jpg",
                    plot =  overMortmedian,
                    width = 18, height = 12, scale = 0.9,
                    dpi = 300, units = "cm", device = 'jpg')  
    
    
    #### FigS. R change from 7 ----   
  
    # Remove inf values 
    mortAllnoInf <- mortAll[!is.infinite(mortAll$nn710), ]
    # model lm
    prep <- mortAllnoInf  |>
      dplyr::group_by(model) |> 
      dplyr::mutate(slope = lm(nn710 ~ nn7, na.action = na.omit)$coefficients[2],
                    significance = summary(lm(nn710 ~ nn7, na.action = na.omit))$coefficients[2, 4],
                    x = mean(nn710),  
                    y = mean(nn7))
    
    
    prep2 <- mortAllnoInf  |>
      dplyr::group_by(model) |> 
      dplyr::do(mod = lm(nn710 ~ nn7, na.action = na.omit, data = .)) |>
      dplyr::mutate(Slope = summary(mod)$coeff[2],
                    Significance = summary(mod)$coeff[2, 4]) |> 
      dplyr::ungroup() |>
      as.data.frame()

    write.csv(prep2[, c("model", "Slope", "Significance")],
              "figures/mort7_10_trends.csv", row.names = FALSE)

    # Plot the trends 
    mort7_10_trends <- 
      ggplot2::ggplot(mortAllnoInf , ggplot2::aes(x = nn7, y = nn710, color = model)) +
      ggplot2::geom_point(alpha = .2) +
      ggplot2::ylim(c(0, 3.5)) +
      ggplot2::xlim(c(.2, 100)) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 1), colour = "#990000", linetype = "dashed") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 1.77), colour = "#990000", linetype = "dashed") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0),
                     strip.text.y = ggplot2::element_text(angle = 0),
                     panel.background = ggplot2::element_blank(), 
                     axis.line = ggplot2::element_line(colour = "black"),
                     legend.key =  ggplot2::element_blank(), 
                     legend.title = ggplot2::element_blank(),
                     strip.background =  ggplot2::element_blank()) +
      ggplot2::scale_colour_manual(values = values_color) +
      ggplot2::scale_fill_manual(values = values_color) +
      ggplot2::xlab(label = expression("R (trees ha"^-1 * "10yr"^-1 * ")(7cm) (mean per site)")) +
      ggplot2::ylab(label = "R (7cm) / R (10cm) (mean per site)") + 
      ggplot2::geom_smooth(data = prep, 
                           ggplot2::aes(x = nn7, y = nn710, group = model), 
                           method = "lm", se = FALSE,
                           formula = y ~ x) +
      ggplot2::geom_text(data = prep, 
                         ggplot2::aes(x = nn7, y = nn710, label = slope),
                         nudge_y = 12, nudge_x = -1)
    
    ggplot2::ggsave("figures/mort7_10_trends.png",
                    plot = mort7_10_trends,
                    width = 16, height = 12, scale = 0.9,
                    dpi = 300, units = "cm", device = 'png')
    
# Complexity model structure-----  

    complBugmannSeild <- readr::read_csv("data/model_structure/dNumScaled.csv")
    complBugmannSeild$Attribute <- NULL
    complBugmannSeild$`HYBRID 4.0` <- NULL
    rownames(complBugmannSeild) <- c("Establishment approach", 
                                     "Establishment probability",
                                     "Number of established trees",
                                     "Ingrowth threshold",
                                     "Environmental influences",
                                     "Light",
                                     "Moisture",
                                     "Temperature",
                                     "Frost",
                                     "Browsing",
                                     "Seed production",
                                     "Dispersal",
                                     "Vegetative reproduction")
    
    # More complex model in the recruitment module? 
    modelComp <- data.frame(model = colnames(complBugmannSeild),
                            complexityRegen = colMeans(complBugmannSeild))
    
    # More and least process complexity per model
    maxAtt <- transform(stack(sapply(complBugmannSeild, which.max))[2:1], 
                        values = row.names(complBugmannSeild)[values])
    colnames(maxAtt) <- c("model", "max")
    
    minAtt <- transform(stack(sapply(complBugmannSeild, which.min))[2:1], 
                        values = row.names(complBugmannSeild)[values])
    colnames(minAtt) <- c("model", "min")
    
    # What attribute is more and least complex per model?
    complexitySum <- merge(maxAtt, minAtt, by = "model")
    complexitySum <- merge(complexitySum, modelComp, by = "model" )
    complexitySum[order(complexitySum$complexityRegen),]
    
    # Across models complexity
    complBugmannSeild$mean <- apply(complBugmannSeild, 1, mean)
    
    #What attribute in average across models is the most complex?
    which.max(complBugmannSeild$mean)
    
    #What attribute in average across models is the least complex?
    which.min(complBugmannSeild$mean)
    
    
## Model traits-----    
 ### Complexity VS overestimation proportion-----

    # Prepare data for species diversity -----
    sigH7_10 <- ggpubr::compare_means(ShannonIndexRecruit ~ dbh, 
                                      data = dat710[!dat710$model == "aDGVM2", ],
                                      group.by = "model", method = "t.test")
    
    write.csv(sigH7_10,"figures/sigH7_10.csv", row.names = FALSE)
    
    # Prepare data for Threshold 7cm 
    overestimationProportion <- modelMean2$diff
    meanComplexity <- complexitySum$complexityRegen
    complexitySum$model2 <- c("4C", "aDGVM2", "ForCEEPS", "ForClim 1", "FORMIND",
                              "iLand", "LandClim", "Landis II", 
                              "LPJ-GUESS", "PICUS", "TreeMig")
    
    tableCompOver <- merge(complexitySum,  modelMean2[, c("diff", "model")], 
                           by.x = "model2", by.y = "model", all.x = TRUE)
    
    colnames(tableCompOver) <- c("model2", "model", "max", "min",
                                 "complexity", "overestimation" )
    tableCompOver <- tableCompOver[, c("model2", "complexity", 
                                         "overestimation")]
    
  
    write.csv(tableCompOver,"figures/tableCompOver.csv", row.names = FALSE)
    
    #Figure
    tableCompOver <- readr::read_csv(here::here("figures", "tableCompOver.csv"))
    
    ggplotRegression <- function(fit) {
      require(ggplot2)
      ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
        geom_point() +
        stat_smooth(method = "lm", col = "blue") +
        labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                           "Intercept =",signif(fit$coef[[1]],5 ),
                           " Slope =",signif(fit$coef[[2]], 5),
                           " P =",signif(summary(fit)$coef[2,4], 5)))
    }
    
    
    fit1 <- lm(overestimation ~ complexity, data = tableCompOver)
    
    ggplot2::ggsave("figures/compOver.png",
                    plot =  ggplotRegression(fit1),
                    width = 25, height = 20, scale = 0.9,
                    dpi = 300, units = "cm", device = 'png')

  ### Emp/process based -----
    overDTsim <- outputsDF |> 
      dplyr::group_by(site, sample, dbh, model) |>
      dplyr::summarise(r.trees = sum(r.trees),
                       r.ba = sum(r.ba),
                       Totba = unique(Totba)) |>
      dplyr::group_by(site, dbh, model) |>
      dplyr::summarise(r.trees = mean(r.trees),
                       r.ba = mean(r.ba),
                       Totba = mean(Totba))
    
  
    rtreesDiff <- ggpubr::compare_means(r.trees ~ model,  
                                    data = overDTsim[overDTsim$dbh == 7,], 
                                    ref.group = "Observed", method = "t.test")
    write.csv(rtreesDiff, "figures/rtreesDiff.csv", row.names = FALSE)
    
    #### GLME -----
    
    rtreesDiffGLME <- lme4::glmer(r.trees ~ model + (1 | site),
                                  family = poisson(link = "log"),
                                  data = overDTsim[overDTsim$dbh == 7,],
                                  verbose = 1)
    
  
    
   
### feedback / not feedback H simulated vs observed -----  

    dat710 <- ShannonIndex |>  
      dplyr::group_by(model, site, dbh) |> 
      dplyr::summarise(ShannonIndexRecruit = mean(ShannonIndexRecruit))
    dat710$dbh <- ordered(dat710$dbh, levels = c("7", "10"))
    
    dat7H <- dat710[dat710$dbh == "7", ]
    
    rtreesDiff <- ggpubr::compare_means(ShannonIndexRecruit ~ model,  
                                        data = dat7H[!dat7H$model == "aDGVM2", ], 
                                        ref.group = "Observed", method = "t.test")
    write.csv(rtreesDiff, "figures/sigH7_SimObs.csv", row.names = FALSE)  
    
    
    sigH7_SimObsGLME <- lme4::lmer(r.trees ~ model + (1 | site),
                                  family = gaussian(link = "identity"),
                                  data = dat7H[!dat7H$model == "aDGVM2", ],
                                  verbose = 1)
    
    
    
# Recruitment niche-----
    
    #Total values across all species per site and sample
    overDTEmp1 <- outputsDF[outputsDF$model == "Observed", ] |>
      dplyr::group_by(site, sample, dbh) |>
      dplyr::summarise(r.trees = sum(r.trees),
                       r.ba = sum(r.ba),
                       Totba = unique(Totba))
    # # mean of samples 
    overDTEmp2 <- overDTEmp1 |>
      dplyr::group_by(site, dbh) |>
      dplyr::summarise(r.trees = mean(r.trees),
                       r.ba = mean(r.ba),
                       Totba = mean(Totba))
    
    envTrend <- outputsDF |>
      dplyr::group_by(site, sample, dbh, model) |>
      dplyr::summarise(r.trees = sum(r.trees),
                       r.ba = sum(r.ba),
                       Totba = unique(Totba), 
                       dds = unique(dds),
                       wb  = unique(wb)) |>
      dplyr::group_by(site, dbh, model) |>
      dplyr::summarise(r.trees = mean(r.trees),
                       r.ba = mean(r.ba),
                       Totba = mean(Totba),
                       dds = unique(dds),
                       wb  = unique(wb))
    
    envTrend <-  envTrend[ envTrend$dbh == "7", ]
    
    # Mean across sites 
    empDataQuartile <- c(0.025, 0.975)
    
    overDTEmp <- overDTEmp2 |>
      dplyr::group_by(dbh) |>
      dplyr::summarise(r.trees_low = quantile(r.trees, empDataQuartile[1],
                                              na.rm = TRUE),
                       r.trees_upp = quantile(r.trees, empDataQuartile[2],
                                              na.rm = TRUE),
                       r.trees = mean(r.trees),
                       r.ba_low = quantile(r.ba, empDataQuartile[1],
                                           na.rm = TRUE),
                       r.ba_upp = quantile(r.ba, empDataQuartile[2],
                                           na.rm = TRUE),
                       r.ba = mean(r.ba),
                       Totba_low = quantile(Totba, empDataQuartile[1],
                                            na.rm = TRUE),
                       Totba_upp = quantile(Totba, empDataQuartile[2],
                                            na.rm = TRUE),
                       Totba = mean(Totba))
    
    overDTEmp7 <- overDTEmp[overDTEmp$dbh == "7", ]
    overDTEmp10 <- overDTEmp[overDTEmp$dbh == "10", ]
    overDTsim7 <- overDTsim[overDTsim$dbh == "7", ]
    overDTsim10 <- overDTsim[overDTsim$dbh == "10", ]
    
    # Divide data in bins to create boxplots
    bins = 10
    bins = bins - 1
    wb_width <- round(diff(range(envTrend$wb)) / bins)
    dds_width <- round(diff(range(envTrend$dds)) / bins)
    ba_width <- round(diff(range(envTrend$Totba)) / bins)
    
    envTrend$wb_cut <- round(envTrend$wb / wb_width) * wb_width
    envTrend$dds_cut <- round(envTrend$dds / dds_width) * dds_width
    envTrend$ba_cut <- round(envTrend$Totba / ba_width) * ba_width
    
    envTrend$ba_cut <- factor(envTrend$ba_cut,
                              levels = sort(unique(envTrend$ba_cut)))
    
    
    #### Fig. R VS BA----
    png(file = "figures/envTrend7Totba.png", width = 16, height = 12, 
        units = "cm", res = 300)
    par(mfrow = c(4, 4), mar = c(2, 3, 1, 1), oma = c(2, 2, 1, 1))
    
    # Prepare Observed GAM model data for all the plots
    max_Totba = as.numeric(quantile(envTrend$Totba, .99, na.rm = TRUE))
    min_Totba = as.numeric(quantile(envTrend$Totba, .01, na.rm = TRUE))
    empModel <- envTrend[envTrend$model == "Observed", ]
    fm.sim <- mgcv::gam(r.trees ~ s(Totba, k = 3), data = empModel,
                        family = mgcv::nb())
    new_dat <- data.frame(
      Totba = seq(min_Totba, max_Totba, length.out = 10 * bins),
      fct_name = seq(1, bins + 1, length.out = 10 * bins))
    
    new_dat <- new_dat[new_dat$Totba < max(empModel$Totba) & new_dat$Totba > min(empModel$Totba), ]
    
    pred.sim <- predict(fm.sim, new_dat, type = "link", se.fit = TRUE)
    invLink <- family(fm.sim)$linkinv
    
    # Plot the boxplot with the observations GAM
    for (modelSel in unique(envTrend$model)) {
      ienvTrendModel <- envTrend[envTrend$model == modelSel,]
      
      max_r <- as.numeric(quantile(ienvTrendModel$r.trees,.99, na.rm = TRUE))
      ienvTrendModel$r.trees[ienvTrendModel$r.trees > max_r] <- max_r
      
      
      axisPlot <- ifelse(modelSel %in% c("Landis II",  "TreeMig", "LPJ-GUESS",
                                         "aDGVM2"), "s", "n")
      
      dataSel <- ienvTrendModel[ienvTrendModel$dbh == 7 & ienvTrendModel$model == modelSel,]
      boxplot(r.trees ~ ba_cut, 
              data = dataSel, 
              xlab = "",
              ylab = "",
              #ylim = c(0, 500),
              main = modelSel,
              xaxt = axisPlot,
              col = values_color[modelSel],
              frame = F)
      
      means <- tapply(dataSel$r.trees, INDEX = dataSel$ba_cut, FUN = mean) # calculate mean
      points(means, pch = 20, cex = 0.5, col = "lightgrey") #add means as circles to each boxplot
      box(bty = "l")
      
      linesData <- data.frame(TotBA = new_dat$fct_name, 
                              r.trees = invLink(pred.sim$fit),
                              UP = invLink(pred.sim$fit + pred.sim$se.fit * 1.96),
                              DOWN = invLink(pred.sim$fit - pred.sim$se.fit * 1.96))
      #[11:79]
      lines(new_dat$fct_name, invLink(pred.sim$fit), col = "red")
      lines(new_dat$fct_name, invLink(pred.sim$fit + pred.sim$se.fit * 1.96),
            lty = 2, col = "red")
      lines(new_dat$fct_name, invLink(pred.sim$fit - pred.sim$se.fit * 1.96), 
            lty = 2, col = "red")
    }
    
    mtext(labMeanRecruitment, side = 2, line = 0, outer = TRUE)
    mtext(labEnvVartotBA, side = 1, line = 1, outer = TRUE)
    
    dev.off()
    
    png(file = "figures/envTrend7Totba_500.png", width = 16, height = 12, 
        units = "cm", res = 300)
    par(mfrow = c(4, 4), mar = c(2, 3, 1, 1), oma = c(2, 2, 1, 1))
    
    # Prepare Observed GAM model data for all the plots
    max_Totba = as.numeric(quantile(envTrend$Totba, .99, na.rm = TRUE))
    min_Totba = as.numeric(quantile(envTrend$Totba, .01, na.rm = TRUE))
    empModel <- envTrend[envTrend$model == "Observed", ]
    fm.sim <- mgcv::gam(r.trees ~ s(Totba, k = 3), data = empModel,
                        family = mgcv::nb())
    new_dat <- data.frame(
      Totba = seq(min_Totba, max_Totba, length.out = 10 * bins),
      fct_name = seq(1, bins + 1, length.out = 10 * bins))
    
    new_dat <- new_dat[new_dat$Totba < max(empModel$Totba) & new_dat$Totba > min(empModel$Totba), ]
    
    pred.sim <- predict(fm.sim, new_dat, type = "link", se.fit = TRUE)
    invLink <- family(fm.sim)$linkinv
    
    # Plot the boxplot with the observations GAM
    for (modelSel in unique(envTrend$model)) {
      ienvTrendModel <- envTrend[envTrend$model == modelSel,]
      
      max_r <- as.numeric(quantile(ienvTrendModel$r.trees,.99, na.rm = TRUE))
      ienvTrendModel$r.trees[ienvTrendModel$r.trees > max_r] <- max_r
      
      
      axisPlot <- ifelse(modelSel %in% c("Landis II",  "TreeMig", "LPJ-GUESS",
                                         "aDGVM2"), "s", "n")
      
      dataSel <- ienvTrendModel[ienvTrendModel$dbh == 7 & ienvTrendModel$model == modelSel,]
      boxplot(r.trees ~ ba_cut, 
              data = dataSel, 
              xlab = "",
              ylab = "",
              ylim = c(0, 500),
              main = modelSel,
              xaxt = axisPlot,
              col = values_color[modelSel],
              frame = F)
      
      means <- tapply(dataSel$r.trees, INDEX = dataSel$ba_cut, FUN = mean) # calculate mean
      points(means, pch = 20, cex = 0.5, col = "lightgrey") #add means as circles to each boxplot
      box(bty = "l")
      
      linesData <- data.frame(TotBA = new_dat$fct_name, 
                              r.trees = invLink(pred.sim$fit),
                              UP = invLink(pred.sim$fit + pred.sim$se.fit * 1.96),
                              DOWN = invLink(pred.sim$fit - pred.sim$se.fit * 1.96))
      #[11:79]
      lines(new_dat$fct_name, invLink(pred.sim$fit), col = "red")
      lines(new_dat$fct_name, invLink(pred.sim$fit + pred.sim$se.fit * 1.96),
            lty = 2, col = "red")
      lines(new_dat$fct_name, invLink(pred.sim$fit - pred.sim$se.fit * 1.96), 
            lty = 2, col = "red")
    }
    
    mtext(labMeanRecruitment, side = 2, line = 0, outer = TRUE)
    mtext(labEnvVartotBA, side = 1, line = 1, outer = TRUE)
    
    dev.off()
    
    
    #### FigS. R VS WB -----
    
    png(file = "figures/envTrend7WB.png", width = 16, height = 12, 
        units = "cm", res = 300)
    par(mfrow = c(4, 4), mar = c(2, 3, 1, 1), oma = c(2, 2, 1, 1))
    
    # Prepare Observed GAM model data for all the plots
    max_r = as.numeric(quantile(envTrend$r.trees,.99, na.rm = TRUE))
    max_wb = as.numeric(quantile(envTrend$wb,.99, na.rm = TRUE))
    min_wb = as.numeric(quantile(envTrend$wb,.01, na.rm = TRUE))
    empModel <- envTrend[envTrend$model == "Observed",]
    fm.sim <- mgcv::gam(r.trees ~ s(wb, k = 3), data = empModel,
                        family = mgcv::nb())
    new_dat <- data.frame(
      wb = seq(min_wb, max_wb, length.out = 10 * bins),
      fct_name = seq(1, bins + 1, length.out = 10 * bins))
    new_dat <- new_dat[new_dat$wb < max(empModel$wb) & new_dat$wb > min(empModel$wb), ]
    
    pred.sim <- predict(fm.sim, new_dat, type = "link", se.fit = TRUE)
    invLink <- family(fm.sim)$linkinv
    
    # Plot 
    for (modelSel in unique(envTrend$model)) {
      ienvTrendModel <- envTrend[envTrend$model == modelSel,]
      axisPlot <- ifelse(modelSel %in% c("Landis II", "TreeMig", "LPJ-GUESS",
                                         "aDGVM2"), "s", "n")
      
      dataSel <- ienvTrendModel[ienvTrendModel$dbh == 7 & ienvTrendModel$model == modelSel, ]
      
      boxplot(r.trees ~ wb_cut, 
              data = dataSel, 
              xlab = "",
              ylab = "",
              #ylim = c(0, 500),
              main = modelSel,
              xaxt = axisPlot,
              col = values_color[modelSel],
              frame = F)
      means <- tapply(dataSel$r.trees, INDEX = dataSel$wb_cut, FUN = mean) # calculate mean
      points(means, pch = 20, cex = 0.5, col = "lightgrey") #add means as circles to each boxplot
      
      box(bty = "l")
      lines(new_dat$fct_name, invLink(pred.sim$fit), col = "red")
      lines(new_dat$fct_name, invLink(pred.sim$fit + pred.sim$se.fit * 1.96),
            lty = 2,col = "red")
      lines(new_dat$fct_name, invLink(pred.sim$fit - pred.sim$se.fit * 1.96), 
            lty = 2,col = "red")
      
    }
    mtext(labMeanRecruitment, side = 2, line = 0, outer = TRUE)
    mtext(labEnvVarWB, side = 1, line = 1, outer = TRUE)
    dev.off()
    
    png(file = "figures/envTrend7WB_500.png", width = 16, height = 12, 
        units = "cm", res = 300)
    par(mfrow = c(4, 4), mar = c(2, 3, 1, 1), oma = c(2, 2, 1, 1))
    
    # Prepare Observed GAM model data for all the plots
    max_r = as.numeric(quantile(envTrend$r.trees,.99, na.rm = TRUE))
    max_wb = as.numeric(quantile(envTrend$wb,.99, na.rm = TRUE))
    min_wb = as.numeric(quantile(envTrend$wb,.01, na.rm = TRUE))
    empModel <- envTrend[envTrend$model == "Observed",]
    fm.sim <- mgcv::gam(r.trees ~ s(wb, k = 3), data = empModel,
                        family = mgcv::nb())
    new_dat <- data.frame(
      wb = seq(min_wb, max_wb, length.out = 10 * bins),
      fct_name = seq(1, bins + 1, length.out = 10 * bins))
    new_dat <- new_dat[new_dat$wb < max(empModel$wb) & new_dat$wb > min(empModel$wb), ]
    
    pred.sim <- predict(fm.sim, new_dat, type = "link", se.fit = TRUE)
    invLink <- family(fm.sim)$linkinv
    
    # Plot 
    for (modelSel in unique(envTrend$model)) {
      ienvTrendModel <- envTrend[envTrend$model == modelSel,]
      axisPlot <- ifelse(modelSel %in% c("Landis II", "TreeMig", "LPJ-GUESS",
                                         "aDGVM2"), "s", "n")
      dataSel <- ienvTrendModel[ienvTrendModel$dbh == 7 & ienvTrendModel$model == modelSel, ]
      boxplot(r.trees ~ wb_cut, 
              data = dataSel, 
              xlab = "",
              ylab = "",
              ylim = c(0, 500),
              main = modelSel,
              xaxt = axisPlot,
              col = values_color[modelSel],
              frame = F)
      means <- tapply(dataSel$r.trees, INDEX = dataSel$wb_cut, FUN = mean) # calculate mean
      points(means, pch = 20, cex = 0.5, col = "lightgrey") #add means as circles to each boxplot
      
      box(bty = "l")
      lines(new_dat$fct_name, invLink(pred.sim$fit), col = "red")
      lines(new_dat$fct_name, invLink(pred.sim$fit + pred.sim$se.fit * 1.96),
            lty = 2,col = "red")
      lines(new_dat$fct_name, invLink(pred.sim$fit - pred.sim$se.fit * 1.96), 
            lty = 2,col = "red")
      
    }
    mtext(labMeanRecruitment, side = 2, line = 0, outer = TRUE)
    mtext(labEnvVarWB, side = 1, line = 1, outer = TRUE)
    dev.off()
    
    
    
    #### FigS. R VS. DDS -----
    
    png(file = "figures/envTrend7DDS.png", width = 16, height = 12, 
        units = "cm", res = 300)
    par(mfrow = c(4, 4), mar = c(2, 3, 1, 1), oma = c(2, 2, 1, 1))
    
    # Prepare Observed GAM model data for all the plots
    max_r = as.numeric(quantile(ienvTrendModel$r.trees, .99, na.rm = TRUE))
    max_dds = as.numeric(quantile(ienvTrendModel$dds, .99, na.rm = TRUE))
    min_dds = as.numeric(quantile(ienvTrendModel$dds, .01, na.rm = TRUE))
    empModel <- envTrend[envTrend$model == "Observed",]
    fm.sim <- mgcv::gam(r.trees ~ s(dds, k = 3), data = empModel,
                        family = mgcv::nb())
    new_dat = data.frame(
      dds = seq(min_dds, max_dds, length.out = 10 * bins),
      fct_name = seq(1, bins + 1, length.out = 10 * bins))
    new_dat <- new_dat[new_dat$dds < max(empModel$dds) & new_dat$dds > min(empModel$dds), ]
    
    pred.sim <- predict(fm.sim, new_dat, type = "link", se.fit = TRUE)
    invLink <- family(fm.sim)$linkinv
    
    for (modelSel in unique(envTrend$model)) {
      ienvTrendModel <- envTrend[envTrend$model == modelSel,]
      dataSel <- ienvTrendModel[ienvTrendModel$dbh == 7 & ienvTrendModel$model == modelSel, ]
      boxplot(r.trees ~ dds_cut, 
              data = dataSel, 
              xlab = "",
              ylab = "",
              #ylim = c(0, 500),
              main = modelSel,
              xaxt = axisPlot,
              col = values_color[modelSel],
              frame = F)
      means <- tapply(dataSel$r.trees, INDEX = dataSel$dds_cut, FUN = mean) # calculate mean
      points(means, pch = 20, cex = 0.5, col = "lightgrey") #add means as circles to each boxplot
      
      box(bty = "l")
      lines(new_dat$fct_name, invLink(pred.sim$fit), col = "red")
      lines(new_dat$fct_name, invLink(pred.sim$fit + pred.sim$se.fit * 1.96),
            lty = 2,col = "red")
      lines(new_dat$fct_name, invLink(pred.sim$fit - pred.sim$se.fit * 1.96), 
            lty = 2,col = "red")
    }
    mtext(labMeanRecruitment, side = 2, line = 0, outer = TRUE)
    mtext(labEnvVarDDS, side = 1, line = 1, outer = TRUE)
    
    dev.off()
    
    
    png(file = "figures/envTrend7DDS_500.png", width = 16, height = 12, 
        units = "cm", res = 300)
    par(mfrow = c(4, 4), mar = c(2, 3, 1, 1), oma = c(2, 2, 1, 1))
    
    # Prepare Observed GAM model data for all the plots
    max_r = as.numeric(quantile(ienvTrendModel$r.trees, .99, na.rm = TRUE))
    max_dds = as.numeric(quantile(ienvTrendModel$dds, .99, na.rm = TRUE))
    min_dds = as.numeric(quantile(ienvTrendModel$dds, .01, na.rm = TRUE))
    empModel <- envTrend[envTrend$model == "Observed",]
    fm.sim <- mgcv::gam(r.trees ~ s(dds, k = 3), data = empModel,
                        family = mgcv::nb())
    new_dat = data.frame(
      dds = seq(min_dds, max_dds, length.out = 10 * bins),
      fct_name = seq(1, bins + 1, length.out = 10 * bins))
    new_dat <- new_dat[new_dat$dds < max(empModel$dds) & new_dat$dds > min(empModel$dds), ]
    
    pred.sim <- predict(fm.sim, new_dat, type = "link", se.fit = TRUE)
    invLink <- family(fm.sim)$linkinv
    
    for (modelSel in unique(envTrend$model)) {
      ienvTrendModel <- envTrend[envTrend$model == modelSel,]
      dataSel <- ienvTrendModel[ienvTrendModel$dbh == 7 & ienvTrendModel$model == modelSel, ]
      boxplot(r.trees ~ dds_cut, 
              data = dataSel, 
              xlab = "",
              ylab = "",
              ylim = c(0, 500),
              main = modelSel,
              xaxt = axisPlot,
              col = values_color[modelSel],
              frame = F)
      means <- tapply(dataSel$r.trees, INDEX = dataSel$dds_cut, FUN = mean) # calculate mean
      points(means, pch = 20, cex = 0.5, col = "lightgrey") #add means as circles to each boxplot
      
      box(bty = "l")
      lines(new_dat$fct_name, invLink(pred.sim$fit), col = "red")
      lines(new_dat$fct_name, invLink(pred.sim$fit + pred.sim$se.fit * 1.96),
            lty = 2,col = "red")
      lines(new_dat$fct_name, invLink(pred.sim$fit - pred.sim$se.fit * 1.96), 
            lty = 2,col = "red")
    }
    mtext(labMeanRecruitment, side = 2, line = 0, outer = TRUE)
    mtext(labEnvVarDDS, side = 1, line = 1, outer = TRUE)
    
    dev.off()
    
    
### Fig. r.ba/Totr.ba & env gradient -----
    
    # Data preparation
    
    for (dbhSel in c(7, 10)) {
      dbhSel <- dbhSel
      simResdbh <- outputsDF |> dplyr::filter(dbh %in% dbhSel)
      simResdbh$r.baShare <- simResdbh$r.ba / simResdbh$Totba
      simResdbh$baShare <- simResdbh$ba / simResdbh$Totba
      
      simResdbh$r.baTotRecShare <- round(simResdbh$r.ba, 4) / round(simResdbh$Totr.ba, 4)
      
      # For this figure we consider that values with 0 r.ba and 0 Tot.r.ba have a 
      # recruitment share of 0 instead of NaN
      simResdbh$r.baTotRecShare [simResdbh$r.ba == 0 & simResdbh$Totr.ba == 0] <- 0
      
      # Aggregate values from simulations to get total recruitment per site and sample
      simResAgg <- simResdbh |>
        dplyr::group_by(site, species, model) |> 
        dplyr::summarise(r.baShareMean = round(mean(r.baShare), 2), 
                         baShareMean = round(mean(baShare), 2), 
                         r.baTot.r.ShareMean = round(mean(r.baTotRecShare),
                                                     2), 
                         dds = unique(dds), 
                         wb = unique(wb)) 
      
      mainSppBAshare <-  simResAgg[simResAgg$species %in% c("Abies alba", 
                                                            "Fagus sylvatica",
                                                            "Picea abies", 
                                                            "Pinus sylvestris",
                                                            "Quercus spp."), ]
      mainSppBAshare$model <- factor(mainSppBAshare$model, levels = modelsOrder)
      mainSppBAshare$dds <- round(mainSppBAshare$dds, 0)
      
      #Divide dds and wb in bins based on all the possible values for all sites
      #not only the ones included in dbh = 7cm
      ddsRange <- range(outputsDF$dds)
      wbRange <- range(outputsDF$wb)
      binSize <- 11
      
      mainSppBAshare <- mainSppBAshare |> 
        dplyr::mutate(dds_bin = cut(dds, 
                                    breaks = seq(ddsRange[1], 
                                                 ddsRange[2], 
                                                 by = (ddsRange[2] - ddsRange[1]) / binSize),
                                    include.lowest = TRUE))
      
      mainSppBAshare <- mainSppBAshare |> 
        dplyr::mutate(wb_bin = cut(wb, 
                                   breaks = seq(wbRange[1], wbRange[2],
                                                by = (wbRange[2] - wbRange[1]) / binSize),
                                   include.lowest = TRUE))
      
      # Mean value per combination of bins
      mainSppBAshareBinsMean <-  aggregate(r.baTot.r.ShareMean ~ wb_bin + dds_bin + model + species, 
                                           mainSppBAshare, mean)
      mainSppBAshareBinsMeanEmp <- mainSppBAshareBinsMean[mainSppBAshareBinsMean$model == "Observed", ]
      mainSppBAshareBinsMeanEmp <- mainSppBAshareBinsMeanEmp[, c("species","dds_bin",
                                                                 "wb_bin", 
                                                                 "r.baTot.r.ShareMean")]
      colnames(mainSppBAshareBinsMeanEmp) <- c("species","dds_bin","wb_bin", 
                                               "r.ShareMeanEMP")
      mainSppBAshareBinsMeanNOEmp <- mainSppBAshareBinsMean[!mainSppBAshareBinsMean$model == "Observed",]
      
      mainSppBAshareBinsMeanDiff <- merge(mainSppBAshareBinsMean, 
                                          mainSppBAshareBinsMeanEmp,
                                          by = c("species", "dds_bin", "wb_bin"),
                                          all.x = TRUE)
      mainSppBAshareBinsMeanDiff$Diff <- (mainSppBAshareBinsMeanDiff$r.baTot.r.ShareMean - mainSppBAshareBinsMeanDiff$r.ShareMeanEMP)
      mainSppBAshareBinsMeanDiff$Diff <- round(mainSppBAshareBinsMeanDiff$Diff, 2)
      
      mainSppBAshareBinsMeanDiff$r.baTot.r.ShareMean[mainSppBAshareBinsMeanDiff$r.baTot.r.ShareMean == 0] <- NA
      
      # Plot 
      heatCircle <- ggplot2::ggplot(data = mainSppBAshareBinsMeanDiff,
                                    mapping = ggplot2::aes(x = dds_bin,
                                                           y = wb_bin,
                                                           fill = Diff)) +
        ggplot2::scale_fill_gradient2(low = "blue", mid = 'white', high = "red",
                                      breaks = c(-1, -0.5, 0, 0.5, 1),
                                      midpoint = 0,
                                      name = expression(bar(R) * " BA share difference")) +
        
        ggplot2::geom_tile() +
        ggplot2::xlab(label = labEnvVarDDS) +
        ggplot2::ylab(label = labEnvVarWB) +
        ggplot2::facet_grid(model ~ species) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                       strip.text.y = ggplot2::element_text(angle = 0),
                       panel.grid.major = ggplot2::element_blank(), 
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_rect(fill = "lightgrey",
                                                                colour = "lightblue"),
                       axis.line = ggplot2::element_line(colour = "black"),
                       strip.background =  ggplot2::element_blank()) +
        ggplot2::scale_x_discrete(labels = c( "[595,786]" = "595", 
                                              "(786,977]" = "",
                                              "(977,1.17e+03]" = "",
                                              "(1.17e+03,1.36e+03]" = "",
                                              "(1.36e+03,1.55e+03]" = "1360",
                                              "(1.55e+03,1.74e+03]" = "",
                                              "(1.74e+03,1.93e+03]" = "",
                                              "(1.93e+03,2.12e+03]" = "",
                                              "(2.12e+03,2.31e+03]" = "",
                                              "(2.31e+03,2.51e+03]" = "2310", 
                                              "(2.51e+03,2.7e+03]" = "")) +
        ggplot2::scale_y_discrete(labels = c("[-277,-138]" = "-277",
                                             "(-138,0.29]"  = "",
                                             "(0.29,139]" = "",
                                             "(139,278]" = "",
                                             "(278,416]" = "278",
                                             "(416,555]" = "",
                                             "(555,693]" = "",
                                             "(693,832]" = "",
                                             "(832,971]" = "832",
                                             "(971,1.11e+03]" = "",
                                             "(1.11e+03,1.25e+03]" = "")) +
        ggplot2::geom_point(ggplot2::aes(x = dds_bin, y = wb_bin, 
                                         size = r.baTot.r.ShareMean), shape = 1) +
        ggplot2::scale_size_binned_area(breaks = c(0.1, 0.3, 0.6, 0.9), limits = c(0.000001, 1)) + 
        ggplot2::labs(size = expression(bar(R) * " BA share")) 
      
      
      ggplot2::ggsave(paste0("figures/HeatEnvGrad_", binSize - 1, "circles_",dbhSel, ".png"),
                      plot = heatCircle,
                      width = 27, height = 60, scale = 0.9,
                      dpi = 300, units = "cm", device = 'png') 
      
      if(dbhSel == 7) {
        ggplot2::ggsave("figures/Figures8.jpg",
                        plot = heatCircle,
                        width = 27, height = 60, scale = 0.9,
                        dpi = 300, units = "cm", device = 'jpg') 
      }
      
      print(paste0("Range of values for recruited BA share and threshold ",dbhSel, " is ", 
                   range(mainSppBAshareBinsMeanDiff$r.baTot.r.ShareMean)[1], "-",
                   range(mainSppBAshareBinsMeanDiff$r.baTot.r.ShareMean)[2] ))
      print(paste0("Range of values for difference with observed data and threshold ",dbhSel, " is ", 
                   range(mainSppBAshareBinsMeanDiff$Diff)[1], "-",
                   range(mainSppBAshareBinsMeanDiff$Diff)[2]))
      
    }
    
   
      
# Map -----
    # IMPORTANT: This is the only figure that is not reproducible
    # because we can not share the location of the plots 

    obsSpp <- outputsDF[outputsDF$model == "Observed", ]
    
    standSpp <- obsSpp |> dplyr::group_by(site) |> dplyr::top_n(1, ba)
    standSpp <- standSpp[, c("site", "species")]
    colnames(standSpp) <- c("site", "stand")
    standSpp <- standSpp |> 
                 dplyr::group_by(site) |>  
                 dplyr::filter(dplyr::row_number() == 1)
    
    recruitSpp <- obsSpp |> dplyr::group_by(site, dbh) |> dplyr::top_n(1, r.ba)
    recruitSpp <- recruitSpp[, c("site", "species", "dbh")]
    colnames(recruitSpp) <- c("site", "recruitment", "dbh")
    recruitSpp7 <- recruitSpp[recruitSpp$dbh == 7,]
    recruitSpp10 <- recruitSpp[recruitSpp$dbh == 10,]
    
    recruitSpp7 <- recruitSpp7 |> 
                  dplyr::group_by(site) |>  
                  dplyr::filter(dplyr::row_number() == 1)
    recruitSpp10 <- recruitSpp10 |> 
      dplyr::group_by(site) |>  
      dplyr::filter(dplyr::row_number() == 1)
    
    # Check if sites that had multiple samples
    # had different dominant spp in recruitment
    recruitSpp1 <- obsSpp |>  
      dplyr::filter(r.ba > 0) |> # do not consider no recruitment 
      dplyr::group_by(site, dbh, sample) |> 
      dplyr::top_n(1, r.ba) #select with higher r.ba
    # Select only dbh 10
    recruitSpp102 <- recruitSpp1[recruitSpp1$dbh == 10,]
    
    
    ###### DO NOT RUN #########

    # #Add location
    # location <- data.table::fread("data/coords_blurred_dt4326.csv")
    # 
    # domSpp7 <- merge(standSpp, recruitSpp7, by = "site")
    # domSpp7 <- merge(domSpp7, location, by.x = "site", by.y = "cluster_plot_id200")
    # 
    # domSpp10 <- merge(standSpp, recruitSpp10, by = "site")
    # domSpp10 <- merge(domSpp10, location, by.x = "site", by.y = "cluster_plot_id200")
    # 
    # # Species to be plotted
    # unique(domSpp10$stand)
    # 
    # library(tidyverse)
    # library(sf)
    # library(rnaturalearth)
    # library(rnaturalearthdata)
    # library(rgeos)
    # library(rgdal)
    # library(raster)
    # world <- ne_countries(scale = "medium", returnclass = "sf")
    # Europe <- world[which(world$continent == "Europe"), ]
    # plotsMap <- ggplot(Europe) +
    #   ggthemes::theme_map() +
    #   geom_sf(fill = "transparent" , colour = "lightgrey",  lwd = 0.25) +
    #   coord_sf(xlim = c(-15, 30), ylim = c(40,55), expand = FALSE) +
    #   geom_point(data = domSpp10, shape = 20, stroke = FALSE,
    #              mapping = aes(x = X, y = Y, color = recruitment),
    #              alpha = 1, size = 2.5) +
    #   scale_color_manual("Dominant species of ingrowth",
    #                      values = c(   "Picea abies" = "#D95F02" ,
    #                                    "Fagus sylvatica" = "#7570B3",
    #                                    "Abies alba" =  "#E6AB02",
    #                                    "Quercus spp." = "#66A61E",
    #                                    "Other spp." = "grey",
    #                                    "Pinus sylvestris" = "#E7298A" ,
    #                                    "Fraxinus excelsior" = "#666666",
    #                                    "Alnus glutinosa" = "#0072B2",
    #                                    "Tilia cordata" = "#A6761D"),
    #                      labels = c("Picea abies" = expression(italic("Picea abies")),
    #                                    "Fagus sylvatica" = expression(italic("Fagus sylvatica")),
    #                                    "Abies alba" =  expression(italic("Abies alba")),
    #                                    "Quercus spp." = expression(italic("Quercus"), atop(" spp."),
    #                                    "Other spp." = expression(atop("Other spp.")),
    #                                    "Pinus sylvestris" = expression(italic("Pinus sylvestris")),
    #                                    "Fraxinus excelsior" = expression(italic("Fraxinus excelsior")),
    #                                    "Alnus glutinosa" = expression(italic("Alnus glutinosa")),
    #                                    "Tilia cordata" = expression(italic("Tilia cordata")))
    # 
    #                                 )) + theme(legend.text.align = 0)
    # 
    # ggplot2::ggsave("figures/Figure1.jpg",
    #                 plot =   plotsMap,
    #                 width = 21, height = 12, scale = 0.9,
    #                 dpi = 300, units = "cm", device = 'png')
    # 
