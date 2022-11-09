################################################################################
# Title: Figures
# Aim: Create figures and analysis for the manuscript
# Author: Olalla Díaz-Yáñez | @olalla | olalladiaz.net &
#         some part of figures were inspired in the work of the regeneration
#         workshop attendees
################################################################################

# Load plotting styles 

    source("code/plottingManuscript.R")

# Load data ----

    outputsDF <- data.table::fread("data/dataOutputs.csv")

# Assign models and dbh to levels 

    outputsDF$model <- factor(outputsDF$model, levels = modelsOrder)
    outputsDF$dbh <- factor(outputsDF$dbh, levels = c("10", "7"))

# Calculate Shannon index by basal area ----    
    
    ## Recruitment Shannon index----
    ShannonDF <- outputsDF 
    ShannonDF$giDIVG <- ShannonDF$r.ba / ShannonDF$Totba
    ShannonDF$lngiDIVG <- log(ShannonDF$r.ba / ShannonDF$Totba)
    ShannonDF$mult <- round(ShannonDF$giDIVG * ShannonDF$lngiDIVG, 2)
    
    # Sum across species in each observation
    ShannonIndex <- data.frame(cbind(aggregate(mult ~ site + sample + model + dbh,
                                               sum, data =  ShannonDF)))
    
    ShannonIndex$mult <- (-1) * (ShannonIndex$mult)
    colnames(ShannonIndex) <- c("site", "sample", "model",
                                "dbh", "ShannonIndexRecruit")
    
    ## Stand Shannon index ----
    ShannonDFAll <- outputsDF 
    ShannonDFAll$giDIVG <- ShannonDFAll$ba / ShannonDFAll$Totba
    ShannonDFAll$lngiDIVG <- log(ShannonDFAll$ba / ShannonDFAll$Totba)
    ShannonDFAll$mult <- round(ShannonDFAll$giDIVG *  ShannonDFAll$lngiDIVG, 2)
    
    # Sum across secies per site and sample
    ShannonIndexAll <- data.frame(cbind(aggregate(mult ~ site + sample + model + dbh,
                                                  sum, data = ShannonDFAll)))
    
    ShannonIndexAll$mult <- (-1) * (ShannonIndexAll$mult)
    colnames(ShannonIndexAll) <- c("site", "sample", "model",
                                   "dbh", "ShannonIndexAllAges")
    
    ShannonIndex <- merge(ShannonIndex, ShannonIndexAll, by = c("site", "sample",
                                                                "model","dbh"))
    
    
    ## Relative Shannon index ----
    
    ShannonIndex$relShannon <- ShannonIndex$ShannonIndexRecruit / ShannonIndex$ShannonIndexAllAges
    
# Levels regeneration ----

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
                                    name = "Diameter threshold",
                                    labels = c('7 cm', '10 cm')) +
        ggplot2::geom_hline(ggplot2::aes(yintercept = quantile(overDTsim$r.trees[overDTsim$dbh == "7" & overDTsim$model == "Empirical"],
                                                               c(0.25),
                                                               na.rm = TRUE)), 
                            colour = "#990000", linetype = "dashed") +
        ggplot2::geom_hline(ggplot2::aes(yintercept = quantile(overDTsim$r.trees[overDTsim$dbh == "10" & overDTsim$model == "Empirical"],
                                                               c(0.70),
                                                               na.rm = TRUE)),
                            colour = "#990000", linetype = "dashed") 
    
    ggplot2::ggsave("figures/recOverAll710.png",
                    plot =  recOverAll,
                    width = 21, height = 12,# scale = 0.9,
                    dpi = 300, units = "cm", 
                    device = 'png') 
    
## R VS. env. trends-----

    #Total values across all species per site and sample
    overDTEmp1 <- outputsDF[outputsDF$model == "Empirical", ] |>
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
    envTrend <- as.data.table(envTrend) 

    
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
    
    # Divide data in bins to crate boxplots
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
    
    # Prepare empirical GAM model data for all the plots
    max_Totba = as.numeric(quantile(envTrend$Totba, .99, na.rm = TRUE))
    min_Totba = as.numeric(quantile(envTrend$Totba, .01, na.rm = TRUE))
    empModel <- envTrend[envTrend$model == "Empirical",]
    fm.sim <- mgcv::gam(r.trees ~ s(Totba, k = 3), data = empModel,
                        family = mgcv::nb())
    new_dat <- data.frame(
        Totba = seq(min_Totba, max_Totba, length.out = 10 * bins),
        fct_name = seq(1, bins + 1, length.out = 10 * bins))
    pred.sim <- predict(fm.sim, new_dat, type = "link", se.fit = TRUE)
    invLink <- family(fm.sim)$linkinv
    
    # Plot the boxplot with the observations GAM
    for (modelSel in unique(envTrend$model)) {
        ienvTrendModel <- envTrend[envTrend$model == modelSel]
        axisPlot <- ifelse(modelSel %in% c("Landis II",  "TreeMig", "LPJ-GUESS",
                                            "aDGVM2"), "s", "n")
        boxplot(r.trees ~ ba_cut, 
                data = ienvTrendModel[dbh == 7 & model == modelSel], 
                xlab = "",
                ylab = "",
                #ylim = c(0, 500),
                main = modelSel,
                xaxt = axisPlot,
                col = values_color[modelSel],
                frame = F)
        box(bty = "l")
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
    
    # Prepare empirical GAM model data for all the plots
    max_r = as.numeric(quantile(envTrend$r.trees,.99, na.rm = TRUE))
    max_wb = as.numeric(quantile(envTrend$wb,.99, na.rm = TRUE))
    min_wb = as.numeric(quantile(envTrend$wb,.01, na.rm = TRUE))
    empModel <- envTrend[envTrend$model == "Empirical",]
    fm.sim <- mgcv::gam(r.trees ~ s(wb, k = 3), data = empModel,
                        family = mgcv::nb())
    new_dat <- data.frame(
        wb = seq(min_wb, max_wb, length.out = 10 * bins),
        fct_name = seq(1, bins + 1, length.out = 10 * bins))
    pred.sim <- predict(fm.sim, new_dat, type = "link", se.fit = TRUE)
    invLink <- family(fm.sim)$linkinv
    
    # Plot 
    for (modelSel in unique(envTrend$model)) {
        ienvTrendModel <- envTrend[envTrend$model == modelSel]
        axisPlot <- ifelse(modelSel %in% c("Landis II",  "TreeMig", "LPJ-GUESS",
                                           "aDGVM2"), "s", "n")
       boxplot(r.trees ~ wb_cut, 
               data = ienvTrendModel[dbh == 7 & model == modelSel], 
               xlab = "",
               ylab = "",
               #ylim = c(0, 500),
               main = modelSel,
               xaxt = axisPlot,
               col = values_color[modelSel],
               frame = F)
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
    
   
    #### FigS. DDS -----
    
    png(file = "figures/envTrend7DDS.png", width = 16, height = 12, 
        units = "cm", res = 300)
    par(mfrow = c(4, 4), mar = c(2, 3, 1, 1), oma = c(2, 2, 1, 1))
    
    # Prepare empirical GAM model data for all the plots
    max_r = as.numeric(quantile(ienvTrendModel$r.trees, .99, na.rm = TRUE))
    max_dds = as.numeric(quantile(ienvTrendModel$dds, .99, na.rm = TRUE))
    min_dds = as.numeric(quantile(ienvTrendModel$dds, .01, na.rm = TRUE))
    empModel <- envTrend[envTrend$model == "Empirical",]
    fm.sim <- mgcv::gam(r.trees ~ s(dds, k = 3), data = empModel,
                        family = mgcv::nb())
    new_dat = data.frame(
        dds = seq(min_dds, max_dds, length.out = 10 * bins),
        fct_name = seq(1, bins + 1, length.out = 10 * bins))
    pred.sim <- predict(fm.sim, new_dat, type = "link", se.fit = TRUE)
    invLink <- family(fm.sim)$linkinv
    
    for (modelSel in unique(envTrend$model)) {
        ienvTrendModel <- envTrend[model == modelSel]
        boxplot(r.trees ~ dds_cut, 
                data = ienvTrendModel[dbh == 7 & model == modelSel], 
                xlab = "",
                ylab = "",
                #ylim = c(0, 500),
                main = modelSel,
                xaxt = axisPlot,
                col = values_color[modelSel],
                frame = F)
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
    
    
    
    