# Code from "Tree regeneration in models of forest dynamics: a key priority for further research"

This repository contains the code for the data analysis and manuscript preparation.

## code/

### dataPreparation.R

This script processes the data submissions and creates the database used in the analysis collected in figures.R. After running this script you will create the file ```data/dataOutputs.csv```

### figures.R

This script analyzes the data (```data/dataOutputs.csv```) and creates figures and tables used in the manuscript. After running this script you will storage all the figures used in  ```manuscript/manuscriptRW.Rmd``` and in  ```manuscript/supplementaryRW.Rmd```  

### plottingManuscript.R

Provides help for the figures creation.

## data/

### model_structure/

It contains the data needed to evaluate model complexity based on the paper: Bugmann, H., & Seidl, R. (2022). The evolution, complexity and diversity of models of long-term forest dynamics. *Journal of Ecology*, *110*(10), 2288-2307. https://doi.org/10.1111/1365-2745.13989

### observations/

This folder will be downloaded when running the script ```dataPreparation.R``` It contains the data from the observed values from the EuFoRIa network. 

### simulations_results/

This folder will be downloaded when running the script ```dataPreparation.R``` It contains a set of folders, one per model, with the simulation results.

### dataOutputs.csv

This file is when running the script ```code/figures.R```. 



## figures/

It contains the map figure (the only figure not reproducible due to coordinates data protection) and will store all the figures outputs from the script figures.R


## mansucript/
It contains the Rmd and related files that create the manuscript and supplementary materials. It also includes the protocol in Rmd. In order to compile these documents you need to first create the figures by running both ```code/dataPreparation.R``` and  ```code/figures.R```



