##############################################################################
# Test Best Hybrid Partition                                                 #
# Copyright (C) 2023                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri           #
# Ferrandin | Prof. Dr. Celine Vens | Dr. Felipe Nakano Kenji                #
#                                                                            #
# Federal University of São Carlos - UFSCar - https://www2.ufscar.br         #
# Campus São Carlos - Computer Department - DC - https://site.dc.ufscar.br   #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br - Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
# Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium               #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
##############################################################################


FolderRoot = "/Test-Best-Hybrid-Partition"
FolderScripts = "/Test-Best-Hybrid-Partition/R"



##################################################################################################
# Runs for all datasets listed in the "datasets.csv" file                                        #
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             #
# delete: if you want, or not, to delete all folders and files generated                         #
##################################################################################################
execute.run.clus <- function(parameters){
  
  FolderRoot = "/Test-Best-Hybrid-Partition"
  FolderScripts = "/Test-Best-Hybrid-Partition/R"
  
  setwd(FolderScripts)
  source("misc.R")
  
  setwd(FolderScripts)
  source("test-clus-silho.R")
  
  
  if(number_cores == 0){
    
    cat("\n\n##########################################################")
    cat("\n# Zero is a disallowed value for number_cores. Please      #")
    cat("\n# choose a value greater than or equal to 1.               #")
    cat("\n############################################################\n\n")
    
  } else {
    
    cl <- parallel::makeCluster(number_cores)
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if(number_cores==1){
      cat("\n\n##########################################################")
      cat("\n# Running Sequentially!                                    #")
      cat("\n############################################################\n\n")
    } else {
      cat("\n\n############################################################")
      cat("\n# Running in parallel with ", number_cores, " cores!         #")
      cat("\n##############################################################\n\n")
    }
  }
  cl = cl
  
  cat("\n\n#########################################################")
  cat("\n# RUN CLUS: Get labels                                  #")
  cat("\n#########################################################\n\n")
  arquivo = paste(parameters$Folders$folderNamesLabels, "/" ,
                  dataset_name, "-NamesLabels.csv", sep="")
  namesLabels = data.frame(read.csv(arquivo))
  colnames(namesLabels) = c("id", "labels")
  namesLabels = c(namesLabels$labels)
  parameters$Config$NamesLabels = namesLabels
  
  
  cat("\n\n#########################################################")
  cat("\n# RUN CLUS: Get the label space                         #")
  cat("\n#########################################################\n\n")
  timeLabelSpace = system.time(resLS <- labelSpace(parameters))
  parameters$LabelSpace = resLS
  
  
  cat("\n\n###################################################")
    cat("\n# RUN CLUS: Get all partitions                    #")
    cat("\n###################################################\n\n")
  timeAllPartitions = system.time(resAP <- get.all.partitions(parameters))
  parameters$All.Partitions = resAP
  
  
  
  if(parameters$Config$Criteria=="maf1"){
    
    cat("\n\n##########################################################")
      cat("\n# RUN CLUS MACRO-F1: Build and Test Partitions           #")
      cat("\n##########################################################\n\n")
    timeBuild = system.time(resBuild <- build.clus.maf1(parameters))
    
    
    cat("\n\n#########################################################")
      cat("\n# RUN CLUS MACRO-F1: Matrix Confusion                   #")
      cat("\n#########################################################\n\n")
    timePreds = system.time(resGather <- gather.preds.clus.maf1(parameters))
    
    
    cat("\n\n##########################################################")
      cat("\n# RUN CLUS MACRO-F1: Evaluation                          #")
      cat("\n##########################################################\n\n")
    timeEvaluate = system.time(resEval <- evaluate.clus.maf1(parameters))
    
    
    cat("\n\n##########################################################")
      cat("\n# RUN CLUS MACRO-F1: Mean 10 Folds                       #")
      cat("\n##########################################################\n\n")
    timeGather = system.time(resGE <- gather.eval.clus.maf1(parameters))
    
    
    cat("\n\n#########################################################")
      cat("\n# RUN CLUS MACRO-F1: Save Runtime                       #")
      cat("\n#########################################################\n\n")
    timesExecute = rbind(timeAllPartitions, timeLabelSpace, 
                         timeBuild, timePreds,
                         timeEvaluate, timeGather)
    setwd(parameters$Folders$folderTested)
    write.csv(timesExecute, "Run-Time-Maf1-Clus.csv")
    
    
  } else if(parameters$Config$Criteria=="mif1"){
    
    cat("\n\n##########################################################")
    cat("\n# RUN CLUS MICRO-F1: Build and Test Partitions           #")
    cat("\n##########################################################\n\n")
    timeBuild = system.time(resBuild <- build.clus.mif1(parameters))
    
    
    cat("\n\n#########################################################")
    cat("\n# RUN CLUS MICRO-F1: Matrix Confusion                   #")
    cat("\n#########################################################\n\n")
    timePreds = system.time(resGather <- gather.preds.clus.mif1(parameters))
    
    
    cat("\n\n##########################################################")
    cat("\n# RUN CLUS MICRO-F1: Evaluation                          #")
    cat("\n##########################################################\n\n")
    timeEvaluate = system.time(resEval <- evaluate.clus.mif1(parameters))
    
    
    cat("\n\n##########################################################")
    cat("\n# RUN CLUS MICRO-F1: Mean 10 Folds                       #")
    cat("\n##########################################################\n\n")
    timeGather = system.time(resGE <- gather.eval.clus.mif1(parameters))
    
    
    cat("\n\n#########################################################")
    cat("\n# RUN CLUS MICRO-F1: Save Runtime                       #")
    cat("\n#########################################################\n\n")
    timesExecute = rbind(timeAllPartitions, timeLabelSpace, 
                         timeBuild, timePreds,
                         timeEvaluate, timeGather)
    setwd(parameters$Folders$folderTested)
    write.csv(timesExecute, "Run-Time-Mif1-Clus.csv")
    
    
    
  } else {
    
    cat("\n\n########################################################")
      cat("\n# RUN CLUS SILHOUETTE: Build and Test Partitions       #")
      cat("\n########################################################\n\n")
    timeBuild = system.time(resBuild <- build.clus.silho(parameters))
    
    
    cat("\n\n#########################################################")
      cat("\n# RUN CLUS SILHOUETTE: Matrix Confusion                 #")
      cat("\n#########################################################\n\n")
    timePreds = system.time(resGather <- gather.preds.clus.silho(parameters))
    
    
    cat("\n\n########################################################")
      cat("\n# RUN CLUS SILHOUETTE: Evaluation                      #")
      cat("\n########################################################\n\n")
    timeEvaluate = system.time(resEval <- evaluate.clus.silho(parameters))
    
    
    cat("\n\n########################################################")
      cat("\n# RUN CLUS SILHOUETTE: Mean 10 Folds                   #")
      cat("\n########################################################\n\n")
    timeGather = system.time(resGE <- gather.eval.clus.silho(parameters))
    
    
    cat("\n\n#######################################################")
      cat("\n# RUN CLUS SILHOUETTE: Save Runtime                   #")
      cat("\n#######################################################\n\n")
    timesExecute = rbind(timeAllPartitions, timeLabelSpace, 
                         timeBuild, timePreds,
                         timeEvaluate, timeGather)
    setwd(parameters$Folders$folderTested)
    write.csv(timesExecute, "Run-Time-Silho-Clus.csv")
    
  }
  
  
  cat("\n\n##########################################################")
    cat("\n# RUN: Stop Parallel                                     #")
    cat("\n##########################################################\n\n")
  parallel::stopCluster(cl) 	
  
  cat("\n\n##########################################################")
    cat("\n# RUN: END                                               #")
    cat("\n##########################################################\n\n")
  gc()
  
  
}

##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
