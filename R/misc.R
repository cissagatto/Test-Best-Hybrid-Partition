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


FolderRoot = "~/Test-Best-Hybrid-Partition"
FolderScripts = "~/Test-Best-Hybrid-Partition/R"




#########################################################################
# Function to correctly convert CSV in ARFF
converteArff <- function(arg1, arg2, arg3){
  str = paste("java -jar ", parameters$Folders$folderUtils,
              "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
  print(system(str))
  cat("\n")
}



##############################################################################
#
##############################################################################
get.all.partitions <- function(parameters){
  
  retorno = list()
  
  pasta.best = paste(parameters$Folders$folderPartitions, 
                     "/", parameters$Config$Dataset.Name, 
                     "/", parameters$Config$Dataset.Name, 
                     "-Best-Silhouete.csv", sep="")
  best = data.frame(read.csv(pasta.best))
  
  num.fold = c(0)
  num.part = c(0)
  num.group = c(0)
  best.part.info = data.frame(num.fold, num.part, num.group)
  
  all.partitions.info = data.frame()
  all.total.labels = data.frame()
  
  f = 1
  while(f<=parameters$Config$Number.Folds){
    
    best.fold = best[f,]
    num.fold = best.fold$fold
    num.part = best.fold$part
    
    Pasta = paste(parameters$Folders$folderPartitions, 
                  "/", parameters$Config$Dataset.Name, "/Split-", 
                  f, sep="")
    pasta.groups = paste(Pasta, "/fold-", f, 
                         "-groups-per-partition.csv", sep="")
    clusters = data.frame(read.csv(pasta.groups))
    groups.fold = data.frame(filter(clusters, partition == num.part))
    
    num.group = groups.fold$num.groups
    best.part.info = rbind(best.part.info, 
                           data.frame(num.fold, num.part, num.group))
    
    nome = paste(Pasta, "/Partition-", num.part, 
                 "/partition-", num.part, ".csv", sep="")
    partitions = data.frame(read.csv(nome))
    partitions = data.frame(num.fold, num.part, partitions)
    partitions = arrange(partitions, group)
    
    all.partitions.info = rbind(all.partitions.info, partitions)
    
    nome.2 = paste(Pasta, "/Partition-", num.part,
                   "/fold-", f, "-labels-per-group-partition-", 
                   num.group, ".csv", sep="")
    labels = data.frame(read.csv(nome.2))
    labels = data.frame(num.fold, labels)
    all.total.labels = rbind(all.total.labels , labels)
    
    f = f + 1
    gc()
  } # fim do fold
  
  setwd(parameters$Folders$folderTested)
  write.csv(best.part.info, "best-part-info.csv", row.names = FALSE)
  write.csv(all.partitions.info, "all.partitions.info.csv", row.names = FALSE)
  write.csv(all.total.labels, "all.total.labels.csv", row.names = FALSE)
  
  retorno$best.part.info = best.part.info[-1,]
  retorno$all.partitions.info = all.partitions.info
  retorno$all.total.labels = all.total.labels
  return(retorno)
  
}

