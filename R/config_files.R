rm(list = ls())

##############################################################################
# CHAINS OF HYBRID PARTITIONS                                                #
# Copyright (C) 2022                                                         #
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
FolderScripts = "~/Test-Best-Hybrid-Partition"


library(stringr)


setwd(FolderRoot)
datasets = data.frame(read.csv("datasets-original.csv"))
n = nrow(datasets)

Implementation = c("clus", "utiml", "mulan", "python")
Implementation.2 = c("c", "u", "m", "p")

Similarity = c("jaccard-3","rogers-2")
Similarity.2 = c("j3", "ro2")

Criteria = c("maf1", "mif1", "silho")
Criteria.2 = c("ma", "mi", "s")

FolderCF = paste(FolderRoot, "/config-files", sep="")
if(dir.exists(FolderCF)==FALSE){dir.create(FolderCF)}

# primeiro é o Implementation
p = 1
while(p<=length(Implementation)){
  
  FolderImplementation = paste(FolderCF, "/", Implementation[p], sep="")
  if(dir.exists(FolderImplementation)==FALSE){dir.create(FolderImplementation)}
  
  # depois é pela medida de Similarity
  s = 1
  while(s<=length(Similarity)){
    
    FolderSimilarity = paste(FolderImplementation, "/", Similarity[s], sep="")
    if(dir.exists(FolderSimilarity)==FALSE){dir.create(FolderSimilarity)}
    
    # agora é o critério de validação
    w = 1
    while(w<=length(Criteria)){
      
      FolderCriteria = paste(FolderSimilarity, "/", Criteria[w], sep="")
      if(dir.exists(FolderCriteria)==FALSE){dir.create(FolderCriteria)}
      
      # por fim o dataset
      d = 1
      while(d<=nrow(datasets)){
        
        ds = datasets[d,]
        
        cat("\n\n=======================================")
        cat("\n", Implementation[p])
        cat("\n", Similarity[s])
        cat("\n", Criteria[w])
        cat("\n", ds$Name)
        cat("\n=======================================\n")
        
        name = paste("t", Implementation.2[p], "", Criteria.2[w], "", 
                     Similarity.2[s], "-", ds$Name, sep="")  
        
        file.name = paste(FolderCriteria, "/", name, ".csv", sep="")
        
        output.file <- file(file.name, "wb")
        
        write("Config, Value",
              file = output.file, append = TRUE)
        
        write("Dataset_Path, /home/biomal/Datasets", 
              file = output.file, append = TRUE)
        
        folder.name = paste("/dev/shm/", name, sep = "")
        
        str1 = paste("Temporary_Path, ", folder.name, sep="")
        write(str1,file = output.file, append = TRUE)
        
        str.1 = paste("/home/biomal/Best-Partitions/", Similarity[s], 
                      "/", Criteria[w], sep="")
        str.2 = paste("Partitions_Path, ", str.1,  sep="")
        write(str.2, file = output.file, append = TRUE)
        
        str0 = paste("Implementation, ", Implementation[p], sep="")
        write(str0, file = output.file, append = TRUE)
        
        str3 = paste("Similarity, ", Similarity[s], sep="")
        write(str3, file = output.file, append = TRUE)
        
        str2 = paste("Criteria, ", Criteria[w], sep="")
        write(str2, file = output.file, append = TRUE)
        
        str3 = paste("Dataset_Name, ", ds$Name, sep="")
        write(str3, file = output.file, append = TRUE)
        
        str4 = paste("Number_Dataset, ", ds$Id, sep="")
        write(str4, file = output.file, append = TRUE)
        
        write("Number_Folds, 10", file = output.file, append = TRUE)
        
        write("Number_Cores, 1", file = output.file, append = TRUE)
        
        close(output.file)
        
        d = d + 1
        gc()
      } # fim do dataset
      
      w = w + 1
      gc()
    } # fim do Implementation
    
    s = s + 1
    gc()
  } # fim da
  
  p = p + 1
  gc()
} # fim do Implementation



###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #                                #
###############################################################################