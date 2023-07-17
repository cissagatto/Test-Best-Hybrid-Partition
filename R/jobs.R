rm(list = ls())

##############################################################################
# COMPLETE CHAINS HPML                                                       #  
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
# PhD Elaine Cecilia Gatto | Prof. PhD. Ricardo Cerri | Prof. PhD. Mauri     #
# Ferrandin | Prof. PhD. Celine Vens | PhD. Felipe Nakano Kenji              #
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


##################################################
# SET WORK SPACE
##################################################
FolderRoot = "~/Test-Best-Hybrid-Partition"
FolderScripts = "~/Test-Best-Hybrid-Partition/R"


##################################################
# PACKAGES
##################################################
library(stringr)


##################################################
# DATASETS INFORMATION
##################################################
setwd(FolderRoot)
datasets = data.frame(read.csv("datasets-original.csv"))
n = nrow(datasets)


##################################################
# WHICH IMPLEMENTATION WILL BE USED?
##################################################
Implementation.1 = c("clus")
Implementation.2 = c("c")


######################################################
# SIMILARITY MEASURE USED TO MODEL LABEL CORRELATIONS
######################################################
Similarity.1 = c("jaccard", "rogers")
Similarity.2 = c("j", "r")


##################################################
# LINKAGE METRIC USED TO BUILT THE DENDROGRAM
##################################################
Dendrogram.1 = c("ward.D2", "single")
Dendrogram.2 = c("w", "s")


######################################################
# CRITERIA USED TO CHOOSE THE BEST HYBRID PARTITION
######################################################
Criteria.1 = c("silho")
Criteria.2 = c("s")


######################################################
FolderJobs = paste(FolderRoot, "/jobs", sep="")
if(dir.exists(FolderJobs)==FALSE){dir.create(FolderJobs)}

FolderCF = "~/Test-Best-Hybrid-Partition/config-files-apptainer"


# IMPLEMENTAÇÃO
p = 1
while(p<=length(Implementation.1)){
  
  FolderImplementation = paste(FolderJobs, "/", Implementation.1[p], sep="")
  if(dir.exists(FolderImplementation)==FALSE){dir.create(FolderImplementation)}
  
  FolderI = paste(FolderCF, "/", Implementation.1[p], sep="")
  
  # SIMILARIDADE
  s = 1
  while(s<=length(Similarity.1)){
    
    FolderSimilarity = paste(FolderImplementation, "/", Similarity.1[s], sep="")
    if(dir.exists(FolderSimilarity)==FALSE){dir.create(FolderSimilarity)}
    
    FolderS = paste(FolderI, "/", Similarity.1[s], sep="")
    
    # DENDROGRAMA
    f = 1
    while(f<=length(Dendrogram.1)){
      
      FolderDendro = paste(FolderSimilarity, "/", Dendrogram.1[f], sep="")
      if(dir.exists(FolderDendro)==FALSE){dir.create(FolderDendro)}
      
      FolderD = paste(FolderS, "/", Dendrogram.1[f], sep="")
      
      # CRITERIA
      w = 1
      while(w<=length(Criteria.1)){
        
        FolderCriteria = paste(FolderDendro, "/", Criteria.1[w], sep="")
        if(dir.exists(FolderCriteria)==FALSE){dir.create(FolderCriteria)}
        
        FolderC = paste(FolderD, "/", Criteria.1[w], sep="")
        
        # DATASET
        a = 1
        d = 1
        while(d<=nrow(datasets)){
          
          ds = datasets[d,]
          
          cat("\n\n=======================================")
          cat("\n", Implementation.1[p])
          cat("\n\t", Similarity.1[s])
          cat("\n\t", Dendrogram.1[f])
          cat("\n\t", Criteria.1[w])
          cat("\n\t", ds$Name)
          
          
          name = paste("tbhp-", ds$Name, sep="")  
          
          # directory name - "/scratch/eg-3s-bbc1000"
          scratch.name = paste("/tmp/", name, sep = "")
          
          # Confi File Name - "eg-3s-bbc1000.csv"
          config.file.name = paste(name, ".csv", sep="")
          
          # sh file name - "~/Global-Partitions/jobs/utiml/eg-3s-bbc1000.sh
          sh.name = paste(FolderCriteria, "/", name, ".sh", sep = "")
          
          # config file name - "~/Global-Partitions/config-files/utiml/eg-3s-bbc1000.csv"
          config.name = paste(FolderC, "/", config.file.name, sep = "")
          
          # start writing
          output.file <- file(sh.name, "wb")
          
          # bash parameters
          write("#!/bin/bash", file = output.file)
          
          str.1 = paste("#SBATCH -J ", name, sep = "")
          write(str.1, file = output.file, append = TRUE)
          
          write("#SBATCH -o %j.out", file = output.file, append = TRUE)
          
          # number of processors
          write("#SBATCH -n 1", file = output.file, append = TRUE)
          
          # number of cores
          write("#SBATCH -c 10", file = output.file, append = TRUE)
          
          # uncomment this line if you are using slow partition
          # write("#SBATCH --partition slow", file = output.file, append = TRUE)
          
          # uncomment this line if you are using slow partition
          # write("#SBATCH -t 720:00:00", file = output.file, append = TRUE)
          
          # comment this line if you are using slow partition
          write("#SBATCH -t 128:00:00", file = output.file, append = TRUE)
          
          # uncomment this line if you need to use all node memory
          write("#SBATCH --mem=0", file = output.file, append = TRUE)
          
          # amount of node memory you want to use
          # comment this line if you are using -mem=0
          # write("#SBATCH --mem-per-cpu=30GB", file = output.file, append = TRUE)
          # write("#SBATCH -mem=0", file = output.file, append = TRUE)
          
          # email to receive notification
          write("#SBATCH --mail-user=elainegatto@estudante.ufscar.br",
                file = output.file, append = TRUE)
          
          # type of notification
          write("#SBATCH --mail-type=ALL", file = output.file, append = TRUE)
          write("", file = output.file, append = TRUE)
          
          # FUNCTION TO CLEAN THE JOB
          str.2 = paste("local_job=", scratch.name, sep = "")
          write(str.2, file = output.file, append = TRUE)
          
          write("function clean_job(){", file = output.file, append = TRUE)
          
          str.3 = paste(" echo", "\"CLEANING ENVIRONMENT...\"", sep = " ")
          write(str.3, file = output.file, append = TRUE)
          
          str.4 = paste(" rm -rf ", "\"${local_job}\"", sep = "")
          write(str.4, file = output.file, append = TRUE)
          
          write("}", file = output.file, append = TRUE)
          
          write("trap clean_job EXIT HUP INT TERM ERR", 
                file = output.file, append = TRUE)
          
          write("", file = output.file, append = TRUE)
          
          
          # MANDATORY PARAMETERS
          write("set -eE", file = output.file, append = TRUE)
          write("umask 077", file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo =============================================================", 
                file = output.file, append = TRUE)
          str.5 = paste("echo SBATCH: RUNNING TBHP FOR ", 
                        ds$Name, sep="")
          write(str.5, file = output.file, append = TRUE)
          write("echo =============================================================", 
                file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo DELETING FOLDER", file = output.file, append = TRUE)
          str.6 = paste("rm -rf ", scratch.name, sep = "")
          write(str.6, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo CREATING FOLDER", file = output.file, append = TRUE)
          str.7 = paste("mkdir ", scratch.name, sep = "")
          write(str.7, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo LISTING SCRATCH", file = output.file, append = TRUE)
          write("cd /tmp", file = output.file, append = TRUE)
          write("ls ", file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo entrando na pasta", file = output.file, append = TRUE)
          str = paste("cd ", name, sep="")
          write(str, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo LISTING SCRATCH/NAME", file = output.file, append = TRUE)
          write("ls ", file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo COPYING SINGULARITY", file = output.file, append = TRUE)
          str.30 = paste("cp /home/u704616/Experimentos-0.sif ", scratch.name, sep ="")
          write(str.30 , file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo CRIANDO TESTED", file = output.file, append = TRUE)
          str.29 = paste("mkdir ", scratch.name, "/Tested", sep="")
          write(str.29, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo CRIANDO DATASET", file = output.file, append = TRUE)
          str.28 = paste("mkdir ", scratch.name, "/Datasets", sep="")
          write(str.28, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo CRIANDO PARTITIONS", file = output.file, append = TRUE)
          str.27 = paste("mkdir ", scratch.name, "/2-Best-Partitions", sep="")
          write(str.27, file = output.file, append = TRUE)
          
          write("", file = output.file, append = TRUE)
          write("echo CRIANDO PARTITIONS", file = output.file, append = TRUE)
          str.27 = paste("mkdir ", scratch.name, "/Partitions", sep="")
          write(str.27, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo CRIANDO pasta", file = output.file, append = TRUE)
          str.26 = paste("mkdir ", scratch.name, "/Datasets/", 
                         ds$Name, sep="")
          write(str.26, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo CRIANDO pasta", file = output.file, append = TRUE)
          str.25 = paste("mkdir ", scratch.name, "/Datasets/", 
                         ds$Name, "/LabelSpace", sep="")
          write(str.25, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo CRIANDO pasta", file = output.file, append = TRUE)
          str.24 = paste("mkdir ", scratch.name, "/Datasets/", 
                         ds$Name, "/NamesLabels", sep="")
          write(str.24, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo CRIANDO pasta", file = output.file, append = TRUE)
          str.23 = paste("mkdir ", scratch.name, "/Datasets/", 
                         ds$Name, "/CrosValidation", sep="")
          write(str.23, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo CRIANDO pasta", file = output.file, append = TRUE)
          str.21 = paste("mkdir ",scratch.name, "/Datasets/", 
                         ds$Name, "/CrosValidation/Tr", sep="")
          write(str.21, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo CRIANDO pasta", file = output.file, append = TRUE)
          str.20 = paste("mkdir ",scratch.name, "/Datasets/", 
                         ds$Name, "/CrosValidation/Ts", sep="")
          write(str.20, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo CRIANDO pasta", file = output.file, append = TRUE)
          str.19 = paste("mkdir ",scratch.name, "/Datasets/", 
                         ds$Name, "/CrosValidation/Vl", sep="")
          write(str.19, file = output.file, append = TRUE)
          
          
          write(" ", file = output.file, append = TRUE)
          write("echo INICIANDO INSTANCIA", file = output.file, append = TRUE)
          str = paste("singularity instance start --bind ~/.config/rclone/:/root/.config/rclone ", 
                      scratch.name, "/Experimentos-0.sif EXPt", a, sep="")
          write(str, file = output.file, append = TRUE)
          
          
          write(" ", file = output.file, append = TRUE)
          write("echo EXECUTANDO", file = output.file, append = TRUE)
          str = paste("singularity run --app Rscript instance://EXPt",
                      a, " ~/Test-Best-Hybrid-Partition/R/tbhp.R \"~/Test-Best-Hybrid-Partition/config-files-apptainer/",
                      Implementation.1[p], "/", Similarity.1[s], "/", 
                      Dendrogram.1[f], "/", Criteria.1[w], "/", 
                      config.file.name, "\"", sep="")
          write(str, file = output.file, append = TRUE)
          
          
          write(" ", file = output.file, append = TRUE)
          write("echo STOP INSTANCIA", file = output.file, append = TRUE)
          str = paste("singularity instance stop EXPt", a, sep="")
          write(str, file = output.file, append = TRUE)
          
          
          write(" ", file = output.file, append = TRUE)
          write("echo DELETING JOB FOLDER", file = output.file, append = TRUE)
          str.13 = paste("rm -rf ", scratch.name, sep = "")
          write(str.13, file = output.file, append = TRUE)
          
          
          write("", file = output.file, append = TRUE)
          write("echo ==================================", 
                file = output.file, append = TRUE)
          write("echo SBATCH: ENDED SUCCESSFULLY", 
                file = output.file, append = TRUE)
          write("echo ==================================", 
                file = output.file, append = TRUE)
          
          close(output.file)
          
          a = a + 1
          d = d + 1
          gc()
        } # FIM DO DATASET
        
        w = w + 1
        gc()
      } # FIM DO CRITERIO
      
      f = f + 1
      gc()
      
    } # FIM DO DENDROGRAMA
    
    s = s + 1
    gc()
  } # FIM DA SIMILARIDADE
  
  p = p + 1
  gc()
} # FIM DA IMPLEMENTAÇÃO


###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #                                #
###############################################################################