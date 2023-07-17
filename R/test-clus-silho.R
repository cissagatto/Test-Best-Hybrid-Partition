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
#
#########################################################################
build.clus.silho <- function(parameters) {
  
  f = 1
  bthpkParalel <- foreach(f = 1:number_folds) %dopar% {
  # while(f<=parameters$Config$Number.Folds){
    
    cat("\nFold: ", f)
    
    #########################################################################
    FolderRoot = "~/Test-Best-Hybrid-Partition"
    FolderScripts = "~/Test-Best-Hybrid-Partition/R"
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    #########################################################################
    cat("\nGetting information about clusters")
    best.part.info = data.frame(parameters$All.Partitions$best.part.info)
    all.partitions.info = data.frame(parameters$All.Partitions$all.partitions.info)
    all.total.labels = data.frame(parameters$All.Partitions$all.total.labels)
    
    best.part.info.f = data.frame(filter(best.part.info, num.fold == f))
    all.total.labels.f = data.frame(filter(all.total.labels, num.fold ==
                                             f))
    partition = data.frame(filter(all.partitions.info, num.fold == f))
    
    #########################################################################
    cat("\nCreating Folders from Best Partitions and Splits Tests")
    Folder.Best.Partition.Split = paste(parameters$Folders$folderPartitions,
                                        "/Split-", f, sep = "")
    
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep = "")
    if (dir.create(Folder.Tested.Split) == FALSE){dir.create(Folder.Tested.Split)}
    
    Folder.BP = paste(parameters$Folders$folderPartitions, "/", 
                      parameters$Config$Dataset.Name,sep = "")
    
    Folder.BPF = paste(Folder.BP, "/Split-", f, sep = "")
    
    Folder.BPGP = paste(Folder.BPF, "/Partition-", best.part.info.f$num.part,
                        sep = "")
    
    #########################################################################
    cat("\n\nOpen Train file")
    train.name.file = paste(parameters$Folders$folderCVTR, "/",
                            parameters$Config$Dataset.Name, "-Split-Tr-",
                            f, ".csv", sep = "")
    train.dataset = data.frame(read.csv(train.name.file))
    
    #########################################################################
    cat("\n\nOpen Test file")
    test.name.file = paste(parameters$Folders$folderCVTS, "/",
                           parameters$Config$Dataset.Name, "-Split-Ts-",
                           f, ".csv", sep = "")
    test.dataset = data.frame(read.csv(test.name.file))
    
    #########################################################################
    cat("\n\nOpen VAlidation file")
    val.name.file = paste(parameters$Folders$folderCVVL,
                          "/", parameters$Config$Dataset.Name,
                          "-Split-Vl-", f, ".csv", sep = "")
    val.dataset = data.frame(read.csv(val.name.file))
    
    
    #########################################################################
    cat("\n\nJoin validation with train")
    train.dataset.final = rbind(train.dataset, val.dataset)
    
    
    #########################################################################
    g = 1
    while (g <= best.part.info.f$num.group) {
      cat("\nPartition: ", g)
      
      #########################################################################
      cat("\ncreating folder")
      Folder.Tested.Group = paste(Folder.Tested.Split, "/Group-", g, sep="")
      if(dir.exists(Folder.Tested.Group) == FALSE){dir.create(Folder.Tested.Group)}
      
      #########################################################################
      cat("\nSpecific Group")
      specificGroup = data.frame(filter(partition, group == g))
      
      
      #########################################################################
      cat("\nTrain: Mount Group")
      train.attributes = train.dataset.final[, parameters$DatasetInfo$AttStart:parameters$DatasetInfo$AttEnd]
      train.classes = select(train.dataset.final, specificGroup$label)
      train.dataset.cluster = cbind(train.attributes, train.classes)
      
      
      #########################################################################
      cat("\nTrain: Save Group")
      train.name.cluster.csv = paste(Folder.Tested.Group,"/",
                                     parameters$Config$Dataset.Name, 
                                     "-split-tr-",f, "-group-",g,
                                     ".csv",sep = "")
      write.csv(train.dataset.cluster, train.name.cluster.csv, row.names = FALSE)
      
      
      #########################################################################
      start = ncol(train.attributes) + 1
      end = ncol(train.dataset.cluster)
      
      
      #########################################################################
      cat("\nTrain: Convert CSV to ARFF and Convert Numeric to Binary")
      train.name.cluster.arff = paste(
        Folder.Tested.Group,
        "/",
        parameters$Config$Dataset.Name,
        "-split-tr-",
        f,
        "-group-",
        g,
        ".arff",
        sep = ""
      )
      arg.tr.csv = train.name.cluster.csv
      arg.tr.arff = train.name.cluster.arff
      arg.targets = paste(start , "-", end, sep = "")
      str.convert = paste(
        "java -jar ",
        parameters$Folders$folderUtils,
        "/R_csv_2_arff.jar ",
        arg.tr.csv,
        " ",
        arg.tr.arff,
        " ",
        arg.targets,
        sep = ""
      )
      cat("\n")
      print(system(str.convert))
      cat("\n")
      
      
      #########################################################################
      cat("\nTrain: Verify and correct {0} and {1}")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arg.tr.arff, sep = "")
      print(system(str0))
      
      
      #########################################################################
      cat("\nTest: Mount Group")
      test.attributes = test.dataset[, parameters$DatasetInfo$AttStart:parameters$DatasetInfo$AttEnd]
      test.classes = select(test.dataset, specificGroup$label)
      test.dataset.cluster = cbind(test.attributes, test.classes)
      
      
      #########################################################################
      cat("\nTest: Save Group")
      test.name.cluster.csv = paste(
        Folder.Tested.Group,
        "/",
        parameters$Config$Dataset.Name,
        "-split-ts-",
        f,
        "-group-",
        g,
        ".csv",
        sep = ""
      )
      write.csv(test.dataset.cluster,
                test.name.cluster.csv,
                row.names = FALSE)
      
      
      #########################################################################
      cat("\nTest: Convert CSV to ARFF and Convert Numeric to Binary")
      test.name.cluster.arff = paste(
        Folder.Tested.Group,
        "/",
        parameters$Config$Dataset.Name,
        "-split-ts-",
        f,
        "-group-",
        g,
        ".arff",
        sep = ""
      )
      arg.ts.csv = test.name.cluster.csv
      arg.ts.arff = test.name.cluster.arff
      arg.targets = paste(start , "-", end, sep = "")
      str.convert = paste(
        "java -jar ",
        parameters$Folders$folderUtils,
        "/R_csv_2_arff.jar ",
        arg.ts.csv,
        " ",
        arg.ts.arff,
        " ",
        arg.targets,
        sep = ""
      )
      cat("\n")
      print(system(str.convert))
      cat("\n")
      
      #########################################################################
      cat("\nTrain: Verify and correct {0} and {1}")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ",arg.ts.arff, sep = "")
      print(system(str0))
      
      
      #########################################################################
      cat("\nCreating .s file for clus")
      if (start == end) {
        nome_config = paste(Folder.Tested.Group,
                            "/",
                            parameters$Config$Dataset.Name,
                            "-split-",
                            f,
                            "-group-",
                            g,
                            ".s",
                            sep = "")
        sink(nome_config, type = "output")
        
        cat("[General]")
        cat("\nCompatibility = MLJ08")
        
        cat("\n\n[Data]")
        cat(paste("\nFile = ", train.name.cluster.arff, sep = ""))
        cat(paste("\nTestSet = ", test.name.cluster.arff, sep = ""))
        
        cat("\n\n[Attributes]")
        cat("\nReduceMemoryNominalAttrs = yes")
        
        cat("\n\n[Attributes]")
        cat(paste("\nTarget = ", end, sep = ""))
        cat("\nWeights = 1")
        
        cat("\n")
        cat("\n[Tree]")
        cat("\nHeuristic = VarianceReduction")
        cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")
        
        cat("\n\n[Model]")
        cat("\nMinimalWeight = 5.0")
        
        cat("\n\n[Output]")
        cat("\nWritePredictions = {Test}")
        cat("\n")
        sink()
        
        #######################################################################
        cat("\n\nExecute CLUS")
        str = paste("java -jar ",
                    parameters$Folders$folderUtils,
                    "/Clus.jar ",
                    nome_config,
                    sep = "")
        print(system(str))
        
      } else {
        nome_config = paste(Folder.Tested.Group,
                            "/",
                            parameters$Config$Dataset.Name,
                            "-split-",
                            f,
                            "-group-",
                            g,
                            ".s",
                            sep = "")
        sink(nome_config, type = "output")
        
        cat("[General]")
        cat("\nCompatibility = MLJ08")
        
        cat("\n\n[Data]")
        cat(paste("\nFile = ", train.name.cluster.arff, sep = ""))
        cat(paste("\nTestSet = ", test.name.cluster.arff, sep = ""))
        
        cat("\n\n[Attributes]")
        cat("\nReduceMemoryNominalAttrs = yes")
        
        cat("\n\n[Attributes]")
        cat(paste("\nTarget = ", start, "-", end, sep = ""))
        cat("\nWeights = 1")
        
        cat("\n")
        cat("\n[Tree]")
        cat("\nHeuristic = VarianceReduction")
        cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")
        
        cat("\n\n[Model]")
        cat("\nMinimalWeight = 5.0")
        
        cat("\n\n[Output]")
        cat("\nWritePredictions = {Test}")
        cat("\n")
        sink()
        
        #######################################################################
        cat("\n\nExecute CLUS")
        str = paste("java -jar ",
                    parameters$Folders$folderUtils,
                    "/Clus.jar ",
                    nome_config,
                    sep = "")
        print(system(str))
        
      }
      
      #########################################################################
      cat("\n\nOpen predictions")
      nomeDoArquivo = paste(
        Folder.Tested.Group,
        "/",
        parameters$Config$Dataset.Name,
        "-split-",
        f,
        "-group-",
        g,
        ".test.pred.arff",
        sep = ""
      )
      predicoes = data.frame(foreign::read.arff(nomeDoArquivo))
      
      #########################################################################
      cat("\nSPLIT PREDICTIS")
      if (start == end) {
        cat("\n\nOnly one label in this group")
        
        #########################################################################
        cat("\n\nSave Y_true")
        setwd(Folder.Tested.Group)
        classes = data.frame(predicoes[, 1])
        names(classes) = colnames(predicoes)[1]
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        #########################################################################
        cat("\n\nSave Y_true")
        rot = paste("Pruned.p.", colnames(predicoes)[1], sep = "")
        pred = data.frame(predicoes[, rot])
        names(pred) = colnames(predicoes)[1]
        setwd(Folder.Tested.Group)
        write.csv(pred, "y_predict.csv", row.names = FALSE)
        
        #########################################################################
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        gc()
        
      } else {
        #########################################################################
        cat("\nMore than one label in this group")
        comeco = 1 + (end - start)
        
        
        #########################################################################
        cat("\n\nSave Y_true")
        classes = data.frame(predicoes[, 1:comeco])
        setwd(Folder.Tested.Group)
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        
        #########################################################################
        cat("\n\nSave Y_true")
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        nomeColuna = c()
        t = 1
        while (t <= n_r) {
          nomeColuna[t] = paste("Pruned.p.", rotulos[t], sep = "")
          t = t + 1
          gc()
        }
        pred = data.frame(predicoes[nomeColuna])
        names(pred) = rotulos
        setwd(Folder.Tested.Group)
        write.csv(pred, "y_predict.csv", row.names = FALSE)
        gc()
      } # end DO ELSE
      
      # deleting files
      um = paste(Folder.Tested.Group, "/", 
                 parameters$Config$Dataset.Name, "-split-", f, 
                 "-group-", g, ".model", sep = "")
      dois = paste(Folder.Tested.Group, "/", 
                   parameters$Config$Dataset.Name, "-split-", f, 
                   "-group-", g, ".s", sep ="")
      tres = paste(Folder.Tested.Group, "/", 
                   parameters$Config$Dataset.Name, "-split-tr-", f, 
                   "-group-", g, ".arff", sep ="")
      quatro = paste(Folder.Tested.Group, "/", 
                     parameters$Config$Dataset.Name, "-split-ts-", f, 
                     "-group-", g, ".arff", sep="")
      cinco = paste(Folder.Tested.Group, "/", 
                    parameters$Config$Dataset.Name, "-split-tr-", f, 
                    "-group-", g, ".csv", sep ="")
      seis = paste(Folder.Tested.Group, "/", 
                   parameters$Config$Dataset.Name, "-split-ts-", f, 
                   "-group-", g, ".csv", sep ="")
      sete = paste(Folder.Tested.Group, "/", 
                   parameters$Config$Dataset.Name, "-split-", f, 
                   "-group-", g, ".out", sep ="")
      oito = paste(Folder.Tested.Group, "/",
                   "Variance_RHE_1.csv", sep ="")
      
      setwd(Folder.Tested.Group)
      unlink(um, recursive = TRUE)
      unlink(dois, recursive = TRUE)
      unlink(tres, recursive = TRUE)
      unlink(quatro, recursive = TRUE)
      unlink(cinco, recursive = TRUE)
      unlink(seis, recursive = TRUE)
      unlink(sete, recursive = TRUE)
      unlink(oito, recursive = TRUE)
      
      g = g + 1
      gc()
    } # end grupos
  
    #f = f + 1  
    gc()
  } # ending folds
  
  gc()
  cat("\n############################################################")
  cat("\n# CLUS SILHOUETTE: End build.clus.silho                    #")
  cat("\n############################################################")
  cat("\n\n\n\n")
}


######################################################################
#
######################################################################
gather.preds.clus.silho <- function(parameters) {
  
  f = 1
  gatherR <- foreach(f = 1:parameters$Config$Number.Folds) %dopar% {
  # while(f<=parameters$Config$Number.Folds){
    cat("\nFold: ", f)
    
    FolderRoot = "~/Test-Best-Hybrid-Partition"
    FolderScripts = "~/Test-Best-Hybrid-Partition/R"
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    ###################################################################
    apagar = c(0)
    y_true = data.frame(apagar)
    y_pred = data.frame(apagar)
    
    ###################################################################
    Folder.Split.Test = paste(parameters$Folders$folderTested, 
                            "/Split-", f, sep = "")
    
    
    #########################################################################
    cat("\nGetting information about clusters")
    best.part.info = data.frame(parameters$All.Partitions$best.part.info)
    all.partitions.info = data.frame(parameters$All.Partitions$all.partitions.info)
    all.total.labels = data.frame(parameters$All.Partitions$all.total.labels)
    
    best.part.info.f = data.frame(filter(best.part.info, num.fold == f))
    all.total.labels.f = data.frame(filter(all.total.labels, num.fold ==
                                             f))
    partition = data.frame(filter(all.partitions.info, num.fold == f))
    
    
    #########################################################################
    cat("\nCreating Folders from Best Partitions and Splits Tests")
    Folder.Best.Partition.Split = paste(parameters$Folders$folderPartitions,
                                        "/Split-", f, sep = "")
    
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep = "")
    
    Folder.BP = paste(parameters$Folders$folderPartitions, "/", 
                      parameters$Config$Dataset.Name,sep = "")
    
    Folder.BPF = paste(Folder.BP, "/Split-", f, sep = "")
    
    Folder.BPGP = paste(Folder.BPF, "/Partition-", 
                        best.part.info.f$num.part, sep = "")
    
    #########################################################################
    g = 1
    while (g <= best.part.info.f $num.group) {
      cat("\n\nGroup: ", g)
      
      Folder.Group.Test = paste(Folder.Split.Test, "/Group-", g, sep = "")
      
      cat("\n\nGather y_true ", g)
      setwd(Folder.Group.Test)
      y_true_gr = data.frame(read.csv("y_true.csv"))
      y_true = cbind(y_true, y_true_gr)
      
      setwd(Folder.Group.Test)
      cat("\n\nGather y_predict ", g)
      y_pred_gr = data.frame(read.csv("y_predict.csv"))
      y_pred = cbind(y_pred, y_pred_gr)
      
      cat("\n\nDeleting files")
      unlink("y_true.csv", recursive = TRUE)
      unlink("y_predict.csv", recursive = TRUE)
      
      oito = paste(Folder.Group.Test, "/",
                   "Variance_RHE_1.csv", sep ="")
      
      unlink(oito, recursive = TRUE)
      
      g = g + 1
      gc()
    }
    
    cat("\n\nSave files ", g, "\n")
    setwd(Folder.Split.Test)
    y_pred = y_pred[, -1]
    y_true = y_true[, -1]
    write.csv(y_pred, "y_predict.csv", row.names = FALSE)
    write.csv(y_true, "y_true.csv", row.names = FALSE)
    
    #f = f + 1
    gc()
  } # end do foreach
  
  gc()
  cat("\n#####################################################")
  cat("\n# CLUS SILHOUETTE: End gather.preds.clus.silho      #")
  cat("\n######################################################")
  cat("\n\n\n\n")
  
} # end da função


#######################################################################
#
#######################################################################
evaluate.clus.silho <- function(parameters) {
  
  f = 1
  avalParal <- foreach(f = 1:parameters$Config$Number.Folds) %dopar% {
    cat("\nFold: ", f)
    
    FolderRoot = "~/Test-Best-Hybrid-Partition"
    FolderScripts = "~/Test-Best-Hybrid-Partition/R"
    
    # data frame
    apagar = c(0)
    confMatPartitions = data.frame(apagar)
    partitions = c()
    
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep = "")
    
    # get the true and predict lables
    setwd(Folder.Tested.Split)
    y_true = data.frame(read.csv("y_true.csv"))
    y_pred = data.frame(read.csv("y_predict.csv"))
    
    # compute measures multilabel
    y_true2 = data.frame(sapply(y_true, function(x)
      as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2 ,
                                  labelIndices = seq(1, ncol(y_true2)),
                                  name = "y_true2")
    y_pred2 = sapply(y_pred, function(x)
      as.numeric(as.character(x)))
    
    #cat("\n\t\tSave Confusion Matrix")
    setwd(Folder.Tested.Split)
    salva3 = paste("Conf-Mat-Fold-", f, ".txt", sep = "")
    sink(file = salva3, type = "output")
    confmat = multilabel_confusion_matrix(y_true3, y_pred2)
    print(confmat)
    sink()
    
    # creating a data frame
    confMatPart = multilabel_evaluate(confmat)
    confMatPart = data.frame(confMatPart)
    names(confMatPart) = paste("Fold-", f, sep = "")
    namae = paste("Split-", f, "-Evaluated.csv", sep = "")
    write.csv(confMatPart, namae)
    
    # delete files
    setwd(Folder.Tested.Split)
    unlink("y_true.csv", recursive = TRUE)
    unlink("y_predict.csv", recursive = TRUE)
    
    gc()
  } # end folds
  
  gc()
  cat("\n############################################################")
  cat("\n# CLUS SILHOUETTE: End evaluate.clus.silho                 #")
  cat("\n############################################################")
  cat("\n\n\n\n")
}




######################################################################
#
######################################################################
gather.eval.clus.silho <- function(parameters) {
  
  
  ##########################################################################
  apagar = c(0)
  avaliado.final = data.frame(apagar)
  nomes = c("")
  
  # from fold = 1 to number_folders
  f = 1
  while(f<=parameters$Config$Number.Folds){
    
    cat("\n#======================================================")
    cat("\n# Fold: ", f)
    cat("\n#======================================================\n")
    
    # vector with names
    measures = c("accuracy","average-precision","clp","coverage","F1",
                 "hamming-loss","macro-AUC", "macro-F1","macro-precision",
                 "macro-recall","margin-loss","micro-AUC","micro-F1",
                 "micro-precision","micro-recall","mlp","one-error",
                 "precision","ranking-loss", "recall","subset-accuracy","wlp")
    
    ##########################################################################
    # "/dev/shm/ej3-GpositiveGO/Tested/Split-1"
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep="")
    
    
    ######################################################################
    setwd(Folder.Tested.Split)
    str = paste("Split-", f, "-Evaluated.csv", sep="")
    avaliado = data.frame(read.csv(str))
    avaliado.final= cbind(avaliado.final, avaliado[,2])
    nomes[f] = paste("Fold-", f, sep="")
    
    f = f + 1
    gc()
    
  } # end folds
  
  
  avaliado.final = avaliado.final[,-1]
  names(avaliado.final) = nomes
  avaliado.final = cbind(measures, avaliado.final)
  setwd(Folder.Tested.Split)
  write.csv(avaliado, paste("Evaluated-Fold-", f, ".csv", sep=""),
            row.names = FALSE)
  
  # calculando a média dos 10 folds para cada medida
  media = data.frame(apply(avaliado.final[,-1], 1, mean))
  media = cbind(measures, media)
  names(media) = c("Measures", "Mean10Folds")
  
  setwd(parameters$Folders$folderTested)
  write.csv(media, "Mean10Folds.csv", row.names = FALSE)
  
  mediana = data.frame(apply(avaliado.final[,-1], 1, median))
  mediana = cbind(measures, mediana)
  names(mediana) = c("Measures", "Median10Folds")
  
  setwd(parameters$Folders$folderTested)
  write.csv(mediana, "Median10Folds.csv", row.names = FALSE)
  
  dp = data.frame(apply(avaliado.final[,-1], 1, sd))
  dp = cbind(measures, dp)
  names(dp) = c("Measures", "SD10Folds")
  
  setwd(parameters$Folders$folderTested)
  write.csv(dp, "desvio-padrão-10-folds.csv", row.names = FALSE)
  
  gc()
  cat("\n#############################################################")
  cat("\n# CLUS SILHOUETTE: End gather.eval.clus.silho               #")
  cat("\n#############################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
