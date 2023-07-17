##############################################################################
# TEST BEST PARTITION MICRO-F1 CLUS                                          #
# Copyright (C) 2021                                                         #
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
# Ferrandin | Federal University of Sao Carlos                               #
# (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos | Computer Department  #
# (DC: https://site.dc.ufscar.br/) | Program of Post Graduation in Computer  #
# Science (PPG-CC: http://ppgcc.dc.ufscar.br/) | Bioinformatics and Machine  #
# Learning Group (BIOMAL: http://www.biomal.ufscar.br/)                      #
#                                                                            #
##############################################################################


###########################################################################
#
###########################################################################
FolderRoot = "/Test-Best-Hybrid-Partition"
FolderScripts = "/Test-Best-Hybrid-Partition/R"


##################################################################################################
# FUNCTION MOUNT HYBRID PARTITION                                                                #
#   Objective                                                                                    #
#   Parameters                                                                                   #
#   Return                                                                                       #
#     performance classifier                                                                     #
##################################################################################################
build.clus.mif1 <- function(parameters){  
  
  f = 1
  bthpkParalel <- foreach(f = 1:number_folds) %dopar%{

    cat("\nFold: ", f)

    FolderRoot = "/Test-Best-Hybrid-Partition"
    FolderScripts = "/Test-Best-Hybrid-Partition/R"

    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")

    ########################################################################################
    cat("\nSelect Best Partition for", f, "\n")
    FolderBP = paste(diretorios$folderPartitions, "/", dataset_name, sep="")
    setwd(FolderBP)
    # flags-best-macroF1-partitions.csv
    bestPart = data.frame(read.csv(paste(dataset_name, "-best-microF1-partitions.csv", sep="")))
    bestPart2 = bestPart[f,]
    num.part = as.numeric(bestPart2$part)

    ########################################################################################
    cat("\nGet the number of groups for this partition in this fold \n")
    FolderBPG = paste(FolderBP, "/Split-", f, sep="")
    setwd(FolderBPG)
    num.groups = data.frame(read.csv(paste("fold-",f, "-groups-per-partition.csv", sep="")))
    num.groups2 = filter(num.groups, partition == num.part)
    num.groups3 = as.numeric(num.groups2$num.groups)

    ########################################################################################
    FolderBPGP = paste(FolderBPG, "/Partition-", num.part, sep="")

    ######################################################################################################################
    #cat("\nSpecific Group: ", k, "\n")
    setwd(FolderBPGP)
    particao = data.frame(read.csv(paste("partition-", num.part,".csv", sep="")))

    ########################################################################################
    cat("\nOpen Train file ", f, "\n")
    setwd(diretorios$folderCVTR)
    nome_arq_tr = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
    arquivo_tr = data.frame(read.csv(nome_arq_tr))

    ########################################################################################
    cat("\nOpen Validation file ", f, "\n")
    setwd(diretorios$folderCVVL)
    nome_arq_vl = paste(dataset_name, "-Split-Vl-", f, ".csv", sep="")
    arquivo_vl = data.frame(read.csv(nome_arq_vl))

    ########################################################################################
    cat("\nOpen Test file ", f, "\n")
    setwd(diretorios$folderCVTS)
    nome_arq_ts = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
    arquivo_ts = data.frame(read.csv(nome_arq_ts))

    ########################################################################################
    FolderSplitTest = paste(diretorios$folderResultsDataset, "/Split-", f, sep="")
    if(dir.exists(FolderSplitTest)==FALSE){
      dir.create(FolderSplitTest)
    }

    ########################################################################################
    #cat("\nMount Groups of Labels for Fold ", f, "\n")
    k = 1
    while(k<=num.groups3){

      cat("\nPartition: ", k)

      ######################################################################################################################
      cat("\ncreating folder")
      FolderTestGroup = paste(FolderSplitTest, "/Group-", k, sep="")
      if(dir.exists(FolderTestGroup)== FALSE){
        dir.create(FolderTestGroup)
      }

      ######################################################################################################################
      cat("\nSpecific Group: ", k, "\n")
      setwd(FolderBPGP)
      grupoEspecifico = filter(particao, group == k)

      ######################################################################################################################
      cat("\nTRAIN: Mount Group ", k, "\n")
      atributos_tr = arquivo_tr[ds$AttStart:ds$AttEnd]
      n_a = ncol(atributos_tr)
      classes_tr = select(arquivo_tr, grupoEspecifico$label)
      n_c = ncol(classes_tr)
      grupo_tr = cbind(atributos_tr, classes_tr)
      fim_tr = ncol(grupo_tr)
      ######################################################################################################################
      cat("\nVALIDATION: Mount Group ", k, "\n")
      atributos_vl = arquivo_vl[ds$AttStart:ds$AttEnd]
      n_a = ncol(atributos_vl)
      classes_vl = select(arquivo_vl, grupoEspecifico$label)
      n_c = ncol(classes_vl)
      grupo_vl = cbind(atributos_vl, classes_vl)
      fim_vl = ncol(grupo_vl)
      ncol(grupo_vl)
      nrow(grupo_vl)

      grupo_tr2 = rbind(grupo_tr, grupo_vl)
      ncol(grupo_tr2)
      nrow(grupo_tr2)

      ######################################################################################################################
      cat("\n\tTRAIN: Save Group", k, "\n")
      setwd(FolderTestGroup)
      nome_tr = paste(dataset_name, "-split-tr-", f, "-group-", k, ".csv", sep="")
      write.csv(grupo_tr2, nome_tr, row.names = FALSE)

      ######################################################################################################################
      cat("\n\tINICIO FIM TARGETS: ", k, "\n")
      inicio = ds$LabelStart
      fim = fim_tr
      ifr = data.frame(inicio, fim)
      write.csv(ifr, "inicioFimRotulos.csv", row.names = FALSE)

      ######################################################################################################################
      cat("\n\tTRAIN: Convert Train CSV to ARFF ", k , "\n")
      nome_arquivo_2 = paste(dataset_name, "-split-tr-", f, "-group-", k, ".arff", sep="")
      arg1Tr = nome_tr
      arg2Tr = nome_arquivo_2
      arg3Tr = paste(inicio, "-", fim, sep="")
      str = paste("java -jar ", diretorios$folderUtils, "/R_csv_2_arff.jar ", arg1Tr, " ", arg2Tr, " ", arg3Tr, sep="")
      print(system(str))

      ######################################################################################################################
      cat("\n\tTRAIN: Verify and correct {0} and {1} ", k , "\n")
      arquivo = paste(FolderTestGroup, "/", arg2Tr, sep="")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
      print(system(str0))

      ######################################################################################################################
      cat("\n\tTEST: Mount Group: ", k, "\n")
      atributos_ts = arquivo_ts[ds$AttStart:ds$AttEnd]
      classes_ts = select(arquivo_ts, grupoEspecifico$label)
      grupo_ts = cbind(atributos_ts, classes_ts)
      fim_ts = ncol(grupo_ts)
      cat("\n\tTest Group Mounted: ", k, "\n")

      ######################################################################################################################
      cat("\n\tTEST: Save Group ", k, "\n")
      setwd(FolderTestGroup)
      nome_ts = paste(dataset_name, "-split-ts-", f, "-group-", k, ".csv", sep="")
      write.csv(grupo_ts, nome_ts, row.names = FALSE)

      ######################################################################################################################
      cat("\n\tTEST: Convert CSV to ARFF ", k , "\n")
      nome_arquivo_3 = paste(dataset_name, "-split-ts-", f,"-group-", k, ".arff", sep="")
      arg1Ts = nome_ts
      arg2Ts = nome_arquivo_3
      arg3Ts = paste(inicio, "-", fim, sep="")
      str = paste("java -jar ", diretorios$folderUtils, "/R_csv_2_arff.jar ", arg1Ts, " ", arg2Ts, " ", arg3Ts, sep="")
      print(system(str))

      ######################################################################################################################
      cat("\n\tTEST: Verify and correct {0} and {1} ", k , "\n")
      arquivo = paste(FolderTestGroup, "/", arg2Ts, sep="")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arquivo, sep="")
      cat("\n")
      print(system(str0))
      cat("\n")

      ######################################################################################################################
      cat("\nCreating .s file for clus")
      if(inicio == fim){

        nome_config = paste(dataset_name, "-split-", f, "-group-", k, ".s", sep="")
        sink(nome_config, type = "output")

        cat("[General]")
        cat("\nCompatibility = MLJ08")

        cat("\n\n[Data]")
        cat(paste("\nFile = ", nome_arquivo_2, sep=""))
        cat(paste("\nTestSet = ", nome_arquivo_3, sep=""))

        cat("\n\n[Attributes]")
        cat("\nReduceMemoryNominalAttrs = yes")

        cat("\n\n[Attributes]")
        cat(paste("\nTarget = ", fim, sep=""))
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

        ######################################################################################################################

        cat("\nExecute CLUS: ", k , "\n")
        nome_config2 = paste(FolderTestGroup, "/", nome_config, sep="")
        str = paste("java -jar ", diretorios$folderUtils, "/Clus.jar ", nome_config2, sep="")
        print(system(str))

      } else {

        nome_config = paste(dataset_name, "-split-", f, "-group-", k, ".s", sep="")
        sink(nome_config, type = "output")

        cat("[General]")
        cat("\nCompatibility = MLJ08")

        cat("\n\n[Data]")
        cat(paste("\nFile = ", nome_arquivo_2, sep=""))
        cat(paste("\nTestSet = ", nome_arquivo_3, sep=""))

        cat("\n\n[Attributes]")
        cat("\nReduceMemoryNominalAttrs = yes")

        cat("\n\n[Attributes]")
        cat(paste("\nTarget = ", inicio, "-", fim, sep=""))
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

        cat("\nExecute CLUS: ", k , "\n")
        nome_config2 = paste(FolderTestGroup, "/", nome_config, sep="")
        str = paste("java -jar ", diretorios$folderUtils, "/Clus.jar ", nome_config2, sep="")
        print(system(str))

      }

      ####################################################################################
      cat("\n\nOpen predictions")
      nomeDoArquivo = paste(FolderTestGroup, "/", dataset_name, "-split-", f,"-group-", k, ".test.pred.arff", sep="")
      predicoes = data.frame(foreign::read.arff(nomeDoArquivo))

      ####################################################################################
      cat("\nS\nPLIT PREDICTIS")
      if(inicio == fim){
        cat("\n\nOnly one label in this group")

        ####################################################################################
        cat("\n\nSave Y_true")
        setwd(FolderTestGroup)
        classes = data.frame(predicoes[,1])
        names(classes) = colnames(predicoes)[1]
        write.csv(classes, "y_true.csv", row.names = FALSE)

        ####################################################################################
        cat("\n\nSave Y_true")
        rot = paste("Pruned.p.", colnames(predicoes)[1], sep="")
        pred = data.frame(predicoes[,rot])
        names(pred) = colnames(predicoes)[1]
        setwd(FolderTestGroup)
        write.csv(pred, "y_predict.csv", row.names = FALSE)

        ####################################################################################
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        gc()

      } else {

        ####################################################################################
        library("foreign")

        ####################################################################################
        cat("\n\nMore than one label in this group")
        comeco = 1+(fim - inicio)


        ####################################################################################
        cat("\n\nSave Y_true")
        classes = data.frame(predicoes[,1:comeco])
        setwd(FolderTestGroup)
        write.csv(classes, "y_true.csv", row.names = FALSE)


        ####################################################################################
        cat("\n\nSave Y_true")
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        nomeColuna = c()
        t = 1
        while(t <= n_r){
          nomeColuna[t] = paste("Pruned.p.", rotulos[t], sep="")
          t = t + 1
          gc()
        }
        pred = data.frame(predicoes[nomeColuna])
        names(pred) = rotulos
        setwd(FolderTestGroup)
        write.csv(pred, "y_predict.csv", row.names = FALSE)
        gc()
      } # FIM DO ELSE

      # deleting files
      um = paste(dataset_name, "-split-", f, "-group-", k, ".model", sep="")
      dois = paste(dataset_name, "-split-", f, "-group-", k, ".s", sep="")
      tres = paste(dataset_name, "-split-tr-", f, "-group-", k, ".arff", sep="")
      quatro = paste(dataset_name, "-split-ts-", f, "-group-", k, ".arff", sep="")
      cinco = paste(dataset_name, "-split-tr-", f, "-group-", k, ".csv", sep="")
      seis = paste(dataset_name, "-split-ts-", f, "-group-", k, ".csv", sep="")
      #sete = paste(dataset_name, "-split-", f, "-group-", k, ".out", sep="")

      setwd(FolderTestGroup)
      unlink(um, recursive = TRUE)
      unlink(dois, recursive = TRUE)
      unlink(tres, recursive = TRUE)
      unlink(quatro, recursive = TRUE)
      unlink(cinco, recursive = TRUE)
      unlink(seis, recursive = TRUE)
      #unlink(sete, recursive = TRUE)

      k = k + 1
      gc()
    } # end grupos

        gc()
  } # ending folds

  gc()
  cat("\n##################################################################################################")
  cat("\n# BUILD AND TEST HYBRID PARTITIONS: END                                                          #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION SPLITS PREDCTIONS HYBRIDS                                                             #
#   Objective                                                                                    #
#      From the file "test.pred.arff", separates the real labels and the predicted labels to     #
#      generate the confusion matrix to evaluate the partition.                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       DsFolds: folder dataset                                                                  #
#       FolderHybPart: path of hybrid partition validation                                       #
#       FolderHybrid: path of hybrid partition test                                              #
#   Return                                                                                       #
#       true labels and predict labels                                                           #
##################################################################################################
gather.clus.mif1 <- function(parameters){  

  # start build partitions
  # do fold 1 até o último fold
  f = 1
  gatherR <- foreach(f = 1:number_folds) %dopar%{
    
    cat("\nFold: ", f)
    
    FolderRoot = "/Test-Best-Hybrid-Partition"
    FolderScripts = "/Test-Best-Hybrid-Partition/R"
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")

    ########################################################################################
    apagar = c(0)
    y_true = data.frame(apagar)
    y_pred = data.frame(apagar)

    ########################################################################################
    FolderSplitTest = paste(diretorios$folderResultsDataset, "/Split-", f, sep="")

    ########################################################################################
    cat("\nSelect Best Partition for", f, "\n")
    FolderBP = paste(diretorios$folderPartitions, "/", dataset_name, sep="")
    setwd(FolderBP)
    bestPart = data.frame(read.csv(paste(dataset_name, "-best-microF1-partitions.csv", sep="")))
    bestPart2 = bestPart[f,]
    num.part = as.numeric(bestPart2$part)

    ########################################################################################
    cat("\nGet the number of groups for this partition in this fold \n")
    FolderBPG = paste(FolderBP, "/Split-", f, sep="")
    setwd(FolderBPG)
    num.groups = data.frame(read.csv(paste("fold-",f, "-groups-per-partition.csv", sep="")))
    num.groups2 = filter(num.groups, partition == num.part)
    num.groups3 = as.numeric(num.groups2$num.groups)

    ########################################################################################
    FolderBPGP = paste(FolderBPG, "/Partition-", num.part, sep="")

    g = 1
    while(g<=num.groups3){

      cat("\n\nGroup: ", g)

      FolderGroup = paste(FolderSplitTest, "/Group-", g, sep="")

      cat("\n\nGather y_true ", g)
      setwd(FolderGroup)
      y_true_gr = data.frame(read.csv("y_true.csv"))
      y_true = cbind(y_true, y_true_gr)

      setwd(FolderGroup)
      cat("\n\nGather y_predict ", g)
      y_pred_gr = data.frame(read.csv("y_predict.csv"))
      y_pred = cbind(y_pred, y_pred_gr)

      cat("\n\nDeleting files")
      unlink("y_true.csv", recursive = TRUE)
      unlink("y_predict.csv", recursive = TRUE)
      unlink("inicioFimRotulos.csv", recursive = TRUE)

      g = g + 1
      gc()
    }

    cat("\n\nSave files ", g, "\n")
    setwd(FolderSplitTest)
    y_pred = y_pred[,-1]
    y_true = y_true[,-1]
    write.csv(y_pred, "y_predict.csv", row.names = FALSE)
    write.csv(y_true, "y_true.csv", row.names = FALSE)

    f = f + 1
    gc()
  } # fim do foreach

  gc()
  cat("\n##################################################################################################")
  cat("\n# Gather Predicts: END                                                                           #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")

} # fim da função


##################################################################################################
# FUNCTION EVALUATION HYBRID PARTITIONS                                                          #
#   Objective                                                                                    #
#      Evaluates the hybrid partitions                                                           #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
#       Assessment measures for each hybrid partition                                            #
##################################################################################################
evaluate.clus.mif1 <- function(parameters){  

  # from fold = 1 to number_folder
  f = 1
  avalParal <- foreach(f = 1:number_folds) %dopar%{
    
    cat("\nFold: ", f)
    
    FolderRoot = "/Test-Best-Hybrid-Partition"
    FolderScripts = "/Test-Best-Hybrid-Partition/R"
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")

    # data frame
    apagar = c(0)
    confMatPartitions = data.frame(apagar)
    partitions = c()

    # specifyin folder for the fold
    FolderSplitTest = paste(diretorios$folderResultsDataset, "/Split-", f, sep="")

    # get the true and predict lables
    setwd(FolderSplitTest)
    y_true = data.frame(read.csv("y_true.csv"))
    y_pred = data.frame(read.csv("y_predict.csv"))

    # compute measures multilabel
    y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
    y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))

    #cat("\n\t\tSave Confusion Matrix")
    setwd(FolderSplitTest)
    salva3 = paste("Conf-Mat-Fold-", f, ".txt", sep="")
    sink(file=salva3, type="output")
    confmat = multilabel_confusion_matrix(y_true3, y_pred2)
    print(confmat)
    sink()

    # creating a data frame
    confMatPart = multilabel_evaluate(confmat)
    confMatPart = data.frame(confMatPart)
    names(confMatPart) = paste("Fold-", f, sep="")
    namae = paste("Split-", f,"-Evaluated.csv", sep="")
    write.csv(confMatPart, namae)

    # delete files
    setwd(FolderSplitTest)
    unlink("y_true.csv", recursive = TRUE)
    unlink("y_predict.csv", recursive = TRUE)

    gc()
  } # end folds

  gc()
  cat("\n##################################################################################################")
  cat("\n# Evaluation Folds: END                                                                          #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}




##################################################################################################
# FUNCTION GATHER EVALUATIONS                                                                    #
#   Objective                                                                                    #
#       Gather metrics for all folds                                                             #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderHybrid: path of hybrid partition results                                           #
#   Return                                                                                       #
#       Assessment measures for all folds                                                        #
##################################################################################################
gather.eval.clus.mif1 <- function(parameters){  


  # vector with names
  measures = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
               "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
               "recall","subset-accuracy","wlp")

  # data frame
  apagar = c(0)
  avaliado4 = data.frame(apagar)
  folds = c(0)
  nomesFolds = c(0)

  # from fold = 1 to number_folders
  f = 1
  while(f<=number_folds){

    cat("\nFold: ", f)

    # specifying folder for the fold
    FolderSplitTest = paste(diretorios$folderResultsDataset, "/Split-", f, sep="")
    setwd(FolderSplitTest)
    str = paste("Split-", f, "-Evaluated.csv", sep="")
    avaliado = data.frame(read.csv(str))
    names(avaliado)[1] = "medidas"
    avaliado2 = data.frame(avaliado[order(avaliado$medidas, decreasing = FALSE),])
    avaliado3 = data.frame(avaliado2[,-1])
    avaliado4 = cbind(avaliado4, avaliado3)
    nomesFolds[f] = paste("Fold-", f, sep="")

    f = f + 1
    gc()

  } # end folds


  gc()
  cat("\n##################################################################################################")
  cat("\n# Evaluated Partition: END                                                                       #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
