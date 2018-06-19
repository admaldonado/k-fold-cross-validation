# Creado por Ana D. Maldonado
# 18 abril 2018
#############################################################################################
####### VALIDACION CRUZADA EN K HOJAS CON METRICA PARA EVALUAR CLASIFICADORES ###############
# ARGUMENTS
# df            el dataframe (solo variables discretas)
# dag           el grafo 
# target.var    string. nombre de la variable que se quiere clasificar. 
# k             el numero de hojas para el k-fold cross validation
# method        el metodo para estimar los parametros de la red
# seed          semilla para reproducir los experimentos (particion aleatoria de hojas)
# 
# Utiliza el paquete bnlearn para aprender los modelos.

require(bnlearn)

multiclassCrossValidation <- function(df, dag, target.var, k = 10, method = c("mle", "bayes"), seed = NULL){
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  # Barajar los datos aleatoriamente
  rndData<-df[sample(nrow(df)),]
  
  # Crear k hojas de igual tamano
  folds <- cut(seq(1,nrow(rndData)), breaks = k, labels = FALSE)
  
  # Crear vectores para almacenar resultados
  ns <- length(levels(df[,target.var]))  # numero de estados
  
  accuracy <- c()
  av.precision <- rep(0,ns)
  av.recall <- rep(0,ns)
  av.fscore <- rep(0,ns)
  BS <- c()
  # Perform 10 fold cross validation
  for(i in 1:k){
    # Dividir los datos por hoja utilizando la funcion which() 
    index <- which(folds==i,arr.ind=TRUE)
    testData <- rndData[index, ]
    trainData <- rndData[-index, ]
    
    # Usar los datos train y test para aprender (train) y predecir (test) los modelos 
    
    # APRENDER los parametros de la red
    bn.d = bn.fit(dag, trainData, method = method)    
    
    # PREDECIR
    pred = predict(object = bn.d, node = target.var, data = testData, method = "bayes-lw", prob = T) 
    
    
    # VALIDAR los modelos (como de bien predice)
    # Construye una matriz de confusion (cm) para evaluar el clasificador (filas: observacion; columnas: prediccion)
    # PARA LA CLASE C1:           # PARA LA CLASE C2:           # PARA LA CLASE C3:
    #       PREDICCIONES                PREDICCIONES                  PREDICCIONES
    #       C1 C2 C3                    C1 C2 C3                      C1 C2 C3
    # O C1  TP FN FN              O C1  TN FP                   O C1  TN    FP
    # B C2  FP TN                 B C2  FN TP FN                B C2     TN FP
    # S C3  FP    TN              S C3     FP TN                S C3  FN FN TP
    
    # TP: true positive; TN: true negative; FP: false positive; FN: false negative
    
    cat("\n",paste("=========== ITERATION ", i," ============","\n","======================================","\n"))
    #print(paste("======================================"))
    
    cm <- table(testData[, target.var], pred); cm    
    
    print(cm)
    
    # Calcula las metricas para evaluar el clasificador
    accuracy[i] <- sum(diag(cm))/sum(cm); accuracy
    cat(paste("\n","Accuracy = ", accuracy[i]),"\n")
    
    cat("\n","========== Precision ========","\n")
    precision <- diag(cm) / colSums(cm); precision # TP / (TP+FP)
    #av.precision <- av.precision + precision
    av.precision <- rowSums(cbind(av.precision, precision), na.rm = T)
    print(precision)
    
    cat("\n","========== Recall ========","\n")
    recall <- (diag(cm) / rowSums(cm)); recall # TP / (TP+FN)
    #av.recall <- av.recall + recall
    av.recall <- rowSums(cbind(av.recall, recall), na.rm = T)
    print(recall)
    
    cat("\n","========= F score ========","\n")
    fscore <- (2*precision*recall)/(precision+recall); fscore 
    #av.fscore <- av.fscore + fscore
    av.fscore <- rowSums(cbind(av.fscore, fscore), na.rm = T)
    print(fscore)
    
    # Calcular Brier score. Primero, prepara el dataframe (comp.BS)
    
    cat("\n","========= Brier Score ========","\n")
    comp.BS <- as.data.frame(t(attr(pred, "prob")))
    comp.BS$OBS <- testData[,target.var]
    
    BS[i] <- brier.score(comp.BS); BS
    cat(paste("BS = ", BS[i]),"\n")
  }
  results <- list(accuracy = mean(accuracy), precision = av.precision/k, recall = av.recall/k, fcore = av.fscore/k, BS = mean(BS))
  return(results)
}









