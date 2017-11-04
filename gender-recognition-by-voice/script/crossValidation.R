# importa bibilioteca
library(e1071)
library(xlsx)

#---------------------------------------------------------------------------
# cross.partition -> Divide o dataset em k subconjuntos, com no maximo 1 
# elemento de diferan?a, e distribui??o proporcional das classes
#
# Parametros: data -> dataset binario e balanceado
#             k -> Numero de particoes que serao geradas
#
# Retorno: lista contendo os subconjuntos do dataset original
#--------------------------------------------------------------------------
cross.partition <- function( data , k=100 ){

  data.class1 <- subset( data , data$label == 'male' )    #Subset contendo apenas as instancias da classe 1
  data.class2 <- subset( data , data$label == 'female' )  #Subset contendo apenas as instancias da classe 2

  num.elem <- nrow(data) %/% k    # numero de instancias por conjunto (parte inteira)
  num.class <- num.elem %/% 2     # numero de instancias de cada classe
  
  partitions <- list()  # lista contendo os k conjuntos de instancias
   
  for(i in 1:k){

    instances.class1 <- sample( 1:nrow(data.class1) , num.class , replace=FALSE )     # pega num.class instancias da claase 1 aleatoriamente
    instances.class2 <- sample( 1:nrow(data.class2) , num.class , replace=FALSE )     # pega num.class instancias da claase 2 aleatoriamente

    partitions[[i]] <- rbind( data.class1[instances.class1,] , data.class2[instances.class2,] )   # cria particao contendo as instancias sorteadas, e adiciona ? lista
    
    data.class1 <- data.class1[-instances.class1,]    # remove particoes j? seleciondas do dataframe da classe 1 
    data.class2 <- data.class2[-instances.class2,]    # remove particoes j? seleciondas do dataframe da classe 2 
  }
  
  i <- 1
  
  # distribui??o de instancias restantes da classe 1
  while( nrow(data.class1) > 0 ){
    
    partitions[[i]] <- rbind( partitions[[i]] , data.class1[1,])
    data.class1 <- data.class1[-1,]
    i <- i+1
      
  }
  
  # distribui??o de instancias restantes da classe 2
  while( nrow(data.class2) > 0 && i <= length(partitions) ){
    
    partitions[[i]] <- rbind( partitions[[i]] , data.class2[1,])
    data.class2 <- data.class2[-1,]
    i <- i+1
      
  }
  
  if( nrow(data.class2) > 0 ){
    i <- 1
    
    while( nrow(data.class2) > 0 ){
      
      partitions[[i]] <- rbind( partitions[[i]] , data.class2[1,])
      data.class2 <- data.class2[-1,]
      i <- i+1
      
    }
  }
<<<<<<< HEAD
  
=======
  browser()
>>>>>>> 1ffbe95d2d81caecf1f06db3b00c6df26ce57d3b
  return(partitions)
  
}

#---------------------------------------------------------------------------
# training.partition-> cria particao de trainamento
#
# Parametros: list.part -> lista de datasets
#             part.test -> indice da particao de teste
#--------------------------------------------------------------------------
training.partition<-function( list.part , part.test ){
  
  data.training <- data.frame()
  
  for(i in 1:length(list.part)){
    if(i != part.test){
      data.training <- rbind(list.part[[i]],data.training)
    }
  }
  return(data.training)
}

#---------------------------------------------------------------------------
# partition.print -> Imprimi lista de datasets
#
# Parametros: list.part -> lista de datasets
#--------------------------------------------------------------------------
partition.print <- function( list.data ){
  
  for(i in 1:length(list.data)){
    print(list.data[[i]])
  }
  
}

#---------------------------------------------------------------------------
# cross.valid.svm  -> executa cross valida??o para SVM
#
# Parametros: data    -> data frame
#             k       -> numero de particoes
#             cKernel -> kernel utilizado pela svm
#             nDegree -> parametro degree da svm
#             nGamma  -> parametro gamma da svm
#             nCoef0  -> parametro coef0 da svm
#             nCost   -> parametro cost da svm  
#--------------------------------------------------------------------------
cross.valid.svm <- function(data, k=100, cKernel, nDegree, nGamma, nCoef0, nCost){
  particoes <- cross.partition(data,k)   # nLista contendo as particoes para a validacao cruzada
  measures <- list()                     # Lista contendo os resultados (medidas) de cada execucao
  
  for(i in 1:length(particoes)){
    training <- training.partition(particoes,i)   # amostra de treinamento
    test <- particoes[[i]]                        # amostra de testes
    svm.model <- svm( label~. , data=training , kernel=cKernel , cost=nCost , gamma=nGamma , degree=nDegree , coef0=nCoef0 , type="C" )  # Executa treinamento
    svm.pred <- predict(svm.model, test[,-6], type = "class")   #Executa teste
    measures[[i]] <- measures.calc( svm.pred , test)   # calcula medidas de acuracia
  }
  
  mean <- measures.mean(measures)   # calcula media dos resultados
  
  # cria linha do dataset de resultados
  result <- data.frame(kernel=cKernel,degree=nDegree,gamma=nGamma,coef0=nCoef0,cost=nCost,erro=mean[[1]],sensibilidade=mean[[2]],precisao=mean[[3]])
  
  return(result)
}

#---------------------------------------------------------------------------
# measures.calc -> Calcula medidas de acuracia (erro, precisao e sensibilidade)
#
# Parametros: predicao da svm, data frame de teste
#--------------------------------------------------------------------------
measures.calc <- function(predict,teste){
  pos <- nrow(teste[teste$label=="male",])     # elementos positivos (class male)
  neg <- nrow(teste[teste$label=="female",])   # elementos negativos (class female)
  tot <- pos + neg # total de elementos
  tp  <- 0         # true positive
  tn  <- 0         # true negative
  fp  <- 0         # false positive
  fn  <- 0         # false negative

  predict <- as.data.frame(predict)
  
  for(i in 1:tot){
    if(teste[i,6]=='male' && predict[i,1]=='male') tp <- tp+1 
    else if (teste[i,6]=='male' && predict[i,1]=='female')  fn <- fn+1
    else if (teste[i,6]=='female' && predict[i,1]=='female') tn <- tn+1
    else if (teste[i,6]=='female' && predict[i,1]=='male') fp <- fp+1
  }
  
  err <- (fp + fn) / tot  # erro
  sens <- tp / pos        # sensibilidade
  
  if(tp > 0) prec <- tp / (tp + fp)  # precisao
  else prec <- 0
  
  measures <- c(err,sens,prec)
  
  return(measures)
}

#---------------------------------------------------------------------------
# measures.mean -> Calcula a media das medidas (erro, precisao e sensibilidade)
#
# Parametros: lista de medidas
#--------------------------------------------------------------------------
measures.mean <- function(measures){
  
  err <- 0   # erro
  sens <- 0  # precisao
  prec <- 0  # sensibilidade
  
  for(i in 1:length(measures)){
    err <- err + measures[[i]][1]
    sens <- sens + measures[[i]][2]
    prec <- prec + measures[[i]][3]
  }
  
  err <- err / length(measures)
  sens <- sens / length(measures)
  prec <- prec / length(measures)
  
  mean <- c(err,sens,prec)
  
  return(mean)
}

#---------------------------------------------------------------------------
# main -> executa diversas chamadas a funcao cross.valid.svm, variando 
#         a parametriza??o da svm.
#
# Parametros: data -> data.frame
#             k    -> numero de particoes utilizadas na validacao cruzada
#--------------------------------------------------------------------------
main <- function(data, k=100){
  
  data.result <- data.frame()
  
  aC0 <- c(0.1,0.5,1,5,10)  # coef0
  aC1 <- c(0.1,0.5,1,5,10)  # cost
  aD  <- c(0.1,0.5,1,5,10)  # degree
  aG  <- c(0.1,0.5,1,5,10)  # gamma
 
  # Kernel linear
  for(c1 in 1:length(aC1)){
    data.aux <- cross.valid.svm(data=data,k=k,cKernel='linear',nDegree=0,nGamma=0,nCoef0=0,nCost=aC1[c1])
    data.result <- rbind(data.result,data.aux)
    print(sprintf("Kernel = linear, custo = %i.",c1))
  }
  
  # Kernel polynomial
  for(d in 1:length(aD)){
    for(g in 1:length(aG)){
      for(c0 in 1:length(aC0)){
        for(c1 in 1:length(aC1)){
          data.aux <- cross.valid.svm(data=data,k=k,cKernel='polynomial',nDegree=aD[d],nGamma=aG[g],nCoef0=aC0[c0],nCost=aC1[c1])
          data.result <- rbind(data.result,data.aux)
          print(sprintf("Kernel = polynomial, custo = %i, degree = %i, gamma = %i, coef0 = %i.", c1, d, g, c0))
        }
      }
    }
  }
  
  # Kernel radial
  for(g in 1:length(aG)){
    for(c1 in 1:length(aC1)){
      data.aux <- cross.valid.svm(data=data,k=k,cKernel='radial',nDegree=0,nGamma=aG[g],nCoef0=0,nCost=aC1[c1])
      data.result <- rbind(data.result,data.aux)
      print(sprintf("Kernel = radial, custo = %i, gamma = %i.", c1, g))
    }
  }
  
  # Kernel sigmoid
  for(g in 1:length(aG)){
    for(c0 in 1:length(aC0)){
      for(c1 in 1:length(aC1)){
        data.aux <- cross.valid.svm(data=data,k=k,cKernel='sigmoid',nDegree=0,nGamma=aG[g],nCoef0=aC0[c0],nCost=aC1[c1])
        data.result <- rbind(data.result,data.aux)
        print(sprintf("Kernel = sigmoid, custo = %i, gamma = %i, coef0 = %i.", c1, g, c0))
      }
    }
  }

<<<<<<< HEAD
  write.xlsx(data.result,"C:/projects/tests/gender-recognition-by-voice/reports/svm_pca_k10.xlsx")
=======
  write.xlsx(data.result,"C:/Projects/tests/gender-recognition-by-voice/reports/pca_svm.xlsx")
>>>>>>> 1ffbe95d2d81caecf1f06db3b00c6df26ce57d3b
  
  return(data.result)
  
}

