
cv.sizes <- function(n, k=10) {
  res <- n %/% k #result of division
  rem <- n %%  k #rest of division(mod)
  x <- 0
  sizes = c()
  
  for (i in 1:k) {
    if (rem > 0) {
      x <- 1 
      rem <- rem - 1
    }
    else {
      x <- 0
    }
    sizes = append(sizes, res + x)
  }
  return(sizes)
}


cv.partition <- function(formula, data, k=10){
  
  # processes formula: (1) process the call
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "k=10"), names(mf), 0)
  mf <- mf[c(1, m)]
  
  # processes formula: (2) set up the model frame
  f <- formula(formula)
  mf[[1]] <- as.name("model.frame")
  mf$formula <- f
  mf <- eval(mf,parent.frame())
  
  # processes formula: (3) extract class and variables from the model
  y <- model.response(mf)
  x <- model.matrix(f, data = mf, rhs = 1)
  z <- model.matrix(f, data = mf, rhs = 2)
  
  folds = list()
  n = length(y)
  predictors <- as.data.frame(table(y))
  sizes = cv.sizes(n,k=k)
  
  for (c in 1:length(predictors)){
    # proporcao por label
    proportion <- predictors$Freq[c]/n

    # take a number of sample
    v <- as.numeric(rownames(x[y==predictors[c,1],]))

    for (i in 1:k){
      # Solve division uneven of predictors
      limit <- 0
      if ((i %% 2)==0) {
        limit <- floor(sizes[i]*proportion)
      }
      else{
        limit <- ceiling(sizes[i]*proportion)
      }

      # Get a random sample of given size
      s <- sample(length(v), limit)

      # Append random sample to list of folds
      if (c==1) {
        folds[[i]] = v[s]
      }
      else {
        folds[[i]] = c(folds[[i]],v[s])
      }
      # remove sample from values
      v = setdiff(v,v[s])
    }
  }
  return(folds)
  #print(sizes)
}

chum <- cv.partition(label~., data, 5)

chum[[1]]

for (i in 1:length(chum)) {
  
  print(table(chum[[i]]$label))
  
}

data[1]
