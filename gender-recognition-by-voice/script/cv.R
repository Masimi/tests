
cv.sizes <- function(n, k=10) {
  res <- n %/% k #resultado da divisao
  rem <- n %%  k #resto da divisao(mod)
  x <- 0
  sizes = c()
  
  for (i in 1:k) {
    if (rem>0) {
      x <- 1 
      rem <- rem - 1
    }
    else {
      x <- 0
    }
    sizes = append(sizes, res + x)
  }
  sizes
}

cv.partition <- function(y, x, k=10){
  indices = list()
  n = length(y)
  labels <- as.data.frame(table(y))
  sizes = cv.sizes(n,k=k)
  
  for (j in 1:length(labels)){
    # proporcao por label
    prop <- labels$Freq[j]/n
    
    # take a number of sample
    v <- as.numeric(rownames(x[y==labels[j,1],]))
    
    for (i in 1:k){
      #Resolve divisão desigual de lables
      limit <- 0
      if ((i %% 2)==0) {
        limit <- floor(sizes[i]*prop)
      }else{
        limit <- ceiling(sizes[i]*prop)
      }
      
      # take a random sample of given size
      s <- sample(length(v), limit)
      
      # append random sample to list of indices
      if (j==1) {
        indices[[i]] = v[s]
      } else {
        indices[[i]] = c(indices[[i]],v[s])
      }
      # remove sample from values
      v = setdiff(v,v[s])
    }
  }
  indices
}
