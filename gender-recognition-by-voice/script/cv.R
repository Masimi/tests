
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

cv.partion <- function(y, x, p=0.5, k=10){
  indices = list()
  n = length(y)
  label <- unique(y)
  
  sizes = cv.sizes(n,k=k)
  values = 1:n
  
  for (i in 1:length(label)){
   indices = append(indices, label[i])
  }
  
  indices
}

cv.testing = function(n, k=10) {

  
  indices = list()
  sizes = cv.sizes(n, k=k)
  values = 1:n
  for (i in 1:k) {
    # take a random sample of given size
    s = sample(values, sizes[i])
    # append random sample to list of indices
    indices[[i]] = s
    # remove sample from values
    values = setdiff(values, s)
  }
  indices
}



