.fA.multi <- function(Xa, 
                      X, 
                      sigma){
  
  if(missing(Xa)) stop("class A is missing from set")
  if(missing(X)) stop("X input is missing")
  
  m <- nrow(Xa) # total number of training patterns from selected category
  p <- ncol(Xa) # number of input factors
  
  kernl <- (1 / ((det((2*pi)*sigma))^0.5))

  if(is.nan(kernl)){ # nan is returned when kernl -> 0, may be due to small sample size
    kernl <- 0
  }
  
  f <- prod(kernl,
            1/m,
            sum(mapply(FUN = .patterns.multi,
                       Xa = split(Xa, seq(1, nrow(Xa), 1)),
                       MoreArgs = list(X = X,
                                       sigma_inverse = MASS::ginv(sigma)),
                       SIMPLIFY = TRUE)))
  
  if(is.nan(f)){ # asymptotically approaches zero then set to zero
    f <- 0
  }
  
  return(f)
}

.patterns.multi <- function(Xa, 
                            X, 
                            sigma_inverse){
  
  return(exp(-0.5*(Xa-X)%*%sigma_inverse%*%t(Xa-X))) # mahalanobis + gaussian kernel
}

.pnn.create <- function(){ # create empty list for pnn
  
  nn <- list(model = "Probabilistic Neural Network", 
             set = NULL)
  
  return(nn)
}

.pnn.guess.probabilities <- function(nn, 
                                     X){
  
  results <- vector()
  
  for(category in nn$categories){
    Xa <- nn$set[nn$set[,nn$category.column] == category,]
    Xa <- as.matrix(Xa[,-nn$category.column])
    
    if(length(Xa)==0){ #handle zero example case
      results <- c(results, 0)
    }else{
      results <- c(results, .fA.multi(Xa, 
                                      X,
                                      sigma = nn$alpha*nn$sigma))
    }
  }
  
  probs <- results / sum(results) #standard output layer
  
  names(probs) <- nn$categories #probabilities of each category
  
  return(probs)
}
