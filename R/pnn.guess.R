pnn.guess <-
function(nn, 
                      X){
  
  X <- matrix(X, ncol = nn$k)
  probs <- .pnn.guess.probabilities(nn, X)
  
  if(is.na(probs[1])){ 
    results <- NA 
  }else{
    category <- names(probs[which.max(probs)])
    results <- list(category = category, 
                    probabilities = probs)
  }
  
  return(results)
}
