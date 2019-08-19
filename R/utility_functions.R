
#' Splits a data frame into train and test sets.
#'
#' Utility function to randomly split a data frame into train and test sets.
#'
#' @param df a data frame.
#' @param pctTrain numeric value that specifies the percentage of rows to be included in the train set. The remaining rows are added to the test set.
#' @return a list with the first element being the train set and the second element the test set.
#'
#' @examples
#' set.seed(1234)
#'
#' dataset <- friedman1
#'
#' nrow(dataset) # print number of rows
#'
#' split1 <- split_train_test(dataset, pctTrain = 70) # select 70% for training
#'
#' nrow(split1$trainset) # number of rows of the train set
#'
#' nrow(split1$testset) # number of rows of the test set
#'
#' head(split1$trainset) # display first rows of train set
#'
#' @export
split_train_test <- function(df, pctTrain){

  if(pctTrain <= 0 || pctTrain >= 100) stop("pctTrain must be greater than 0 and less than 100")

  n <- nrow(df)
  ntrain <- floor((pctTrain/100) * n)
  ntest <- n - ntrain

  idxs <- sample(n, size = ntrain, replace = F)

  trainset <- df[idxs,]

  testset <- df[-idxs,]

  return(list(trainset=trainset, testset=testset))
}


scale_zero_one <- function(x, skip = -1){
  #x is a data frame
  for(i in 1:ncol(x)){

    if(skip == i)next;

    c <- x[,i]
    max <- max(c)
    min <- min(c)
    if(max==min){
      x[,i] <- max
    }
    else{
      x[,i] <- (c - min) / (max - min)
    }
  }
  x
}
