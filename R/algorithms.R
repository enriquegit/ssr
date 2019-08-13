#' Fits a Semi-supervised Regression Model
#'
#' This function implements the \emph{Co-training by Committee} and \emph{self-learning} semi-supervised regression algorithms with a set of \emph{n} base regressor(s) specified by the user.
#' When only one model is present in the list of regressors, self-learning is performed.
#'
#' The Co-training by Committee implementation is based on Hady et al. (2009). It consists of a set of \emph{n} base models (the committee), each, initially trained with independent bootstrap samples from the labeled training set \code{L}. The Out-of-Bag (OOB) elements are used for validation. The training set for each base model \emph{b} is augmented by selecting the most relevant elements from the unlabeled data set \code{U}. To determine the most relevant elements for each base model \emph{b}, the other models (excluding \emph{b}) label a set of data \code{pool.size} points sampled from \code{U} by taking the average of their predictions. For each newly labeled data point, the base model \emph{b} is trained with its current labeled training data plus the new data point and the error on its OOB validation data is computed. The top \code{gr} points that reduce the error the most are kept and used to augment the labeled training set of \emph{b} and removed from \code{U}.
#'
#' When the \code{regressors} list contains a single model, \emph{self-learning} is performed. In this case, the base model labels its own data points as opposed to Co-training by Committee in which the data points for a given model are labeled by the other models.
#'
#' In the original paper, Hady et al. (2009) use the same type of regressor for the base models but with different parameters to introduce diversity. The \code{ssr} function allows the user to specify any type of regressors as the base models. The regressors can be models from the \code{caret} package, other packages, or custom functions. Models from other packages or custom functions need to comply with certain structure. First, the model's function used for training must have a formula as its first parameter and a parameter named \code{data} that accepts a data frame as the training set. Secondly, the \code{predict()} function must have the trained model as its first parameter. Most of the models from other libraries follow this pattern. If they do not follow this pattern, you can still use them by writting a wrapper function. To see examples of all those cases, please check the Vignettes.
#'
#' @param theFormula a \code{\link[stats]{formula}} that specifies the response and the predictor variables.
#' Two formats are supported: \code{"Y ~ ."} and \code{"Y ~ var1 + var2 + ... + varn"}.
#' @param L a data frame that contains the initial labeled training set.
#' @param U a data frame that contains the unlabaled data.
#' If the provided data frame has the response variable as one of its columns, it will be discarded.
#' @param regressors a list of custom functions and/or strings naming the regression models to be used.
#' The strings must contain a valid name of a regression model from the \code{caret} package.
#' The list of available regression models from the \code{caret} package can be found \href{https://topepo.github.io/caret/available-models.html}{here}.
#' Functions must be named, e.g., \code{list(linearModel=lm)}. List names for models defined with strings are optional.
#' A list can contain both, strings and functions: \code{list("kknn", linearModel=lm)}.
#' Examples can be found in the Vignettes.
#' @param regressors.params a list of lists that specifies the parameters for each custom function.
#' For \code{caret} models specified as strings in \code{regressors}, parameters cannot be passed, use \code{NULL} instead.
#' The parameters are specified with a named list.
#' For example, if \code{regressors = list("lm", knn=knnreg)}, the number of nearest neighbours for knn can be set with \code{list(NULL, list(k = 7))}.
#' @param pool.size specifies the number of candidate elements to be sampled from the unlabeled set \code{U}.
#' The best candidate elements from the pool are labeled and added to the training set.
#' The \code{gr} parameter controls how many of the best candidates are used to augment the training set at each iteration.
#' This parameter has big influence in computational time since in each iteration, \code{pool.size * length(regressors)} models are trained and evaluated in order to find the best candidate data points.
#' @param gr an integer specifiying the \emph{growth rate}, ie., how many of the best elements from the pool are added to the training set for each base model at each iteration.
#' @param maxits an integer that specifies the maximum number of iterations.
#' The training phase will terminate either when \code{maxits} is reached or when \code{U} becomes empty.
#' @param testdata a data frame containing the test set to be evaluated within each iteration.
#' If \code{verbose = TRUE} and \code{plotmetrics = TRUE} the predictive performance of the model on the test set will be printed/plotted for each iteration.
#' @param shuffle a boolean specifying whether or not to shuffle the data frames rows before training the models. Defaults to \code{TRUE}.
#' Some models like neural networks are sensitive to row ordering. Often, you may want to shuffle before training.
#' @param verbose a boolean specifying whether or not to print diagnostic information to the console within each iteration.
#' If \code{testdata} is provided, the information includes performance on the test set such as RMSE and improvement percent with respect to the initial model before data from \code{U} was used.
#' Default is \code{TRUE}.
#' @param plotmetrics a boolean that specifies if performance metrics should be plotted for each iteration when \code{testdata} is provided. Default is \code{FALSE}.
#' @param U.y an optional numeric vector with the true values fo the response variable for the unlabeled set \code{U}.
#' If this parameter is \code{!= NULL} then, the true values will be used to determine the best candidates to augment the training set
#' and the true values will be kept when adding them to the training set.
#' \emph{This parameter should be used with caution} and is intended to be used to generate an upper bound model for comparison purposes only.
#' This is to simulate the case when the model can label the unlabaled data points used to augment the training set with 100\% accuracy.
#' @return A list object of class "ssr" containing:
#'
#' \bold{models} A list of the final trained models in the last iteration.
#'
#' \bold{formula} The formula provided by the user in \code{theFormula}.
#'
#' \bold{regressors} The list of initial \code{regressors} set by the user with formatted names.
#'
#' \bold{regressors.names} The names of the regressors \code{names(regressors)}.
#'
#' \bold{regressors.params} The initial list of parameters provided by the user.
#'
#' \bold{pool.size} The initial \code{pool.size} specified by the user.
#'
#' \bold{gr} The initial \code{gr} specified by the user.
#'
#' \bold{testdata} A boolean indicating if test data was provided by the user: \code{!is.null(testdata)}.
#'
#' \bold{U.y} A boolean indicating if \code{U.y} was provided by the user: \code{!is.null(U.y)}.
#'
#' \bold{numits} The total number of iterations performed by the algorithm.
#'
#' \bold{shuffle} The initial \code{shuffle} value specified by the user.
#'
#' \bold{valuesRMSE} A numeric vector with the Root Mean Squared error on the \code{testdata} for each iteration.
#' The length is the number of iterations + 1.
#' The first position \code{valuesRMSE[1]} stores the initial RMSE before using any data from \code{U}.
#'
#' \bold{valuesRMSE.all} A numeric matrix with \emph{n} rows and \emph{m} columns.
#' Stores Root Mean Squared Errors of the individual regression models.
#' The number of rows is equal to the number of iterations + 1 and the number of columns is equal to the number of regressors.
#' A column represents a regressor in the same order as they were provided in \code{regressors}.
#' Each row stores the RMSE for each iteration and for each regression model.
#' The first row stores the initial RMSE before using any data from \code{U}.
#'
#' \bold{valuesMAE} Stores Mean Absolute Error information. Equivalent to \bold{valuesRMSE}.
#'
#' \bold{valuesMAE.all} Stores Mean Absolute Errors of the individual regression models. Equivalent to \bold{valuesRMSE.all}
#'
#' \bold{valuesCOR} Stores Pearson Correlation information. Equivalent to \bold{valuesRMSE}.
#'
#' \bold{valuesCOR.all} Stores the Pearson Correlation of the individual regression models. Equivalent to \bold{valuesRMSE.all}
#'
#' @references
#' Hady, M. F. A., Schwenker, F., & Palm, G. (2009). Semi-supervised Learning for Regression with Co-training by Committee. In International Conference on Artificial Neural Networks (pp. 121-130). Springer, Berlin, Heidelberg.
#'
#' @examples
#' dataset <- friedman1 # Load friedman1 dataset.
#'
#' set.seed(1234)
#'
#' # Split the dataset into 70% for training and 30% for testing.
#' split1 <- split_train_test(dataset, pctTrain = 70)
#'
#' # Choose 5% of the train set as the labeled set L and the remaining will be the unlabeled set U.
#' split2 <- split_train_test(split1$trainset, pctTrain = 5)
#'
#' L <- split2$trainset
#'
#' U <- split2$testset[, -11] # Remove the labels.
#'
#' testset <- split1$testset
#'
#' # Define list of regressors.
#' regressors <- list("lm", knn=caret::knnreg)
#'
#' # Fit the model.
#' model <- ssr("Ytrue ~ .", L, U, regressors = regressors, testdata = testset, maxits = 10)
#'
#' # Plot RMSE.
#' plot(model)
#'
#' # Get the predictions on the testset.
#' predictions <- predict(model, testset)
#'
#' # Calculate RMSE on the test set.
#' sqrt(mean((predictions - testset$Ytrue)^2))
#'
#' @export
ssr <- function(theFormula,
                    L,
                    U,
                    regressors = list("lm", "rvmLinear"),
                    regressors.params = NULL,
                    pool.size=10,
                    gr=1,
                    maxits=20,
                    testdata=NULL,
                    shuffle=TRUE,
                    verbose = TRUE,
                    plotmetrics = FALSE,
                    U.y = NULL){


  regressors.names <- getRegressorsCaretNames(regressors) # Original regressors names for caret
  regressors <- setRegressorsNames(regressors)
  regressors.types <- sapply(regressors, class)

  # Check that U is not null and has a minimum number of rows.
  if (is.null(U) || nrow(U) < 10) {
    stop("U must be not NULL and have at least 10 instances.")
  }

  # Make sure pool.size has valid values.
  if (is.null(pool.size) || pool.size < 1) {
    stop("pool.size must be at least 1.")
  }

  # Check that testdata has at least 2 rows.
  if (is.null(testdata) == FALSE && nrow(testdata) < 2) {
    stop("testdata must have at least 2 rows.")
  }

  if (is.null(regressors.params)){
    regressors.params <- lapply(1:length(regressors.names), function(e){NULL})
  }

  if (length(regressors) != length(regressors.params)){
    stop("regressors and regressors.params must have the same number of elements.")
  }

  # Process formula
  theFormula <- as.formula(theFormula)
  yvar <- all.vars(theFormula[[2]]) # Get the dependent variable.

  # Check data types
  cyvar <- class(L[,yvar])
  if (cyvar != "numeric" && cyvar != "integer") {
    stop("The response variable in L must be of type numeric or integer")
  }

  if (is.null(U.y) == FALSE) {
    cyvar <- class(U.y)
    if (cyvar != "numeric" && cyvar != "integer") {
      stop("If U.y is not NULL then it must be of type numeric or integer")
    }
    if (length(U.y) != nrow(U)){
      stop("The number of rows in U must be the same as the number of elements in U.y")
    }
  }

  # Issue a warning if U.y is supplied
  if (is.null(U.y) == FALSE){
    warning("U.y was provided. Be cautious when providing this parameter since this will assume
            that the labels from U are known. This is intended to be used to estimate a performance upper bound.")
  }

  # Check if U contains labels, if so, remove them.
  idx <- which(colnames(U) == yvar)
  if(length(idx) > 0){
    U <- U[,-idx]
  }

  # Reorder dataframes so y is at the end.
  L <- moveYtoEnd(L, theFormula)
  U <- moveYtoEnd(U, theFormula)

  # Set plotting params
  if (plotmetrics) {
    old_pars <- par(mfrow = c(1, 2))
    on.exit(par(old_pars))
  }

  # Add ids to U to keep track of rows.
  ids <- 1:nrow(U)
  U <- cbind(ids, U)

  # Add also ids to U.y (if provided by user).
  if (is.null(U.y) == FALSE) {
    U.y <- data.frame(ids, trueLabels = U.y)
  }

  # Initialize committee members. ---------------------------------

  # Regressors are stored here.
  models <- list()

  # Bag instances. B is the boostrapped labeled data for each model.
  B <- list()

  # Out-of-Bag instances. These are used for validation.
  OOB <- list()

  # Number of regressors
  nr <- length(regressors)

  # Number of instances in the labeled set L.
  nL <- nrow(L)

  # Define "not in" operator
  `%nin%` = Negate(`%in%`)

  # Train the regressors with the initial labeled data.
  for(i in 1:nr){

    iB <- sample(nL, size = nL, replace = T) # indices of B elements

    iOOB <- which(1:nL  %nin% iB) # indices of OOB elements

    tmpB <- L[iB, ]

    if(shuffle) tmpB <- shuffleRows(tmpB)

    B <- c(B, list(tmpB)) # Populate B with bootstrapped samples from L.

    OOB <- c(OOB, list(L[iOOB,])) # Populate OOB with the out-of-bag elements.

    if (regressors.types[i] == "character") {
      m <- caret::train(theFormula, data = B[[i]], method = regressors.names[i]) # Train the regressor using B.
    }
    else {
      if(is.null(regressors.params[[i]])){
        m <- do.call(regressors[[i]], c(list(theFormula, data = B[[i]])))
      }
      else{
        m <- do.call(regressors[[i]], c(list(theFormula, data = B[[i]]), regressors.params[[i]]))
      }

    }

    models <- c(models, list(m))

  } # end for, for training regressors.

  # Initialize variables to store performance results on the testdata during each iteration.

  # Store the RMSE of the committee.
  # valuesRMSE[1] stores iteration 0, i.e., before using unlabeled data.
  valuesRMSE <- NULL

  # Store RMSE for each regressor individually.
  valuesRMSE.all <- NULL

  valuesMAE <- NULL # Mean Absolute Error.

  valuesMAE.all <- NULL

  valuesCOR <- NULL # Pearson Correlation.

  valuesCOR.all <- NULL

  # Compute initial performance metrics on testdata (if provided).
  if(is.null(testdata)==F){
    res <- predict_models(models, testdata)

    rmse.m <- sqrt(mean((res$preds - testdata[,yvar])^2))
    valuesRMSE <- c(valuesRMSE, rmse.m)
    rmse.all <- computeRMSE(res$all, testdata, yvar)
    valuesRMSE.all <- rbind(valuesRMSE.all, rmse.all)

    mae.m <- mean(abs(res$preds - testdata[,yvar]))
    valuesMAE <- c(valuesMAE, mae.m)
    mae.all <- computeMAE(res$all, testdata, yvar)
    valuesMAE.all <- rbind(valuesMAE.all, mae.all)

    cor.m <- cor(res$preds, testdata[,yvar])
    valuesCOR <- c(valuesCOR, cor.m)
    cor.all <- computeCOR(res$all, testdata, yvar)
    valuesCOR.all <- rbind(valuesCOR.all, cor.all)
  }

  if(verbose){
    if(is.null(testdata) == FALSE){
      print(paste0("Initial RMSE on testdata: ", format(round(valuesRMSE[1], 4), nsmall = 4)))
    }
  }

  # *********** Start learning *************

  count <- 1

  while(count <= maxits && nrow(U) > 0){

    for(i in 1:nr){

      nel <- min(nrow(U), pool.size) # Number of elements to draw from U.

      idxs <- sample(nrow(U), size = nel, replace = FALSE)

      U_AUX <- U[idxs,]

      U.y_AUX <- NULL

      if(is.null(U.y) == FALSE){
        U.y_AUX <- U.y[idxs,]
      }

      relevant <- selectRelevantExamples(i, U_AUX, OOB[[i]], B[[i]],
                                         gr, theFormula, models,
                                         regressors,
                                         regressors.names,
                                         regressors.types,
                                         regressors.params,
                                         U.y_AUX, shuffle)

      if(is.null(relevant))next;

      # Update U, i.e, remove the selected relevant elements from U.
      idxs <- which(U$ids %in% relevant$ids)

      U <- U[-idxs,]
      if(is.null(U.y) == FALSE){
        U.y <- U.y[-idxs,]
      }

      # Augment the labeled set B: B union relevant.
      tmp <- rbind(B[[i]],relevant[,-1])

      if(shuffle) tmp <- shuffleRows(tmp)

      B[[i]] <- tmp

      if (regressors.types[i] == "character") {
        m <- caret::train(theFormula, data = B[[i]], models[[i]]$method)
      }
      else {
        if(is.null(regressors.params[[i]])){
          m <- do.call(regressors[[i]], c(list(theFormula, data = B[[i]])))
        }
        else{
          m <- do.call(regressors[[i]], c(list(theFormula, data = B[[i]]), regressors.params[[i]]))
        }
      }

      models[[i]] <- m
    }# end of for

    # Compute performance metrics.
    if(is.null(testdata)==F){
      res <- predict_models(models, testdata)

      rmse.m <- sqrt(mean((res$preds - testdata[,ncol(testdata)])^2))
      valuesRMSE <- c(valuesRMSE, rmse.m)
      rmse.all <- computeRMSE(res$all, testdata, yvar)
      valuesRMSE.all <- rbind(valuesRMSE.all, rmse.all)

      mae.m <- mean(abs(res$preds - testdata[,ncol(testdata)]))
      valuesMAE <- c(valuesMAE, mae.m)
      mae.all <- computeMAE(res$all, testdata, yvar)
      valuesMAE.all <- rbind(valuesMAE.all, mae.all)

      cor.m <- cor(res$preds, testdata[,ncol(testdata)])
      valuesCOR <- c(valuesCOR, cor.m)
      cor.all <- computeCOR(res$all, testdata, yvar)
      valuesCOR.all <- rbind(valuesCOR.all, cor.all)
    }

    if(verbose){
      if(is.null(testdata) == FALSE){
        impr <- ((valuesRMSE[1]-valuesRMSE[count+1]) / valuesRMSE[1])*100
        msg <- paste0("Iteration ", count, " (testdata) RMSE: ", format(round(valuesRMSE[count+1], 4), nsmall = 4),
                     " Improvement: ", format(round(impr, 2), nsmall = 2),"%")


        print(msg)
      }
      else{
        print(paste0("Iteration ", count))
      }
    }

    if(plotmetrics){
      if(is.null(valuesRMSE) == FALSE && length(valuesRMSE) > 0){
        plot(0:(length(valuesRMSE)-1),valuesRMSE, type="l", col="blue", main = "test RMSE", xlab = "iteration", ylab = "RMSE")
        plot(0:(length(valuesCOR)-1),valuesCOR, type="l", col="red", main = "test Correlation", xlab = "iteration", ylab = "Correlation")
      }
    }

    count <- count + 1

  }# end of while


  res <- structure(list(models = models,
                        formula = theFormula,
                        regressors = regressors,
                        regressors.names = names(regressors),
                        regressors.params = regressors.params,
                        pool.size = pool.size,
                        gr = gr,
                        testdata = !is.null(testdata), # Wether or not testdata was provided
                        U.y = !is.null(U.y), # Wether or not U.y was provided
                        numits = (count - 1),
                        shuffle = shuffle,
                        valuesRMSE = valuesRMSE,
                        valuesRMSE.all = valuesRMSE.all,
                        valuesMAE = valuesMAE,
                        valuesMAE.all = valuesMAE.all,
                        valuesCOR = valuesCOR,
                        valuesCOR.all = valuesCOR.all),
                   class = "ssr")

  return(res)
}

selectRelevantExamples <- function(pos,
                                   U_AUX,
                                   V, # V = OOB[[pos]]
                                   L_i, # L_i = B[[pos]]
                                   gr,
                                   theFormula,
                                   models,
                                   regressors,
                                   regressors.names,
                                   regressors.types,
                                   regressors.params,
                                   U.y_AUX = NULL,
                                   shuffle = TRUE){

  # pos: index of the committee member to be excluded (if #models > 1).
  # V: validation set

  # Calculate validation error of h_j using V

  m <- models[[pos]]

  predictions <- predict(m, V)

  yvar <- all.vars(theFormula[[2]])

  e_1 <- sqrt(mean((predictions - V[,yvar])^2))

  delta_e <- NULL

  preds <- NULL

  for(i in 1:nrow(U_AUX)){

    x_u <- U_AUX[i,]

    prediction <- NULL

    if(is.null(U.y_AUX) == FALSE){
      prediction <- U.y_AUX[i,2]
    }
    else{
      prediction <- predict_x_u(models, x_u[,-1], pos)
    }

    preds <- c(preds, prediction)

    tmp.x_u <- cbind(x_u[,-1], prediction)
    colnames(tmp.x_u)[ncol(tmp.x_u)] <- yvar

    tmp <- rbind(L_i, tmp.x_u)

    if(shuffle) tmp <- shuffleRows(tmp)

    if (regressors.types[pos] == "character") {
      tmp.m <- caret::train(theFormula, data = tmp, method = m$method)
    }
    else {
      if(is.null(regressors.params[[pos]])){
        tmp.m <- do.call(regressors[[pos]], c(list(theFormula, data = tmp)))
      }
      else{
        tmp.m <- do.call(regressors[[pos]], c(list(theFormula, data = tmp), regressors.params[[pos]]))
      }
    }

    predictions <- predict(tmp.m, V)

    e <- sqrt(mean((predictions - V[,yvar])^2))

    delta_e <- c(delta_e, (e_1 - e) / e_1)
  } # end for

  #idxs <- which(delta_e > 0 & delta_e > constants$minDelta)
  idxs <- which(delta_e > 0)

  if(length(idxs) == 0) return(NULL)

  tmp <- cbind(U_AUX[idxs,], y=preds[idxs], deltas=delta_e[idxs])

  colnames(tmp)[ncol(tmp)-1] <- yvar

  ub <- min(gr, nrow(tmp))

  tmp <- tmp[order(-tmp$deltas),] # order (descending)

  pi <- tmp[1:ub,] # select most relevant elements

  return(pi[,-ncol(pi)])

}

#' Plots a ssr Object
#'
#' Plots the results of a fitted ssr object if a testset was provided when fitting the model.
#'
#' This function generates performance plots to quickly inspect the results of the fitted model.
#' The fitted model contains all the necessary data so the user can create custom plots, if required.
#'
#' @param x a fitted object of class "ssr".
#' @param metric the type of metric to be plotted ("rmse", "mae", "cor"), defaults to "rmse".
#' @param ptype an integer specifying the type of plot. The default 1, plots the performance metric of the fitted model.
#' Any value different of 1, plots the performance metric of the individual regressors used to build the model.
#' @param ... additional arguments to be passed to the plot function.
#'
#' @return a NULL invisible object.
#'
#' @examples
#' dataset <- friedman1 # Load dataset.
#'
#' set.seed(1234)
#'
#' # Prepare the data.
#' split1 <- split_train_test(dataset, pctTrain = 70)
#' split2 <- split_train_test(split1$trainset, pctTrain = 5)
#' L <- split2$trainset
#' U <- split2$testset[, -11]
#' testset <- split1$testset
#' regressors <- list("lm", knn=caret::knnreg)
#'
#' # Fit the model.
#' model <- ssr("Ytrue ~ .", L, U, regressors = regressors, maxits = 10, testdata = testset)
#'
#' # Plot the RMSE of the fitted model.
#' plot(model, metric = "rmse", ptype = 1)
#'
#' # Plot the MAE of each individual regressor.
#' plot(model, metric = "MAE", ptype = 2)
#' @export
#' @importFrom graphics abline legend lines par plot
plot.ssr <- function(x, metric = "rmse", ptype = 1, ...){

  if(x$testdata == FALSE){ # No test data to plot.
    warning("Could not generate plot since testdata was not provided.")
    return(invisible(NULL))
  }

  if(ptype != 1){
    plotAllRegressors(x, metric, ...)
    return(invisible())
  }

  values <- x$valuesRMSE

  if(metric == "mae"){
    values <- x$valuesMAE
  }
  else if(metric == "cor"){
    values <- x$valuesCOR
  }

  ylimits <- c(min(values), max(values))

  # Plot legend outside chart: http://dr-k-lo.blogspot.com/2014/03/the-simplest-way-to-plot-legend-outside.html

  # Set/Save plotting params
  old_pars <- par(oma = c(1, 0, 0, 0))
  on.exit(par(old_pars))

  plot(0:x$numits, values, type = "l", col="blue",
       ylab = metric, xlab = "iteration", main = metric, ...)

  abline(h = values[1], lty = 2)

  # Set/Save plotting params
  old_pars2 <- par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  on.exit(par(old_pars2), add = TRUE, after = FALSE)

  legend("bottom",
         legend = c("Iteration 0"),
         col = c("black"),
         lty = 2,
         cex = 0.8,
         text.font = 4,
         bg = 'lightblue',
         xpd = TRUE,
         horiz = TRUE,
         inset = c(0, 0),
         bty = "y")

  return(invisible(NULL))
}

plotAllRegressors <- function(object, metric, ...){

  values <- object$valuesRMSE.all

  if(metric == "mae"){
    values <- object$valuesMAE.all
  }
  else if(metric == "cor"){
    values <- object$valuesCOR.all
  }

  ylimits <- c(min(values), max(values))

  # Set/Save plotting params
  old_pars <- par(oma = c(1, 0, 0, 0))
  on.exit(par(old_pars))

  plot(0:object$numits, values[,1], type = "l", col=2, ylim = ylimits,
       ylab = metric, xlab = "iteration", main = metric, ...)

  if(length(object$regressors.names) > 1){
    for(i in 2:length(object$regressors.names)){
      lines(0:object$numits, values[,i], col = i + 1)
      #abline(h = values[1,i], lty = 2, col = i + 1)
    }
  }


  lcolors <- 2
  if(length(object$regressors.names) > 1) lcolors <- 2:(length(object$regressors.names)+1)

  # Set/Save plotting params
  old_pars2 <- par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  on.exit(par(old_pars2), add = TRUE, after = FALSE)

  legend("bottom",
         legend=object$regressors.names,
         bg='lightblue',
         text.font = 4,
         cex = 0.8,
         lty = 1,
         xpd = TRUE,
         horiz = TRUE,
         inset = c(0, 0),
         bty = "y",
         col=lcolors)

}

predict_x_u <- function(models, x_u, pos = 1){

  # pos: index of learner to be excluded.

  n <- length(models)

  # If there is only 1 model (self-learning)
  if(n == 1){
    return(predict(models[[pos]], x_u))
  }

  acum <- 0

  for(j in 1:n){

    if(j == pos)next;

    acum <- acum + predict(models[[j]], x_u)
  }

  return(acum / (n-1))
}

predict_models <- function(models, newdata){

  nl <- length(models)
  N <- nrow(newdata)
  M <- matrix(data = rep(0,N*nl), nrow = N)

  for(i in 1:nl){
    tmp.m <- models[[i]]
    tmp <- predict(tmp.m, newdata)
    M[,i] <- tmp
  }

  res <- list(preds=rowMeans(M), all=M)

  return(res)
}

#' Predictions From a Fitted ssr Object
#'
#' Returns a vector of predicted responses from the fitted ssr object.
#'
#' @param object fitted object of class ssr.
#' @param newdata data frame with the input variables from which the response varaible is to be predicted.
#' @param ... additional arguments (not used)
#' @return A numeric vector with the predictions for each row of the input data frame.
#' @examples
#'
#' # See ?ssr for an example.
#'
#' @export
predict.ssr <- function(object, newdata, ...){

  nl <- length(object$models)
  N <- nrow(newdata)
  M <- matrix(data = rep(0,N*nl), nrow = N)

  for(i in 1:nl){
    tmp.m <- object$models[[i]]
    tmp <- predict(tmp.m, newdata)
    M[,i] <- tmp
  }

  res <- rowMeans(M)

  return(res)
}

computeRMSE <- function(M, testdata, yvar){

  rmses <- NULL

  for(i in 1:ncol(M)){
    tmp <- sqrt(mean((M[,i] - testdata[,yvar])^2))
    rmses <- c(rmses, tmp)
  }
  return(rmses)
}

computeMAE <- function(M, testdata, yvar){

  maes <- NULL

  for(i in 1:ncol(M)){
    tmp <- mean(abs(M[,i] - testdata[,yvar]))
    maes <- c(maes, tmp)
  }
  return(maes)
}

#' @import stats
computeCOR <- function(M, testdata, yvar){

  cors <- NULL

  for(i in 1:ncol(M)){
    tmp <- cor(M[,i], testdata[,yvar])
    cors <- c(cors, tmp)
  }
  return(cors)
}

shuffleRows <- function(df){
  n <- nrow(df)
  idxs <- sample(n, n, FALSE)
  df <- df[idxs,]
  return(df)
}

moveYtoEnd <- function(df, theFormula){
  # Moves the response variable Y to the end and removes variables not in theFormula.
  # If df is unlabeled, it just removes variables not in theFormula.

  cnames <- colnames(df)

  yvar <- all.vars(theFormula[[2]])

  if (yvar %in% cnames == FALSE){ # This is the unlabeled data U.

    thevars <- all.vars(theFormula[[3]])

    if(length(thevars) == 1 && thevars == ".") thevars <- cnames

    df <- subset(df, select=thevars)

  } else { # This is the labeled data L.

    df <- model.frame(theFormula, data = df)

    cnames <- colnames(df)

    idx <- which(cnames == yvar)

    idxs <- which(cnames != yvar)

    orderedNames <- c(cnames[idxs], cnames[idx])

    df <- subset(df, select=orderedNames)

  }

  return(df)
}

getRegressorsCaretNames <- function(regressors){

  cnames <- character(length = length(regressors))
  types <- sapply(regressors, class)
  idxs <- which(types == "character")
  cnames[idxs] <- sapply(regressors[idxs], function(e){e[[1]]})
  return(cnames)
}

setRegressorsNames <- function(regressors){

  rnames <- character(length = length(regressors))

  types <- sapply(regressors, class)

  idxs <- which(types == "character")

  rnames[idxs] <- sapply(regressors[idxs], function(e){e[[1]]})

  tmpNames <- names(regressors)

  idxs <- which(tmpNames != "")

  rnames[idxs] <- tmpNames[idxs]

  names(regressors) <- rnames

  return(regressors)
}
