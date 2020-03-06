#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### ------   Loading the Packages   -------
#### ---------------------------------------
#### ---------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------

library(keras)
library(tensorflow)
library(tfestimators)
library(tfdatasets)
library(reticulate)
library(dplyr)
library(ggplot2)
library(psych)
library(plot3D)
library(readxl)
library(mgcv)
library(lattice)

sys <- import("sys")
np <- import("numpy")

#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### ------    Loading the Dataset   -------
#### ---------------------------------------
#### ---------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------

boston_housing <- dataset_boston_housing()
c(c(train.data, train.targets), c(test.data, test.target)) %<-% boston_housing

#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### ------       Data Analaysis     -------
#### ---------------------------------------
#### ---------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------

#### There are 14 input features, and these input features they all are in different scales, therefore it is necessary to normalise them in order 
#### to guarantee a proper training. Also the normalization should not create any problem as we are not doing Granger causality or inference 
#### but definition of the optimal Neural Network Architecture: 
summary(Boston)

#### Now we are normalizing the data. It is important to remember that the test data are unseen data, and therefore when rescaling them we need 
#### to use the mean and the standard deviation from the train data. 
mean <- apply(train.data, 2, mean)
std <- apply(train.data, 2, sd)

train.data <- scale(train.data, center = mean, scale = std)
test.data <- scale(test.data, center = mean, scale = std)

#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### --       Costum L1 Regularizer       --
#### ---------------------------------------
#### ---------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------

create_l1 <- function(lambda, alpha) {
  function(w) {
    
    ## Defining alpha and lambda.
    lamb <- np$float64(lambda)
    alpha <- np$float64(alpha)
    
    ## Defining the squared group dimension to which multiply the penalty.
    const.coeff1 <- np$float64(np$sqrt(np$multiply(np$float64(k_int_shape(w)[1]), np$float64(k_int_shape(w)[2]))))
    const.coeff2 <- np$float64(np$sqrt(k_int_shape(w)[1]))
    
    ## Computing the Frobenious Norm for Matrices (Group Lasso).
    element1 <- (((lamb*(1-alpha))*const.coeff1)*(tf$norm(w, ord = "fro", axis = tuple(0L, 1L))))
    
    ## Computing the Euclidean Norm for Vectors (In Between Penalty).
    element2 <- ((lamb*alpha)*const.coeff2)*(k_sum((tf$norm(w, ord = "euclidean", axis = 1L)))) 
    
    ## Computing the overall penalty.
    penalty <- (element1 + element2)
    
    return(penalty)
    
  }
}

#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### ----       Building the model      ----
#### ---------------------------------------
#### ---------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------

#### As we will build the model multiple times, we create a function to create a model with "n" hidden layers (the number of hidden layers must 
#### be adjusted manually), that applies the customized penalty. For the optimization we are using Stochastic Gradient Descent with momentum > 0, 
#### that avails itself of the Nesterov condition. 

build.model <- function(lambda.input, alpha.input){
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = 10, activation = "relu", input_shape = dim(train.data)[[2]]) %>% 
    layer_dense(units = 11, activation = "relu", kernel_regularizer = create_l1(lambda = lambda.input, alpha = alpha.input)) %>% 
    layer_dense(units = 11, activation = "relu", kernel_regularizer = create_l1(lambda = lambda.input, alpha = alpha.input)) %>% 
    layer_dense(units = 11, activation = "relu", kernel_regularizer = create_l1(lambda = lambda.input, alpha = alpha.input)) %>% 
    layer_dense(units = 1)
  
  model %>% compile(optimizer = optimizer_sgd(lr = 0.01, momentum = 0.99, nesterov = TRUE), loss = "mse", metrics = c("mae"))
  
}

#### Following the function to perform k-fold cross validation over a combination of alpha and lambda is defined. The function allows the 
#### customization of the hyperplane and of the number of folds. See Function for information about the input parameters.

k.fold.param <- function(start.alpha, end.alpha, alpha.interv, start.lambda, end.lambda, lambda.interv, num_epoch, n.fold, condition.graph, 
                         train.data, train.targets){
  
  ## PURPOSE: 
  ## To return the list of the optimal lambda and alpha based on k-fold cross validation.  
  
  ## INPUTS:
  ## start.alpha: Starting value for the domain of alpha (defined by a sequence).
  ## end.alpha: End value of the domain of alpha (defined by a sequence).
  ## alpha.interv: Intervals in the sequence that defines the domain of alpha.
  ## start.lambda: Starting value for the domain of lambda (defined by a sequence).
  ## end.lambda: End value of the domain of lambda (defined by a sequence).
  ## lambda.interv: Intervals in the sequence that defines the domain of lambda.
  ## num_epochs: Number of Epochs for each FNN constructed
  ## n.fold: Number of folds to be used for k-fold-cross validation. 
  ## condition.graph: If TRUE a graph representing the smoothed Validation MAE by epoch will be plotted for each alpha and lambda. 
  ## train.data: Covariates from the train set. 
  ## train.target: Target from the train set. 
  
  ## OUTPUTS:
  ## A vector containing the optimal combination of alpha and lambda with corresponsing average MAE (Average over the k-folds)
  
  ## Defining Input traps
  if(!is.numeric(start.alpha)) stop("Error: Starting Value of Alpha non numeric")
  if(!is.numeric(start.lambda)) stop("Error: Starting Value of Lambda non numeric")
  if(!is.numeric(end.alpha)) stop("Error: End value of Alpha non numeric")
  if(!is.numeric(end.lambda)) stop("Error: End value of Lambda non numeric")
  if(!is.numeric(alpha.interv)) stop("Error: Alpha steps non numeric")
  if(!is.numeric(lambda.interv)) stop("Error: Lambda steps non numeric")
  if(!is.numeric(num_epoch)) stop("Error: Number of Epochs non numeric")
  if(!is.numeric(n.fold)) stop("Error: Number of Folds non numeric")
  if(!is.logical(condition.graph)) stop("Error: Condition.graph is not a boolean operator")
  if(length(start.alpha) > 1) stop("Error: Starting Value of Alpha is not a scalar")
  if(length(start.lambda) > 1) stop("Error: Starting Value of Lambda is not a scalar")
  if(length(end.alpha) > 1) stop("Error: End Value of Alpha is not a scalar")
  if(length(end.lambda) > 1) stop("Error: End Value of Lambda is not a scalar")
  if(length(alpha.interv) > 1) stop("Error: Alpha steps is not a scalar")
  if(length(lambda.interv) > 1) stop("Error: Lambda steps is not a scalar")
  if(length(train.data) < 2) stop("Error: Train.set is a scalar")
  if(length(train.targets) < 2) stop("Error: Train.target is a scalar")
  if(start.alpha < 0) stop("Error: Starting Value of Alpha is negative")
  if(start.lambda < 0) stop("Error: Starting Value of Lambda is negative")
  if(end.alpha < 0) stop("Error: End Value of Alpha is negative")
  if(end.lambda < 0) stop("Error: End Value of Lambda is negative")
  if(alpha.interv < 0) stop("Error: Alpha step is negative") 
  if(lambda.interv < 0) stop("Error: Lambda step is negative")
  if(num_epoch < 0) stop("Error: Number of Epoch is negative")
  if(n.fold < 0) stop("Error: Number of folds is negative")
  if(end.alpha > 1) stop("Error: Upper Value of Alpha is greater than 1")
  if(end.lambda > 1) stop("Error: Upper Value of Lambda is greater than 1")
  
  ## Defining the hyperplane
  searchGrid <- expand.grid(alpha = seq(from = star.alpha, to = end.alpha, by = alpha.interv), 
                            lambda = seq(from = start.lambda, to = end.lambda, by = lambda.interv))
  
  ## In here we create for each observation a number between 1 and n.fold that defines which observation belongs to each of the n.folds
  indices <- sample(1:nrow(train.data), replace = FALSE)
  folds <- cut(1: length(indices), breaks = n.fold, labels = FALSE)
  
  all_scores <- c()
  all_mea_histories <- NULL
  
  optimal.sparsity <- apply(searchGrid, 1, function(parameterList){
    
    current.alpha <- parameterList[["alpha"]]
    current.lambda <- parameterList[["lambda"]]
    
    for(i in 1:n.fold){
      
      cat("Processing Fold #", i, "\n")
      
      ## In here we are defining the fold that will be used to validate.
      val.indices <- which(folds == i, arr.ind = TRUE)
      val.data <- train.data[val.indices,]
      val.targets <- train.targets[val.indices]
      
      ## In here we are defininf the folds that will be used to train. 
      partial.train.data <- train.data[-val.indices,]
      partial.train.targets <- train.targets[-val.indices]
      
      model <- build.model(lambda.input = current.lambda, alpha.input = current.alpha)
      
      history <- model %>% fit(partial.train.data, partial.train.targets, validation_data = list(val.data, val.targets), 
                               epochs = num_epochs, batch_size = nrow(train.data), verbose = 0)
      
      mea_history <- history$metrics$val_mean_absolute_error
      all_mea_histories <- rbind(all_mea_histories, mea_history)
      
      results <- model %>% evaluate(val.data, val.targets, verbose = 0)
      
      all_scores <- c(all_scores, results$mean_absolute_error)
      
    }
    
    ### Taking the average of the four out-of-sample scores to evalue the final result: 
    average.score <- mean(all_scores)
    
    
    if(condition.graph = TRUE){
      
      ### We compute the average of the per-epoch MEA scores for all folds
      avearge_mea_histroy <- data.frame(epoch = seq(1:ncol(all_mea_histories)), validation_mae = apply(all_mea_histories, 2, mean))
      
      ### We use ggplot to plot a smooth version of the graph: 
      ggplot(avrage_mea_history, aes(x = epoch, y = validation_mae)) + geom_smooth()
      
    } else{
      
      cat("No Graph for alpha = ", current.alpha, "and lambda = ", current.lambda)
      
    }
    
    output <- c(average.score, current.alpha, current.lambda)
    
  })
  
  output <- as.data.frame(t(optimal.sparsity))
  best.comb <- output[which(output[,1] == min(output[,1], na.rm = TRUE), arr.ind = TRUE),]

  return(best.comb)
  
}

final.param <- k.fold.param(start.alpha = 0.01, end.alpha = 1, alpha.interv = 0.05, start.lambda = 0.01, end.lambda = 1, 
                            lambda.interv = 0.05, num_epoch = 500, n.fold = 4, condition.graph = FALSE, train.data = train.data, 
                            train.targets = train.targets)

#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### ---   Building the final model      ---
#### ---------------------------------------
#### ---------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------

### Once the optimal alpha and Lambda are defined, We can fit the final model in which we return the number of neurons active at each epoch 
### and return the results on tensor board, without having to cross validate. All the external functions are the same as defined above, 
### including the model structure. It creates an interative graph that is updated at each epoch, and it allows the visualisation of the graph 
### on Tensorboard. 


#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------

#### In here we define the function that will be used to count the neurons that are different from zero after the penalisation, This is done 
#### by applying the Lasso property of the hard threshold (See also Scardapane code)

count_neurons <- function(model){
  
  store <- c()

  for(l in model$layers){
    ### For each layer checks how many neurons are greater than the threshold (as for the Lasso)
    nonzero.neurons <- np$sum(np$sum(np$abs(l$get_weights()[0L]), axis = 1L) > 10**(-3)) ## In here you need to insert the value of Lambda
    store <- c(store, nonzero.neurons)
  }
  
  ### We obtain the total number of neurons that is greater than the sum. 
  final <- sum(store)
  
  return(final)
  
}

#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------

#### Following we need to create an R6 Object that will be used to call the count_neuron function as Call back. 

TrainHistory <- R6::R6Class("TrainHistory", inherit = KerasCallback, public = list(
  
  losses = NULL, 
  neurons = NULL,
  model = NULL,
  on_train_begin = function(logs = list()){
    self$losses = c()
    self$neurons = c(count_neurons(self$model))
  },
  on_batch_end = function(batch, logs = list()){
    self$losses <- c(self$losses, logs[["loss"]])
    self$neurons <- c(self$neurons, count_neurons(self$model))
  }
))

#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------

#### Setting the parameters for the model
lambda.input <- 0.3
alpha.input <- 0.01

#### Setting the graph for Tensorboard
tf$reset_default_graph()
k_set_session(tf$Session())

model <- keras_model_sequential() %>% 
  layer_dense(units = 11, activation = "relu", input_shape = dim(train.data)[2]) %>% 
  layer_dense(units = 10, activation = "relu", create_l1(lambda = lambda.input, alpha = alpha.input)) %>% 
  layer_dense(units = 10, activation = "relu", create_l1(lambda = lambda.input, alpha = alpha.input)) %>% 
  layer_dense(units = 1)


history <- TrainHistory$new()

model %>% compile(optimizer = optimizer_sgd(lr = 0.01, momentum = 0.99, nesterov = TRUE), loss = "mse", metrics = c("mae"))

model %>% fit(train.data, train.labels, epochs = 1000, validation_split = 0.2, verbose = 0, 
              batch_size = nrow(train.data), callbacks = list(history)) 

### This is the correct way of computing the mean absolute error.
results <- model %>% evaluate(train.data, train.labels, verbose = 0)


#### -----------------------------------------------------------------------------------------------------------------------------------------------

#### If we want to visualise the results on Tensorboard
history <- model %>% fit(train.data, train.labels, epochs = 1000, validation_split = 0.2, verbose = 1, 
                         callbacks = callback_tensorboard(log_dir = "logs/run_a", histogram_freq = 1,
                                                          write_graph = TRUE, write_images = FALSE, embeddings_freq = 0,
                                                          embeddings_layer_names = NULL, embeddings_metadata = NULL))
tensorboard("logs/run_a")

#### -----------------------------------------------------------------------------------------------------------------------------------------------

#### Plotting the number of neurons at each epoch, we are going to have epoch + 1 datapoints as also the initial number of neurons will be 
#### reported. 

neurons.per.epoch <- history$neurons

plot(neurons.per.epoch, main = "Number of Neuron per Epoch", xlab = "Number of Epoch", ylab = "Number of Neurons", type = "b", lty = 2, lwd = 1, 
     pch = 19)

#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### ---------------------------------------
#### ---------------------------------------
#### ---       Creating the plane        ---
#### ---------------------------------------
#### ---------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------
#### -----------------------------------------------------------------------------------------------------------------------------------------------

#### The code adopted to construct the plane is reported below. In particular the number of hidden nodes is defined using a for loop, while the 
#### number of hidden layers must be changed manually at the end of each loop.In particular, the number of hidden layers is equal to the number of 
#### hidden layers to which we want to apply the regularization, if no regularization is applied to the first hidden layer (variable selection), 
#### then we will have Z - 1 hidden layers with regularization. 

n.hidden <- 100
n.layers <- 14
num.epochs <- 500
validation.split <- 0.2

lambda.input <- 0.3
alpha.input <- 0.01

#### Defining the x and y axis for the 3D plot. 
x <- seq(from = 1, to = n.hidden, by = 1)
y <- seq(from = 1, to = n.layers, by = 1)

#### Creating the vectors where to store the nodes.
store.nodes <- rep(0, times = n.hidden)
store.validationnodes <- rep(0, times = n.hidden)


for (i in 1:n.hidden){
  
  cat("Number of Hidden Nodes #", i, "\n")
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = i, activation = "relu", input_shape = dim(train.data)[2]) %>% 
    layer_dense(units = i, activation = "relu", kernel_regularizer = create_l1(lambda = lambda.input, alpha = alpha.input)) %>% 
    layer_dense(units = 1)
  
  
  model %>% compile(optimizer = optimizer_sgd(lr = 0.01, momentum = 0.99, nesterov = TRUE), loss = "mse", metrics = c("mae"))
  
  history <- model %>% fit(train.data, train.targets, epochs = num.epochs, validation_split = validation.split, verbose = 0,
                           batch_size = nrow(train.data))
  
  ## In sample Error 
  store.nodes[i] <- (history$metrics$mean_absolute_error[num.epochs])
  
  ## Out of Sample Error 
  store.validationnodes[i] <- (history$metrics$val_mean_absolute_error[num.epochs])
  
}

#### Creating .csv files to store the results. 
write.csv(x = store.nodes, file = "result.csv", row.names = FALSE, col.names = TRUE)
write.csv(x = store.validationnodes, file = "result1.csv", row.names = FALSE, col.names = TRUE)


#### Each column created in the csv file is then added to a third file, where each column will take the n.hidden rows MSE, one page for in-sample 
#### and the other page for the out-of-sample MSE. Following the matrix with the different values will be imported. 

z <- read_excel("/Users/tulliomancini/Desktop/penalisedplane.xlsx", sheet = "Insample")
z <- as.matrix(z) 

#### At first we create a normal 3D plot, which for construction will not be smooth. In here we report the code to plot the same graph from different 
#### angles.
persp3D(x, y, z, phi = 30, theta = 225, expand = 0.6, ylab = "N. Hidden Layers", 
        xlab = "N. Hidden Nodes", zlab = "MSE", box = TRUE, border = NA,  
        col.palette = "heat.colors", ticktype = "detailed",  bty = "b2", main = "Deep Neural Network MSE",
        col = ramp.col(c("blue", "lightblue", "green", "yellow", "red")))

persp3D(x, y, z, theta = 150, phi = 20, expand = 0.6, ylab = "N. Hidden Layers", 
        xlab = "N. Hidden Nodes", zlab = "MSE", box = TRUE, border = NA,  
        col.palette = heat.colors, ticktype = "detailed",  bty = "b2", contour = list(nlevels = 4, col = "red"),
        image = list(col = grey (seq(0, 1, length.out = 100))), main = "Deep Neural Network MSE",
        col = ramp.col(c("blue", "lightblue", "green", "yellow", "red")))


#### Following, we use the lattice package and the procedure below to create a smoother version of the graph above from one angle that should 
#### report all the different perspectives analysed from the previous two graphs. 
z1 <- data.frame(y = rep(seq_len(ncol(z)), each = nrow(z)), x = rep(seq_len(nrow(z)), times = ncol(z)), 
                 z = c(z))

mod <- gam(z ~ te(x, y, k = 10), data = z1)
m2 <- matrix(fitted(mod), ncol = 15)

wireframe(m2, drape = TRUE, aspect  = c(80/87, 0.5), light.source = c(10, 0, 10), 
          col.regions = colorRampPalette(c("blue", "lightblue", "green", "orange", "yellow", "red"))(100),
          screen = list(z = -10, x = -60), xlab = "Number of Hidden nodes", ylab = list("N. Hidden Layers", rot = 84), 
          zlab = "MSE", scales = list(arrows = FALSE, col = "black", font = 1, tck = 0.6))


