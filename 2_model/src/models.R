run_model_01 <- function(dat) {

  # configure
  l2_lambda <- 0.5 #regularization factors, L1 and L2 regularization
  l1_lambda <- 0.5

  # declare and compile the network
  network <- keras_model_sequential() %>%
    layer_dense(units = 12, input_shape = c(12),
                kernel_regularizer = regularizer_l1_l2(l1_lambda, l2_lambda),
                kernel_initializer = initializer_random_uniform(0,1),
                activation = "relu") %>%
    layer_dense(units = 12, kernel_regularizer = regularizer_l1_l2(l1_lambda, l2_lambda),
                kernel_initializer = initializer_random_uniform(0,1),
                activation = "relu") %>%
    layer_dense(units = 12, kernel_regularizer = regularizer_l1_l2(l1_lambda, l2_lambda),
                activation = "relu", kernel_initializer = initializer_random_uniform(0,1)) %>%
    layer_dense(units = 1, kernel_regularizer = regularizer_l1_l2(l1_lambda, l2_lambda),
                kernel_initializer = initializer_random_uniform(0,1))

  # compile the model. keras objects are modified in place!
  network %>% compile(
    optimizer = optimizer_adadelta(clipnorm = 1),
    loss = "mean_squared_error")

  # fit the model
  history <- network %>% fit(
    dat$train_input, dat$train_target, epochs = 10000, batch_size = 1000,
    callbacks = callback_early_stopping(patience = 500, monitor = "loss"),
    validation_split = 0.1)

  # package and return the results
  return(list(id=1, description="as in Anuj's paper", serialized=serialize_model(network), history=history))
}

run_model_02 <- function(dat) {

  # configure
  reg <- regularizer_l2(0.5)
  init <- initializer_random_uniform(0,1)

  # declare and compile the network
  network <- keras_model_sequential() %>%
    layer_dense(
      units = 12, input_shape = c(12),
      kernel_regularizer = reg,
      kernel_initializer = init,
      activation = "relu") %>%
    # layer_dropout(rate = 0.3) %>%
    layer_dense(
      units = 12,
      kernel_regularizer = reg,
      kernel_initializer = init,
      activation = "relu") %>%
    # layer_dropout(rate = 0.3) %>%
    layer_dense(
      units = 12,
      kernel_regularizer = reg,
      kernel_initializer = init,
      activation = "relu") %>%
    # layer_dropout(rate = 0.3) %>%
    layer_dense(
      units = 1,
      kernel_regularizer = reg,
      kernel_initializer = init)

  # compile the model. keras objects are modified in place!
  network %>% compile(
    optimizer = optimizer_adam(lr = 0.005),
    loss = "mean_squared_error")

  # fit the model
  history <- network %>%
    fit(dat$train_input, unname(dat$train_target), epochs = 2000, batch_size = 1024,
        callbacks = callback_early_stopping(patience = 500, monitor = "loss"),
        validation_split = 0.1)

  # package and return the results
  return(list(id=2, description="Anuj's paper but L2 reg, Adam optimization, batch_size=1024", serialized=serialize_model(network), history=history))
}

run_model_03 <- function(dat) {

  # configure
  reg <- regularizer_l2(0.5)
  init <- initializer_random_uniform(0,1)

  # declare and compile the network
  network <- keras_model_sequential() %>%
    layer_dense(
      units = 12, input_shape = c(12),
      kernel_regularizer = reg,
      kernel_initializer = init,
      activation = "relu",
      layer_dropout(rate = 0.3)) %>%
    layer_dense(
      units = 12,
      kernel_regularizer = reg,
      kernel_initializer = init,
      activation = "relu",
      layer_dropout(rate = 0.3)) %>%
    layer_dense(
      units = 12,
      kernel_regularizer = reg,
      kernel_initializer = init,
      activation = "relu",
      layer_dropout(rate = 0.3)) %>%
    layer_dense(
      units = 1,
      kernel_regularizer = reg,
      kernel_initializer = init)

  network %>% compile(
    optimizer = optimizer_adam(lr = 0.005),
    loss = "mean_squared_error") #keras objects are modified in place!

  # fit the model
  history <- network %>%
    fit(dat$train_input, unname(dat$train_target), epochs = 2000, batch_size = 1024,
        callbacks = callback_early_stopping(patience = 500, monitor = "loss"),
        validation_split = 0.1)

  # package and return the results
  return(list(id=3, description="Anuj's paper but L2 reg, Adam optimization, batch_size=1024, 30% dropout", serialized=serialize_model(network), history=history))
}
