rm(list=ls())

library(dplyr)
library(keras)

source('src/functions.R')
obs <- readRDS('in/nhd_13293262.rds')

data.table::fwrite(obs, file = "in/mendota_obs.csv")
meteo <- feather::read_feather('in/Mendota_meteo.feather') %>% mutate(date = as.Date(time))
temps <- glmtools::resample_to_field(nc_file = "in/output.nc", field_file = 'in/mendota_obs.csv') %>% 
  mutate(date = as.Date(DateTime)) %>% filter(!is.na(Modeled_temp)) 
#NAs in input result in NaNs from loss function

all_model_temps_short <- glmtools::get_temp('in/output.nc',  reference = "surface", z_out = seq(0,25,0.5)) %>% 
  mutate(DateTime = as.Date(DateTime))
all_model_temps <- all_model_temps_short %>% 
  tidyr::gather(key = "Depth", value = "Modeled_temp", -DateTime) %>% 
  mutate(Depth = as.numeric(gsub(pattern = "temp_", replacement = "", x = Depth))) %>% 
  filter(!is.na(Modeled_temp))
  
all_model_temps_drivers <- left_join(all_model_temps, meteo, by = c(DateTime = "date")) %>% 
  augment_features()

#all data, adding yday, growing deg days, snow to binary, binary freezing
temps_meteo_joined <- dplyr::left_join(temps, meteo, by = "date") %>% augment_features() %>% 
  select(-DateTime)
n_obs <- nrow(temps_meteo_joined)

#training on middle third for now
#median_date <- median(temps_meteo_joined$date)
training <- slice(temps_meteo_joined, (n_obs/3):(2/3*n_obs)) 
test <- anti_join(temps_meteo_joined, training) 

train_targets <- training$Observed_temp
train_dates <- training$date
test_targets <- test$Observed_temp
test_dates <- test$date

#scaling all data based on training set --- might be ok here to scale test 
#based on itself, assuming the training set is representative of it?
train_data <- as.matrix(select(training, -Observed_temp, -date)) %>% scale()
test_data <- as.matrix(select(test, -Observed_temp, -date)) %>% 
  scale(center = attr(train_data, "scaled:center"), 
        scale = attr(train_data, "scaled:scale"))
all_model_unlabeled_data <- as.matrix(select(all_model_temps_drivers, -DateTime)) %>% 
  scale(center = attr(train_data, "scaled:center"), 
        scale = attr(train_data, "scaled:scale"))

#build the neural net
l2_lambda <- 0.5 #regularization factors, L1 and L2 regularization
l1_lambda <- 0.5
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
summary(network)
network %>% compile(
  optimizer = optimizer_adadelta(clipnorm = 1),
  loss = "mean_squared_error",
  metrics = "mean_squared_error") #keras objects are modified in place!

#fitting 
#paper: 10k max epochs, patience 500
history <- network %>% fit(train_data, train_targets, epochs = 5000, batch_size = 1000,
                callbacks = callback_early_stopping(patience = 500, monitor = "loss"),
                validation_split = 0.1)
preds_train <- network %>% predict(train_data)
preds_test <- network %>% predict(test_data)
preds_unlabeled_model <- network %>% predict(all_model_unlabeled_data) %>% as_tibble() %>% 
  rename(Modeled_temps = V1) %>% 
  bind_cols(all_model_temps_drivers[c("Depth", "DateTime")]) 

pred_temps <- preds_unlabeled_model %>% 
  tidyr::spread(key = Depth, value = "Modeled_temps",sep = "_") %>% 
  as.data.frame()

preds_obs_res <- left_join(preds_unlabeled_model, obs, by = c("DateTime", "Depth")) %>% 
  mutate(resid = Modeled_temps - temp) %>% 
  filter(!is.na(Modeled_temps)) %>% select(Depth, DateTime, resid) #%>% 
  #tidyr::spread(key = Depth, value = "resid",sep = "_") %>% as.data.frame()

# diff_temps <- all_model_temps_short
# n_cols <- ncol(all_model_temps_short)
# diff_temps[,2:n_cols] <- preds_unlabeled_model[,2:n_cols] - all_model_temps_short[,2:n_cols]    
#plot_heatmap_hack(all_model_temps_short, pred_temps, preds_obs_res)

#RMSEs
rmse <- function(x1,x2){sqrt(mean((x1 - x2)^2))}

glm_train_rmse <- rmse(training$Modeled_temp, training$Observed_temp)
glm_test_rmse <- rmse(test$Modeled_temp, test$Observed_temp)
nn_train_rmse <- rmse(preds_train, training$Observed_temp)
nn_test_rmse <- rmse(preds_test, test$Observed_temp)
results <- data.frame(Model = c("GLM", "NN"), Train_RMSE = c(glm_train_rmse, nn_train_rmse),
           Test_RMSE = c(glm_test_rmse, nn_test_rmse))
print(paste("Training obs: ", nrow(training)))
print(paste("Test obs:", nrow(test)))
print(results)
training_res <- bind_cols(date = train_dates, preds = preds_train, obs = training$Observed_temp) %>% 
  mutate(abs_res = abs(preds_train - obs), res = preds_train - obs, month = lubridate::month(date)) 
 
library(ggplot2)
colscale <- scale_fill_gradientn(colours = c("violet","blue","cyan", "green3", "yellow", "orange", "red"),
                     breaks = seq(-10,15,5), values = scales::rescale(seq(-10,15,5)),
                     limits = c(-10, 20))
nn_resid_plot <- ggplot(preds_obs_res, aes(x = DateTime, y = Depth)) + geom_tile(aes(fill=resid)) + 
  colscale + scale_y_reverse() + ggtitle("Modeled w/NN - observed")
temps_resids <- all_model_temps %>% left_join(obs, by = c("DateTime", "Depth")) %>% 
  mutate(resid = Modeled_temp - temp)
glm_resid_plot <- ggplot(temps_resids, aes(x = DateTime, y = Depth)) + geom_tile(aes(fill=resid)) + 
   colscale + scale_y_reverse() + ggtitle("GLM - observed")
png("glm_nn_resid_compare.png", width = 20, height = 12, units = "in", res = 200)
gridExtra::grid.arrange(glm_resid_plot, nn_resid_plot, nrow=2)
dev.off()

all_resids <- temps_resids %>% rename(glm_resid = resid) %>% 
  left_join(preds_obs_res, c("DateTime", "Depth")) %>% 
  mutate(abs_glm_resid_minus_abs_nn_resid = abs(glm_resid) - abs(resid))
ggplot(all_resids, aes(x = DateTime, y = Depth)) + 
  geom_tile(aes(fill=abs_glm_resid_minus_abs_nn_resid)) + 
  scale_y_reverse() + ggtitle("abs(GLM residual) - abs(NN residual)") + 
  scale_fill_gradient2(name = "Deg. C")
