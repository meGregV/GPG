if(!require(tidyverse)) install.packages('tidyverse', repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages('caret', repos = "http://cran.us.r-project.org")
if(!require(doParallel)) install.packages('doParallel', repos = "http://cran.us.r-project.org")

if(!require(pls)) install.packages('pls', repos = "http://cran.us.r-project.org")
if(!require(elasticnet)) install.packages('elasticnet', repos = "http://cran.us.r-project.org")

if(!require(nnet)) install.packages('nnet', repos = "http://cran.us.r-project.org")
if(!require(earth)) install.packages('earth', repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages('kernlab', repos = "http://cran.us.r-project.org")

if(!require(party)) install.packages('party', repos = "http://cran.us.r-project.org")
if(!require(RWeka)) install.packages('RWeka', repos = "http://cran.us.r-project.org")
if(!require(ipred)) install.packages('ipred', repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages('randomForest', repos = "http://cran.us.r-project.org")
if(!require(Cubist)) install.packages('Cubist', repos = "http://cran.us.r-project.org")


##############################################
# grab data for modeling
# remove NA
##############################################
if (!file.exists('cleaned.RDS')){
  source('data.clean.R')}

cleaned  <- readRDS('cleaned.rds')
transformed <- readRDS('transformed.RDS')

dat <- cleaned %>% 
    select(c(DiffMeanHourlyPercent)) %>% 
    cbind(transformed) %>% 
    drop_na()

##############################################
#  Create model sets (split on train and test)
##############################################
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = dat$DiffMeanHourlyPercent,
                                  times = 1, p = 0.1, list = FALSE)  
GPG_train <- dat[-test_index,]
GPG_test <- dat[test_index,]

#  split data into predictor space and response
trainX <- select(GPG_train,!c(DiffMeanHourlyPercent))
trainY <- GPG_train$DiffMeanHourlyPercent

testX <- select(GPG_test,!c(DiffMeanHourlyPercent))
testY <- GPG_test$DiffMeanHourlyPercent


###############################################
# 2. Run multiple models in parallel 
# using the same cross validation
###############################################  

# The same 5 fold cross-validation were used for each model. 
ctrl <-  trainControl(method="cv", number = 5)

# parallel processing 
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores(logical = T))  ## for example, my machine has 4 cores
registerDoParallel(cl)  ## All subsequent models are then run in parallel

###################### LINEAR MODELS and variations #####################
set.seed(1, sample.kind="Rounding")
train_lm <- train(DiffMeanHourlyPercent ~ ., method = 'lm', data = GPG_train,
                  trControl = ctrl) ## Ordinary Least Squares

set.seed(1, sample.kind="Rounding")
train_rlm <- train(DiffMeanHourlyPercent ~ ., method = 'rlm', data = GPG_train,
                   trControl = ctrl) ## Robust Linear Regression w/ cv

set.seed(1, sample.kind="Rounding")
train_pls <- train(DiffMeanHourlyPercent ~ ., method = 'pls', data = GPG_train,
                   trControl = ctrl) ## Partial Least Squares

set.seed(1, sample.kind="Rounding")
enetGrid <- expand.grid(.lambda = c(0, 0.01, .1),  
                        .fraction = seq(.05, 1, length = 20))
train_enet<- train(DiffMeanHourlyPercent ~ ., method = 'enet', data = GPG_train,
                   tuneGrid = enetGrid,
                   trControl = ctrl) ## elastic net - ridge/lasso regression

##################### NON-LINEAR MODELS ##########################
set.seed(1, sample.kind="Rounding")
nnetGrid <- expand.grid(.decay = c(0, 0.01, .1), 
                        .size = c(1:5),
                        .bag = FALSE)
train_nn <- train(DiffMeanHourlyPercent ~ ., method = 'avNNet', data = GPG_train,
                  tuneGrid = nnetGrid,
                  linout = TRUE,
                  trace=FALSE,
                  maxit= 500,
                  MaxNWts = 5 * (ncol(trainX) + 1) + 5 + 1,
                  trControl=ctrl) ## averaged neural networks

set.seed(1, sample.kind="Rounding")
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
train_mars <-train(DiffMeanHourlyPercent ~ ., method = 'earth', data = GPG_train, 
                   tuneGrid = marsGrid,
                   trControl = ctrl)  ## Multivariate Adaptive Regression Splines

set.seed(1, sample.kind="Rounding")
train_svmR <- train(DiffMeanHourlyPercent ~ ., method = 'svmRadial', data = GPG_train, 
                    tuneLength = 14,
                    trControl = ctrl) ## support vector machines - radial kernel

set.seed(1, sample.kind="Rounding")
svmGrid <- expand.grid(degree = 1:2, 
                       scale = c(0.01, 0.005, 0.001), 
                       C = 2^(-2:5))
train_svmP <- train(DiffMeanHourlyPercent ~ .,method = 'svmPoly', data = GPG_train, 
                  tuneGrid = svmGrid,
                  trControl = ctrl) ## SVM - poly kernel

set.seed(1, sample.kind="Rounding")
train_knn <- train(DiffMeanHourlyPercent ~ ., method = 'knn', data = GPG_train, 
                         tuneGrid = data.frame(.k = 1:20),
                         trControl = ctrl) ## K-nearest Neighbors

################## REGRESSION TREES and RULE-BASED MODELS ############################
train_m5 <- train(DiffMeanHourlyPercent ~ .,method = 'M5', data = GPG_train, ## model tree
                   trControl = ctrl)
# plot(train_m5)
set.seed(1, sample.kind="Rounding")
train_treebag <- train(DiffMeanHourlyPercent ~ .,method = 'treebag', data = GPG_train, ## bagged tree
                       nbagg = 50,
                       trControl = ctrl)

set.seed(1, sample.kind="Rounding")
mtryGrid <- data.frame(mtry = floor(seq(5, ncol(trainX), length = 5))) ## random forest
train_rf <- train(DiffMeanHourlyPercent ~ ., method = 'rf', data = GPG_train,
                  tuneGrid = mtryGrid,
                  ntree = 1000,
                  importance = TRUE,
                  trControl = ctrl)


set.seed(1, sample.kind="Rounding")
cbGrid <- expand.grid(committees = c(1:10, 20, 50, 75, 100), 
                      neighbors = c(0, 1, 5, 9))
train_cubist <- train (DiffMeanHourlyPercent ~ ., method = 'cubist', data = GPG_train,
                       tuneGrid = cbGrid,
                       trControl = ctrl)

stopCluster(cl) ## When you are done
registerDoSEQ()  ## if issues stopping

##############################################################
#  Assess Models and their performance for all trained models
##############################################################
train_models <- list('Linear Reg' = train_lm,
                     'Robust Linear' = train_rlm,
                     'Partial Least Squares' = train_pls,
                     'Elastic Net' = train_enet,
                     'Neural Networks' = train_nn,
                     'MARS' = train_mars,
                     'SVM Radial' = train_svmR,
                     'SVM Poly' = train_svmP,
                     'KNN'= train_knn,
                     'Model trees' = train_m5,
                     'Bagged Tree' = train_treebag,
                     'Random Forest' = train_rf,
                     'Cubist' = train_cubist)

saveRDS(train_models, 'trained_models.rds')


train_resamps <-  resamples(train_models)

p1 <- bwplot(train_resamps, metric = 'RMSE')
p2 <- bwplot(train_resamps, metric = 'MAE')
p3 <- bwplot(train_resamps, metric = 'Rsquared')


summary(train_resamps)

predicted <-  lapply(train_models, function(x)  predict(x, newdata = GPG_test))
test_resamps <- sapply(predicted, function(x) postResample(x, testY))

############################################################
#  Assess selected Models performance on test data
############################################################

#  lm | Neural Networks | SVMr

selected_models <- train_models[c('Linear Reg', 'SVM Radial', 'Neural Networks')]
resample_train <- lapply(selected_models, function(x) postResample(predict(x, GPG_train), trainY))
resample_test <- lapply(selected_models, function(x) postResample(predict(x, GPG_test), testY))

trn <- cbind(model = names(resample_train), unnest_wider(tibble(models=resample_train), models)) 
trn[,-1] <- round(trn[,-1],2)
tst <- cbind(model = names(resample_test), unnest_wider(tibble(models=resample_test), models)) 
tst[,-1] <- round(tst[,-1],2)


trn['dataType'] = 'Train'
tst['dataType'] = 'Test'

#a <- tibble(merge(trn, tst,  by = 'model',
#           suffixes = c('.train', '.test')))


long_tbl <- rbind(trn, tst) %>%
  pivot_longer(cols =!c('model', 'dataType'), names_to = 'metric', values_to='value')

gg1 <- ggplot(long_tbl, aes(x=model, y=value, shape = dataType, colour = metric )) + 
  geom_point() +
  facet_wrap(~metric,scale="free_y",ncol=1) +
  theme_bw() +
  theme(panel.spacing=grid::unit(0,"lines"),
        strip.background=element_blank(), strip.text.x=element_blank(),
        axis.title = element_blank())

# obs vs predicted
#predVals <- extractPrediction(list(train_lm, train_nn, train_cubist), testX, testY)
#plotObsVsPred(predVals)

# residuals vs obs
results <- bind_rows(data.frame('obs'=trainY, 'pred'=train_lm$finalModel$fitted.values, 'model' = 'Linear_Reg', 'dataType' = 'Training'),
                     data.frame('obs'=testY, 'pred'=predict(train_lm, GPG_test),   'model' = 'Linear_Reg', 'dataType' = 'Test'),
                     data.frame('obs'=trainY, 'pred'=predict(train_nn, GPG_train), 'model' = 'Avg_Neural_Ntwks', 'dataType' = 'Training'),
                     data.frame('obs'=testY, 'pred'=predict(train_nn, GPG_test),   'model' = 'Avg_Neural_Ntwks', 'dataType' = 'Test'),
                     )

results['residuals']= results$obs - results$pred

#obs vs predicted
ggplot(results, aes(x=pred, y=obs)) +
  geom_point(alpha=1/5, colour='sienna3') +
  geom_abline(colour='blue') +
  facet_grid(rows=vars(dataType), cols= vars(model), switch = 'y', scales = 'free')  +
  theme_bw() +
  theme(strip.text.x = element_text(size=8, face='bold'),
        strip.text.y = element_text(size=10, face='bold'),
        strip.background = element_rect(colour='lightblue', fill='white'))

# residuals vs predicted
ggplot(results, aes(x=pred, y=residuals)) + 
  geom_point(alpha=1/8, colour='steelblue') + 
  geom_abline(slope = 0, colour='indianred') +
  facet_grid(rows=vars(dataType), cols= vars(model), switch = 'y', scales = 'free')  +
  theme_bw() +
  theme(strip.text.x = element_text(size=8, face='bold'),
        strip.text.y = element_text(size=10, face='bold'),
        strip.background = element_rect(colour='lightblue', fill='white'))



