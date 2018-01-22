strata2.sampling <- function(data, df, n.covar, k) {
  # @data: the data for sampling;
  # @df: degree of freedom, variation from 5 and increasing by 5 in each step;
  # @n.covar: number of covariates entering model;
  # @k: the number of fold, if df and n.covar missing
  

  if(!is.factor(data$response)){
      data$response <- as.factor(data$response)
  }
  require(caret)
  size <- df * n.covar
  portion = size / nrow(data)
  
  if (!is.data.frame(data)) stop("The data should be data frame format!!")
  if ((missing(df) & missing(n.covar)) & missing(k)) stop("Please provide the degree of freedom and 
                                                          # of covariates or the # of k fold")

  trt_tru <- subset(data, treatment == 1 & response == 1)
  trt_fal <- subset(data, treatment == 1 & response == 0)
  cnt_tru <- subset(data, treatment == 0 & response == 1)
  cnt_fal <- subset(data, treatment == 0 & response == 0)
  
  set.seed(12345)
  if (missing(k)) {
    trt_tru_training_flag <- rep(0, nrow(trt_tru))
    trt_tru_flag <- sample(1:nrow(trt_tru), round(portion * nrow(trt_tru)), replace = FALSE)
    trt_tru_training_flag[trt_tru_flag] <- 1
    # trt_tru_sample <- trt_tru[trt_tru_flag, ]
    
    trt_fal_training_flag <- rep(0, nrow(trt_fal))
    trt_fal_flag <- sample(1:nrow(trt_fal), round(portion * nrow(trt_fal)), replace = FALSE)
    trt_fal_training_flag[trt_fal_flag] <- 1 
    # trt_fal_sample <- trt_fal[trt_fal_flag, ]
    
    cnt_tru_training_flag <- rep(0, nrow(cnt_tru))
    cnt_tru_flag <- sample(1:nrow(cnt_tru), round(portion * nrow(cnt_tru)), replace = FALSE)
    cnt_tru_training_flag[cnt_tru_flag] <- 1
    # cnt_tru_sample <- cnt_tru[cnt_tru_flag, ]
    
    cnt_fal_training_flag <- rep(0, nrow(cnt_fal))
    cnt_fal_flag <- sample(1:nrow(cnt_fal), round(portion * nrow(cnt_fal)), replace = FALSE)
    cnt_fal_training_flag[cnt_fal_flag] <- 1
    # cnt_fal_sample <- cnt_fal[cnt_fal_flag, ]
    
    univ <- rbind(trt_tru, trt_fal, cnt_tru, cnt_fal)
    training_flag <- c(trt_tru_training_flag, trt_fal_training_flag, cnt_tru_training_flag,
                       cnt_fal_training_flag)

    return(data.frame(univ, training_flag = training_flag))
  }
  
  if (!missing(k)) {
    trt_tru_flag <- createFolds(trt_tru$response, k, FALSE)
    trt_tru_sample <- data.frame(trt_tru, foldid = trt_tru_flag)
    
    trt_fal_flag <- createFolds(trt_fal$response, k, FALSE)
    trt_fal_sample <- data.frame(trt_fal, foldid = trt_fal_flag)
    
    cnt_tru_flag <- createFolds(cnt_tru$response, k, FALSE)
    cnt_tru_sample <- data.frame(cnt_tru, foldid = cnt_tru_flag)
    
    cnt_fal_flag <- createFolds(cnt_fal$response, k, FALSE)
    cnt_fal_sample <- data.frame(cnt_fal, foldid = cnt_fal_flag)
    
    return(rbind(trt_tru_sample, trt_fal_sample, cnt_tru_sample, cnt_fal_sample))
  }
}
