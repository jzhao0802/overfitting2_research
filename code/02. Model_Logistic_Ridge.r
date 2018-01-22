
rm(list=ls())

library(glmnet)
library(xlsx)
library(snowfall)
options(digits=7)

source('C:\\work\\working materials\\overfitting2\\code\\strata1.sampling.R')
#logistic regression and Ridge regression
path_output <- 'C:\\work\\working materials\\overfitting2\\model_output'
setwd(path_output)
#check correlation matrix
corr <- function(thresh){
    raw_data <- read.table('data_forModel.csv', sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    
    
    covariance_matrix <- cor(raw_data)
    write.xlsx(covariance_matrix , 'covariance_matrix.xlsx', row.names=T, append=T, showNA=T)
    pdf("covariance_matrix.pdf", height=6, width=8, pointsize=12)
    heatmap(covariance_matrix)
    dev.off()
    
    high_corr <- numeric()
    for(i in 2:ncol(raw_data)-1){
        for(j in (i+1):ncol(raw_data)){
            if(covariance_matrix[i,j] > thresh){
                #print(paste(covar_forCorr[i] , covar_forCorr[j] , sep=' : '))
                high_corr <- rbind(high_corr, c(Var1=names(raw_data)[i], Var2=names(raw_data)[j], Corr=covariance_matrix[i,j]))
            }
        }
    }
    high_corr <- as.data.frame(high_corr)
    high_corr <- high_corr[order(high_corr[, 3], decreasing=T), ]
    write.xlsx(high_corr , 'high_covariance_list.xlsx',row.names=T, append=T, showNA=T)
    return(high_corr)
    
}
output1 <- corr(0.3)

#check the distribution of binary variables including odds ratio
table_check_byAllBinaryCovar <- function(){
    raw_data <- read.table('data_forModel.csv', sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    
    #raw_data_omitMiss <- na.omit(raw_data)
    #var_list <- names(raw_data_omitMiss)
    
    key <- numeric()
    pos_pos_lst <- numeric()
    pos_pos_numeric_lst <- numeric()
    response <- raw_data[, 'response']
    num_response_1 <- sum(response)
    num_response_0 <- sum(1-response)
    for(i in setdiff(names(raw_data), 'response')){
        eval(parse(text=paste('var <- raw_data$', i, sep='')))
        tb <- table(var, response)
        if(dim(tb)[1] ==1 & '0' %in% rownames(tb)){
            pos <- 0
            neg <- sum(tb)
            pos_pos <- 0
            covar1_response1 <- 0
            covar0_response1 <- tb[1,2]
            covar1_response0 <- 0
            covar0_response0 <- tb[1,1]
            odds_ratio_my <- ((covar1_response1/sum(tb))/(covar1_response0/sum(tb)))/((covar0_response1/pos)/(covar0_response0/pos))    #odds_ratio=odds(x==1)/odds(x==0) = ((p(Y=1|x=1)/p(Y=0|x=1))/(p(Y=1|x=0)/p(Y=0|x=0))
            
        }else{
            pos <- sum(tb[2,])
            neg <- sum(tb[1,])
            pos_pos <- tb[2,2]
            covar1_response1 <- tb[2,2]
            covar0_response1 <- tb[1,2]
            covar1_response0 <- tb[2,1]
            covar0_response0 <- tb[1,1]
            odds_ratio_my <- ((covar1_response1/sum(tb[1, ]))/(covar1_response0/sum(tb[1, ])))/((covar0_response1/sum(tb[2, ]))/(covar0_response0/sum(tb[2, ])))    #odds_ratio=odds(x==1)/odds(x==0) = ((p(Y=1|x=1)/p(Y=0|x=1))/(p(Y=1|x=0)/p(Y=0|x=0))
        }
        #odds_ratio_my <- ((covar1_response1/sum(tb[1, ]))/(covar1_response0/sum(tb[1, ])))/((covar0_response1/sum(tb[2, ]))/(covar0_response0/sum(tb[2, ])))    #odds_ratio=odds(x==1)/odds(x==0) = ((p(Y=1|x=1)/p(Y=0|x=1))/(p(Y=1|x=0)/p(Y=0|x=0))
        contigency_table<- matrix(c(covar1_response1, covar0_response1, covar1_response0, covar0_response0), nc=2, byrow=T)
        association_test<- fisher.test(contigency_table , alternative = "two.sided")
        #association_test <- fisher.test(x=as.factor(var), y=as.factor(response), alternative='two.sided')
        #association_test <- fisher.test(x=as.factor(response), y=as.factor(var), alternative='two.sided')
        
        p_value<- association_test$p.value
        odds_ratio<- as.vector(association_test$estimate)
                
        pos_pos_numeric_lst <- rbind(pos_pos_numeric_lst, c(num_response_0=num_response_0, num_response_1=num_response_1, 
                                                            num_of_BinaryCovar_0=neg, num_of_BinaryCovar_1=pos, num_of_BinaryCovar_1_response_0=covar1_response0, num_of_BinaryCovar_1_response_1=pos_pos, Odds_ratio=odds_ratio, P_value=p_value))
        #pos_pos_lst <- rbind(pos_pos_lst, c(cohort=cohort, response_variable=resp, binary_variable=i, num_response_0=num_response_0, num_response_1=num_response_1, 
        #                                   num_of_BinaryCovar_0=neg, num_of_BinaryCovar_1=pos, num_of_BinaryCovar_1_response_1=pos_pos))
        
        
    }
    #row.names(data.frame(pos_pos_numeric_lst)) <- NULL
    #tb_summary <- as.data.frame(pos_pos_lst)
    tb_summary <- as.data.frame(pos_pos_numeric_lst)
    rownames(tb_summary) <- setdiff(names(raw_data), 'response')
    #tb_summary$Definition <- covar_definition[match(tb_summary$binary_variable, covar_definition[, 1]), 2]
    write.xlsx(tb_summary, file='Descriptive_table_allBinary.xlsx', row.names=T, append=T, showNA=T)
    
    return(tb_summary)
}

table_check_byBinaryCovar_1 <- table_check_byAllBinaryCovar()
#5 covariates have o patient number in response==1 and covar=1
#?delete? or how to process such covariates

#Logistic regression model using ten-folds CV

sfLibrary(snowfall)
sfLibrary(glmnet)
sfLibrary(xlsx)
sfSource('C:\\work\\working materials\\overfitting2\\code\\strata_sampling.R')

#remove variables with abs(coeff)>5 for more than 10 times in all the 50 times repeat.

dataset_all <- c('data_forModel')
dataset <- 1
time.start <- Sys.time()
cat('logistic start time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')
logsitic_covarDrop <- function(dataset, n.repeat){
    raw_data <- read.table(paste(dataset_all[dataset], '.csv', sep=''), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    response <- raw_data[, 'response']
    covar_list_model <- setdiff(names(raw_data), 'response')
    model_data <- raw_data
    na_check <- apply(apply(model_data, 2, is.na), 2, sum)
    n.covar <- ncol(model_data)-1
    max.df <- nrow(model_data)/n.covar
    resp.prop <- sum(response==1)/nrow(model_data)
    pvalue_thresh <- 0.05
    table <- numeric()
    covar_drop_list <- list()
    for(df in seq(5, max.df, 5)){
        cat('degree of freedom: ', df, ' now starting!\n')
        df_outcome <- df*resp.prop
        #parallel the repeat sampling part
        num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
        sfInit(parallel=TRUE, cpus=num_pros, type="SOCK")
        sfLibrary(snowfall)
        sfLibrary(ROCR)
        sfLibrary(glmnet)
        sfLibrary(xlsx)
        sfClusterEval(library("ROCR"))
        sfClusterEval(library("glmnet"))
        
        sfSource('C:\\work\\working materials\\overfitting2\\code\\strata1.sampling.R')
        
        
        sfExport('strata2.sampling', 'model_data', 'df', 'n.covar', 'pvalue_thresh')
        
        #cumul_cnt_repeat <- numeric(n.covar)
        run_repeat <- function(times){
            #sampling training data and test data
            temp <- strata2.sampling(model_data, df, n.covar)
            training_data <- temp[temp$training_flag==1, setdiff(names(model_data), 'training_flag')]
            test_data <- temp[temp$training_flag==0, setdiff(names(model_data), 'training_flag')]
            #get esitmate auc (estimate_auc) using 10 folds CV
            #10 folds cross-validation
            n.fold <- 10
            
            temp2 <- strata2.sampling(data=training_data, df=df, n.covar=n.covar, k=n.fold)
            training_data <- temp2[, -ncol(temp2)]
            foldid <- temp2$foldid
            
            cumul_cnt <- numeric(n.covar)
            for(i in 1:n.fold){
                 cv_training_data<- training_data[foldid!=i,]
                 cv_test_data<- training_data[foldid==i,]
                 #stepwise
                 fit_std<- glm(response~., data=cv_training_data, family=binomial)
                 cnt <- ifelse(abs(coef(fit_std)[-1]) > 5 | is.na(coef(fit_std)[-1]), 1, 0) 
                     
                 cumul_cnt <- cumul_cnt+cnt
                 
            }
            cnt_repeat <- ifelse(cumul_cnt >3, 1, 0)
            #cumul_cnt_repeat <- cumul_cnt_repeat+cnt_repeat
            return(cnt_repeat)

        }
        drop_lst <- sfClusterApplyLB(1:n.repeat, run_repeat)
        drop_df <- as.data.frame(drop_lst)
        cnt <- apply(drop_df, 1, sum)
        covar_drop <- names(cnt[cnt>10])
        eval(parse(text=paste('covar_drop_list$df', df, '<- covar_drop', sep='')))
    }
    return(covar_drop_list)    
    
}
covar_drop <- logsitic_covarDrop(1, 50)
covar_drop_num <- unlist(lapply(1:length(covar_drop), function(X){length(covar_drop[[X]])}))

dataset_all <- c('data_forModel')
dataset <- 1
time.start <- Sys.time()
cat('logistic start time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')
logistic_v1 <- function(dataset, n.repeat){
    raw_data <- read.table(paste(dataset_all[dataset], '.csv', sep=''), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    response <- raw_data[, 'response']
    covar_list_model <- setdiff(names(raw_data), 'response')
    model_data0 <- raw_data
    na_check <- apply(apply(model_data0, 2, is.na), 2, sum)
    n.covar <- ncol(model_data0)-1
    max.df <- nrow(model_data0)/n.covar
    resp.prop <- sum(response==1)/nrow(model_data0)
    pvalue_thresh <- 0.05
    table <- numeric()
    coef_list <- list()
    for(df in seq(5, max.df, 5)){
        cat('degree of freedom: ', df, ' now starting!\n')
        df_outcome <- df*resp.prop
        #drop the covariates selected by |coef| >5
        #eval(parse(paste('covar_drop <- covar_drop$df', df, sep='')))
        #eval(parse(text=paste('model_data <- subset(model_data, select=-covar_drop$df', df, ')', sep='')))
        eval(parse(text=paste('model_data <- model_data0[, setdiff(names(model_data0), covar_drop$df', df, ')]', sep='')))
        #parallel the repeat sampling part
        num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
        sfInit(parallel=TRUE, cpus=num_pros, type="SOCK")
        sfLibrary(snowfall)
        sfLibrary(ROCR)
        sfLibrary(glmnet)
        sfLibrary(xlsx)
        sfClusterEval(library("ROCR"))
        sfClusterEval(library("glmnet"))
        
        sfSource('C:\\work\\working materials\\overfitting2\\code\\strata1.sampling.R')
        
        
        sfExport('strata2.sampling', 'model_data', 'df', 'n.covar', 'pvalue_thresh')
        
        
        run_repeat <- function(times){
            #sampling training data and test data
            temp <- strata2.sampling(model_data, df, n.covar)
            training_data <- temp[temp$training_flag==1, setdiff(names(model_data), 'training_flag')]
            test_data <- temp[temp$training_flag==0, setdiff(names(model_data), 'training_flag')]
            
            #fit logistic regression on training
            fit<- glm(response~., data=training_data, family=binomial)
            #get training auc
            training_pred<- predict(fit, training_data, type="response")
            training_auc<- auc(as.numeric(as.vector(training_data$response)) , training_pred)
            #get test auc
            test_pred <- predict(fit, test_data, type='response')
            test_auc<- auc(as.numeric(as.vector(test_data$response)) , test_pred)
            #get significant num and odds ratio on treatment variable
            coef<- data.frame(Coefficient=round(coef(fit), 7) , Odds_ratio=round(exp(coef(fit)), 7))
            Coefficient=round(coef(fit), 7)
            odds_ratio=round(exp(coef(fit)), 7)
            p_value<- round(summary(fit)$coef[, "Pr(>|z|)"], 7)
            
            sign_lg<- ifelse(p_value[match('treatment', names(p_value))] <= pvalue_thresh , 1 , 0)
            OR_lg<- odds_ratio[match('treatment', names(odds_ratio))]
            
            
            #get esitmate auc (estimate_auc) using 10 folds CV
            #10 folds cross-validation
            n.fold <- 10
            
            temp2 <- strata2.sampling(data=training_data, df=df, n.covar=n.covar, k=n.fold)
            training_data <- temp2[, -ncol(temp2)]
            foldid <- temp2$foldid
            test_pred_bind <- numeric()
            test_resp <- numeric()
            coef_sum <- numeric(length=ncol(training_data)-1)
            for(i in 1:n.fold){
                err_cat <- tryCatch(
                {                cv_training_data<- training_data[foldid!=i,]
                                 cv_test_data<- training_data[foldid==i,]
                                 test_resp <- c(test_resp, as.vector(cv_test_data[, 'response']))
                                 #stepwise
                                 fit_std<- glm(response~., data=cv_training_data, family=binomial)
                                 #coef<- data.frame(Coefficient=round(coef(fit_std), 7) , Odds_ratio=round(exp(coef(fit_std)), 7))
                                 coef <- coef(fit_std)
                                 coef_sum <- coef_sum+coef
                                 #training_pred<- predict(fit_std, cv_training_data, type="response")
                                 test_pred<- predict(fit_std, cv_test_data, type="response")
                                 
                                 test_pred_bind <- c(test_pred_bind, test_pred)
                                 
                    
                }, 
                error=function(e) e)
                if(inherits(err_cat, 'error')) next
                
            }
            coef_fold <- coef_sum/n.fold
            
            esti_auc<- auc(as.numeric(test_resp) , test_pred_bind)
            #output the parallel list
            repeat_out_i <- list(auc = c(repeatId=times, training_auc=training_auc, test_auc=test_auc, esti_auc =esti_auc, sigNum=sign_lg, OR=OR_lg), coef_repeat_i=coef_fold)
            return(repeat_out_i)
            
        }
        repeat_result <- sfClusterApplyLB(1:n.repeat, run_repeat)
        repeat_result1 <- as.data.frame(t(as.data.frame(lapply(1:length(repeat_result), function(X)repeat_result[[X]][[1]]))))
        rownames(repeat_result1) <- NULL
        colnames(repeat_result1) <- c('repeatId', 'training_auc', 'test_auc', 'esti_auc', 'sigNum', 'OR')
        esti_auc <- mean(repeat_result1$esti_auc)
        training_auc <- mean(repeat_result1$training_auc)
        test_auc<- mean(repeat_result1$test_auc)
        diff_auc <- training_auc-test_auc
        rate_auc <- diff_auc/training_auc
        treatment_sigNum <- sum(repeat_result1$sigNum, na.rm=T)
        treatment_OR <- mean(repeat_result1$OR, na.rm=T)
        
        table <- rbind(table, c(dataset=dataset, df1=df, df2=df_outcome, training_auc=training_auc, test_auc=test_auc, estimate_auc=esti_auc, 
                                diff_auc=diff_auc, rate_auc=rate_auc, treatment_sigNum=treatment_sigNum, treatment_OR=treatment_OR))
        repeat_coef <- as.data.frame(lapply(1:length(repeat_result), function(X)repeat_result[[X]][[2]]))

        df_coef <- apply(repeat_coef, 1, mean)
        eval(parse(text=paste('coef_list$df', df, ' <- df_coef', sep='')))
        
    }
    colnames(table) <- c('Dataset', 'Degrees of freedom based on obs', 'Degrees of freedom based on outcomes', 'AUC on training', 'AUC on test',  'Estimate AUC', 'Difference in AUC between training and test',
                         '% difference between AUC on training and test', '# of times treatment variable significant', 'Mean OR for treatment variable')
    #write.xlsx(table , 'overfitting_result_std_logistic_rmvCovar.xlsx', sheetName=paste('dataset_', dataset, sep=''), row.names=F, append=T, showNA=T)
    
    return(list(table=table, coef=coef_list))
    
}

logistic_result <- logistic_v1(1, 50)
auc_result <- logistic_result[[1]]
coef_list <- logistic_result[[2]]
num_coef_retain <- unlist(lapply(coef_list, function(X)length(X)))
auc_result1 <- cbind(auc_result, num_coef_retain)
colnames(auc_result1)<- c(colnames(auc_result), '# of covar retained')
#write.xlsx(auc_result , 'overfitting_result_std_logistic_rmvCovar.xlsx', sheetName=paste('dataset_', dataset, sep=''), row.names=F, append=T, showNA=T)
write.csv(auc_result1, 'overfitting_result_std_logistic_rmvCovar.csv', row.names=F)
save(coef_list, file='logisticCoef_afterRemoveCovar.RData')
cat('logistic end time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')
cat('logistic used time is ', (Sys.time()-time.start), '\n')
#logistic used time is  16min


dataset_all <- c('data_forModel')
dataset <- 1
time.start <- Sys.time()
cat('logistic start time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')

ridge_v1 <- function(dataset, n.repeat){
    
    raw_data <- read.table(paste(dataset_all[dataset], '.csv', sep=''), sep=',', head=T)
    names(raw_data)<- tolower(names(raw_data))
    
    dim(raw_data)
    na_check <- apply(apply(raw_data, 2, is.na), 2, sum)
    response <- raw_data[, 'response']
    covar_list_model <- setdiff(names(raw_data), 'response')
    model_data <- raw_data
    na_check <- apply(apply(model_data, 2, is.na), 2, sum)
    n.covar <- ncol(model_data)-2
    max.df <- nrow(model_data)/n.covar
    resp.prop <- sum(response==1)/nrow(model_data)
    
    table <- numeric()
    
    
    for(df in seq(5, max.df, 5)){
        df_outcome <- df*resp.prop
        #parallel the repeat sampling part
        num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
        sfInit(parallel=TRUE, cpus=num_pros, type="SOCK")
        sfLibrary(snowfall)
        sfLibrary(glmnet)
        sfLibrary(xlsx)
        sfClusterEval(library("glmnet"))
        
        sfSource('C:\\work\\working materials\\overfitting2\\code\\strata1.sampling.R')
        
        
        sfExport('strata2.sampling', 'model_data', 'df', 'n.covar')
       
        run_repeat <- function(times){    
            #sampling training data and test data
            temp <- strata2.sampling(model_data, df, n.covar)
            training_data <- temp[temp$training_flag==1, setdiff(names(model_data), 'training_flag')]
            test_data <- temp[temp$training_flag==0, setdiff(names(model_data), 'training_flag')]
            
            k.folds<- 10                                                           # tenfold cross-validation
            temp2 <- strata2.sampling(training_data, df=df, n.covar=n.covar, k=k.folds)
            training_data <- temp2[, -ncol(temp2)]
            foldid <- temp2$foldid
            training_data_lasso <- training_data
            test_data_lasso <- test_data
            
            table(training_data_lasso$response , foldid) # QC
            
            # Calculating initial lambda and the lambda sequence
            training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
            test_matrix<- model.matrix(response~., data=test_data_lasso)[,-1]     # removes intercept term
            
            initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=0, standardize=F)$lambda  # calculating the initial lambda
            lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500)) # get a length=500 descending sequence from initial lambda to 0
            cv_auc <- matrix(nr=k.folds , nc=length(lambda_seq))
            
            # Tenfold cross-validation
            for(i in 1:k.folds){
                cv_training_data_lasso<- training_data_lasso[foldid!=i,]                                            # select 9 folds as training data
                cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
                cv_test_data_lasso<- training_data_lasso[foldid==i,]                                                # select 1 fold as test data
                cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1]
                
                fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                                   lambda=lambda_seq, family="binomial", alpha=0, standardize=F)
                test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
                test_pred_avg<- apply(test_pred, 2, function(x){auc(as.numeric(as.vector(cv_test_data_lasso$response)) , x)}) #length= num of lambda
                test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
                
                cv_auc[i,]<- test_pred_avg                                                              # calculate the AUC based on left-out fold
            }
            
            total_model<- glmnet(x=training_matrix, y=training_data_lasso$response, lambda=lambda_seq, family="binomial", alpha=0, standardize=F)
            cv_auc_mean<- apply(cv_auc , 2 , function(x) mean(x[!is.na(x)]))                          # mean of the AUC by each lambda 
            optimum_model<- which.max(cv_auc_mean)  # select the lambda with largest AUC as the optimum one
            cv_auc_mean_optimum <- mean(cv_auc_mean[optimum_model]) #please be carefull here 
            #list_lasso_lambda<- rbind(list_lasso_lambda , c(times , lambda_seq[optimum_model]))# store optimum lambda sequence 
            #cv_auc_rep_list <- rbind(cv_auc_rep_list, cv_auc_mean_optimum) #marked when parallel running for repeat exacute.
            training_obs<- predict(total_model, training_matrix, type="response")[,optimum_model]
            training_auc<- auc(as.numeric(as.vector(training_data$response)) , training_obs)
            #training_auc_rep_list <- rbind(training_auc_rep_list, training_auc)#marked when parallel running for repeat exacute.
            #get test_auc
            test_obs<- predict(total_model, test_matrix, type="response")[,optimum_model]
            test_auc<- auc(as.numeric(as.vector(test_data$response)) , test_obs)
            #test_auc_rep_list <- rbind(test_auc_rep_list, test_auc) #marked when parallel running for repeat exacute.
            
            #refit training data using logistic regression to get the significant condition of treatment varaible
            # calculate p-value by re-estimate standard logistic model based on non-zero variables by using the optimum lambda
            pvalue_thresh <- 0.05
            model_coef<- total_model$beta[,optimum_model]
            odds_ratio<- exp(model_coef)[model_coef != 0]
            non_zero_var<- names(model_coef)[model_coef != 0]
            re_model_var<- c('response', non_zero_var)
            re_fit<- glm(response~., data=training_data_lasso[,match(re_model_var , names(training_data_lasso))], family=binomial)
            p_value<- summary(re_fit)$coef[, "Pr(>|z|)"]
            
            retain_lasso<- ifelse('treatment' %in% non_zero_var , 1 , 0)
            if(retain_lasso == 0){
                sign_lasso<- NA
                OR_lasso<- NA
            }else{
                sign_lasso<- ifelse(p_value[match('treatment', names(p_value))] <= pvalue_thresh , 1 , 0)
                OR_lasso<- odds_ratio[match('treatment', names(odds_ratio))]
            }
            #retain_lasso_rep <- c(retain_lasso_rep, retain_lasso) #marked when parallel running for repeat exacute.
            #sign_lasso_rep <- c(sign_lasso_rep, sign_lasso) #marked when parallel running for repeat exacute.
            #OR_lasso_rep <- c(OR_lasso_rep, OR_lasso) #marked when parallel running for repeat exacute.
                        
            #output the parallel list
            repeat_out_i <- c(times, training_auc, test_auc, cv_auc_mean_optimum, retain_lasso, sign_lasso, OR_lasso)
            return(repeat_out_i)
            
        }
        repeat_result <- sfClusterApplyLB(1:n.repeat, run_repeat)
        repeat_result1 <- as.data.frame(t(as.data.frame(repeat_result)))
        rownames(repeat_result1) <- NULL
        colnames(repeat_result1) <- c('repeatId', 'training_auc', 'test_auc', 'esti_auc', 'retainNum', 'sigNum', 'OR')
        #cv_auc_rep_vct <- as.vector(cv_auc_rep_list)
        #cv_auc_sd <- sd(cv_auc_rep_vct)
        esti_auc <- mean(repeat_result1$esti_auc)
        training_auc <- mean(repeat_result1$training_auc)
        test_auc<- mean(repeat_result1$test_auc)
        diff_auc <- training_auc-test_auc
        rate_auc <- diff_auc/training_auc
        treatment_retainNum <- sum(repeat_result1$retainNum, na.rm=T)
        treatment_sigNum <- sum(repeat_result1$sigNum, na.rm=T)
        treatment_OR <- mean(repeat_result1$OR, na.rm=T)
        table <- rbind(table, cbind(dataset=dataset, df1=df, df2=df_outcome, training_auc=training_auc, test_auc=test_auc, estimate_auc=esti_auc, 
                       diff_auc=diff_auc, rate_auc=rate_auc, treatment_retainNum=treatment_retainNum, treatment_sigNum=treatment_sigNum, treatment_OR=treatment_OR))
        
    }
    colnames(table) <- c('Dataset', 'Degrees of freedom based on obs', 'Degrees of freedom based on outcomes', 'AUC on training', 'AUC on test',  'Estimate AUC', 'Difference in AUC between training and test',
                          '% difference between AUC on training and test', '# of times treatment variable retained' , '# of times treatment variable significant', 'Mean OR for treatment variable')
    #write.xlsx(table , 'overfitting_result_ridge.xlsx', sheetName=paste('dataset_', dataset, sep=''), row.names=F, append=T, showNA=T)
    #write.csv(table, 'overfitting_result_ridge.csv', row.names=F)
    return(table)
}
ridge_result <- ridge_v1(1, 50)
write.csv(ridge_result, 'overfitting_result_ridge.csv', row.names=F)
cat('logistic end time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')
cat('logistic used time is ', (Sys.time()-time.start), '\n')


