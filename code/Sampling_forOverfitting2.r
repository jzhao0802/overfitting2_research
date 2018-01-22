#sampling training data and test data on condition that both treatment and response have the same proportion
degree=5
fold.num=10

sampling_onRespAndTreat <- function(data, setSeed=FALSE, seed, degree, forCV=FALSE, fold.num=10){
    raw_data <- data
    tb <- table(raw_data[, c('treatment', 'response')])
    num_treat0_resp0 <- tb[1,1]
    num_treat0_resp1 <- tb[1,2]
    num_treat1_resp0 <- tb[2,1]
    num_treat1_resp1 <- tb[2,2]
    idx_treat0_resp0 <- which(raw_data$treatment==0 & raw_data$response==0)
    idx_treat0_resp1 <- which(raw_data$treatment==0 & raw_data$response==1)
    idx_treat1_resp0 <- which(raw_data$treatment==1 & raw_data$response==0)
    idx_treat1_resp1 <- which(raw_data$treatment==1 & raw_data$response==1)
    #QC
    num_treat0_resp0+num_treat0_resp1+num_treat1_resp0+num_treat1_resp1==nrow(raw_data)
    length(c(idx_treat0_resp0,idx_treat0_resp1, idx_treat1_resp0, idx_treat1_resp1))==nrow(raw_data)
    
    if(forCV==FALSE){
        d <- degree
        n.covar <- length(names(raw_data))-1
        n.training <- d*n.covar
        prop.tr <- n.training/nrow(raw_data)
        #get each part of the cross table based on response and treatmentj
        training<- numeric(length=nrow(raw_data))
        if(setSeed==T){
            set.seed(seed)    
        }
        
        idx_treat0_resp0_training<- sample(idx_treat0_resp0 , num_treat0_resp0*prop.tr)
        idx_treat0_resp1_training<- sample(idx_treat0_resp1 , num_treat0_resp1*prop.tr)
        idx_treat1_resp0_training<- sample(idx_treat1_resp0 , num_treat1_resp0*prop.tr)
        idx_treat1_resp1_training<- sample(idx_treat1_resp1 , num_treat1_resp1*prop.tr)
        training[c(idx_treat0_resp0_training,idx_treat0_resp1_training,idx_treat1_resp0_training, idx_treat1_resp1_training)]=1
        table(raw_data[training==1,c('treatment', 'response')])
        table(raw_data[training==0,c('treatment', 'response')])
        #output the training data and test data
        dataset <- list(training_data=raw_data[training==1,], test_data=raw_data[training==0,])
        return(dataset)
        
    }else if(forCV==TRUE){
        if(setSeed==T){
            set.seed(seed)    
        }
        idx_treat0_resp0_foldid<- sample(rep(1:fold.num, length=num_treat0_resp0))
        idx_treat0_resp1_foldid<- sample(rep(1:fold.num, length=num_treat0_resp1))
        idx_treat1_resp0_foldid<- sample(rep(1:fold.num, length=num_treat1_resp0))
        idx_treat1_resp1_foldid<- sample(rep(1:fold.num, length=num_treat1_resp1))
        foldid <- c(idx_treat0_resp0_foldid, idx_treat0_resp1_foldid, idx_treat1_resp0_foldid, idx_treat1_resp1_foldid)
        #QC
        table(foldid)
        return(foldid)
    }
    
} 
tr <- sampling_onRespAndTreat(raw_data, degree=5, forCV=F)[[1]]
f <- sampling_onRespAndTreat(tr, forCV=T, fold.num=10)

