library(xlsx)


path_input <- 'C:\\work\\working materials\\presentation\\Gilenya vs Tecfidera POC\\data'
path_output <- 'C:\\work\\working materials\\overfitting2\\model_output'

file_input <- 'pred_analytics_file.csv'

dataset1 <- read.table(paste(path_input, file_input, sep='\\'), sep=',', header=T) #4429 83

raw_data <- dataset1

setwd(path_output)

# change names of all variables into lower case
names(raw_data)<- tolower(names(raw_data))

# qc raw data, data missing etc.
dim(raw_data)
which(is.na(raw_data) == T)

tr <- 'idx_rx'
resp <- 'post_relapse_persist'
colnames(raw_data)[which(names(raw_data)==tr)]='treatment'
colnames(raw_data)[which(names(raw_data)==resp)]='response'


# treatment variable: idx_rx=1 means the index medication is Gilenya
raw_data$treatment <- ifelse(raw_data$treatment==1, 1, 0)

# response variable: post_relapse_persist
#raw_data$response<- raw_data$post_relapse_persist

# get rid of row names
rownames(raw_data)<- NULL

#-----------------------------------------------------------------------
#    Make binary flags for all levels of non-binary categorical variables.
#------------------------------------------------------------------------

der_sex_female<- ifelse(raw_data$der_sex=='F' , 1 , 0)
der_sex_male<- ifelse(raw_data$der_sex=='M' , 1 , 0)

pat_region_E<- ifelse(raw_data$pat_region=='E' , 1 , 0)
pat_region_MW<- ifelse(raw_data$pat_region=='MW' , 1 , 0)
pat_region_S<- ifelse(raw_data$pat_region=='S' , 1 , 0)
pat_region_W<- ifelse(raw_data$pat_region=='W' , 1 , 0)
sum(pat_region_E + pat_region_MW + pat_region_S + pat_region_W) == length(raw_data$pat_region)

idx_paytype_C<- ifelse(raw_data$idx_paytype=='C' , 1 , 0)
idx_paytype_M<- ifelse(raw_data$idx_paytype=='M' , 1 , 0)
idx_paytype_R<- ifelse(raw_data$idx_paytype=='R' , 1 , 0)
idx_paytype_S<- ifelse(raw_data$idx_paytype=='S' , 1 , 0)
idx_paytype_U<- ifelse(raw_data$idx_paytype=='U' , 1 , 0)
sum(idx_paytype_C+ idx_paytype_M+ idx_paytype_R+ idx_paytype_S+ idx_paytype_U) == length(raw_data$idx_paytype)

idx_prodtype_D<- ifelse(raw_data$idx_prodtype=='D' , 1 , 0)
idx_prodtype_H<- ifelse(raw_data$idx_prodtype=='H' , 1 , 0)
idx_prodtype_I<- ifelse(raw_data$idx_prodtype=='I' , 1 , 0)
idx_prodtype_P<- ifelse(raw_data$idx_prodtype=='P' , 1 , 0)
idx_prodtype_S<- ifelse(raw_data$idx_prodtype=='S' , 1 , 0)
idx_prodtype_U<- ifelse(raw_data$idx_prodtype %in% c('U','R') , 1 , 0)
sum(idx_prodtype_D+ idx_prodtype_H+ idx_prodtype_I+ idx_prodtype_P+ idx_prodtype_S+ idx_prodtype_U) == length(raw_data$idx_prodtype)

# '01' = General practice/Family practice, '02' = Internal medicine, '03' = Neurology, '04' = Other, '05' = unknown"
idx_spec_01<- ifelse(raw_data$idx_spec==1 , 1 , 0)
idx_spec_02<- ifelse(raw_data$idx_spec==2 , 1 , 0)
idx_spec_03<- ifelse(raw_data$idx_spec==3 , 1 , 0)
idx_spec_04<- ifelse(raw_data$idx_spec==4 , 1 , 0)
idx_spec_05<- ifelse(raw_data$idx_spec==5 , 1 , 0)
sum(idx_spec_01 + idx_spec_02 + idx_spec_03 + idx_spec_04 + idx_spec_05) == length(raw_data$idx_spec)

new_create_binary_covar<- cbind(#non_persistent,
    der_sex_female,
    der_sex_male,
    pat_region_E,
    pat_region_MW,
    pat_region_S,
    pat_region_W,
    idx_paytype_C,
    idx_paytype_M,
    idx_paytype_R,
    idx_paytype_S,
    idx_paytype_U,
    idx_prodtype_D,
    idx_prodtype_H,
    idx_prodtype_I,
    idx_prodtype_P,
    idx_prodtype_S,
    idx_prodtype_U,
    idx_spec_01,
    idx_spec_02,
    idx_spec_03,
    idx_spec_04,
    idx_spec_05
)

#-----------------------------------------------------------------------
#    Create quartiles / bins for all continuous and count variables.
#	Make sure observations with the same value are in the same bin / quartile
#	For count variables make sure after quartile each cell have no less than 15% patients
#------------------------------------------------------------------------
quartiles<- function(variable){
    eval(parse(text = paste('re<- raw_data$' , variable , sep='')))
    q<- quantile(re , c(1,2,3)/4)
    x4<- ifelse(re >= q[3] , 1 , 0)
    x3<- ifelse(re >= q[2] & re < q[3] , 1 , 0)
    x2<- ifelse(re >= q[1] & re < q[2] , 1 , 0)
    x1<- ifelse(re < q[1] , 1 , 0)
    result<- data.frame(x1 , x2 , x3 , x4)
    colnames(result)<- paste(variable , 1:4 , sep='_')
    range_of_bin<- c(paste('<', q[1], sep=''), paste('[', q[1], ',', q[2], ')', sep=''), 
                     paste('[', q[2], ',', q[3], ')', sep=''), paste('>=', q[3], sep=''))
    return(result)
}

pre_non_ms_total_allowed_quartiles <- quartiles('pre_non_ms_total_allowed')
pre_ms_total_allowed_quartiles <- quartiles('pre_ms_total_allowed')
pre_non_ms_pharmacy_allowed_quartiles <- quartiles('pre_non_ms_pharmacy_allowed')
pre_ms_pharmacy_allowed_quartiles <- quartiles('pre_ms_pharmacy_allowed')
pre_non_ms_medical_allowed_quartiles <- quartiles('pre_non_ms_medical_allowed')
pre_ms_medical_allowed_quartiles <- quartiles('pre_ms_medical_allowed')

num_pre_meds_quartiles<- quartiles('num_pre_meds')
num_pre_op_dx_quartiles<- quartiles('num_pre_op_dx')

age_1<- ifelse(raw_data$age < 35 , 1 , 0)
age_2<- ifelse(raw_data$age >= 35 & raw_data$age <= 44 , 1 , 0)
age_3<- ifelse(raw_data$age >= 45 & raw_data$age <= 54 , 1 , 0)
age_4<- ifelse(raw_data$age >= 55 , 1 , 0)
age_quartiles<- data.frame(age_1, age_2, age_3, age_4)
sum(age_quartiles[,1]) + sum(age_quartiles[,2]) + sum(age_quartiles[,3]) + sum(age_quartiles[,4])

pchrlson_1<- ifelse(raw_data$pchrlson==0 , 1 , 0)
pchrlson_2<- ifelse(raw_data$pchrlson==1 , 1 , 0)
pchrlson_3<- ifelse(raw_data$pchrlson >= 2 , 1 , 0)
pchrlson_quartiles<- data.frame(pchrlson_1 , pchrlson_2 , pchrlson_3)

num_pre_mri_any_1<- ifelse(raw_data$num_pre_mri_any==0, 1, 0)
num_pre_mri_any_2<- ifelse(raw_data$num_pre_mri_any==1, 1, 0)
num_pre_mri_any_3<- ifelse(raw_data$num_pre_mri_any==2, 1, 0)
num_pre_mri_any_4<- ifelse(raw_data$num_pre_mri_any>=3, 1, 0)
num_pre_mri_any_quartiles<- data.frame(num_pre_mri_any_1, num_pre_mri_any_2, num_pre_mri_any_3, num_pre_mri_any_4)

num_pre_cort_oral_1<- ifelse(raw_data$num_pre_cort_oral==0, 1, 0)
num_pre_cort_oral_2<- ifelse(raw_data$num_pre_cort_oral==1, 1, 0)
num_pre_cort_oral_3<- ifelse(raw_data$num_pre_cort_oral>=2, 1, 0)
num_pre_cort_oral_quartiles<- data.frame(num_pre_cort_oral_1, num_pre_cort_oral_2, num_pre_cort_oral_3)

num_pre_cort_iv_1<- ifelse(raw_data$num_pre_cort_iv==0, 1, 0)
num_pre_cort_iv_2<- ifelse(raw_data$num_pre_cort_iv %in% 1:2, 1, 0)
num_pre_cort_iv_3<- ifelse(raw_data$num_pre_cort_iv>=3, 1, 0)
num_pre_cort_iv_quartiles<- data.frame(num_pre_cort_iv_1, num_pre_cort_iv_2, num_pre_cort_iv_3)

num_pre_relapses_1<- ifelse(raw_data$num_pre_relapses==0, 1 , 0)
num_pre_relapses_2<- ifelse(raw_data$num_pre_relapses==1, 1 , 0)
num_pre_relapses_3<- ifelse(raw_data$num_pre_relapses>=2, 1 , 0)
num_pre_relapses_quartiles<- data.frame(num_pre_relapses_1, num_pre_relapses_2, num_pre_relapses_3)

# persist_days is not suitable to convert into binary form
#persist_days_1<- ifelse(raw_data$persist_days<180, 1, 0)
#persist_days_2<- ifelse(raw_data$persist_days>=180, 1, 0)
#persist_days_quartiles<- data.frame(persist_days_1 , persist_days_2)

new_create_quartile_covar<- cbind(
    pre_non_ms_total_allowed_quartiles,
    pre_ms_total_allowed_quartiles,
    pre_non_ms_pharmacy_allowed_quartiles,
    pre_ms_pharmacy_allowed_quartiles,
    pre_non_ms_medical_allowed_quartiles,
    pre_ms_medical_allowed_quartiles,
    num_pre_meds_quartiles,
    num_pre_op_dx_quartiles,
    age_quartiles,
    pchrlson_quartiles,
    num_pre_mri_any_quartiles,
    num_pre_cort_oral_quartiles,
    num_pre_cort_iv_quartiles,
    num_pre_relapses_quartiles
    #persist_days_quartiles
)
dim(new_create_quartile_covar)
#[4429,52]


# create a vector containing all the covariates, it doesn't include treatment and response variable
total_covar_list<-c('persistent',
                    'persist_days',
                    'pre_rx_any',
                    paste('pre_rx', 1:11, sep=''),
                    'pre_ampyra1',
                    'pre_mri_any',
                    'pre_90_mri_any',
                    'pre_90_cort_any',
                    'pre_cort_oral',
                    'pre_cort_iv',
                    paste('pre_comor', 1:33, sep=''),
                    'pre_relapse',
                    'pre_relapse_90',
                    'pre_ip_relapse',
                    colnames(new_create_binary_covar),
                    colnames(new_create_quartile_covar)
)
length(total_covar_list)
#130

continuous_covar<- c(
    'pre_non_ms_total_allowed',
    'pre_ms_total_allowed',
    'pre_non_ms_pharmacy_allowed',
    'pre_ms_pharmacy_allowed',
    'pre_non_ms_medical_allowed',
    'pre_ms_medical_allowed',
    'num_pre_meds',
    'num_pre_op_dx',
    'age',
    'pchrlson',
    'num_pre_mri_any',
    'num_pre_cort_oral',
    'num_pre_cort_iv',
    'num_pre_relapses'
)
length(continuous_covar) #14

#write.csv(total_covar_list , 'variable attribution.csv', row.names=F, quote=F)

# codify the attribute of each variable about whether it is summary or detail variable
covar_attr<- read.table('variable attribution.csv', header=T, sep=',')
dim(covar_attr)#[131,4]

colnames(covar_attr)<- tolower(colnames(covar_attr))
colnames(covar_attr)
all(total_covar_list %in% covar_attr$variable)
total_covar_list<- total_covar_list[match(intersect(covar_attr$variable, total_covar_list) , total_covar_list)] # keep the same order

summary_covar<- as.character(covar_attr$variable[which(covar_attr$summary_variable == 1)])
detail_covar<- as.character(covar_attr$variable[which(covar_attr$detail_variable == 1)])
length(summary_covar) ; length(detail_covar)


#standarize the response and treatment variable names
tr <- 'idx_rx'
resp <- 'post_relapse_persist'
colnames(raw_data)[which(names(raw_data)==tr)]='treatment'
colnames(raw_data)[which(names(raw_data)==resp)]='response'

raw_data_trans<- cbind(raw_data, new_create_binary_covar, new_create_quartile_covar)
dim(raw_data_trans)
raw_data_trans <- raw_data_trans[, match(total_covar_list, names(raw_data_trans))]
raw_data_trans$response <- raw_data$response
raw_data_trans$treatment <- raw_data$treatment

#delete those covariate who have the same value
flag <- unlist(lapply(1:ncol(raw_data_trans), function(X) length(levels(as.factor(raw_data_trans[, X])))==1))
raw_data_trans[, which(flag==T)] <- NULL
raw_data_trans <- raw_data_trans[, names(raw_data_trans)[-which(flag==T)]]
#QC
flag <- unlist(lapply(1:ncol(raw_data_trans), function(X) length(levels(as.factor(raw_data_trans[, X])))==1))
length(flag)

#delete the unused persistent variable
raw_data_trans$persist_days <- NULL
names(raw_data_trans)

#select reference variables
pos.num <- unlist(lapply(names(raw_data_trans), function(X){sum(raw_data_trans[, X])}))
pos.df <- as.data.frame(cbind(var=names(raw_data_trans), pos.num))
#write.csv(pos.df, 'pos_num.csv', row.names=F)
#get reference variables
temp <- read.table("pos_num.csv", header=T, sep=',')
reference <- as.character(temp[!is.na(temp[, 3]), 1])

data_model <- raw_data_trans[, setdiff(names(raw_data_trans), reference)]
dim(data_model)
#[1] 4429  118

#QC missing data and drop them
na_check1 <- apply(apply(data_model, 2, is.na), 2, sum)
sum(na_check1)
data_model_noMiss <- na.omit(data_model)
na_check <- apply(apply(data_model_noMiss, 2, is.na), 2, sum)
#data_model_noMiss$treatment <- ifelse(data_model_noMiss$treatment==1, 1, 0)

write.csv(data_model_noMiss, 'data_forModel.csv', row.names=F)

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




