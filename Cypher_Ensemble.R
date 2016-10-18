library(dplyr)
setwd("/Users/lalitsachan/Dropbox/Cypher/")

d=read.csv("bank-full.csv",sep=";",stringsAsFactors = F)



d=d %>% 
  mutate(education=ifelse(education=="unknown",0,
                          ifelse(education=="primary",1,
                          ifelse(education=="secondary",2,3)))) %>% 
  mutate(default=as.numeric(default=="yes"),
         housing=as.numeric(housing=="yes"),
         loan=as.numeric(loan=="yes"),
         month=match(month,tolower(month.abb)),
         marital_div=as.numeric(marital=="divorced"),
         marital_married=as.numeric(marital=="married"),
         cont_cell=as.numeric(contact=="cellular"),
         cont_unknown=as.numeric(contact=="unknown"),
         pout_fail=as.numeric(poutcome=="failure"),
         pout_other=as.numeric(poutcome=="other"),
         pout_success=as.numeric(poutcome=="success"),
         y=as.numeric(y=="yes")
         ) %>% 
  select(-job,-marital,-contact,-poutcome)

normalize=function(x) {(x-min(x))/(max(x)-min(x))}
d[,-13]=as.data.frame(lapply(d[,-13],normalize))

##
set.seed(3)
s=sample(1:nrow(d),0.8*nrow(d))
d_train=d[s,]
d_test=d[-s,]


### KNN
knn_pred=class::knn(d_train[,-13],d_test[,-13],d_train[,13],k=50)
table(knn_pred,d_test[,13])
## GBM
set.seed(3)
gbm.fit=gbm::gbm(y~.,
            data=d_train,
            distribution = "bernoulli",
            n.trees = 2000,interaction.depth = 3,
            n.minobsinnode = 5,
            shrinkage = 0.05,
            verbose=T,
            n.cores=3)
k=gbm::gbm.perf(gbm.fit,method="OOB")
k


gbm_pred=as.numeric(gbm::predict.gbm(gbm.fit,d_test,n.trees = k,type="response")>0.2)

table(gbm_pred,d_test[,13])

# ET
ET.fit=extraTrees::extraTrees(d_train[,-13],as.factor(d_train$y))
et_pred=predict(ET.fit,d_test[,-13])
table(et_pred,d_test[,13])


fit_rf=randomForest::randomForest(factor(y)~.,do.trace=T,data=d_train)
fit_rf

rf_pred=predict(fit_rf,newdata=d_test)

# SVM
svm.fit=e1071::svm(d_train[,-13],d_train[,13],type="C-classification")
svm_pred=predict(svm.fit,d_test[,-13])

# logistic

log.fit=step(glm(y~.,data=d_train,family="binomial"))
log_pred=as.numeric(predict(log.fit,d_test,type="response")>0.2)

my_pred=data.frame(knn_pred=as.numeric(as.character(knn_pred)),
                   gbm_pred,
                  et_pred= as.numeric(as.character(et_pred)),
                  rf_pred=as.numeric(as.character(rf_pred)),
                  svm_pred=as.numeric(as.character(svm_pred)),
                  log_pred
                  )

View(cor(my_pred[,1:6]))



max_freq=function(x){
  
  t=sort(table(x),decreasing = T)
  return(as.numeric(names(t)[1]))
}

my_pred$majority_vote=apply(my_pred,1,max_freq)
my_pred$w_majority_vote=apply(my_pred,1,function(x) max_freq(c(rep(x[2],4),x[-2])))


# precision=tp/(tp+fp)
# recall=tp/(tp+fn)
# f1=2(precision*recall)/(precision+recall)

real=d_test[,13]
model_perf=data.frame(model="dummy",precision=99,recall=99,f1=99,acc=99)
for(i in 1:ncol(my_pred)){
  tp=sum(real==1 & my_pred[,i]==1)
  fp=sum(real==0 & my_pred[,i]==1)
  fn=sum(real==1 & my_pred[,i]==0)
  tn=sum(real==0 & my_pred[,i]==0)
  acc=(tp+tn)/(tp+tn+fp+fn)
  precision=tp/(tp+fp)
  recall=tp/(tp+fn)
  f1=2*(precision*recall)/(precision+recall)
  
  model_perf=rbind(model_perf,c(NA,precision,recall,f1,acc))
}

model_perf=model_perf[-1,]
model_perf$model=names(my_pred)


### blending

s1=sample(nrow(d_train),0.7*nrow(d_train))
d_train_a=d_train[s1,]
d_train_b=d_train[-s1,]

set.seed(3)
gbm.layer1=gbm::gbm(y~.,
                 data=d_train_a,
                 distribution = "bernoulli",
                 n.trees = 2000,interaction.depth = 3,
                 n.minobsinnode = 5,
                 shrinkage = 0.05,
                 verbose=T,
                 n.cores=3)
k=gbm::gbm.perf(gbm.layer1,method="OOB")
k

d_train_b$gbm=gbm::predict.gbm(gbm.layer1,d_train_b,n.trees = k,type="response")
d_test$gbm=gbm::predict.gbm(gbm.layer1,d_test,n.trees = k,type="response")
log.layer2=step(glm(y~.,data=d_train_b,family="binomial"))

log_pred=as.numeric(predict(log.layer2,d_test,type="response")>0.15)

tp=sum(real==1 & log_pred==1)
fp=sum(real==0 & log_pred==1)
fn=sum(real==1 & log_pred==0)
tn=sum(real==0 & log_pred==0)
acc=(tp+tn)/(tp+tn+fp+fn)
precision=tp/(tp+fp)
recall=tp/(tp+fn)
f1=2*(precision*recall)/(precision+recall)

precision;recall;f1;acc

## stacking
d_train_b$gbm=NULL
gbm.a=gbm::gbm(y~.,
               data=d_train_a,
               distribution = "bernoulli",
               n.trees = 2000,interaction.depth = 3,
               n.minobsinnode = 5,
               shrinkage = 0.05,
               verbose=T,
               n.cores=3)
ka=gbm::gbm.perf(gbm.a,method="OOB")
gbm.b=gbm::gbm(y~.,
               data=d_train_b,
               distribution = "bernoulli",
               n.trees = 2000,interaction.depth = 3,
               n.minobsinnode = 5,
               shrinkage = 0.05,
               verbose=T,
               n.cores=3)
kb=gbm::gbm.perf(gbm.b,method="OOB")

d_train_a$gbm=gbm::predict.gbm(gbm.b,d_train_a,n.trees = kb,type="response")
d_train_b$gbm=gbm::predict.gbm(gbm.a,d_train_b,n.trees = ka,type="response")

d_train=rbind(d_train_a,d_train_b)
log.layer2=step(glm(y~.,data=d_train,family="binomial"))

gbm.full=gbm.b=gbm::gbm(y~.-gbm,
                        data=d_train,
                        distribution = "bernoulli",
                        n.trees = 2000,interaction.depth = 3,
                        n.minobsinnode = 5,
                        shrinkage = 0.05,
                        verbose=T,
                        n.cores=3)
kfull=kb=gbm::gbm.perf(gbm.full,method="OOB")
d_test$gbm=gbm::predict.gbm(gbm.full,d_test,n.trees = kfull,type="response")

log_pred=as.numeric(predict(log.layer2,d_test,type="response")>0.2)

tp=sum(real==1 & log_pred==1)
fp=sum(real==0 & log_pred==1)
fn=sum(real==1 & log_pred==0)
tn=sum(real==0 & log_pred==0)
acc_stack=(tp+tn)/(tp+tn+fp+fn)
precision_stack=tp/(tp+fp)
recall_stack=tp/(tp+fn)
f1_stack=2*(precision*recall)/(precision+recall)

precision;recall;f1;acc
precision_stack;recall_stack;f1_stack;acc_stack
