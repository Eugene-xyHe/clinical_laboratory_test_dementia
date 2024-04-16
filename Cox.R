# Cox
library(survival)

Outcome <- 'Surv(dementia_months,dementia_status==1)~'
Demographic<-c('Age','location','Sex','APOE4')
Temporal<-c('season','time_of_day','fastingtime')
SES<-c('Education','townsend')
Lifestyle<-c('Alcohol','Smoking','BMI')
medication<-c('TClower','insulin')

c1 <- c(Demographic,Temporal)
c2 <- SES
c3 <- Lifestyle 
c4 <- medication

Uni_cox<- function(x){
  FML1<- as.formula(paste(Outcome,paste(c(x,c1),collapse=" + ")))
  FML4<- as.formula(paste(Outcome,paste(c(x,c1,c2,c3,c4),collapse=" + ")))
  FML <- list(FML1,FML4)#FML2,FML3
  cox <- cox1 <-model <- list()
  for (i in 1:2){
    cox[[i]]<- coxph(FML[[i]],data=blood)
    cox1[[i]] <-summary(cox[[i]])
    HR <- cox1[[i]]$coefficients[1,2]
    LCI <- cox1[[i]]$conf.int[1,3]
    UCI <- cox1[[i]]$conf.int[1,4]
    PH_p <- as.data.frame(cox.zph(cox[[i]])[[1]])['GLOBAL','p']
    model[[i]] <- data.frame('Characteristics'=x,
                             'nevent/N'=sprintf("%d/%d", cox1[[i]]$nevent,cox1[[i]]$n),
                             'HR (95% CI)'=sprintf("%.3f ( %.3f, %.3f )", HR,LCI,UCI),
                             'P value'=cox1[[i]]$coefficients[1,5],
                             'HR'= HR,
                             'LCI'=LCI,
                             'UCI'=UCI,
                             'PH_p'=PH_p)}
  Uni_cox <- data.frame(model)
  return(Uni_cox)  
} 

variable.names<- colnames(blood)[2:31]

result <- lapply(variable.names, Uni_cox)
result <- ldply(result,data.frame)

result$P.value.bon<- p.adjust(result$P.value,"bonferroni",30)
result$P.value.fdr<- p.adjust(result$P.value,"fdr",30)
result$P.value.1.bon<-p.adjust(result$P.value.1,"bonferroni",30)
result$P.value.1.fdr<-p.adjust(result$P.value.1,"fdr",30)