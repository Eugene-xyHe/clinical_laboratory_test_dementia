# RCS   
library(rms)

Demographic<-c('Age','Sex','location','APOE4')
Temporal<-c('season','time_of_day','fastingtime')
SES<-c('Education','townsend')
Lifestyle<-c('Alcohol','Smoking','BMI')
medication<-c('TClower','insulin')

c1 <- c(Demographic,Temporal)
c2 <- SES
c3 <- Lifestyle 
c4 <- medication

c_all <- c(c1,c2,c3,c4)
dd <- datadist(blood1) 
options(datadist='dd') 


FML<- as.formula(paste0 (Outcome,'rcs(','CA',',4)+',paste(c_all,collapse="+")))
fit<-cph(FML,data=blood1)
quan<-quantile(blood['CA'],na.rm=TRUE)
q1 <- quantile(blood['CA'],0.01,na.rm = TRUE)
q99 <- quantile(blood['CA'],0.99,na.rm = TRUE)
label= paste0('P total ',RCS['CA','total_b'],'\n','P non-linear ',RCS['CA','nonl_b'])
p2 <- ggplot()+geom_line(data=Predict(update(fit),CA,fun=exp,ref.zero = TRUE), aes(CA,yhat),linetype="solid",size=1.2,alpha = 0.7,colour="#E9941F")+
  geom_ribbon(data=Predict(update(fit),CA,fun=exp,ref.zero = TRUE), aes(CA,ymin = lower, ymax = upper),alpha = 0.2,fill="#E9941F")+
  geom_hline(yintercept=1, linetype=2,size=1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  labs(x=paste0(bioname['CA','Field'],' (',bioname['CA','Units'],')') , 
       y="HR (95%CI)")+
  geom_vline(xintercept=c(quan[[2]],quan[[4]]),linetype="dotted",size=1,colour = "#E9941F",alpha = 0.7)+
  scale_y_continuous(limits = c(0.8,2) , breaks = c(0.9,1.0,1.5,2.0,2.5,3))+
  scale_x_continuous(limits = c(q1,q99))