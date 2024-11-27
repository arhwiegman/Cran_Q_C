df <- df_LW %>% 
  mutate(study = ifelse(study=="Walton",1,0)) %>% 
  mutate(study=as.factor(study)) %>%
  #dplyr::select(-study) %>%
  dplyr::select(HLR_md,area_m2,LR_kgNhay,Q_m3d,L_kgy,study,RE_prp) %>%
  mutate(across(-c(RE_prp,study),log10)) %>% 
  as.data.frame

df %>% ggpairs(aes(color=as.factor(study),alpha=0.6))

library(betareg)
frm <- "RE_prp ~ HLR_md+area_m2+LR_kgNhay+study"
frm1 <- "RE_prp ~ Q_m3d + LR_kgNhay + study"
frm2 <- "RE_prp ~ L_kgy + HLR_md + study"
frm_beta <- paste(frm,"|  LR_kgNhay")
frm1_beta <- paste(frm1,"|  LR_kgNhay")
frm2_beta <- paste(frm2,"|  HLR_md")

mbeta <- betareg(frm_beta,data=df)
m <- mbeta
m %>% summary
m %>% plot
m %>% AIC()
mbeta1 <- betareg(frm1_beta,data=df)
m <- mbeta1
m %>% summary
m %>% plot
m %>% AIC()
mbeta2 <- betareg(frm2_beta,data=df)
m <- mbeta2
m %>% summary
m %>% plot
m %>% AIC()



mqbinom1 <- glm(frm1,family=quasibinomial,
               data=df)
mqbinom2 <- glm(frm2,family=quasibinomial,
               data=df)
m <- mqbinom2
m %>% summary
par(mfrow=c(2,2))
m %>% plot
m %>% AIC()
par(mfrow=c(2,2))
plot(mqbinom1$fitted.values,mqbinom2$fitted.values)
abline(0,1)
plot(mbeta1$fitted.values,mbeta2$fitted.values)
abline(0,1)
plot(mqbinom1$fitted.values,mbeta1$fitted.values)
abline(0,1)
plot(mqbinom2$fitted.values,mbeta2$fitted.values)
abline(0,1)

mbinom <- glm(frm,family=binomial,data=df)
mqbinom <- glm(frm,family=quasibinomial,
               data=df)
mnorm <- glm(frm,
              data=df)
mlnorm <- glm(frm,
              family=gaussian(link=log),
              data=df)

par(mfrow=c(2,2))
mnorm%>% summary
mnorm %>% plot
mnorm %>% AIC()
mbinom %>% summary
mbinom %>% plot
mbinom %>% AIC()
mqbinom %>% summary
mqbinom %>% plot
mqbinom %>% AIC()
mlnorm%>% summary
mlnorm %>% plot
mlnorm %>% AIC()
mbeta %>% summary
mbeta %>% plot
mbeta %>% AIC()


plot.new()
par(mfrow=c(2,2))
plot(mnorm$fitted.values~mqbinom$fitted.values)
abline(0,1)
plot(d$RE_prp~mqbinom$fitted.values)
abline(0,1)
plot(d$RE_prp~mbeta$fitted.values)
abline(0,1)
plot(mbinom$fitted.values~mbeta$fitted.values)
abline(0,1)




d %>% summary()
X <- data.frame(area_m2 = runif(min=1,max=9,n=100), 
                 LR_kgNhay = runif(min=0,max=6,n=100), 
                 HLR_md = runif(min=-6,max=3,n=100),
                 study = as.factor(rbinom(n=100,size=1,p=0.5))) %>%
  mutate(Q_m3d = area_m2 * HLR_md,
         L_kgy = area_m2 * LR_kgNhay) %>%
  mutate(RE_pct_lm = predict(mnorm,newdata=.,type="response"),
         RE_pct_lnorm = predict(mlnorm,newdata=.,type="response"),
         RE_pct_beta = predict(mbeta,newdata=.,type="response"),
         RE_pct_qbinom = predict(mqbinom,newdata=.,type="response")) 
print(X)

X %>% summary()

ggplot(X) + geom_point(aes(X$area_m2,X$RE_pct_beta,
                           color=HLR_md))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)

ggplot(X) + geom_point(aes(X$RE_pct_lm,X$RE_pct_beta,
                           color=study))+
  geom_abline(aes(slope=1,intercept=0))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)
ggplot(X) + geom_point(aes(X$RE_pct_lm,X$RE_pct_qbinom,color=HLR_md,
                           color=HLR_md))+
  geom_abline(aes(slope=1,intercept=0))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)
ggplot(X) + geom_point(aes(RE_pct_beta,
                           RE_pct_qbinom,
                           fill=HLR_md),pch=21)+
  geom_abline(aes(slope=1,intercept=0))+
  geom_vline(xintercept=1)+
  geom_hline(yintercept=1)+
  xlim(c(0,1))
fn_pred_RE_pct_from_Cheng(10^seq(1,5,by=1))
set.seed(71)
mrf <- randomForest::randomForest(RE_prp~.,data=df,importance=T,proximity=T)
mrf$importance %>% as.data.frame %>% arrange(desc(`%IncMSE`))
# VARIABLE SELECTIOn

full = glm(RE_prp~.,family=quasibinomial,data=df)
stepAIC(full, direction=c("both"))

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)

trControl <- trainControl(method="LOOCV")
caret_model <- train(RE_prp~.,
                     df,
                     method='glmStepAIC', # This method fits best model stepwise.
                     direction="both", # Direction
                     trControl=trControl)
best <- bestglm(df,family=gaussian(link=log))
best$BestModels
best %>% summary()
best$BestModel %>% summary
best$BestModel %>% plot()
plot(best$BestModel$fitted.values,best$BestModel$model$y)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))