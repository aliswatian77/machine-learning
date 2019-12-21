esca_cancer= read.csv(file = 'C:/Users/Dell/Desktop/ESCA.csv', header = TRUE)

head(esca_cancer)
dim(data)
library(survMisc)
library(ggplot2)
library(ggfortify)
library(survival)
library(survminer)
library(ggpubr)
library(magrittr)
library(ggfortify)
colnames(esca_cancer[,2:9])
colnames(esca_cancer[,10:17])
#analysis
       
time =esca_cancer[,"DSS.time"]        
ind = esca_cancer[,"DSS"]         
covariates <- c("Patho_tumor_stage", "Histo_type","Histo_grade")
for (i in 1:length(covariates)){
  print(covariates[i])
  print(coxph(Surv(time, ind) ~ get(covariates[i]), data=esca_cancer))
}
fit <- survfit(Surv(time, ind) ~ histological_grade , #modify the variables in the model
               data = esca_cancer) 

ggsurvplot(fit, data = esca_cancer,
           tables.y.text = FALSE,
           tables.theme = theme_cleantable(),
           xlab = "Time in days",
           pval = T,
           pval.coord = c(0.05, 0.05),
           legend.labs = c( "Stage I","StageII","Stage III", "Stage IV"),
           risk.table = TRUE, 
           fontsize = 5,
           ggtheme = theme_bw(base_family = "sans", base_size = 12))



#Multivariate Analysis of clinical variables

MVmodel <- coxph(Surv(time, ind) ~Patho_tumor_stage+Histo_type + Hsto_grade, data=esca_cancer)

summary(MVmodel)

aa_fit <-aareg(Surv(time, ind) ~ ajcc_pathologic_tumor_stage + histological_type + histological_grade , 
               data = esca_cancer)

