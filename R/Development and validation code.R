#set correct file upload location
getwd()
setwd("C:/Users/marcv/OneDrive/Bureaublad/UM EPID Alles/EPI4934 - Topics in Epidemiology/Clinical Prediction Models")

#clear environment
rm(list = ls()) 

#load data into RStudio
data <- read.delim("ovarian.txt") #missing data was already imputed before so no need to check or do

#load required packages
library(pmsampsize)
library(rms)
library(pROC)
library(ggplot2)
library(rmda)

#----sample size calculation for development
#get prevalence of outcome (malignancy)
mean(data$mal) * 100 #32.65

pmsampsize(type="b", parameters=8 , shrinkage=0.9, prevalence=0.3265134 , cstatistic=0.75 )
#= this gives a minimum sample size for development of 393

#----develop model (with RCS for non-linear relations)
model_dev_mal <- lrm(data=data, mal~rcs(Age,3) + Ascites + papflow + rcs(soldmaxorig,3) + wallreg + Shadows, x=TRUE, y=TRUE)
model_dev_mal

#or and CI to report for clarity
coefs <- coef(model_dev_mal)
se <- sqrt(diag(vcov(model_dev_mal)))
results <- data.frame(OR = exp(coefs), lower = exp(coefs - 1.96 * se), upper = exp(coefs + 1.96 * se))
round(results, 2)

#develop model (without RCS for non-linear relations)
model_dev_mal_norcs <- lrm(data=data, mal~Age + Ascites + papflow + soldmaxorig + wallreg + Shadows, x=TRUE, y=TRUE)
model_dev_mal_norcs

#or and CI to report for clarity
coefs_rcs <- coef(model_dev_mal_norcs)
se_rcs <- sqrt(diag(vcov(model_dev_mal_norcs)))
results <- data.frame(OR = exp(coefs_rcs), lower = exp(coefs_rcs - 1.96 * se_rcs), upper = exp(coefs_rcs + 1.96 * se_rcs))
round(results, 2)

#----internal validation using bootstrapping (200)
validate(model_dev_mal, B=200)

##discrimination
#c-statistic of apparent performance and internal validation
c_apparent=0.5*(0.8012+1) #0.90 c-statistic apparent
c_corrected=0.5*(0.8000+1) #0.90 c-statistic after bootstrapping procedure

#ROC for the model
#get predicted probabilities from development model
pred_probs <- predict(model_dev_mal, type="fitted")

roc_apparent <- roc(data$mal, pred_probs)
roc_dataframe <- data.frame(specificity = roc_apparent$specificities, sensitivity = roc_apparent$sensitivities)

#plot
auc_val <- auc(roc_apparent)
auc_ci  <- ci.auc(roc_apparent, conf.level = 0.95)
auc_label <- paste0("AUC = ", round(auc_val, 2), "\n95% CI: ", round(auc_ci[1], 2), " \u2013 ", round(auc_ci[3], 2))

ggplot(roc_dataframe, aes(x = 1 - specificity, y = sensitivity)) + geom_line(color = "black") +
  geom_abline(linetype = "dashed", color = "gray") + scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + annotate("text", x = 0.75, y = 0.25, label = auc_label, size = 5) +
  labs(x = "1 - Specificity", y = "Sensitivity") + theme_classic(base_size = 14)

#diagnostic accuracy for 5%, 10%, 15% and 20% risk cut-offs
cutoffs <- c(0.05, 0.10, 0.15, 0.20)

#function to calculate diagnostic accuracy cut-offs
diag_accuracy <- function(probs, outcome, cutoff) {
  pred <- ifelse(probs >= cutoff, 1, 0)
  
  TP <- sum(pred == 1 & outcome == 1)
  TN <- sum(pred == 0 & outcome == 0)
  FP <- sum(pred == 1 & outcome == 0)
  FN <- sum(pred == 0 & outcome == 1)
  
  sens <- TP / (TP + FN)
  spec <- TN / (TN + FP)
  ppv  <- TP / (TP + FP)
  npv  <- TN / (TN + FN)
  
  ci_sens <- prop.test(TP, TP + FN, correct = FALSE)$conf.int
  ci_spec <- prop.test(TN, TN + FP, correct = FALSE)$conf.int
  ci_ppv  <- prop.test(TP, TP + FP, correct = FALSE)$conf.int
  ci_npv  <- prop.test(TN, TN + FN, correct = FALSE)$conf.int
  
  fmt <- function(est, ci) {
    sprintf("%.3f (%.3f\u2013%.3f)", est, ci[1], ci[2])
  }
  
  data.frame(
    Cutoff      = cutoff,
    Sensitivity = fmt(sens, ci_sens),
    Specificity = fmt(spec, ci_spec),
    PPV         = fmt(ppv,  ci_ppv),
    NPV         = fmt(npv,  ci_npv)
  )
}

results_diag_accuracy <- do.call(rbind, lapply(cutoffs, diag_accuracy, probs = pred_probs, outcome = data$mal))
print(results_diag_accuracy, row.names = FALSE)

##calibration
#calibration plot for bootstrapped model
calplotdata <- calibrate(model_dev_mal, B=200)

plot(calplotdata, lwd = 2, xlim = c(0, 1), ylim = c(0, 1), xlab = "Predicted Probability", 
     ylab = "Observed Probability", subtitles = FALSE, legend=FALSE)
legend("bottomright", legend = c("Ideal", "Apparent", "Optimism-corrected", "95% CI"), col = c("gray60", "gray40", 
       "black", "gray85"), lty = c(2, 1, 1, 1), lwd = c(2, 2, 2, 1), bty = "n")

#Brier score for bootstrapped model
#as seen in validate function. Brier for apparent = 0.1105, brier for validated(optimism corrected) = 0.1110


#----apply S to model coefficient and refit intercept to allow better external use
#S = 0.9938 as seen from validate function. Basically indicates no overfitting so predictor shrinkage 
#and intercept refit not needed

#----clinical utility
#decision curve analysis
data <- cbind(data, pred_probs) 
dca <- decision_curve(mal~pred_probs, data=data, fitted.risk=TRUE)
plot_decision_curve(dca, curve.names = c("Malignancy model", "Treat all", "Treat none"))
#only for very high risk cut offs of malignancy does the model perform worse. but in practice such a risk
#threshold is not used so not an issue.

