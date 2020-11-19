
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list=ls()) #Command to delete all R memory.

library("lattice")
library("ggplot2")
library("MASS")
library("tseries")
library("forecast")
library("fExtremes")
library("rugarch")
library("fGarch")
library("sn")
library("TSA")
library("fracdiff")

set.seed(88)

Asset <- read.csv(file = "PSD3.csv", sep = ";") #Read index series of PSD3 in .csv from the Working Directory.

############### Extraction of Continuously Compounded Returns - S ###############
# Time Series: daily basis
# [NOTE 1]: ADJUSTED PRICE FOR DIVIDENDS AND INTERESTS ON NET EQUITY

mat_P <- Asset #Assign to "mat_P". All further references primarly to mat_P.
mat_P <- data.frame(mat_P[order(mat_P$Data),]) #Sort "mat_P" by column "Date".

############### Subsetting by indices - S ###############
index_1 <- which(mat_P$Data == "2007-05-30") #Find index of specific value at column "Date".
index_2 <- which(mat_P$Data == "2015-03-27") #Find index of specific value at column "Date".
mat_P <- (mat_P[index_1:index_2,]) #Subset by dates in indices for all columns.
############### Subsetting by indices - E ###############

mat_P <- mat_P[mat_P[,5] !="0",] #Test markets days by trading volume.

P <- mat_P[, 5, drop = F] #Extract "Adj. Close" from 5th column.

rownames(P) <- mat_P$Data #Assign dates as row labels of "P".

n_P <- nrow(P) #Assign number of observations/rows to "n".

Sret <- (P[2:n_P, 1, drop = F]-P[1:(n_P-1), 1, drop = F])/(P[1:(n_P-1), 1, drop = F]) #Calculate simple return.
ret <- (log(P[2:n_P, 1, drop = F]/P[1:(n_P-1), 1, drop = F])) #Calculate continously compounded return (log-return).

colnames(ret) <- c("CC_Return") #Name column of continously compoounded return.

############### Extraction of Continuously Compounded Returns - E ###############

############### Subsetting for Backtest: VaR (unconditional) - S ###############
ret_full <- ret

index_F <- which(rownames(ret_full) == "2015-03-24")

#n_a <- round(n*.650)
#n_b <- round(n*.35)

n_full <- nrow(ret)

ret <- ret_full[1:index_F, , drop = F]
ret_b <-ret_full[(index_F+1):(n_full), , drop = F]

n <- nrow(ret)

############### Subsetting for Backtest: VaR (unconditional) - E ###############

############### Statistical Briefing - S ###############

mean_ret <- mean(ret[,1]) #Mean of returns
var_ret <- var(ret[,1]) #Sample variance of returns: (n-1) as denominator.
sd_ret <- sd(ret[,1]) #Sample standard deviation of returns: (n-1) as denominator.
median_ret <- median(ret[,1]) #Sample median of returns.
max_ret <- max(ret[,1])
min_ret <- min(ret[,1])
skew_ret <- skewness(ret[,1])
kurt_ret <- kurtosis(ret[,1])

plot(ret[,1], xlab = "Time", ylab = "CC Returns", type = "l", main = "CC Returns of PSD3")
lines(rep(mean_ret + 2*sd_ret, n), type = "l", col = "red")
lines(rep(mean_ret - 2*sd_ret, n), type = "l", col = "red")

hist_ret <- hist(ret[,1], breaks="fd", freq = F, plot = T, xlab = "CC returns", ylab = "Rel. Freq.", main = "Histogram of CC Returns of PSD3")
bins_ret <- hist_ret$mids

#a) ACF of ret
acf_ret <- Acf(ret[,1], xlab = "Lag", ylab = "CC Returns of PSD3", main = "ACF of PSD3 CC Returns")

#b) PACF of ret
pacf_ret <- Pacf(ret[,1], xlab = "Lag", ylab = "CC Returns of PSD3", main = "Partial ACF of PSD3 CC Returns")

#C) ACF of ret^2
acf_ret2 <- Acf(ret[,1]^2, xlab = "Lag", ylab = "Squared CC Returns of PSD3", main = "ACF of PSD3 Squared CC Returns")

#Normality Tests
#a) JBS
Test_I <- jarque.bera.test(ret[,1])

#b) SW
Test_II <- shapiro.test(ret[,1])

#c) KS
Test_III <- ks.test(ret[,1], pnorm(ret[,1], mean = mean_ret, sd = sd_ret))

#Weak Stationarity:
#a) KPSS - LEVEL
Test_IV <- kpss.test(ret[,1], null=("Level"))

#b) KPSS - TREND
Test_V <- kpss.test(ret[,1], null=("Trend"))

############### Statistical Briefing - E ###############

############### Distributions - MLE - S ###############

#I) Normal Distribution.
mle_N <- fitdistr(ret[,1], densfun = "normal", lower = c(-Inf, 0))
mean_N <-mle_N$estimate[1]
sd_N <- mle_N$estimate[2]
fit_N <- dnorm(bins_ret, mean = mean_N, sd = sd_N)

#III) T-Student Distribution.
mle_T <- fitdistr(ret[,1], densfun = "t", start = list(m = mean_ret, s = sd_ret, df = 3), lower = c(-Inf, 0))
mean_T <- mle_T$estimate[1]
sd_T <- mle_T$estimate[2]
df_T <- mle_T$estimate[3]
fit_T <- dt((bins_ret-mean_T)/sd_T, df = df_T) * 100

#III) Skew Normal
mle_SNorm <- snormFit(ret[,1])
mean_SNorm <- mle_SNorm$par[1]
sd_SNorm <- mle_SNorm$par[2]
xi_SNorm <- mle_SNorm$par[3]
fit_SNorm <- dsnorm(bins_ret, mean = mean_SNorm, sd = sd_SNorm, xi = xi_SNorm)

#IV) Skew T-Student
mle_ST <- sstdFit(ret[,1])
mean_ST <- mle_ST$estimate[1]
sd_ST <- mle_ST$estimate[2]
nu_ST <- mle_ST$estimate[3]
xi_ST <- mle_ST$estimate[4]
fit_ST <- dsstd(bins_ret, mean = mean_ST, sd = sd_ST, nu = nu_ST, xi = xi_ST)

hist(ret[,1], breaks="fd", freq = F, plot = T, ylim = c(0,40), xlab = "CC returns", ylab = "Rel. Freq.", main = "Histogram of CC Returns of PSD3")
lines(x = bins_ret, y = fit_N, col = "green", lty = 2, lwd = 2)
lines(x = bins_ret, y = fit_T, col = "blue", lty = 4, lwd = 1)
lines(x = bins_ret, y = fit_SNorm, col ="orange", lty = 1, lwd = 1)
lines(x = bins_ret, y = fit_ST, col = "red", lty = 1, lwd = 2)
legend("topleft",                       # x-y coordinates for location of the legend
       legend=c("Fit - Normal", "Fit - T-Student", "Fit - Skew Normal", "Fit - Skew T-Student"),  # Legend labels
       col=c("green", "blue", "orange", "red"), # Color of points or lines
       lty=c(2,4,1,1), # Line type
       lwd=c(2,1,1,2),# Line width
       pt.cex = .6,
       cex = .6,
       bty = 'n'
)

############### Distributions - MLE - E ###############

############### ARIMA(p,d,q) - S ###############
arimas <- auto.arima(ret[,1], d=NA, max.p= 30, max.q = 30, max.P = 0, max.Q = 0, max.order = 60, max.d = 0, max.D = 0, stationary = T, seasonal = T, trace = T)

ARIMA.303 <- Arima(y = ret[,1], order = c(3,0,3), include.constant = F)
ARIMA.303_resid <- ARIMA.303$residuals 
ARIMA.303_LBtest <- Box.test(ARIMA.303_resid, lag = 10, type = c("Ljung-Box"), fitdf = 6)
ARIMA.303_residACF <- Acf(ARIMA.303_resid, xlab = "Lag", ylab = "Resid", main = "Residuals of AR(3)")

ARIMA.303.matrix <- matrix(numeric(), nrow = 6, ncol = 3)
colnames(ARIMA.303.matrix) <- c("parameter", "T-Value", "Critical")
rownames(ARIMA.303.matrix) <- c(paste0("AR",1:3), paste0("MA",1:3))

for (k in 1:6){
  
  ARIMA.303.matrix[k,1] <- ARIMA.303$coef[k]
  ARIMA.303.matrix[k,2] <- ARIMA.303$coef[k]/(ARIMA.303$var.coef[k,k]**.5)
  ARIMA.303.matrix[k,3] <- abs(qt(0.975, lower.tail = F, n - 6))
}



ARIMA.303_fitted <- fitted(ARIMA.303)
ARIMA.303_forecast <- forecast(ARIMA.303, h = 3, level = 0.95)
ARIMA.303_pred <- ARIMA.303_forecast$mean
ARIMA.303_upper <- ARIMA.303_forecast$upper
ARIMA.303_lower <- ARIMA.303_forecast$lower
ARIMA.303_Table <- data.frame(matrix(c(ARIMA.303_pred, ret_b[,1], ret_b[,1] - ARIMA.303_pred, (ret_b[,1] - ARIMA.303_pred)**2), nrow=3, ncol=4))

par(mfcol = c(1,1))
plot(c(ret[(n-9):(n),1], NA, NA, NA), ylim = c(mean_ret - 3 * sd_ret, mean_ret + 3 * sd_ret), xlab = "Time", ylab = "CC Returns", type = "l", main = "CC Returns of PSD3 and ARFIMA(3,0.00943,3) Forecast vs. Actual")
#lines(rep(mean_ret + 2*sd_ret, n + 3), type = "l", col = "red")
#lines(rep(mean_ret - 2*sd_ret, n + 3), type = "l", col = "red")

lines(c(rep(NA, n - (n-9) + 1), ARIMA.303_pred), col = "blue", lwd = 2, lty = 1)
lines(c(rep(NA, n - (n-9) + 1), ARIMA.303_upper), col = "red", lty = 4, lwd = 4)
lines(c(rep(NA, n - (n-9) + 1), ARIMA.303_lower), col = "red", lty = 4, lwd = 4)
lines(c(rep(NA, n - (n-9) + 1), ret_b[,1]), col = "black", lty = 4, lwd = 4)
legend("topleft",                       # x-y coordinates for location of the legend
       legend=c("Predicted - ARFIMA(3,0.00943,3)", "C.I. - 95%", "Actual"),  # Legend labels
       col=c("blue", "red", "black"), # Color of points or lines
       lty=c(1,4,4), # Line type
       lwd=c(1,4,4), # Line width       
       pt.cex = .6,
       cex = .6,
       bty = 'n'
)

############### ARIMA(p,d,q) - E ###############
par(mfcol = c(2,3), mar = c(2, 2, 2, 1), oma = c(0, 0, 0, 0))

#1,1
plot(c(ret[(n-9):(n),1], NA, NA, NA), ylim = c(mean_ret - 3 * sd_ret, mean_ret + 3 * sd_ret), xlab = "Time", ylab = "CC Returns", type = "l")
title(main = "Actual vs. Predicted - h=3", line = 1)
#lines(rep(mean_ret + 2*sd_ret, n + 3), type = "l", col = "red")
#lines(rep(mean_ret - 2*sd_ret, n + 3), type = "l", col = "red")

lines(c(rep(NA, n - (n-9) + 1), ARIMA.303_pred), col = "blue", lwd = 2, lty = 1)
lines(c(rep(NA, n - (n-9) + 1), ARIMA.303_upper), col = "red", lty = 4, lwd = 4)
lines(c(rep(NA, n - (n-9) + 1), ARIMA.303_lower), col = "red", lty = 4, lwd = 4)
lines(c(rep(NA, n - (n-9) + 1), ret_b[,1]), col = "black", lty = 4, lwd = 4)
#lines(c(rep(mean_ret, n - (n-9) + 1 + 3), ret_b[,1]), col = "orange", lty = 4, lwd = 4)
legend("topleft",                       # x-y coordinates for location of the legend
       legend=c("Predicted - ARIMA(3,0,3)", "C.I. - 95%", "Actual"),  # Legend labels
       col=c("blue", "red", "black"), # Color of points or lines
       lty=c(1,4,4), # Line type
       lwd=c(1,4,4), # Line width
       pt.cex = .6,
       cex = .6,
       bty ='n'
)

#2,1
hist.ARIMA.303_resid <- hist(ARIMA.303_resid, breaks="fd", freq = F, plot = T, ylim = c(0,30), xlab = "Residuals of ARIMA(3,0,3)", ylab = "Rel. Freq.", main = "")
title(main = "Histogram of ARIMA(3,0,3) Residuals", line = 1)
lines(x = hist.ARIMA.303_resid$mids, y = hist.ARIMA.303_resid$density, col = "orange", lty = 2, lwd = 2)

#1,2
Acf(ARIMA.303_resid, xlab = "Lag", ylab = "Residuals of ARIMA(3,0,3)", main = "")
title(main = "ACF of ARIMA(3,0,3) Residuals", line = 1)

#2,2
Pacf(ARIMA.303_resid, xlab = "Lag", ylab = "Residuals of ARIMA(3,0,3)", main = "")
title(main = "Partial ACF of ARIMA(3,0,3) Residuals", line = 1)

#1,3
Acf(ARIMA.303_resid^2, xlab = "Lag", ylab = "Sqr. CC Returns of PSD3", main = "")
title(main = "ACF of ARIMA(3,0,3) Sqr. Residuals", line = 1)

#2,3
Pacf(ARIMA.303_resid^2, xlab = "Lag", ylab = "Sqr. CC Returns of PSD3", main = "")
title(main = "Partial ACF of ARIMA(3,0,3) Sqr. Residuals", line = 1)

############################################################################################
par(mfcol = c(2,3), mar = c(2, 2, 2, 1), oma = c(0, 0, 0, 0))

#1,1
plot(ret[,1], xlab = "Time", ylab = "CC Returns", type = "l")
title(main = "CC Returns of PSD3", line = 1)
lines(rep(mean_ret + 2*sd_ret, n), type = "l", col = "red")
lines(rep(mean_ret - 2*sd_ret, n), type = "l", col = "red")
lines(ARIMA.303_pred, col = "green", lty = 4)
legend("topright",                       # x-y coordinates for location of the legend
       legend=c("Actual", "2 Std. Dev.", "Fitted - ARIMA(3, 0, 3)"),  # Legend labels
       col=c("black", "red", "green"), # Color of points or lines
       lty=c(1,1,4), # Line type
       lwd=c(1,1,1), # Line width
       pt.cex = .6,
       cex = .6,
       bty = 'n'
)

#2,1
hist(ret[,1], breaks="fd", freq = F, plot = T, ylim = c(0,40), xlab = "CC returns", ylab = "Rel. Freq.", main = "")
title(main = "Histogram of CC Returns of PSD3", line = 1)
lines(x = bins_ret, y = fit_N, col = "green", lty = 2, lwd = 2)
lines(x = bins_ret, y = fit_T, col = "blue", lty = 4, lwd = 1)
lines(x = bins_ret, y = fit_SNorm, col ="orange", lty = 1, lwd = 1)
lines(x = bins_ret, y = fit_ST, col = "red", lty = 1, lwd = 2)
legend("topright",                       # x-y coordinates for location of the legend
       legend=c("Fit - Normal", "Fit - T-Student", "Fit - Skew Normal", "Fit - Skew T-Student"),  # Legend labels
       col=c("green", "blue", "orange", "red"), # Color of points or lines
       lty=c(2,4,1,1), # Line type
       lwd=c(2,1,1,2), # Line width
       pt.cex = .8,
       cex = 0.8,
       bty="n"
)

#1,2
Acf(ret[,1], xlab = "Lag", ylab = "CC Returns of PSD3", main = "")
title(main = "ACF of PSD3 CC Returns", line = 1)

#2,2
Pacf(ret[,1], xlab = "Lag", ylab = "CC Returns of PSD3", main = "")
title(main = "Partial ACF of PSD3 CC Returns", line = 1)

#1,3
Acf(ret[,1]^2, xlab = "Lag", ylab = "Sqr. CC Returns of PSD3", main = "")
title(main = "ACF of PSD3 Sqr. CC Returns", line = 1)

#2,3
Pacf(ret[,1]^2, xlab = "Lag", ylab = "Sqr. CC Returns of PSD3", main = "")
title(main = "Partial ACF of PSD3 Sqr. CC Returns", line = 1)

#rm(list=ls()) #Command to delete all R memory.

##################################################################################################################
diff_ARFIMA <-fdGPH(ret[,1])$d
ret_ARFIMA <- diffseries(c(ret[,1]), diff_ARFIMA)
arfima <- arfima(ret[,1], d = diff_ARFIMA, estim = "mle")

arfima_resid <- arfima$residuals 
arfima_LBtest <- Box.test(arfima_resid, lag = 10, type = c("Ljung-Box"), fitdf = 6)

arfima.matrix <- matrix(numeric(), nrow = 7, ncol = 3)
colnames(arfima.matrix) <- c("parameter", "T-Value", "Critical")
rownames(arfima.matrix) <- c("d", paste0("AR",1:3), paste0("MA",1:3))

compose.coef <- c(arfima$d, arfima$ar[1],arfima$ar[2], arfima$ar[3], arfima$ma[1],
                  arfima$ma[2],arfima$ma[3])

for (k in 1:7){
  
  arfima.matrix[k,1] <- compose.coef[k]
  arfima.matrix[k,2] <- compose.coef[k]/(arfima$stderror.dpq[k])
  arfima.matrix[k,3] <- abs(qt(0.975, lower.tail = F, n - 6))
}

arfima_fitted <- arfima$fitted
arfima_forecast <- forecast(arfima, h = 3, level = 0.95)
arfima_pred <- arfima_forecast$mean
arfima_upper <- arfima_forecast$upper
arfima_lower <- arfima_forecast$lower
arfima_Table <- data.frame(matrix(c(arfima_pred, ret_b[,1], ret_b[,1] - arfima_pred, (ret_b[,1] - arfima_pred)**2), nrow=3, ncol=4))

par(mfcol = c(2,3), mar = c(2, 2, 2, 1), oma = c(0, 0, 0, 0))

#1,1
plot(c(ret[(n-9):(n),1], NA, NA, NA), ylim = c(mean_ret - 3 * sd_ret, mean_ret + 3 * sd_ret), xlab = "Time", ylab = "CC Returns", type = "l")
title(main = "Actual vs. Predicted - h=3", line = 1)
#lines(rep(mean_ret + 2*sd_ret, n + 3), type = "l", col = "red")
#lines(rep(mean_ret - 2*sd_ret, n + 3), type = "l", col = "red")

lines(c(rep(NA, n - (n-9) + 1), arfima_pred), col = "blue", lwd = 2, lty = 1)
lines(c(rep(NA, n - (n-9) + 1), arfima_upper), col = "red", lty = 4, lwd = 4)
lines(c(rep(NA, n - (n-9) + 1), arfima_lower), col = "red", lty = 4, lwd = 4)
lines(c(rep(NA, n - (n-9) + 1), ret_b[,1]), col = "black", lty = 4, lwd = 4)
#lines(c(rep(mean_ret, n - (n-9) + 1 + 3), ret_b[,1]), col = "orange", lty = 4, lwd = 4)
legend("topright",                       # x-y coordinates for location of the legend
       legend=c("Predicted - ARFIMA(3,0.00943,3)", "C.I. - 95%", "Actual"),  # Legend labels
       col=c("blue", "red", "black"), # Color of points or lines
       lty=c(1,4,4), # Line type
       lwd=c(1,4,4), # Line width
       pt.cex = .8,
       cex = .6,
       bty = 'n'
)

#2,1
hist.arfima_resid <- hist(arfima_resid, breaks="fd", freq = F, plot = T, ylim = c(0,30), xlab = "Residuals of ARFIMA(3,0.00943,3)", ylab = "Rel. Freq.", main = "")
title(main = "Histogram of ARFIMA(3,0.00943,3) Residuals", line = 1)
lines(x = hist.arfima_resid$mids, y = hist.arfima_resid$density, col = "orange", lty = 2, lwd = 2)

#1,2
Acf(arfima_resid, xlab = "Lag", ylab = "Residuals of ARFIMA(3,0.00943,3)", main = "")
title(main = "ACF of ARFIMA(3,0.00943,3) Residuals", line = 1)

#2,2
periodogram(arfima_resid, xlab = "Lag", ylab = "Residuals of ARFIMA(3,0.00943,3)", main = "")
title(main = "Periodogram of ARFIMA(3,0.00943,3) Residuals", line = 1)

#1,3
Acf(arfima_resid^2, xlab = "Lag", ylab = "Sqr. CC Returns of PSD3", main = "")
title(main = "ACF of ARFIMA(3,0.00943,3) Sqr. Residuals", line = 1)

#2,3
periodogram(arfima_resid^2, xlab = "Lag", ylab = "Sqr. CC Returns of PSD3", main = "")
title(main = "Periodogram ACF of ARFIMA(3,0.00943,3) Sqr. Residuals", line = 1)

############### ARCH(p) - S ###############

garch.spec <- list()
garch.models <- list()
garch.resid <- list(data.frame())
garch.fitted <- list(data.frame())

garch.info <- matrix(nrow = 4, ncol = 9)
rownames(garch.info) <- (c("Akaike", "Bayes", "Shibata", "Hannan-Quinn"))
colnames(garch.info) <- paste0("ARCH(", 1:9, ")")

for (p in 1:9) {
  
  garch.spec[p] <- 
    ugarchspec(
      mean.model = list(armaOrder = c(3,3), include.mean = F)
      , variance.model = list(garchOrder = c(p, 0))
      , distribution.model = "std")
  
  ##### PROBLEMAS AO NOMEAR COLUNAS
  garch.models[p] <- ugarchfit(spec = garch.spec[[p]], data = ret[,1])
  #names(garch.models)[p] <- paste0("ARCH(",p,")")
  
  garch.info[,p] <- infocriteria(garch.models[[p]])[,1] 
  #colnames(garch.info[p]) <- paste0("ARCH(",p,")")
  
  garch.fitted[[p]] <- garch.models[[p]]@fit$fitted.value
  names(garch.fitted)[[p]] <- paste0("ARCH(",p,")")
  
  garch.resid[[p]] <- garch.models[[p]]@fit$residuals
  names(garch.resid)[[p]] <- paste0("ARCH(",p,")")
  print(p)
  
}

best.models <- vector("list", 6)
names(best.models) <- c("Norm", "SNorm", "Std", "Sstd", "GED", "SGED")

#O menor AIC para distribuição normal dos erros é o do modelo ARIMA(3,0,3)-1 + GARCH(0,3) 
best.models$Norm <- garch.models[[3]]

#O menor AIC para distribuição normal assimétrica dos erros é o do modelo ARIMA(3,0,3)-1 + GARCH(0,3) 
#Problemas para convergência com modelo ARIMA(3,0,3)-1 + GARCH(0,1)
best.models$SNorm <- garch.models[[3]]

#O menor AIC para distribuição T-Student dos erros é o do modelo ARIMA(3,0,3)-1 + GARCH(0,2) 
#Problemas para convergência com modelo ARIMA(3,0,3)-1 + GARCH(0,1)
best.models$Std <- garch.models[[2]]

#O menor AIC para distribuição T-Student assimétrica dos erros é o do modelo ARIMA(3,0,3)-1 + GARCH(0,6) 
best.models$Sstd <- garch.models[[6]]

#O menor AIC para distribuição GED dos erros é o do modelo ARIMA(3,0,3)-1 + GARCH(0,9) 
best.models$GED <- garch.models[[9]]

#O menor AIC para distribuição GED dos erros é o do modelo ARIMA(3,0,3)-1 + GARCH(0,9) 
best.models$SGED <- garch.models[[9]]

#verificação de todos os CRITÉRIOS INFORMATIVOS
matrix.ICs <- sapply(best.models, infocriteria)
rownames(matrix.ICs) <- rownames(infocriteria(best.models[[1]]))
colnames(matrix.ICs) <- c("ARCH(3)-Norm", "ARCH(3)-SNorm", "ARCH(2)-Std", "ARCH(6)-Sstd", "ARCH(9)-GED", "ARCH(9)-SGED")
matrix.ICs
#O melhor modelo é o que assume distribuição t-student para os erros!

garch <- best.models$Std
garch_resid <- garch@fit$residuals
garch.forecasting <- ugarchforecast(fitORspec = garch, n.ahead = 3)
garch_pred <- ugarchforecast(fitORspec = garch, n.ahead = 3)@forecast$seriesFor

#Parâmetro Shape é o grau de liberdade aplicado ao quantil normalizado da t-student
garch_upper <- garch_pred + -1*qstd(0.025, nu = garch@fit$coef[10])*garch.forecasting@forecast$sigmaFor
garch_lower <- garch_pred - -1*qstd(0.025, nu = garch@fit$coef[10])*garch.forecasting@forecast$sigmaFor

############### ARCH(p) - E ###############

par(mfcol = c(2,3), mar = c(2, 2, 2, 1), oma = c(0, 0, 0, 0))

#1,1
plot(ret[,1], xlab = "Time", ylab = "CC Returns", type = "l")
title(main = "CC Returns of PSD3", line = 1)
lines(rep(mean_ret + 2*sd_ret, n), type = "l", col = "red")
lines(rep(mean_ret - 2*sd_ret, n), type = "l", col = "red")
lines(garch@fit$fitted.values, col = "green", lty = 4)
legend("topright",                       # x-y coordinates for location of the legend
       legend=c("Actual", "2 Std. Dev.", "Fitted - ARIMA(3,0,3)-1 + ARCH(2)"),  # Legend labels
       col=c("black", "red", "green"), # Color of points or lines
       lty=c(1,1,4), # Line type
       lwd=c(1,1,1), # Line width
       pt.cex = .8,
       cex = .8,
       bty = 'n'
)

#2,1
hist(ret[,1], breaks="fd", freq = F, plot = T, ylim = c(0,40), xlab = "CC returns", ylab = "Rel. Freq.", main = "")
title(main = "Histogram of CC Returns of PSD3", line = 1)
lines(x = bins_ret, y = fit_N, col = "green", lty = 2, lwd = 2)
lines(x = bins_ret, y = fit_T, col = "blue", lty = 4, lwd = 1)
lines(x = bins_ret, y = fit_SNorm, col ="orange", lty = 1, lwd = 1)
lines(x = bins_ret, y = fit_ST, col = "red", lty = 1, lwd = 2)
legend("topleft",                       # x-y coordinates for location of the legend
       legend=c("Fit - Normal", "Fit - T-Student", "Fit - Skew Normal", "Fit - Skew T-Student"),  # Legend labels
       col=c("green", "blue", "orange", "red"), # Color of points or lines
       lty=c(2,4,1,1), # Line type
       lwd=c(2,1,1,2), # Line width
       pt.cex = .6,
       cex = 0.8,
       bty = 'n'
)

#1,2
Acf(ret[,1], xlab = "Lag", ylab = "CC Returns of PSD3", main = "")
title(main = "ACF of PSD3 CC Returns", line = 1)

#2,2
Pacf(ret[,1], xlab = "Lag", ylab = "CC Returns of PSD3", main = "")
title(main = "Partial ACF of PSD3 CC Returns", line = 1)

#1,3
Acf(ret[,1]^2, xlab = "Lag", ylab = "Sqr. CC Returns of PSD3", main = "")
title(main = "ACF of PSD3 Sqr. CC Returns", line = 1)

#2,3
Pacf(ret[,1]^2, xlab = "Lag", ylab = "Sqr. CC Returns of PSD3", main = "")
title(main = "Partial ACF of PSD3 Sqr. CC Returns", line = 1)

##################################################################################################

#1,1
plot(c(ret[(n-9):(n),1], NA, NA, NA), ylim = c(mean_ret - 3 * sd_ret, mean_ret + 3 * sd_ret), xlab = "Time", ylab = "CC Returns", type = "l")
title(main = "Actual vs. Predicted - h=3", line = 1)
#lines(rep(mean_ret + 2*sd_ret, n + 3), type = "l", col = "red")
#lines(rep(mean_ret - 2*sd_ret, n + 3), type = "l", col = "red")

lines(c(rep(NA, n - (n-9) + 1), garch_pred), col = "blue", lwd = 2, lty = 1)
lines(c(rep(NA, n - (n-9) + 1), garch_upper), col = "red", lty = 4, lwd = 4)
lines(c(rep(NA, n - (n-9) + 1), garch_lower), col = "red", lty = 4, lwd = 4)
lines(c(rep(NA, n - (n-9) + 1), ret_b[,1]), col = "black", lty = 4, lwd = 4)
#lines(c(rep(mean_ret, n - (n-9) + 1 + 3), ret_b[,1]), col = "orange", lty = 4, lwd = 4)
legend("topleft",                       # x-y coordinates for location of the legend
       legend=c("ARIMA(3,0,3)-1 + GARCH(0,2)", "C.I. - 95%", "Actual"),  # Legend labels
       col=c("blue", "red", "black"), # Color of points or lines
       lty=c(1,4,4), # Line type
       lwd=c(1,4,4), # Line width
       pt.cex = .8,
       cex = .8,
       bty = 'n'
)

#2,1
hist.garch_resid <- hist(garch_resid, breaks="fd", freq = F, plot = T, ylim = c(0,30), xlab = "Residuals", ylab = "Rel. Freq.", main = "")
title(main = "Hist. of ARIMA(3,0,3)-1 + GARCH(0,2) Resids", line = 1)
lines(x = hist.garch_resid$mids, y = hist.garch_resid$density, col = "orange", lty = 2, lwd = 2)

#1,2
Acf(garch_resid, xlab = "Lag", ylab = "Resids. of ARIMA(3,0,3)-1 + GARCH(0,2)", main = "")
title(main = "ACF of ARIMA(3,0,3)-1 + GARCH(0,2) Resid.", line = 1)

#2,2
Pacf(garch_resid, xlab = "Lag", ylab = "Resids. of ARIMA(3,0,3)-1 + GARCH(0,2)", main = "")
title(main = "PACF of ARIMA(3,0,3)-1 + GARCH(0,2) Resid.", line = 1)

#1,3
Acf(garch_resid^2, xlab = "Lag", ylab = "Sqr. CC Returns of PSD3", main = "")
title(main = "ACF of ARIMA(3,0,3)-1 + GARCH(0,2) Sqr. Resid.", line = 1)

#2,3
Pacf(garch_resid^2, xlab = "Lag", ylab = "Sqr. CC Returns of PSD3", main = "")
title(main = "PACF of ARIMA(3,0,3)-1 + GARCH(0,2) Sqr. Resid.", line = 1)
