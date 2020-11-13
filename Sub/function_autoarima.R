fnc.autoarima <- function(df_PSD){
  Xt <- df_PSD$Xt[vec.Leve]
############### ARIMA(p,d,q) - S ###############
arimas <- forecast::auto.arima(Xt, d=NA, max.p= 5, max.q = 5, max.P = 0, max.Q = 0, max.order = 5, max.d = 0, max.D = 0, stationary = T, seasonal = T, trace = T)

summary(arimas)

ARIMA.301 <- Arima(y = Xt, order = c(3,0,1), include.constant = F)

summary(ARIMA.301)

hist(ARIMA.301$residuals,breaks = 'fd')

ARIMA.301_resid <- ARIMA.301$residuals 
ARIMA.301_LBtest <- Box.test(ARIMA.301_resid, lag = 10, type = c("Ljung-Box"), fitdf = 6)

ARIMA.301_residACF <- Acf(ARIMA.301_resid, xlab = "Lag", ylab = "Resid", main = "Residuals of AR(3)")

ARIMA.301.matrix <- matrix(numeric(), nrow = 4, ncol = 3)
colnames(ARIMA.301.matrix) <- c("parameter", "T-Value", "Critical")
rownames(ARIMA.301.matrix) <- c(paste0("AR",1:3), paste0("MA",1:1))

for (k in 1:4){
  
  ARIMA.301.matrix[k,1] <- ARIMA.301$coef[k]
  ARIMA.301.matrix[k,2] <- ARIMA.301$coef[k]/(ARIMA.301$var.coef[k,k]**.5)
  ARIMA.301.matrix[k,3] <- abs(qt(0.975, lower.tail = F, length(Xt) - 4))
}



ARIMA.301_fitted <- fitted(ARIMA.301)
ARIMA.301_forecast <- forecast(ARIMA.301, h = 3, level = 0.95)
ARIMA.301_pred <- ARIMA.301_forecast$mean
ARIMA.301_upper <- ARIMA.301_forecast$upper
ARIMA.301_lower <- ARIMA.301_forecast$lower
ARIMA.301_Table <- data.frame(matrix(c(ARIMA.301_pred, ret_b[,1], ret_b[,1] - ARIMA.301_pred, (ret_b[,1] - ARIMA.301_pred)**2), nrow=3, ncol=4))

par(mfcol = c(1,1))
plot(c(ret[(n-9):(n),1], NA, NA, NA), ylim = c(mean_ret - 3 * sd_ret, mean_ret + 3 * sd_ret), xlab = "Time", ylab = "CC Returns", type = "l", main = "CC Returns of WEGE3 and ARFIMA(3,0.00943,3) Forecast vs. Actual")
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
}