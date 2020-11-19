if(!dir.exists(paste0("../Stochastic_Processes_PSD_Output/Model/Pricing"))){
  dir.create(paste0("../Stochastic_Processes_PSD_Output/Model/Pricing"))
}else{
  unlink(paste0("../Stochastic_Processes_PSD_Output/Model/Pricing"), recursive = TRUE, force = T)
  dir.create(paste0("../Stochastic_Processes_PSD_Output/Model/Pricing"))
}

rm(list = ls()); gc();
#### Libraries, Options and Parameters.####
options(java.parameters = "-Xmx48g",scipen=999)
set.seed(20201118)
#Calls for libraries (or installs them if missing).
#It is worth noting that this code has to be executed on RStudio IDE due to
#the rstudioapi package. It offers the rstudio::getActiveDocumentContext() that
#allows relative paths.

vec.pkg <- c("yuima","latex2exp",'MASS','astsa','forecast','gridExtra',"rstudioapi","lubridate","tictoc",
             'tidyquant','readxl',"tidyverse")
vec.newpkg <- vec.pkg[!(vec.pkg %in% installed.packages()[,"Package"])]
if(length(vec.newpkg)) install.packages(vec.newpkg)
lapply(vec.pkg, require, character.only = TRUE)
rm(vec.pkg,vec.newpkg)

#Sets the current folder of the script as the working directory.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dataframe_PSD <- read_rds('../Stochastic_Processes_PSD_Output/dataframe_PSD.rds')

vminDate <- ymd('2020-10-01')
vmaxDate <-  ymd('2020-10-31')
arg_LoadStep <- c('Leve','Médio','Pesado')[2]

df_PSD <- dataframe_PSD %>% filter(SubMkt == 'SE', Date >= ymd('2020-10-01')) %>% 
  mutate(Fase = case_when(
    Month %in% 1 ~ 'jan',
    Month %in% 2:3 ~ 'fev-mar',
    Month %in% 4:7~ 'abr-jul',
    Month %in% 8:12 ~ 'ago-nov'
  )) %>% 
  mutate(Fase = factor(x = Fase, ordered=T,levels=c('jan','fev-mar','abr-jul','ago-nov'))) %>% 
  arrange(Date,Hour_Hourly,SubMkt) %>% group_by(SubMkt,LoadStep) %>% 
  mutate(index.hC_t = 1:n()) %>% ungroup()

df_window <- df_PSD %>% filter(LoadStep == arg_LoadStep) %>% 
  filter(Date <= vmaxDate & Date >= vminDate) %>% 
  ungroup()

N_t <- nrow(df_window)

#### Ornstein-Uhlenbeck - x_tc####
Xtc <- df_window$Xt

model_OU <- setModel(drift = "eta*(mu-x)", diffusion = "sigma", xinit = 0, 
                     state.variable = "x", time.variable = "t", solve.variable = "x")

SDE_OU <- setYuima(model = model_OU, data = setData(Xtc))

OU_start_list <- list(eta = .2, mu = .05, sigma = 0.0)
OU_lower_list <- list(eta = -.2, mu = -.2, sigma = 0.0)
OU_mle <- qmle(SDE_OU, start = OU_start_list, lower = OU_lower_list,method = "L-BFGS-B")

summary(OU_mle)

df_real_orange <- df_PSD %>% filter(LoadStep == arg_LoadStep) %>% 
  filter(Date > vmaxDate) %>% 
  ungroup()

notional <- df_real_orange %>% filter(first(PSD)==PSD) %>% 
  distinct(PSD) %>% as.numeric()

Tc <- nrow(df_real_orange)

X0_simulation <- df_window %>% filter(LoadStep == arg_LoadStep) %>% tail(.,1)

B <- 10000

vmin_plot <- min(c(Xtc,df_real_orange$Xt));vmax_plot <- max(c(Xtc,df_real_orange$Xt))

mat_OU <-matrix(numeric(),nrow = length(N_t:(N_t+Tc)), ncol = B) 

for(b in 1:B){
  simulation_OU <- simulate(model_OU,xinit = X0_simulation$Xt[1],
                     true.param=list(eta = OU_mle@coef['eta'],mu = OU_mle@coef['mu'],sigma = OU_mle@coef['sigma'])
  )
  
  mat_OU[,b] <- simulation_OU@data@original.data[round(seq(from=1,to=101,length.out = 39))]
}

k_perc <- 0
png("../Stochastic_Processes_PSD_Output/Model/Pricing/MonteCarlo_SDE_OU.png")
plot(1:N_t, y =Xtc, xlab = paste0("Índice tc - Carga ",arg_LoadStep), ylab = "Xt", 
     xlim = c(0,N_t+Tc), type = "l", 
     ylim = c(min(c(vmin_plot,mat_OU)),
              max(c(vmax_plot,mat_OU))
              ),cex=0.65,
     main = TeX(paste0('$Orstein-Uhlenbeck \\; \\left(\\eta = ',round(OU_mle@coef['eta'],2),',\\;',
                       '\\mu = ',round(OU_mle@coef['mu'],2),',\\;',
                       '\\sigma = ',round(OU_mle@coef['sigma'],2),
                       '\\right)$'))
)

apply(mat_OU[,sample(1:B,100,replace = F)], 2, function(t, y) lines(t, y, col = 'gray'), t = (N_t):(N_t+Tc))

abline(h=k_perc, col="red", lwd=2, lty=2)

legend("topright",legend = c(
  paste0("Strike - K% = ", round(k_perc*100,2),'%'),
  paste0("100 Simulações Plotadas")), lwd=3:3, lty=2:1, box.lwd=0,
       col = c("red","gray"),box.col = "white",bg = "white",cex = 1,pt.cex=1)

lines(x = (N_t):(N_t+Tc),y = c(X0_simulation$Xt[1],df_real_orange$Xt), lwd=3, type = 'l',col='orange')
dev.off()

png("../Stochastic_Processes_PSD_Output/Model/Pricing/Histograma_XT.png")
hist(mat_OU[Tc,], breaks="fd", xlab = "", main = paste0('Histograma de X(T), N = ',B), cex=0.65)
abline(v=k_perc, col="red", lwd=3, lty=2)
legend("topright",legend = c(paste("Strike - K% = ", k_perc)), lwd=3, lty=2, col = c("red"),
       box.lwd = 0,box.col = "white",bg = "white",cex = 0.65,pt.cex=0.7)

dev.off()
#sapply(mat_OU[Tc,]-k_perc,function(x) max(0,x))

print(paste0('Preço da Call Spread Option é de ',round(mean(sapply(mat_OU[Tc,]-k_perc,function(x) max(0,x)))*notional,2),
  ' com notional de ',round(notional,2),'.')
  )


#### Anexo ####

png("../Stochastic_Processes_PSD_Output/Model/Pricing/Anexo_MonteCarlo_SDE_OU.png")
plot(1:N_t, y =Xtc, xlab = paste0("Índice tc - Carga ",arg_LoadStep), ylab = "Xt", 
     xlim = c(0,N_t+Tc), type = "l", 
     ylim = c(min(c(vmin_plot,mat_OU)),
              max(c(vmax_plot,mat_OU))
     ),cex=0.65,
     main = TeX(paste0('$Orstein-Uhlenbeck \\; \\left(\\eta = ',round(OU_mle@coef['eta'],2),',\\;',
                       '\\mu = ',round(OU_mle@coef['mu'],2),',\\;',
                       '\\sigma = ',round(OU_mle@coef['sigma'],2),
                       '\\right)$'))
)

apply(mat_OU[,1:B], 2, function(t, y) lines(t, y, col = 'gray'), t = (N_t):(N_t+Tc))

abline(h=k_perc, col="red", lwd=2, lty=2)

legend("topright",legend = c(
  paste0("Strike - K% = ", round(k_perc*100,2),'%'),
  paste0("Simulação - N = ",B)), lwd=3:3, lty=2:1, box.lwd=0,
  col = c("red","gray"),box.col = "white",bg = "white",cex = 1,pt.cex=1)

lines(x = (N_t):(N_t+Tc),y = c(X0_simulation$Xt[1],df_real_orange$Xt), lwd=3, type = 'l',col='orange')
dev.off()
