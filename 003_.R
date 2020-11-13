if(!dir.exists(paste0("../Stochastic_Processes_PSD_Output/Model/"))){
  dir.create(paste0("../Stochastic_Processes_PSD_Output/Model/"))
}else{}

cols <- c("Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C", 'Outlier' = "black",
          "Outlier - Leve" = 'green', "Outlier - Médio" = "orange", "Outlier - Pesado" = "#E41A1C")

df_PSD <- dataframe_PSD %>% filter(SubMkt == 'SE', Date >= ymd('2020-07-01')) %>% 
  arrange(Date,Hour_Hourly,SubMkt) %>% group_by(SubMkt,LoadStep) %>% 
  mutate(index.hC_t = 1:n()) %>% ungroup()

#png("../Stochastic_Processes_PSD_Output/Model/Histogram_StepLoad.png")
v_threshold <- .2

p <- df_PSD %>% 
  mutate(BinOut = if_else(abs(Xt)>=v_threshold,'Outlier',as.character(LoadStep))) %>% 
  mutate(Xt = if_else(abs(Xt)>=v_threshold,sign(Xt)*v_threshold,Xt)) %>% 
  ggplot(data =., aes(x = Xt, fill = BinOut)) +
  
  geom_histogram(aes(y = stat(count) / sum(count)), bins = 30) +
#  geom_histogram(aes(y = ..density..), bins = 30) + 
  
  # y-axis' label as percentage
  #  scale_y_continuous(labels=scales::percent) +
  
  scale_fill_manual(values = cols) +
  
  #  scale_x_continuous(labels = scales::percent) +
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90)) +
  
  facet_grid(rows=vars(LoadStep),cols=vars(Year,SubMkt),margins="TRUE",scales='free_x')  + 
  theme_bw() + 
  # Set legend position of SubMkt
  theme(legend.position = 'none', legend.title = element_blank())  +
  xlab(TeX(paste0('$X_{t}^{','Carga','}','= \\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) +
  ylab(label = element_blank());print(p)
#dev.off()

cols <- c('Horário'='#000000',"Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")
png("../Stochastic_Processes_PSD_Output/Model/SDE_Xt.png")
p<- ggplot()+
  geom_line(data=df_PSD,aes(x = timestamp,y = Xt,color='Horário'))+
  geom_point(data=df_PSD,aes(x = timestamp,y=Xt,color=LoadStep),shape=1) +
  scale_color_manual(values = cols) +
  
  #  scale_x_continuous(labels = scales::percent) +
  # x-axis' label rotation

  scale_x_datetime(breaks='1 weeks ',date_labels ="%d/%b") + 
  
  facet_grid(rows=vars(LoadStep),scales='free')  + 
  theme_bw() + 

  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank())+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.title.x = element_text(size = 5)) +
  xlab(TeX(paste0('$X_{t}^{','Carga','}','= \\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) + 
  xlab(label = element_blank()) +
  ylab(label = element_blank());print(p)
dev.off()

acf2(df_PSD$Xt[vec.Leve],max.lag = 24)
acf2(df_PSD$Xt[vec.Medio],max.lag = 24)
acf2(df_PSD$Xt[vec.Pesado],max.lag = 24)
source('Sub/function_autoarima.R')
fnc_autoarima(ret = )

names(df_PSD)
vec.Type1 <- df_PSD$Type_LoadStep=='Tipo 1'
vec.Type2 <- df_PSD$Type_LoadStep=='Tipo 2'

vec.Leve <- df_PSD$LoadStep=='Leve'
vec.Medio <- df_PSD$LoadStep=='Médio'
vec.Pesado <- df_PSD$LoadStep=='Pesado'

vec.D.Chg_LS <- df_PSD$Dummy_ChgLoadStep==1
vec.D.Chg_Day <- df_PSD$Dummy_ChgDay==1
vec.D.Chg_Both <- (vec.D.Chg_Day*vec.D.Chg_LS)==1





ggplot()+
  geom_line(data=df_PSD,aes(x = timestamp,y = h_t,color=LoadStep))+
  scale_colour_manual(values = cols)

ggplot()+
  geom_line(data=df_PSD,aes(x = index.hC_t,y = Xt,color='Horário'))+
  scale_colour_manual(values = cols) + 
  theme(axis.text.x = element_text(angle = 90)) +
  
  facet_grid(rows=vars(LoadStep))  + 
  theme_bw() + 
  # Set legend position of SubMkt
  theme(legend.position = 'none', legend.title = element_blank())  +
  #xlab(TeX(paste0('$\\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) + 
  ylab(label = element_blank())

#View(df_PSD %>% filter(LoadStep=='Leve'))

ggplot()+
  geom_line(data=df_PSD,aes(x = timestamp,y = hC_t,color=LoadStep))+
  scale_colour_manual(values = cols)

acf2(df_PSD$h_t,max.lag = 48)
acf2(df_PSD$h_t[vec.Type1],max.lag = 48)
acf2(df_PSD$h_t[vec.Type2],max.lag = 48)

acf2(df_PSD$Xt,max.lag = 48)
acf2(df_PSD$Xt[!vec.D.Chg_Day],max.lag = 48)
acf2(df_PSD$Xt[!vec.D.Chg_LS],max.lag = 48)
acf2(df_PSD$Xt[!vec.D.Chg_Both],max.lag = 48)
acf2(df_PSD$Xt[!vec.D.Chg_LS&!vec.D.Chg_Day],max.lag = 48)


acf2(df_PSD$Xt[vec.Leve],max.lag = 24)
acf2(df_PSD$Xt[vec.Leve&!vec.D.Chg_Day],max.lag = 48)
acf2(df_PSD$Xt[vec.Leve&!vec.D.Chg_LS],max.lag = 48)
acf2(df_PSD$Xt[vec.Leve&!vec.D.Chg_Both],max.lag = 48)
acf2(df_PSD$Xt[vec.Leve&!vec.D.Chg_LS&!vec.D.Chg_Day],max.lag = 48)

sum(vec.Leve)
sum(vec.Leve&!vec.D.Chg_Day)
sum(vec.Leve&!vec.D.Chg_LS)
sum(vec.Leve&!vec.D.Chg_Both)
sum(vec.Leve&!vec.D.Chg_LS*!vec.D.Chg_Day)

acf2(df_PSD$hC_t[vec.Medio],max.lag = 48)
acf2(df_PSD$hC_t[vec.Medio&!vec.D.Chg_Day],max.lag = 48)
acf2(df_PSD$hC_t[vec.Medio&!vec.D.Chg_LS],max.lag = 48)
acf2(df_PSD$hC_t[vec.Medio&!vec.D.Chg_Both],max.lag = 48)
acf2(df_PSD$hC_t[vec.Medio&!vec.D.Chg_LS&!vec.D.Chg_Day],max.lag = 48)

sum(vec.Medio)
sum(vec.Medio&!vec.D.Chg_Day)
sum(vec.Medio&!vec.D.Chg_LS)
sum(vec.Medio&!vec.D.Chg_Both)
sum(vec.Medio&!vec.D.Chg_LS&!vec.D.Chg_Day)

acf2(df_PSD$hC_t[vec.Pesado],max.lag = 48)
acf2(df_PSD$hC_t[vec.Pesado&!vec.D.Chg_Day],max.lag = 48)
acf2(df_PSD$hC_t[vec.Pesado&!vec.D.Chg_LS],max.lag = 48)
acf2(df_PSD$hC_t[vec.Pesado&!vec.D.Chg_Both],max.lag = 48)
acf2(df_PSD$hC_t[vec.Pesado&!vec.D.Chg_LS&!vec.D.Chg_Day],max.lag = 48)

sum(vec.Pesado)
sum(vec.Pesado&!vec.D.Chg_Day)
sum(vec.Pesado&!vec.D.Chg_LS)
sum(vec.Pesado&!vec.D.Chg_Both)
sum(vec.Pesado&!vec.D.Chg_LS&!vec.D.Chg_Day)