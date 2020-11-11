if(!dir.exists(paste0("../Stochastic_Processes_PSD_Output/Model/"))){
  dir.create(paste0("../Stochastic_Processes_PSD_Output/Model/"))
}else{}

cols <- c('Horário'='#000000',"Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")

df_PSD <- dataframe_PSD %>% filter(SubMkt == 'SE', Year == '2020') %>% 
  arrange(Date,Hour_Hourly,SubMkt) %>% group_by(SubMkt,LoadStep) %>% 
  mutate(index.hC_t = 1:n()) %>% ungroup()

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
  geom_line(data=df_PSD,aes(x = timestamp,y = h_t,color='Horário'))+
  scale_colour_manual(values = cols) + 
  theme(axis.text.x = element_text(angle = 90)) +
  
  facet_grid(rows=vars(SubMkt),cols=vars(Year,LoadStep))  + 
  theme_bw() + 
  # Set legend position of SubMkt
  theme(legend.position = 'none', legend.title = element_blank())  +
  #xlab(TeX(paste0('$\\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) + 
  ylab(label = element_blank())

ggplot()+
  geom_line(data=df_PSD,aes(x = timestamp,y = h_t,color=LoadStep))+
  scale_colour_manual(values = cols)

ggplot()+
  geom_line(data=df_PSD,aes(x = index.hC_t,y = hC_t,color='Horário'))+
  scale_colour_manual(values = cols) + 
  theme(axis.text.x = element_text(angle = 90)) +
  
  facet_grid(rows=vars(LoadStep))  + 
  theme_bw() + 
  # Set legend position of SubMkt
  theme(legend.position = 'none', legend.title = element_blank())  +
  #xlab(TeX(paste0('$\\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) + 
  ylab(label = element_blank())

View(df_PSD %>% filter(LoadStep=='Leve'))

ggplot()+
  geom_line(data=df_PSD,aes(x = timestamp,y = hC_t,color=LoadStep))+
  scale_colour_manual(values = cols)

acf2(df_PSD$h_t,max.lag = 48)
acf2(df_PSD$h_t[vec.Type1],max.lag = 48)
acf2(df_PSD$h_t[vec.Type2],max.lag = 48)

df %>% filter(LoadStep == 'Leve',)

acf2(df_PSD$hC_t[vec.Leve],max.lag = 48)
acf2(df_PSD$hC_t[vec.Leve&!vec.D.Chg_Day],max.lag = 48)
acf2(df_PSD$hC_t[vec.Leve&!vec.D.Chg_LS],max.lag = 48)
acf2(df_PSD$hC_t[vec.Leve&!vec.D.Chg_Both],max.lag = 48)
acf2(df_PSD$hC_t[vec.Leve&!vec.D.Chg_LS&!vec.D.Chg_Day],max.lag = 48)

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


sarima(ICTI,p=1,d=0,q=0)
ICTI.for <- sarima.for(ICTI,p=1,d=0,q=0,n.ahead=2)



cols <- c('Horário'='gray',"Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")
#000000
df_PSD %>%
  ggplot(data=.,aes(x = timestamp,y = (PSDh),color='Horário'))+geom_line() +
  geom_point(aes(y=PSD,colour=LoadStep),shape=1,alpha=0.2) +
  scale_colour_manual(values = cols) +
  facet_grid(rows=vars(SubMkt),cols=vars(Year),
             scales = "free") + theme_bw() +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_datetime(breaks='1 month',date_labels ="%b/%y") + 
  # x and y variable labels
  xlab(label = element_blank()) + ylab(label = element_blank()) 

df_PSD %>%
  ggplot(data=.,aes(x = timestamp,y = (PSDh),color='Horário'), shape = '-')+geom_point() +
  geom_point(aes(y=PSD,colour=LoadStep),shape=1,alpha=0.2) +
  scale_colour_manual(values = cols) +
  facet_grid(rows=vars(SubMkt, LoadStep),cols=vars(Year)) + theme_bw() +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_datetime(breaks='1 month',date_labels ="%b/%y") + 
  # x and y variable labels
  xlab(label = element_blank()) + ylab(label = element_blank()) 

df_PSD %>%
  ggplot(data=.,aes(x = timestamp,y = (PSDh),color='Horário'), shape = '-')+geom_point() +
  geom_point(aes(y=PSD,colour=LoadStep),shape=1,alpha=0.2) +
  scale_colour_manual(values = cols) +
  facet_grid(rows=vars(SubMkt, LoadStep),cols=vars(Month_label), scales = 'free') + theme_bw() +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_datetime(breaks='1 month',date_labels ="%b/%y") + 
  # x and y variable labels
  xlab(label = element_blank()) + ylab(label = element_blank()) 

for(arg_LoadStep in unique(dataframe_PSD$LoadStep)){
  p <- dataframe_PSD %>% 
    filter(LoadStep == arg_LoadStep) %>% 
    ggplot(data =., aes(x = log_diff, fill = LoadStep)) +
    
    #geom_histogram(aes(y = stat(count) / sum(count)), bins = 8) +
    geom_histogram(aes(y = ..density..)) + 
    
    # y-axis' label as percentage
    #  scale_y_continuous(labels=scales::percent) +
    
    scale_fill_manual(values = cols) +
    
    #  scale_x_continuous(labels = scales::percent) +
    # x-axis' label rotation
    theme(axis.text.x = element_text(angle = 90)) +
    
    facet_grid(rows=vars(SubMkt),cols=vars(Type_LoadStep),margins = 'Year')  + 
    theme_bw() + 
    # Set legend position of SubMkt
    theme(legend.position = 'none', legend.title = element_blank())  +
    xlab(TeX(paste0('$log \\left(\\frac{PLDh_{t}}{PLD^{',arg_LoadStep,'}_{t}} \\right)$'))) +
    ylab(label = element_blank())
  print(p)
}

df_toplot <- df_PSD #%>% filter(ID_Week<=2+min(ID_Week))

ggplot()+
  #  geom_bar(data=df_toplot,aes(x = timestamp,y = (PSDh),fill='Horário'),stat='identity') +
  geom_area(data=df_toplot,aes(x = timestamp,y = (PSDh),fill='Horário'),stat='identity') +
  geom_point(data=df_toplot,aes(x = timestamp,y=PSD,colour=LoadStep),shape=1,stat='identity') +
  scale_fill_manual(values = cols) +
  scale_colour_manual(values = cols) +
  
  facet_grid(rows=vars(SubMkt),cols=vars(Year),
             scales = "free") + theme_bw() +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  
  scale_x_datetime(breaks='2 weeks',date_labels ="%d/%b/%y",
                   limits = c(min(df_toplot$timestamp),max(df_toplot$timestamp))) + 
  # x and y variable labels
  xlab(label = element_blank()) + ylab(label = element_blank())

for(arg_LoadStep in unique(dataframe_PSD$LoadStep)){  
  p <- df_PSD %>% filter(LoadStep == arg_LoadStep) %>% 
    filter(abs(diff_perc)<.5) %>% 
    ggplot(data =., aes(x = diff_perc, fill = LoadStep)) +
    
    #geom_histogram(aes(y = stat(count) / sum(count)), bins = 8) +
    geom_histogram(aes(y = ..density..)) + 
    
    # y-axis' label as percentage
    #  scale_y_continuous(labels=scales::percent) +
    
    scale_fill_manual(values = cols) +
    
    #  scale_x_continuous(labels = scales::percent) +
    # x-axis' label rotation
    theme(axis.text.x = element_text(angle = 90)) +
    
    facet_grid(rows=vars(SubMkt),cols=vars(LoadStep),margins = 'Year')  + 
    theme_bw() + 
    # Set legend position of SubMkt
    theme(legend.position = 'none', legend.title = element_blank())  +
    xlab(TeX(paste0('$log \\left(\\frac{PLDh_{t}}{PLD^{',arg_LoadStep,'}_{t}} \\right)$'))) +
    ylab(label = element_blank())
  print(p)
}  
