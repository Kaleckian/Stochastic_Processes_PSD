#The histograms here truncate the bins at a absolute percentual difference greater than
# 100% (i.e. x<=-1 or x>=+1). This is defined through the whole script with "v_threshold" variable.

cols <- c("Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C", 'Outlier' = "black",
          "Outlier - Leve" = 'green', "Outlier - Médio" = "orange", "Outlier - Pesado" = "#E41A1C")

if(!dir.exists(paste0("../Stochastic_Processes_PSD_Output/Histograms/"))){
  dir.create(paste0("../Stochastic_Processes_PSD_Output/Histograms/"))
}else{}

v_threshold <- 1

#### Histogram for all years and Load Steps.
png(paste0("../Stochastic_Processes_PSD_Output/Histograms/","AllYears_x_LoadStep",".png"))

p <- dataframe_PSD %>% 
  # mutate(BinOut = if_else(abs(X_t)>=v_threshold,paste0('Outlier - ', as.character(LoadStep)),
  #                         as.character(LoadStep))) %>% 
  mutate(X_t = if_else(abs(X_t)>=v_threshold,sign(X_t)*v_threshold,X_t)) %>% 
  
  ggplot(data =., aes(x = X_t, fill = LoadStep)) +
  
  geom_histogram(aes(y = stat(count) / sum(count)), bins = 30) +
  #geom_histogram(aes(y = ..count..), bins = 30) + 
  
  # y-axis' label as percentage
  #  scale_y_continuous(labels=scales::percent) +
  
  scale_fill_manual(values = cols) +
  
  #  scale_x_continuous(labels = scales::percent) +
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90)) +
  
  facet_grid(rows=vars(SubMkt))  + 
  theme_bw() + 
  # Annotation
  annotate("text", x = v_threshold*26/30, y = .05, label = "Outliers")+
  # geom_curve(
  #   aes(x = v_threshold*26/30, y = .05, xend = v_threshold, yend = .00),
  #   arrow = arrow(length = unit(0.03, "npc"))
  # ) +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  xlab(TeX(paste0('$\\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) + 
  ylab(label = element_blank());print(p)
  
dev.off()

#### Histogram OF LOAD STEPS per Type.
for(arg_LoadStep in unique(dataframe_PSD$LoadStep)){
png(paste0("../Stochastic_Processes_PSD_Output/Histograms/Type_x_",arg_LoadStep,".png"))
  
  p <- dataframe_PSD %>% 
    mutate(BinOut = if_else(abs(X_t)>=v_threshold,'Outlier',as.character(LoadStep))) %>% 
    mutate(X_t = if_else(abs(X_t)>=v_threshold,sign(X_t)*v_threshold,X_t)) %>% 
    
    filter(LoadStep == arg_LoadStep) %>% 
    ggplot(data =., aes(x = X_t, fill = BinOut)) +
    
    geom_histogram(aes(y = stat(count) / sum(count)), bins = 30) +
    #geom_histogram(aes(y = ..count..), bins = 30) + 
    
    # y-axis' label as percentage
  #  scale_y_continuous(labels=scales::percent) +
    
    scale_fill_manual(values = cols) +
  
    #  scale_x_continuous(labels = scales::percent) +
    # x-axis' label rotation
    theme(axis.text.x = element_text(angle = 90)) +
    
    facet_grid(rows=vars(SubMkt),cols=vars(LoadStep,Type_LoadStep),margins = 'Year')  + 
    theme_bw() + 
    # Set legend position of SubMkt
    theme(legend.position = 'none', legend.title = element_blank())  +
    xlab(TeX(paste0('$\\left(\\frac{PLDh_{t}}{PLD^{',arg_LoadStep,'}_{t}} \\right) - 1$'))) +
    ylab(label = element_blank());print(p)
dev.off()
}

#### Histogram of LOAD STEPS split by Year

for(arg_LoadStep in unique(dataframe_PSD$LoadStep)){
png(paste0("../Stochastic_Processes_PSD_Output/Histograms/Year_x_",arg_LoadStep,".png"))
  p <- dataframe_PSD %>% 
    
    mutate(BinOut = if_else(abs(X_t)>=v_threshold,'Outlier',as.character(LoadStep))) %>% 
    mutate(X_t = if_else(abs(X_t)>=v_threshold,sign(X_t)*v_threshold,X_t)) %>% 
    
    filter(LoadStep == arg_LoadStep) %>% 
    ggplot(data =., aes(x = X_t, fill = BinOut)) +
    
    geom_histogram(aes(y = stat(count) / sum(count)), bins = 30) +
    #geom_histogram(aes(y = ..count..), bins = 30) + 
    
    # y-axis' label as percentage
    #  scale_y_continuous(labels=scales::percent) +
    
    scale_fill_manual(values = cols) +
    
    #  scale_x_continuous(labels = scales::percent) +
    # x-axis' label rotation
    theme(axis.text.x = element_text(angle = 90)) +
    
    facet_grid(rows=vars(SubMkt),cols=vars(Year),margins = 'Year')  + 
    theme_bw() + 
    # Set legend position of SubMkt
    theme(legend.position = 'none', legend.title = element_blank())  +
    xlab(TeX(paste0('$\\left(\\frac{PLDh_{t}}{PLD^{',arg_LoadStep,'}_{t}} \\right) - 1$'))) + 
    ylab(label = element_blank());print(p)
dev.off()
}

if(!dir.exists(paste0("../Stochastic_Processes_PSD_Output/Histograms/Year"))){
  dir.create(paste0("../Stochastic_Processes_PSD_Output/Histograms/Year"))
}else{}

#### Histogram OF YEARS per LOAD STEPS
for(arg_year in sort(unique(dataframe_PSD$Year))){
png(paste0("../Stochastic_Processes_PSD_Output/Histograms/Year/",arg_year,"_x_LoadStep",".png"))
  
  p <- dataframe_PSD %>% 
    mutate(BinOut = if_else(abs(X_t)>=v_threshold,'Outlier',as.character(LoadStep))) %>% 
    mutate(X_t = if_else(abs(X_t)>=v_threshold,sign(X_t)*v_threshold,X_t)) %>% 
    
    filter(Year == arg_year) %>% 
    
    ggplot(data =., aes(x = X_t, fill = BinOut)) +
    
    geom_histogram(aes(y = stat(count) / sum(count)), bins = 30) +
    #geom_histogram(aes(y = ..count..), bins = 30) + 
    
    # y-axis' label as percentage
    #  scale_y_continuous(labels=scales::percent) +
    
    scale_fill_manual(values = cols) +
    
    #  scale_x_continuous(labels = scales::percent) +
    # x-axis' label rotation
    theme(axis.text.x = element_text(angle = 90)) +
    
    facet_grid(rows=vars(SubMkt),cols=vars(Year,LoadStep))  + 
    theme_bw() + 
    # Set legend position of SubMkt
    theme(legend.position = 'none', legend.title = element_blank())  +
    xlab(TeX(paste0('$\\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) + 
    ylab(label = element_blank());print(p)
dev.off()
}



