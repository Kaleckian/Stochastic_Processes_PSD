#The histograms here truncate the bins at a absolute percentual difference greater than
# 100% (i.e. x<=-1 or x>=+1). This is defined through the whole script with "v_threshold" variable.

cols <- c("Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C", 'Outlier' = "black",
          "Outlier - Leve" = 'green', "Outlier - Médio" = "orange", "Outlier - Pesado" = "#E41A1C")

if(!dir.exists(paste0("../Stochastic_Processes_PSD_Output/Histograms/"))){
  dir.create(paste0("../Stochastic_Processes_PSD_Output/Histograms/"))
}else{}

v_threshold <- 1

#### Histogram for all years (load steps stacked).
png(paste0("../Stochastic_Processes_PSD_Output/Histograms/","AllYears_stacked_LoadSteps",".png"))

p <- dataframe_PSD %>% 
  # mutate(BinOut = if_else(abs(Xt)>=v_threshold,paste0('Outlier - ', as.character(LoadStep)),
  #                         as.character(LoadStep))) %>% 
  mutate(Xt = if_else(abs(Xt)>=v_threshold,sign(Xt)*v_threshold,Xt)) %>% 
  
  ggplot(data =., aes(x = Xt, fill = LoadStep)) +
  
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
  xlab(TeX(paste0('$X_{t}^{','Carga','}','= \\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) + 
  ylab(label = element_blank());print(p)
dev.off()

v_threshold <- .5
#### Histogram per years (load steps stacked).
png(paste0("../Stochastic_Processes_PSD_Output/Histograms/","PerYears_stacked_LoadSteps",".png"))

p <- dataframe_PSD %>% 
  # mutate(BinOut = if_else(abs(Xt)>=v_threshold,paste0('Outlier - ', as.character(LoadStep)),
  #                         as.character(LoadStep))) %>% 
  mutate(Xt = if_else(abs(Xt)>=v_threshold,sign(Xt)*v_threshold,Xt)) %>% 
  
  ggplot(data =., aes(x = Xt, fill = LoadStep)) +
  
  geom_histogram(aes(y = stat(count) / sum(count)), bins = 30) +
  #geom_histogram(aes(y = ..count..), bins = 30) + 
  
  # y-axis' label as percentage
  #  scale_y_continuous(labels=scales::percent) +
  
  scale_fill_manual(values = cols) +
  
  #  scale_x_continuous(labels = scales::percent) +
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90)) +
  
  facet_grid(cols=vars(Year), rows=vars(SubMkt))  + 
  theme_bw() + 
  # Annotation
  annotate("text", x = v_threshold*22/30, y = .03, label = "Outliers")+
  # geom_curve(
  #   aes(x = v_threshold*26/30, y = .05, xend = v_threshold, yend = .00),
  #   arrow = arrow(length = unit(0.03, "npc"))
  # ) +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  xlab(TeX(paste0('$X_{t}^{','Carga','}','= \\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) +
  ylab(label = element_blank());print(p)

dev.off()

#### Histogram of LOAD STEPS split by Year
for(arg_LoadStep in unique(dataframe_PSD$LoadStep)){
  png(paste0("../Stochastic_Processes_PSD_Output/Histograms/Year_x_",arg_LoadStep,".png"))
  p <- dataframe_PSD %>% 
    
    mutate(BinOut = if_else(abs(Xt)>=v_threshold,'Outlier',as.character(LoadStep))) %>% 
    mutate(Xt = if_else(abs(Xt)>=v_threshold,sign(Xt)*v_threshold,Xt)) %>% 
    
    filter(LoadStep == arg_LoadStep) %>% 
    ggplot(data =., aes(x = Xt, fill = BinOut)) +
    
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
    xlab(TeX(paste0('$X_{t}^{',arg_LoadStep,'}','= \\left(\\frac{PLDh_{t}}{PLD^{',arg_LoadStep,'}_{t}} \\right) - 1$'))) +
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
    mutate(BinOut = if_else(abs(Xt)>=v_threshold,'Outlier',as.character(LoadStep))) %>% 
    mutate(Xt = if_else(abs(Xt)>=v_threshold,sign(Xt)*v_threshold,Xt)) %>% 
    
    filter(Year == arg_year) %>% 
    
    ggplot(data =., aes(x = Xt, fill = BinOut)) +
    
    geom_histogram(aes(y = stat(count) / sum(count)), bins = 30) +
    #geom_histogram(aes(y = ..count..), bins = 30) + 
    
    # y-axis' label as percentage
    #  scale_y_continuous(labels=scales::percent) +
    
    scale_fill_manual(values = cols) +
    
    #  scale_x_continuous(labels = scales::percent) +
    # x-axis' label rotation
    theme(axis.text.x = element_text(angle = 90)) +
    
    facet_grid(rows=vars(LoadStep),cols=vars(Year,SubMkt),scales='free_y')  + 
    theme_bw() + 
    # Set legend position of SubMkt
    theme(legend.position = 'none', legend.title = element_blank())  +
    xlab(TeX(paste0('$X_{t}^{','Carga','}','= \\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) +
    ylab(label = element_blank());print(p)
dev.off()
}

v_threshold <- 10
df_PSD <- dataframe_PSD %>% filter(Year == '2020', SubMkt == 'SE')
#### Histogram OF LOAD STEPS per Type.
for(arg_year in unique(df_PSD$Year)){
  png(paste0("../Stochastic_Processes_PSD_Output/Histograms/DENSITY_Type_x_",arg_year,".png"))
  
  p <- df_PSD %>% 
    mutate(BinOut = if_else(abs(Xt)>=v_threshold,'Outlier',as.character(LoadStep))) %>% 
    mutate(Xt = if_else(abs(Xt)>=v_threshold,sign(Xt)*v_threshold,Xt)) %>% 
    
    filter(Year == arg_year) %>% 
    ggplot(data =., aes(x = Xt, fill = BinOut)) +
    
#    geom_histogram(aes(y = stat(count) / sum(count)), bins = 30) +
    geom_histogram(aes(y = ..density..), bins = 30) + 
    
    # y-axis' label as percentage
    #  scale_y_continuous(labels=scales::percent) +

    scale_fill_manual(values = cols) +
    
    #  scale_x_continuous(labels = scales::percent) +
    # x-axis' label rotation
    theme(axis.text.x = element_text(angle = 90)) +
    
    facet_grid(rows=vars(LoadStep),cols=vars(Year,SubMkt,Type_LoadStep),margins="TRUE",scales='free')  + 
    theme_bw() + 
    # Set legend position of SubMkt
    theme(legend.position = 'none', legend.title = element_blank())  +
    xlab(TeX(paste0('$X_{t}^{','Carga','}','= \\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) +
    ylab(label = element_blank());print(p)
  dev.off()
}

