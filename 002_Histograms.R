#cols <- c('Horário'='#000000',"Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")

cols <- c("Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")

if(!dir.exists(paste0("../Stochastic_Processes_PSD_Output/Histograms/"))){
  dir.create(paste0("../Stochastic_Processes_PSD_Output/Histograms/"))
}else{}



for(arg_LoadStep in unique(dataframe_PSD$LoadStep)){
  png(paste0("../Stochastic_Processes_PSD_Output/Histograms/Type_x_",arg_LoadStep,".png"))
  p <- dataframe_PSD %>% 
    filter(LoadStep == arg_LoadStep) %>% 
    ggplot(data =., aes(x = log_diff, fill = LoadStep)) +
    
    #geom_histogram(aes(y = stat(count) / sum(count)), bins = 8) +
    geom_histogram(aes(y = ..density..), bins = 30) + 
    
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
  print(p);dev.off()
}

for(arg_LoadStep in unique(dataframe_PSD$LoadStep)){
  png(paste0("../Stochastic_Processes_PSD_Output/Histograms/Year_x_",arg_LoadStep,".png"))
  p <- dataframe_PSD %>% 
    filter(LoadStep == arg_LoadStep) %>% 
    ggplot(data =., aes(x = log_diff, fill = LoadStep)) +
    
    #geom_histogram(aes(y = stat(count) / sum(count)), bins = 8) +
    geom_histogram(aes(y = ..density..), bins = 30) + 
    
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
    xlab(TeX(paste0('$log \\left(\\frac{PLDh_{t}}{PLD^{',arg_LoadStep,'}_{t}} \\right)$'))) + 
    ylab(label = element_blank())
  print(p);dev.off()
}

if(!dir.exists(paste0("../Stochastic_Processes_PSD_Output/Histograms/Year"))){
  dir.create(paste0("../Stochastic_Processes_PSD_Output/Histograms/Year"))
}else{}

for(arg_year in sort(unique(dataframe_PSD$Year))){
  png(paste0("../Stochastic_Processes_PSD_Output/Histograms/Year/",arg_year,"_x_LoadStep",".png"))
  p <- dataframe_PSD %>% 
    filter(Year == arg_year) %>% 
    ggplot(data =., aes(x = log_diff, fill = LoadStep)) +
    
    #geom_histogram(aes(y = stat(count) / sum(count)), bins = 8) +
    geom_histogram(aes(y = ..density..), bins = 30) + 
    
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
    xlab(TeX(paste0('$log \\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right)$'))) + 
    ylab(label = element_blank())
  print(p);dev.off()
}
