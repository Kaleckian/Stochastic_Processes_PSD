if(!dir.exists(paste0("../Stochastic_Processes_PSD_Output/"))){
  dir.create(paste0("../Stochastic_Processes_PSD_Output/"))
}else{}

jpeg("../Stochastic_Processes_PSD_Output/AllYears_PSD.jpeg")

cols <- c('Horário'='#000000',"Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")

p <- dataframe_PSD %>%
  
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

print(p);dev.off()

for(arg_year in 2018:2020){
  
  if(!dir.exists(paste0("../Stochastic_Processes_PSD_Output/",arg_year))){
    dir.create(paste0("../Stochastic_Processes_PSD_Output/",arg_year))
  }else{}
  
  jpeg(paste0("../Stochastic_Processes_PSD_Output/",arg_year,"/",arg_year,"_PSD.jpeg"))  
  p <- dataframe_PSD %>%
    filter(Year == arg_year) %>%
    ggplot(data=.,aes(x = timestamp,y = (PSDh),color='Horário'))+geom_line() +
    
    geom_point(aes(y=PSD,colour=LoadStep),shape=1,alpha=0.2) +
    
    scale_colour_manual(values = cols) +
    
    facet_grid(rows=vars(SubMkt),cols=vars(Year),
               scales = "free") + theme_bw() + 
    # Set legend position of SubMkt
    theme(legend.position="bottom", legend.title = element_blank()) + 
    # x-axis' label rotation
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    # x-axis' datetime scale
    scale_x_datetime(breaks='20 days',date_labels ="%d/%b") + 
    # x and y variable labels
    xlab(label = element_blank()) + ylab(label = element_blank())# +
    #labs(title=paste0('Ano de ',arg_year))
  print(p);dev.off()
  
  vec.months <- sort(unique(dataframe_PSD$Month[dataframe_PSD$Year==arg_year]))
  
  for(arg_month in vec.months){
    p <- 
      dataframe_PSD %>%
      filter(Year == arg_year) %>%
      filter(Month == arg_month) %>%
      ggplot(data=.,aes(x = timestamp,y = (PSDh),color='Horário'))+geom_line() +
      
      geom_point(aes(y=PSD,colour=LoadStep),shape=1,alpha=0.2) +
      
      scale_colour_manual(values = cols) +
      
      facet_grid(rows=vars(SubMkt),cols=vars(Year,Month_label),
                 scales = "free")  + theme_bw() + 
      # Set legend position of SubMkt
      theme(legend.position="bottom", legend.title = element_blank()) + 
      # x-axis' label rotation
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      # x-axis' datetime scale
      scale_x_datetime(breaks='3 days',date_labels ="%d/%b") + 
      # x and y variable labels
      xlab(label = element_blank()) + ylab(label = element_blank())
    
    vYYYYMM <- paste0(arg_year,ifelse(arg_month<=9,paste0('0',arg_month),arg_month))
    
    jpeg(paste0("../Stochastic_Processes_PSD_Output/",arg_year,"/",vYYYYMM,"_PSD.jpeg"))  
    print(p);dev.off()  
  }
}
  
  
