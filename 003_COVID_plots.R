# if(!dir.exists(paste0("../Stochastic_Processes_PSD_Output/Model"))){
#   dir.create(paste0("../Stochastic_Processes_PSD_Output/Model"))
# }else{
#   unlink(paste0("../Stochastic_Processes_PSD_Output/Model"), recursive = TRUE, force = T)
#   dir.create(paste0("../Stochastic_Processes_PSD_Output/Model"))
# }

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

#### Histogram: Period of Analysis - Aug 1st/Nov 06th ####
cols <- c("Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C", 'Outlier' = "black",
          "Outlier - Leve" = 'green', "Outlier - Médio" = "orange", "Outlier - Pesado" = "#E41A1C")


#png("../Stochastic_Processes_PSD_Output/Model/Histogram_Xt_StepLoad.png")
p <- df_PSD %>% 
#  mutate(BinOut = if_else(abs(Xt)>=v_threshold,'Outlier',as.character(LoadStep))) %>% 
#  mutate(Xt = if_else(abs(Xt)>=v_threshold,sign(Xt)*v_threshold,Xt)) %>% 
  ggplot(data =., aes(x = Xt, fill = LoadStep)) +

  geom_histogram(aes(y = ..density..), binwidth = .03) + 

  # y-axis' label as percentage
  #  scale_y_continuous(labels=scales::percent) +
  
  scale_fill_manual(values = cols) +
  
  #  scale_x_continuous(labels = scales::percent) +
  # x-axis' label rotation
  theme(axis.tex_t.x = element_text(angle = 90)) +
  
  facet_grid(rows=vars(LoadStep),cols=vars(Year,SubMkt),margins="TRUE",scales='free_x')  + 
  theme_bw() + 
  # Set legend position of SubMkt
  theme(legend.position = 'none', legend.title = element_blank())  +
  xlab(TeX(paste0('$X_{t}^{','Carga','}','= \\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) +
  ylab(label = element_blank());print(p)
#dev.off()

#png("../Stochastic_Processes_PSD_Output/Model/Histogram_x_t_StepLoad.png")
p <- df_PSD %>%
  ggplot(data =., aes(x = x_t, fill = LoadStep)) +

  #geom_histogram(aes(y = stat(count) / sum(count)), binwidth = .03) +
  geom_histogram(aes(y = ..density..), binwidth = .03) +

  # y-axis' label as percentage
  #  scale_y_continuous(labels=scales::percent) +

  scale_fill_manual(values = cols) +

  #  scale_x_continuous(labels = scales::percent) +
  # x-axis' label rotation
  theme(axis.tex_t.x = element_text(angle = 90)) +

  facet_grid(rows=vars(LoadStep),cols=vars(Year,SubMkt),margins="TRUE",scales='free_x')  +
  theme_bw() +
  # Set legend position of SubMkt
  theme(legend.position = 'none', legend.title = element_blank())  +
  xlab(TeX(paste0('$x_{t}^{','Carga','}','= \\log \\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right)$'))) +
  ylab(label = element_blank());print(p)
#dev.off()


#### Time Series ####
cols <- c('Horário'='#000000',"Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")

#Uncomment ##png(...) and ##dev.off() lines to print the plots in .png.
#png("../Stochastic_Processes_PSD_Output/Model/LoadStepCOVID_2020_PSD.png")
p <- df_PSD %>%
  
  ggplot(data=.,aes(x = timestamp,y = (PSDh),color='Horário'))+geom_line() +
  
  geom_point(aes(y=PSD,colour=LoadStep),shape=1) +
  
  scale_colour_manual(values = cols) +
  
  facet_grid(rows=vars(SubMkt,LoadStep),cols=vars(Year,Fase),
             scales = "free_x",space='free_x') + theme_bw() +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_datetime(breaks='1 weeks ',date_labels ="%d/%b") + 
  # x and y variable labels
  xlab(label = element_blank()) + ylab(label = element_blank());print(p)
#dev.off()

cols <- c('Horário'='#000000',"Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")
#Uncomment ##png(...) and ##dev.off() lines to print the plots in .png.
#png("../Stochastic_Processes_PSD_Output/Model/COVID_2020_PSD.png")
p <- df_PSD %>%
  
  ggplot(data=.,aes(x = timestamp,y = (PSDh),color='Horário'))+geom_line() +
  
  geom_point(aes(y=PSD,colour=LoadStep),shape=1) +
  
  scale_colour_manual(values = cols) +
  
  facet_grid(rows=vars(SubMkt),cols=vars(Year,Fase),
             scales = "free_x",space='free_x') + theme_bw() +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_datetime(breaks='1 weeks ',date_labels ="%d/%b") + 
  # x and y variable labels
  xlab(label = element_blank()) + ylab(label = element_blank());print(p)
#dev.off()


for(arg_LoadStep in unique((df_PSD$LoadStep))){
  cols <- c('Horário'='#000000',"Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")
  #Uncomment ##png(...) and ##dev.off() lines to print the plots in .png.
  #png(paste0('../Stochastic_Processes_PSD_Output/Model/COVID_2020_PSD_',arg_LoadStep,'.png'))
  p <- df_PSD %>% filter(LoadStep == arg_LoadStep) %>% 
    
    ggplot(data=.,aes(x = index.hC_t,y = (PSDh),color='Horário'))+geom_line() +
    
    geom_point(aes(y=PSD,colour=LoadStep),shape=1) +
    
    scale_colour_manual(values = cols) +
    
    facet_grid(rows=vars(SubMkt),cols=vars(Year,Fase),
               scales = "free_x",space='free_x') + theme_bw() +
    # Set legend position of SubMkt
    theme(legend.position="bottom", legend.title = element_blank()) + 
    # x-axis' label rotation
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    
    # x and y variable labels
    xlab(label = element_blank()) + ylab(label = element_blank());print(p)
  #dev.off()
}





cols <- c("Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")
#png("../Stochastic_Processes_PSD_Output/Model/OU_Xt_COVID_2020_PSD.png")

p <- df_PSD %>% 
  
  ggplot(data=.,aes(x = timestamp,y = (Xt),color=LoadStep))+geom_line() +

  scale_colour_manual(values = cols) +

  facet_grid(rows=vars(SubMkt,LoadStep),cols=vars(Year,Fase),
             scales = "free",space='free_x') + theme_bw() +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_datetime(breaks='1 weeks ',date_labels ="%d/%b") + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) + # Multiply by 100 & add %
  # x and y variable labels
  xlab(TeX(paste0('$X_{t}^{','Carga','}','= \\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right) - 1$'))) + 
  ylab(label = element_blank());print(p)
#dev.off()

#png("../Stochastic_Processes_PSD_Output/Model/OU_x_t_COVID_2020_PSD.png")
p <- df_PSD %>% 
  
  ggplot(data=.,aes(x = timestamp,y = (x_t),color=LoadStep))+geom_line() +
  
  scale_colour_manual(values = cols) +
  
  facet_grid(rows=vars(SubMkt,LoadStep),cols=vars(Year,Fase),
             scales = "free",space='free_x') + theme_bw() +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_datetime(breaks='1 weeks ',date_labels ="%d/%b") + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) + # Multiply by 100 & add %
  # x and y variable labels
  xlab(TeX(paste0('$x_{t}^{','Carga','}','= \\log \\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right)$'))) + 
  ylab(label = element_blank());print(p)
#dev.off()

#png("../Stochastic_Processes_PSD_Output/Model/OU_x_t_COVID_2020_PSD.png")
p <- df_PSD %>% 
  
  ggplot(data=.,aes(x = timestamp,y = (PSDh_PSDw),color=LoadStep))+geom_line() +
  
  scale_colour_manual(values = cols) +
  
  facet_grid(rows=vars(SubMkt,LoadStep),cols=vars(Year,Fase),
             scales = "free",space='free_x') + theme_bw() +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_datetime(breaks='1 weeks ',date_labels ="%d/%b") + 
  # x and y variable labels
  xlab(TeX(paste0('$x_{t}^{','Carga','}','= \\log \\left(\\frac{PLDh_{t}}{PLD^{','Carga','}_{t}} \\right)$'))) + 
  ylab(label = element_blank());print(p)
#dev.off()

# PSD weekly
cols <- c("Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")
#png("../Stochastic_Processes_PSD_Output/Week_PSD.png")
p <- dataframe_PSD  %>% 
  mutate(Date = as.POSIXct(strptime(paste0(Start_Date,' 00:00'),"%Y-%m-%d %H:%M"),tz = "America/Buenos_Aires")) %>% 
  select(SubMkt,Date,LoadStep,PSD,Year) %>% distinct() %>% 
  
  ggplot(data=.,aes(x = Date,y = (PSD),color=LoadStep))+geom_line()+
  
#  geom_point(aes(y=PSD,colour=LoadStep),shape=1) +
  
  scale_color_manual(values = cols) +
  
  facet_grid(rows=vars(SubMkt), scales = "free_x") + theme_bw() +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #theme(panel.background = element_blank()) +
  scale_x_datetime(breaks='2 weeks ',date_labels ="%d/%b") + 
  # x and y variable labels
  xlab(label = element_blank()) + ylab(label = element_blank());print(p)
#dev.off()








# # View(dataframe_PSD %>% filter(SubMkt =='SE') %>% 
# #        mutate(Date = as.POSIXct(strptime(paste0(Start_Date,' 00:00'),"%Y-%m-%d %H:%M"),tz = "America/Buenos_Aires")) %>% 
# #        select(SubMkt,Date,LoadStep,PSD,Year) %>% distinct())
# p <- dataframe_PSD  %>% 
#   mutate(Date = as.POSIXct(strptime(paste0(Start_Date,' 00:00'),"%Y-%m-%d %H:%M"),tz = "America/Buenos_Aires")) %>% 
#   select(SubMkt,Date,LoadStep,PSD,Year) %>% distinct() %>% 
#   
#   ggplot(data=.,aes(x = Date,y = log(PSD),color=LoadStep))+geom_line()
# print(p)
