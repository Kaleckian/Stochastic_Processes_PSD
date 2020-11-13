if(!dir.exists(paste0("../Stochastic_Processes_PSD_Output/2020/COVID"))){
  dir.create(paste0("../Stochastic_Processes_PSD_Output/2020/COVID"))
}else{}


cols <- c('Horário'='#000000',"Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")
#cols <- c('Horário'='gray',"Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")

#Uncomment png(...) and #dev.off() lines to print the plots in .png.
png("../Stochastic_Processes_PSD_Output/2020/COVID/COVID_2020_PSD.png")

df_PSD <- dataframe_PSD %>% filter(Year == '2020') %>% mutate(Fase = case_when(
  Month %in% 1 ~ 'jan',
  Month %in% 2:3 ~ 'fev-mar',
  Month %in% 4:7~ 'abr-jul',
  Month %in% 8:12 ~ 'ago-nov'
)) %>% 
  mutate(Fase = factor(x = Fase, ordered=T,levels=c('jan','fev-mar','abr-jul','ago-nov')))

p <- df_PSD %>%
  
  ggplot(data=.,aes(x = timestamp,y = (PSDh),color='Horário'))+geom_line() +
  
  geom_point(aes(y=PSD,colour=LoadStep),shape=1,alpha=0.2) +
  
  scale_colour_manual(values = cols) +
  
  facet_grid(rows=vars(SubMkt),cols=vars(Year,Fase),
             scales = "free_x",space='free_x') + theme_bw() +
  # Set legend position of SubMkt
  theme(legend.position="bottom", legend.title = element_blank()) + 
  # x-axis' label rotation
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_datetime(breaks='2 weeks ',date_labels ="%d/%b") + 
  # x and y variable labels
  xlab(label = element_blank()) + ylab(label = element_blank());print(p)

dev.off()
