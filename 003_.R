df_PSD <- dataframe_PSD %>% filter(SubMkt == 'SE', Year == '2020')

cols <- c('Horário'='#000000',"Leve" = 'green', "Médio" = "orange", "Pesado" = "#E41A1C")

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
