#------------------------------------------------------------------------------------------#
####                      Diametric distribution - Life Rebollo                         ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 19/09/2023                              #
#                              Last modification: 27/09/2023                               #
#------------------------------------------------------------------------------------------#

library(openxlsx)
library(ggplot2)
library(dplyr)
library(plyr)


#### Even to uneven-aged stands ####

setwd('/media/aitor/WDE/iuFOR_trabajo/Proyectos/Life_Rebollo/0_Entregable_C11/2_simanfor/4_septiembre_23/output/')

# data
sg_control <- read.xlsx('SG/LR_SG02_control__Output_Plot_sg02.xlsx', sheet = 'Nodo 16 - Pies inventariados')
sg_expertos <- read.xlsx('SG/LR_SG02_expertos__Output_Plot_sg02.xlsx', sheet = 'Nodo 30 - Pies inventariados')
sg_qp2 <- read.xlsx('SG/LR_SG02_QP2__Output_Plot_sg02.xlsx', sheet = 'Nodo 25 - Pies inventariados')
sg_mix <- read.xlsx('SG/LR_SG02_mix__Output_Plot_sg02.xlsx', sheet = 'Nodo 17 - Pies inventariados')

so_control <- read.xlsx('SO/LR_SO02_control__Output_Plot_so02.xlsx', sheet = 'Nodo 16 - Pies inventariados')
so_expertos <- read.xlsx('SO/LR_SO02_expertos__Output_Plot_so02.xlsx', sheet = 'Nodo 30 - Pies inventariados')
so_qp2 <- read.xlsx('SO/LR_SO02_QP2__Output_Plot_so02.xlsx', sheet = 'Nodo 25 - Pies inventariados')
so_mix <- read.xlsx('SO/LR_SO02_mix__Output_Plot_so02.xlsx', sheet = 'Nodo 25 - Pies inventariados')

# assign scnr code
sg_control$id_scnr <- 'control'
sg_expertos$id_scnr <- 'expertos'
sg_qp2$id_scnr <- 'qp2'
sg_mix$id_scnr <- 'mix'

so_control$id_scnr <- 'control'
so_expertos$id_scnr <- 'expertos'
so_qp2$id_scnr <- 'qp2'
so_mix$id_scnr <- 'mix'

# make a list of dfs
list_of_dfs <- list(sg_control, sg_expertos, sg_qp2, sg_mix, so_control, so_expertos, so_qp2, so_mix)

# manage all dfs
all_plots <- data.frame()

for (k in list_of_dfs){
  
  # delete I and M trees
  k <- k[is.na(k$estado), ]
  
  # create dbh classes
  plot <- plyr::ddply(k, c('ID_parcela', 'id_scnr'), summarise,      
                      CD_0_75 = sum(ifelse(dbh <= 7.5, factor_expansion, 0), na.rm = TRUE),
                      CD_75_125 = sum(ifelse(dbh > 7.5 & dbh <= 12.5, factor_expansion, 0), na.rm = TRUE),
                      CD_125_175 = sum(ifelse(dbh > 12.5 & dbh <= 17.5, factor_expansion, 0), na.rm = TRUE),
                      CD_175_225 = sum(ifelse(dbh > 17.5 & dbh <= 22.5, factor_expansion, 0), na.rm = TRUE),
                      CD_225_275 = sum(ifelse(dbh > 22.5 & dbh <= 27.5, factor_expansion, 0), na.rm = TRUE),
                      CD_275_325 = sum(ifelse(dbh > 27.5 & dbh <= 32.5, factor_expansion, 0), na.rm = TRUE),
                      CD_325_375 = sum(ifelse(dbh > 32.5 & dbh <= 37.5, factor_expansion, 0), na.rm = TRUE),
                      CD_375_425 = sum(ifelse(dbh > 37.5 & dbh <= 42.5, factor_expansion, 0), na.rm = TRUE),
                      CD_425_ = sum(ifelse(dbh > 42.5, factor_expansion, 0), na.rm = TRUE)
  )
  
  # organize data
  plot <- select(plot, -c(ID_parcela, id_scnr))
  plot <- data.frame(t(plot))
  plot <- dplyr::rename(plot, N = t.plot.)
  
  plot$CD <- c(5, 10, 15, 20, 25, 30, 35, 40, 45)
  plot$ID_Inventario <- paste(unique(toupper(k$ID_parcela)), unique(k$id_scnr), sep = '_')
  plot$ID_Parcela <- unique(toupper(k$ID_parcela))
  plot$Escenario <- unique(k$id_scnr)
  
  # round to units
  plot$N <- round(plot$N, digits = 0)
  
  # add plot to the main df
  all_plots <- rbind(all_plots, plot)
}


#### Graph results ####

# graph joined
ggplot(all_plots, aes(x = CD, y = N, fill = Escenario)) +
  
  # data and labels
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_text(aes(label = N), hjust = 0, vjust = -1, size = 5, color = 'darkred') +
  
  # titles
  labs(title = 'Distribución diamétrica resultante de la simulación',
       x = 'Clase Diamétrica (cm)',
       y = 'Densidad (pies/ha)') +
  
  # theme
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) + 
  
  # split by plot
  facet_wrap(~ ID_Parcela, scales = 'free')

ggsave(filename = '../graphs/dbh/dbh_resultados.png', device = 'png', units = 'mm', dpi = 300,
       width = 300, height = 300)  

rm(list = ls())


#### Uneven-aged stands management ####

setwd('/media/aitor/WDE/iuFOR_trabajo/Proyectos/Life_Rebollo/0_Entregable_C11/2_simanfor/4_septiembre_23/output/')

# data
sg_control <- read.xlsx('irregular/LR_irregular_control__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 16 - Pies inventariados')
sg_expertos <- read.xlsx('irregular/LR_irregular_expertos__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 29 - Pies inventariados')
sg_mix <- read.xlsx('irregular/LR_irregular_mix__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 35 - Pies inventariados')
sg_qp2 <- read.xlsx('irregular/LR_irregular_QP2__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 29 - Pies inventariados')

so_control <- read.xlsx('irregular/LR_irregular_control__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 16 - Pies inventariados')
so_expertos <- read.xlsx('irregular/LR_irregular_expertos__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 29 - Pies inventariados')
so_mix <- read.xlsx('irregular/LR_irregular_mix__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 35 - Pies inventariados')
so_qp2 <- read.xlsx('irregular/LR_irregular_QP2__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 29 - Pies inventariados')

# assign scnr code
sg_control$id_scnr <- 'control'
sg_expertos$id_scnr <- 'expertos'
sg_qp2$id_scnr <- 'qp2'
sg_mix$id_scnr <- 'mix'

so_control$id_scnr <- 'control'
so_expertos$id_scnr <- 'expertos'
so_qp2$id_scnr <- 'qp2'
so_mix$id_scnr <- 'mix'

# make a list of dfs
list_of_dfs <- list(sg_control, sg_expertos, sg_qp2, sg_mix, so_control, so_expertos, so_qp2, so_mix)

# manage all dfs
all_plots <- data.frame()

for (k in list_of_dfs){
  
  # delete I and M trees
  k <- k[is.na(k$estado), ]
  
  # create dbh classes
  plot <- plyr::ddply(k, c('ID_parcela', 'id_scnr'), summarise,      
                      CD_0_75 = sum(ifelse(dbh <= 7.5, factor_expansion, 0), na.rm = TRUE),
                      CD_75_125 = sum(ifelse(dbh > 7.5 & dbh <= 12.5, factor_expansion, 0), na.rm = TRUE),
                      CD_125_175 = sum(ifelse(dbh > 12.5 & dbh <= 17.5, factor_expansion, 0), na.rm = TRUE),
                      CD_175_225 = sum(ifelse(dbh > 17.5 & dbh <= 22.5, factor_expansion, 0), na.rm = TRUE),
                      CD_225_275 = sum(ifelse(dbh > 22.5 & dbh <= 27.5, factor_expansion, 0), na.rm = TRUE),
                      CD_275_325 = sum(ifelse(dbh > 27.5 & dbh <= 32.5, factor_expansion, 0), na.rm = TRUE),
                      CD_325_375 = sum(ifelse(dbh > 32.5 & dbh <= 37.5, factor_expansion, 0), na.rm = TRUE),
                      CD_375_425 = sum(ifelse(dbh > 37.5 & dbh <= 42.5, factor_expansion, 0), na.rm = TRUE),
                      CD_425_ = sum(ifelse(dbh > 42.5, factor_expansion, 0), na.rm = TRUE)
  )
  
  # organize data
  plot <- select(plot, -c(ID_parcela, id_scnr))
  plot <- data.frame(t(plot))
  plot <- dplyr::rename(plot, N = t.plot.)
  
  plot$CD <- c(5, 10, 15, 20, 25, 30, 35, 40, 45)
  plot$ID_Inventario <- paste(unique(toupper(k$ID_parcela)), unique(k$id_scnr), sep = '_')
  plot$ID_Parcela <- unique(toupper(k$ID_parcela))
  plot$Escenario <- unique(k$id_scnr)
  
  # round to units
  plot$N <- round(plot$N, digits = 0)
  
  # add plot to the main df
  all_plots <- rbind(all_plots, plot)
}


#### Graph results ####

# graph joined
ggplot(all_plots, aes(x = CD, y = N, fill = Escenario)) +
  
  # data and labels
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_text(aes(label = N), hjust = 0, vjust = -1, size = 5, color = 'darkred') +
  
  # titles
  labs(title = 'Distribución diamétrica resultante de la simulación',
       x = 'Clase Diamétrica (cm)',
       y = 'Densidad (pies/ha)') +
  
  # theme
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) + 
  
  # split by plot
  facet_wrap(~ ID_Parcela, scales = 'free')

ggsave(filename = '../graphs/dbh/dbh_resultados_irregulares.png', device = 'png', units = 'mm', dpi = 300,
       width = 300, height = 300)  



#### Uneven-aged stands management - stand evolution ####


----- no funcionando de momento



setwd('/media/aitor/WDE/iuFOR_trabajo/Proyectos/Life_Rebollo/0_Entregable_C11/2_simanfor/4_septiembre_23/output/')

# data

# Segovia
sg_control <- read.xlsx('irregular/LR_irregular_control__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 16 - Pies inventariados')

sg_expertos <- read.xlsx('irregular/LR_irregular_expertos__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 29 - Pies inventariados')
sg_expertos_50 <- read.xlsx('irregular/LR_irregular_expertos__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 6 - Pies inventariados')
sg_expertos_80 <- read.xlsx('irregular/LR_irregular_expertos__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 15 - Pies inventariados')
sg_expertos_120 <- read.xlsx('irregular/LR_irregular_expertos__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 23 - Pies inventariados')

sg_mix <- read.xlsx('irregular/LR_irregular_mix__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 35 - Pies inventariados')
sg_mix_50 <- read.xlsx('irregular/LR_irregular_mix__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 6 - Pies inventariados')
sg_mix_80 <- read.xlsx('irregular/LR_irregular_mix__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 18 - Pies inventariados')
sg_mix_120 <- read.xlsx('irregular/LR_irregular_mix__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 28 - Pies inventariados')

sg_qp2 <- read.xlsx('irregular/LR_irregular_QP2__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 29 - Pies inventariados')
sg_qp2_50 <- read.xlsx('irregular/LR_irregular_QP2__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 6 - Pies inventariados')
sg_qp2_80 <- read.xlsx('irregular/LR_irregular_QP2__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 15 - Pies inventariados')
sg_qp2_120 <- read.xlsx('irregular/LR_irregular_QP2__Output_Plot_sg02_mix.xlsx', sheet = 'Nodo 23 - Pies inventariados')

# Soria
so_control <- read.xlsx('irregular/LR_irregular_control__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 16 - Pies inventariados')

so_expertos <- read.xlsx('irregular/LR_irregular_expertos__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 29 - Pies inventariados')
so_expertos_50 <- read.xlsx('irregular/LR_irregular_expertos__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 6 - Pies inventariados')
so_expertos_80 <- read.xlsx('irregular/LR_irregular_expertos__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 15 - Pies inventariados')
so_expertos_120 <- read.xlsx('irregular/LR_irregular_expertos__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 23 - Pies inventariados')

so_mix <- read.xlsx('irregular/LR_irregular_mix__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 35 - Pies inventariados')
so_mix_50 <- read.xlsx('irregular/LR_irregular_mix__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 6 - Pies inventariados')
so_mix_80 <- read.xlsx('irregular/LR_irregular_mix__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 18 - Pies inventariados')
so_mix_120 <- read.xlsx('irregular/LR_irregular_mix__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 28 - Pies inventariados')

so_qp2 <- read.xlsx('irregular/LR_irregular_QP2__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 29 - Pies inventariados')
so_qp2_50 <- read.xlsx('irregular/LR_irregular_QP2__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 6 - Pies inventariados')
so_qp2_80 <- read.xlsx('irregular/LR_irregular_QP2__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 15 - Pies inventariados')
so_qp2_120 <- read.xlsx('irregular/LR_irregular_QP2__Output_Plot_so02_mix.xlsx', sheet = 'Nodo 23 - Pies inventariados')

# assign scnr code
sg_control$id_scnr <- 'control'
sg_expertos$id_scnr <- 'expertos'
sg_qp2$id_scnr <- 'qp2'
sg_mix$id_scnr <- 'mix'
sg_expertos_50$id_scnr <- 'expertos_50'
sg_qp2_50$id_scnr <- 'qp2_50'
sg_mix_50$id_scnr <- 'mix_50'
sg_expertos_80$id_scnr <- 'expertos_80'
sg_qp2_80$id_scnr <- 'qp2_80'
sg_mix_80$id_scnr <- 'mix_80'
sg_expertos_120$id_scnr <- 'expertos_120'
sg_qp2_120$id_scnr <- 'qp2_120'
sg_mix_120$id_scnr <- 'mix_120'

so_control$id_scnr <- 'control'
so_expertos$id_scnr <- 'expertos'
so_qp2$id_scnr <- 'qp2'
so_mix$id_scnr <- 'mix'
so_expertos_50$id_scnr <- 'expertos_50'
so_qp2_50$id_scnr <- 'qp2_50'
so_mix_50$id_scnr <- 'mix_50'
so_expertos_80$id_scnr <- 'expertos_80'
so_qp2_80$id_scnr <- 'qp2_80'
so_mix_80$id_scnr <- 'mix_80'
so_expertos_120$id_scnr <- 'expertos_120'
so_qp2_120$id_scnr <- 'qp2_120'
so_mix_120$id_scnr <- 'mix_120'

# assign main scnr code
sg_control$id_main_scnr <- 'control'
sg_expertos$id_main_scnr <- 'expertos'
sg_qp2$id_main_scnr <- 'qp2'
sg_mix$id_main_scnr <- 'mix'
sg_expertos_50$id_main_scnr <- 'expertos'
sg_qp2_50$id_main_scnr <- 'qp2'
sg_mix_50$id_main_scnr <- 'mix'
sg_expertos_80$id_main_scnr <- 'expertos'
sg_qp2_80$id_main_scnr <- 'qp2'
sg_mix_80$id_main_scnr <- 'mix'
sg_expertos_120$id_main_scnr <- 'expertos'
sg_qp2_120$id_main_scnr <- 'qp2'
sg_mix_120$id_main_scnr <- 'mix'

so_control$id_main_scnr <- 'control'
so_expertos$id_main_scnr <- 'expertos'
so_qp2$id_main_scnr <- 'qp2'
so_mix$id_main_scnr <- 'mix'
so_expertos_50$id_main_scnr <- 'expertos'
so_qp2_50$id_main_scnr <- 'qp2'
so_mix_50$id_main_scnr <- 'mix'
so_expertos_80$id_main_scnr <- 'expertos'
so_qp2_80$id_main_scnr <- 'qp2'
so_mix_80$id_main_scnr <- 'mix'
so_expertos_120$id_main_scnr <- 'expertos'
so_qp2_120$id_main_scnr <- 'qp2'
so_mix_120$id_main_scnr <- 'mix'

# make a list of dfs
list_of_dfs <- list(sg_control, sg_expertos, sg_qp2, sg_mix, so_control, so_expertos, so_qp2, so_mix,
                    sg_expertos_50, sg_qp2_50, sg_mix_50, so_expertos_50, so_qp2_50, so_mix_50,
                    sg_expertos_80, sg_qp2_80, sg_mix_80, so_expertos_80, so_qp2_80, so_mix_80,
                    sg_expertos_120, sg_qp2_120, sg_mix_120, so_expertos_120, so_qp2_120, so_mix_120)

# manage all dfs
all_plots <- data.frame()

for (k in list_of_dfs){
  
  # delete I and M trees
  k <- k[is.na(k$estado), ]
  
  # create dbh classes
  plot <- plyr::ddply(k, c('ID_parcela', 'id_scnr'), summarise,      
                      CD_0_75 = sum(ifelse(dbh <= 7.5, factor_expansion, 0), na.rm = TRUE),
                      CD_75_125 = sum(ifelse(dbh > 7.5 & dbh <= 12.5, factor_expansion, 0), na.rm = TRUE),
                      CD_125_175 = sum(ifelse(dbh > 12.5 & dbh <= 17.5, factor_expansion, 0), na.rm = TRUE),
                      CD_175_225 = sum(ifelse(dbh > 17.5 & dbh <= 22.5, factor_expansion, 0), na.rm = TRUE),
                      CD_225_275 = sum(ifelse(dbh > 22.5 & dbh <= 27.5, factor_expansion, 0), na.rm = TRUE),
                      CD_275_325 = sum(ifelse(dbh > 27.5 & dbh <= 32.5, factor_expansion, 0), na.rm = TRUE),
                      CD_325_375 = sum(ifelse(dbh > 32.5 & dbh <= 37.5, factor_expansion, 0), na.rm = TRUE),
                      CD_375_425 = sum(ifelse(dbh > 37.5 & dbh <= 42.5, factor_expansion, 0), na.rm = TRUE),
                      CD_425_ = sum(ifelse(dbh > 42.5, factor_expansion, 0), na.rm = TRUE)
  )
  
  # organize data
  plot <- select(plot, -c(ID_parcela, id_scnr))
  plot <- data.frame(t(plot))
  plot <- dplyr::rename(plot, N = t.plot.)
  
  plot$CD <- c(5, 10, 15, 20, 25, 30, 35, 40, 45)
  plot$ID_Inventario <- paste(unique(toupper(k$ID_parcela)), unique(k$id_scnr), sep = '_')
  plot$ID_Parcela <- unique(toupper(k$ID_parcela))
  plot$Escenario_especifico <- unique(k$id_scnr)
  plot$Escenario <- unique(k$id_main_scnr)
  
  # round to units
  plot$N <- round(plot$N, digits = 0)
  
  # code to split graphs
  plot$code <- ifelse(plot$Escenario %in% c('control', 'expertos', 'qp2', 'mix'), 'general', '')
  plot_copy <- plot
  plot$code <- ifelse(plot$Escenario %in% c('expertos', 'expertos_50', 'expertos_80', 'expertos_120'), 'expertos', plot$code)
  plot$code <- ifelse(plot$Escenario %in% c('mix', 'mix_50', 'mix_80', 'mix_120'), 'mix', plot$code)
  plot$code <- ifelse(plot$Escenario %in% c('qp2', 'qp2_50', 'qp2_80', 'qp2_120'), 'qp2', plot$code)
  
  # duplicate just coincidence into 'general' and others
  if(plot_copy$code[1] == 'general'){plot <- rbind(plot, plot_copy)}
  
  # add plot to the main df
  all_plots <- rbind(all_plots, plot)
}


#### Graph results ####

all_plots_general <- all_plots[all_plots$code == 'general',]

# graph joined
ggplot(all_plots, aes(x = CD, y = N, fill = Escenario)) +
  
  # data and labels
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_text(aes(label = N), hjust = 0, vjust = -1, size = 5, color = 'darkred') +
  
  # titles
  labs(title = 'Distribución diamétrica resultante de la simulación',
       x = 'Clase Diamétrica (cm)',
       y = 'Densidad (pies/ha)') +
  
  # theme
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) + 
  
  # split by plot
  facet_wrap(~ ID_Parcela, scales = 'free')

ggsave(filename = '../graphs/dbh/dbh_resultados_irregulares.png', device = 'png', units = 'mm', dpi = 300,
       width = 300, height = 300)  
