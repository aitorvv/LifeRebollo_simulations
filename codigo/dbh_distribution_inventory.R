#------------------------------------------------------------------------------------------#
####                      Diametric distribution - Life Rebollo                         ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 19/09/2023                              #
#                              Last modification: 27/09/2023                               #
#------------------------------------------------------------------------------------------#

library(openxlsx)
library(ggplot2)
library(dplyr)

setwd('/media/aitor/WDE/iuFOR_trabajo/Proyectos/Life_Rebollo/0_Entregable_C11/2_simanfor/4_septiembre_23/')

# data
so <- read.xlsx('input/SO02_regular.xlsx', sheet = 'Parcelas')
sg <- read.xlsx('input/SG02_irregular.xlsx', sheet = 'Parcelas')

# extract data
so <- select(so, c("CD_0_75", "CD_75_125", "CD_125_175", "CD_175_225", "CD_225_275", 
                   "CD_275_325", "CD_325_375", "CD_375_425", "CD_425_"))
sg <- select(sg, c("CD_0_75", "CD_75_125", "CD_125_175", "CD_175_225", "CD_225_275", 
                   "CD_275_325", "CD_325_375", "CD_375_425", "CD_425_"))
             
# organize data
so <- data.frame(t(so))
sg <- data.frame(t(sg))

so <- rename(so, N = X1)
sg <- rename(sg, N = X1)

so$CD <- c(5, 10, 15, 20, 25, 30, 35, 40, 45)
sg$CD <- c(5, 10, 15, 20, 25, 30, 35, 40, 45)

so$Inventario <- 'SO02'
sg$Inventario <- 'SG02'

# unir df
df <- rbind(so, sg)

# round to units
df$N <- round(df$N, digits = 0)

# graph joined
ggplot(df, aes(x = CD, y = N, fill = Inventario)) +
  
  # data and labels
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_text(aes(label = N), hjust = 0, vjust = -1, size = 5, color = 'darkred') +
  
  # titles
  labs(title = 'Distribución diamétrica de las parcelas de inventario utilizadas para la simulación',
       x = 'Clase Diamétrica (cm)',
       y = 'Densidad (pies/ha)') +
  
  # theme
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c('grey', 'black'))

ggsave(filename = '../../3_manuscrito/dbh_distribution_inventory/dbhs.png', device = 'png', units = 'mm', dpi = 300,
       width = 300, height = 300)    
