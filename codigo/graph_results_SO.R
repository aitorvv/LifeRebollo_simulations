#------------------------------------------------------------------------------------------#
####                       Groupe SIMANFOR results on a single df                       ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 31/03/2022                              #
#                              Last modification: 27/09/2023                               #
#------------------------------------------------------------------------------------------#



#### Summary ####

# Extended explanation here: 
# https://github.com/simanfor/resultados/blob/main/analisis_resultados/analisis_resultados_SIMANFOR.R


#### Basic steps ####

# libraries
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# set directory
general_dir <- "/media/aitor/WDE/iuFOR_trabajo/Proyectos/Life_Rebollo/0_Entregable_C11/2_simanfor/4_septiembre_23/output/SO/"
setwd(general_dir)


#### Read SIMANFOR outputs (just plot information) ####

plots <- tibble()  # will contain plot data
directory <- list.dirs(path = ".")  # will contain folder names

# for each subfolder...
for (folder in directory){ 
  
  # each subfolder is stablished as main one
  specific_dir <- paste(general_dir, "substract", folder, sep = "")
  specific_dir <- gsub("substract.", "", specific_dir)
  setwd(specific_dir)
  
  # extract .xlsx files names
  files_list <- list.files(specific_dir, pattern="xlsx")
  
  # for each file...
  for (doc in files_list){
    
    # read plot data
    plot_data <- read_excel(doc, sheet = "Parcelas")
    
    # create a new column with its name                
    plot_data$File_name <- doc  
    
    # add information to plot df
    ifelse(length(plots) == 0, plots <- rbind(plot_data), plots <- rbind(plots, plot_data))
  }
}


#### Data management - stand evolution ####

# make a copy of the data
df <- plots  

# function to round on ages on 5 years step
redondeo <- function(x, base){  
  base * round(x/base)
}                                 

# remove initial load
df <- df[!df$Accion == "Carga Inicial", ]

# round ages
df$T <- redondeo(df$T, 5) 

# get scenario code
#df$n_scnr <- substr(df$Scenario_file_name, 17, 19)
df$n_scnr <- sub("LR_SO02_|\\.json", "", df$Nombre_archivo_escenario)
df$n_scnr <- sub(".json", "", df$n_scnr)

# change labels to a better understanding
#df$n_scnr <- ifelse(df$n_scnr == 'gal_QP2_modificado_2', 'QP2_modificado_2', df$n_scnr)
#df$n_scnr <- ifelse(df$n_scnr == 'gal_QP2_modificado_3', 'QP2_modificado_3', df$n_scnr)
#df$n_scnr <- ifelse(df$n_scnr == 'gal_QP2_modificado_4', 'QP2_modificado_4', df$n_scnr)
# df$n_scnr <- ifelse(df$n_scnr == 'guia_calidad', 'calidad', df$n_scnr)
# df$n_scnr <- ifelse(df$n_scnr == 'guia_multiple', 'obj_multiple', df$n_scnr)
#df$n_scnr <- ifelse(df$n_scnr == 'comite_expertos_2', 'c_expertos_2', df$n_scnr)
#df$n_scnr <- ifelse(df$n_scnr == 'comite_expertos_3', 'c_expertos_3', df$n_scnr)
#df$n_scnr <- ifelse(df$n_scnr == 'comite_expertos_4', 'c_expertos_4', df$n_scnr)

# skip QP2 (it has the same data as QP2_modificado)
# df <- df[df$n_scnr != 'gal_QP2',]

# delete empty rows
df <- df[!is.na(df$n_scnr), ]

# fill empty values
df$V_extraido <- ifelse(is.na(df$V_extraido) | df$V_extraido == '', 0, df$V_extraido)
df$V_extraido <- (df$V_extraido * df$V_con_corteza) / 100

# calculate extracted volume in consecutive harvests
df_extraido <- df %>% group_by(Nombre_archivo_escenario, Edad_de_escenario, Accion, T, n_scnr) %>% summarize(V_extraido = sum(V_extraido))

# clean data
rm(plot_data, directory, doc, files_list, folder, general_dir, specific_dir)


#### Graph results ####


#### Density ####

graph_1 <- 
  ggplot(df, aes(x = T, y = N, 
                 group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Densidad de la parcela SO02",  
       # subtitle = "",  
       x = "Edad de la masa (años)",  
       y = "Densidad (pies/ha)"   
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title  
        # plot.subtitle = element_text(size = 15, hjust = 0.5),  
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) + 
  
  # plot data
  geom_point() +  # points
  geom_line()   # lines

# geom_abline(slope = 0, intercept = 17.5, col = 'red')

# one graph per scenario
#facet_wrap(~ n_scnr, scale = 'free')

# watch and save graph
graph_1
ggsave(filename = '../../graphs/SO/N.png', device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)


#### Mortality ####

graph_1 <- 
  ggplot(df, aes(x = T, y = N_muerto, 
                 group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Mortalidad natural de la parcela SO02",  
       # subtitle = "",  
       x = "Edad de la masa (años)",  
       y = "Densidad (pies/ha)"   
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title  
        # plot.subtitle = element_text(size = 15, hjust = 0.5),  
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) + 
  
  # plot data
  geom_point() +  # points
  geom_line()   # lines

# geom_abline(slope = 0, intercept = 17.5, col = 'red')

# one graph per scenario
#facet_wrap(~ n_scnr, scale = 'free')

# watch and save graph
graph_1
ggsave(filename = '../../graphs/SO/mortalidad.png', device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)


#### Ingrowth ####

graph_1 <- 
  ggplot(df, aes(x = T, y = N_incorporado, 
                 group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Masa incorporada de manera natural en la parcela SO02",  
       # subtitle = "",  
       x = "Edad de la masa (años)",  
       y = "Densidad (pies/ha)"   
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title  
        # plot.subtitle = element_text(size = 15, hjust = 0.5),  
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) + 
  
  # plot data
  geom_point() +  # points
  geom_line()   # lines

# geom_abline(slope = 0, intercept = 17.5, col = 'red')

# one graph per scenario
#facet_wrap(~ n_scnr, scale = 'free')

# watch and save graph
graph_1
ggsave(filename = '../../graphs/SO/masa_incorporada.png', device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)


#### G stand ####

graph_1 <-
  ggplot(df, aes(x = T, y = G,
                 group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Área basimétrica de la parcela SO02",
       # subtitle = "",
       x = "Edad de la masa (años)",
       y = "Área basimétrica (m²/ha)"
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title
        # plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) +
  
  # plot data
  geom_point() +  # points
  geom_line() +  # lines
  
  geom_abline(slope = 0, intercept = 17.5, col = 'red')

# one graph per scenario
#facet_wrap(~ n_scnr, scale = 'free')

# watch and save graph
graph_1
ggsave(filename = '../../graphs/SO/G.png', device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)


#### V harvested ####
# 
# graph_1 <-
#   ggplot(df_extraido, aes(x = T, y = V_extraido,
#                           group = n_scnr, colour = n_scnr)) +  # group by scnr
#   
#   # text
#   labs(title = "Volumen de madera extraída de la parcela SO02",
#        # subtitle = "",
#        x = "Edad de la masa (años)",
#        y = "Volumen extraído (m³/ha)"
#   ) +
#   
#   # text position and size
#   theme(plot.title = element_text(size = 20, hjust = 0.5), # title
#         # plot.subtitle = element_text(size = 15, hjust = 0.5),
#         axis.title = element_text(size = 15),  # axis
#         legend.title = element_text(size = 15),  # legend title
#         legend.text = element_text(size = 12)) +  # legend content
#   
#   # set colors and legend name manually
#   scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) +
#   
#   # plot data
#   geom_point() +  # points
#   # geom_line() +  # lines
#   
#   geom_abline(slope = 0, intercept = 40, col = 'orange') +
#   geom_abline(slope = 0, intercept = 30, col = 'red') +
#   geom_abline(slope = 0, intercept = 20, col = 'red4')
# 
# # one graph per scenario
# #facet_wrap(~ n_scnr, scale = 'free')
# 
# # watch and save graph
# graph_1
# ggsave(filename = '../../graphs/SO/V_extraido.png', device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)


#### Hart-Becking ####

graph_1 <-
  ggplot(df, aes(x = T, y = HartBecking__marco_real,
                 group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Índice de Hart-Becking de la parcela SO02",
       # subtitle = "",
       x = "Edad de la masa (años)",
       y = "Índice de Hart-Becking"
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title
        # plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) +
  
  # plot data
  geom_point() +  # points
  geom_line() +  # lines
  
  geom_abline(slope = 0, intercept = 24, col = 'red') +
  geom_abline(slope = 0, intercept = 28, col = 'red4')


# one graph per scenario
#facet_wrap(~ n_scnr, scale = 'free')

# watch and save graph
graph_1
ggsave(filename = '../../graphs/SO/Hart.png', device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)


#### Data management - stand and accumulated wood evolution ####

# make a copy of the data
df <- plots

# function to round on ages on 5 years step
redondeo <- function(x, base){
  base * round(x/base)
}

# remove initial load
df <- df[!df$Accion == "Carga Inicial", ]

# calculate differences per scenario step on the desired variables
# that is the first step to record losses and gains due to thinning
df <- df %>%
  group_by(File_name, Nombre_archivo_escenario) %>%
  mutate(V_diff = V_con_corteza - lag(V_con_corteza),
         SIERRA_GRUESA_LR_diff = `SIERRA_GRUESA-LIFEREBOLLO` - lag(`SIERRA_GRUESA-LIFEREBOLLO`),
         SIERRA_LR_diff = `SIERRA-LIFEREBOLLO` - lag(`SIERRA-LIFEREBOLLO`),
         DUELAS_INTONA_diff = `DUELAS_INTONA-LIFEREBOLLO` - lag(`DUELAS_INTONA-LIFEREBOLLO`),
         DUELAS_FONDO_INTONA_diff = `DUELAS_FONDO_INTONA-LIFEREBOLLO` - lag(`DUELAS_FONDO_INTONA-LIFEREBOLLO`),
         MADERA_LAMINADA_GAMIZ_diff = `MADERA_LAMINADA_GAMIZ-LIFEREBOLLO` - lag(`MADERA_LAMINADA_GAMIZ-LIFEREBOLLO`),
         VARIOS_GARCIA_VARONA_diff = `VARIOS_GARCIA_VARONA-LIFEREBOLLO` - lag(`VARIOS_GARCIA_VARONA-LIFEREBOLLO`),
         CARBON_diff = CARBON - lag(CARBON),
         WT_diff = WT - lag(WT),
  )

# create a new df with accumulated values
new_df <- tibble()

# for each scenario...
for(scnr in unique(df$Nombre_archivo_escenario)){
  
  # get data
  scnr <- df[df$Nombre_archivo_escenario == scnr, ]
  
  # for each plot in the scenario...
  for(plot in unique(scnr$File_name)){
    
    # get data
    plot <- scnr[scnr$File_name == plot, ]
    
    # stablish initial values for accumulated variables as 0
    all_V <- all_SIERRA_GRUESA_LR <- all_SIERRA_LR <- all_DUELAS_INTONA <- all_DUELAS_FONDO_INTONA <-
      all_MADERA_LAMINADA_GAMIZ <- all_VARIOS_GARCIA_VARONA <- all_CARBON <- all_WT <- 0
    
    # for each row...
    for(row in 1:nrow(plot)){
      
      # select data
      new_row <- plot[row, ]
      
      # if it is row 1, then initial values must be taken
      if(row == 1){
        
        # get initial value
        all_V <- new_row$V_con_corteza
        all_SIERRA_GRUESA_LR <- new_row$`SIERRA_GRUESA-LIFEREBOLLO`
        all_SIERRA_LR <- new_row$`SIERRA-LIFEREBOLLO`
        all_DUELAS_INTONA <- new_row$`DUELAS_INTONA-LIFEREBOLLO`
        all_DUELAS_FONDO_INTONA <- new_row$`DUELAS_FONDO_INTONA-LIFEREBOLLO`
        all_MADERA_LAMINADA_GAMIZ <- new_row$`MADERA_LAMINADA_GAMIZ-LIFEREBOLLO`
        all_VARIOS_GARCIA_VARONA <- new_row$`VARIOS_GARCIA_VARONA-LIFEREBOLLO`
        all_CARBON <- new_row$CARBON
        all_WT <- new_row$WT
        
        # add value to the row
        new_row$V_all <- all_V
        new_row$SIERRA_GRUESA_LR_all <- all_SIERRA_GRUESA_LR
        new_row$SIERRA_LR_all <- all_SIERRA_LR
        new_row$DUELAS_INTONA_all <- all_DUELAS_INTONA
        new_row$DUELAS_FONDO_INTONA_all <- all_DUELAS_FONDO_INTONA
        new_row$MADERA_LAMINADA_GAMIZ_all <- all_MADERA_LAMINADA_GAMIZ
        new_row$VARIOS_GARCIA_VARONA_all <- all_VARIOS_GARCIA_VARONA
        new_row$CARBON_all <- all_CARBON
        new_row$WT_all <- all_WT
        
        # if it is another row, then difference between rows is added in abs()
      } else {
        
        # add increment to the previous value
        all_V <- all_V + abs(new_row$V_diff)
        all_SIERRA_GRUESA_LR <- all_SIERRA_GRUESA_LR + abs(new_row$SIERRA_GRUESA_LR_diff)
        all_SIERRA_LR <- all_SIERRA_LR + abs(new_row$SIERRA_LR_diff)
        all_DUELAS_INTONA <- all_DUELAS_INTONA + abs(new_row$DUELAS_INTONA_diff)
        all_DUELAS_FONDO_INTONA <- all_DUELAS_FONDO_INTONA + abs(new_row$DUELAS_FONDO_INTONA_diff)
        all_MADERA_LAMINADA_GAMIZ <- all_MADERA_LAMINADA_GAMIZ + abs(new_row$MADERA_LAMINADA_GAMIZ_diff)
        all_VARIOS_GARCIA_VARONA <- all_VARIOS_GARCIA_VARONA + abs(new_row$VARIOS_GARCIA_VARONA_diff)
        all_CARBON <- all_CARBON + abs(new_row$CARBON_diff)
        all_WT <- all_WT + abs(new_row$WT_diff)
        
        # add value to the row
        new_row$V_all <- all_V
        new_row$SIERRA_GRUESA_LR_all <- all_SIERRA_GRUESA_LR
        new_row$SIERRA_LR_all <- all_SIERRA_LR
        new_row$DUELAS_INTONA_all <- all_DUELAS_INTONA
        new_row$DUELAS_FONDO_INTONA_all <- all_DUELAS_FONDO_INTONA
        new_row$MADERA_LAMINADA_GAMIZ_all <- all_MADERA_LAMINADA_GAMIZ
        new_row$VARIOS_GARCIA_VARONA_all <- all_VARIOS_GARCIA_VARONA
        new_row$CARBON_all <- all_CARBON
        new_row$WT_all <- all_WT
      }
      
      # add new row to a new df
      new_df <- rbind(new_df, new_row)
      
    } # row
  } # plot
} # scenario

# round ages
new_df$T <- redondeo(new_df$T, 5)

# get scenario code
new_df$n_scnr <- sub("LR_SO02_|\\.json", "", new_df$Nombre_archivo_escenario)
new_df$n_scnr <- sub(".json", "", new_df$n_scnr)

# change labels to a better understanding
#new_df$n_scnr <- ifelse(new_df$n_scnr == 'gal_QP2_modificado_2', 'QP2_modificado_2', new_df$n_scnr)
#new_df$n_scnr <- ifelse(new_df$n_scnr == 'gal_QP2_modificado_3', 'QP2_modificado_3', new_df$n_scnr)
#new_df$n_scnr <- ifelse(new_df$n_scnr == 'gal_QP2_modificado_4', 'QP2_modificado_4', new_df$n_scnr)
# df$n_scnr <- ifelse(df$n_scnr == 'guia_calidad', 'calidad', df$n_scnr)
# df$n_scnr <- ifelse(df$n_scnr == 'guia_multiple', 'obj_multiple', df$n_scnr)
#new_df$n_scnr <- ifelse(new_df$n_scnr == 'comite_expertos_2', 'c_expertos_2', new_df$n_scnr)
#new_df$n_scnr <- ifelse(new_df$n_scnr == 'comite_expertos_3', 'c_expertos_3', new_df$n_scnr)
#new_df$n_scnr <- ifelse(new_df$n_scnr == 'comite_expertos_4', 'c_expertos_4', new_df$n_scnr)

# delete empty rows
new_df <- new_df[!is.na(new_df$n_scnr), ]

rm(df, new_row, plot, plots, scnr, all_V, all_DUELAS_INTONA, all_CARBON, all_SIERRA_GRUESA_LR,
   all_WT, all_MADERA_LAMINADA_GAMIZ, all_SIERRA_LR, all_DUELAS_FONDO_INTONA, all_VARIOS_GARCIA_VARONA,
   row, redondeo)


#### Graph results ####


#### WT and C ####

graph_1 <-
  ggplot(new_df, aes(x = T, y = CARBON_all,
                     group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Biomasa total y carbono acumulado en la parcela SO02",
       subtitle = "incluye madera extraída en las cortas",
       x = "Edad de la masa (años)",
       y = "Biomasa - Carbono (t/ha)"
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) +
  
  # plot data
  geom_point() +  # points
  geom_line() +  # lines
  
  # plot extra data
  geom_point(shape = 0, aes(x = T, y = WT_all,
                            group = n_scnr, colour = n_scnr)) +  # points
  geom_line(aes(x = T, y = WT_all,
                group = n_scnr, colour = n_scnr))   # lines

# one graph per scenario
#facet_wrap(~ n_scnr, scale = 'free')

# watch and save graph
graph_1
ggsave(filename = '../../graphs/SO/wood/WT_C.png', device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)


#### Wood uses ####

graph_1 <-
  ggplot(new_df, aes(x = T, y = V_all,
                     group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Volumen de madera total de la parcela SO02",
       subtitle = "incluye madera extraída en las cortas",
       x = "Edad de la masa (años)",
       y = "Volumen (m³/ha)"
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) +
  
  # plot data
  geom_point() +  # points
  geom_line()   # lines

# watch and save graph
graph_1
ggsave(filename = '../../graphs/SO/wood/V_all.png', device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)


graph_2 <-
  ggplot(new_df, aes(x = T, y = SIERRA_LR_all, group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Volumen de madera total destinada a sierra (Life Rebollo) en la parcela SO02",
       subtitle = "incluye madera extraída en las cortas",
       x = "Edad de la masa (años)",
       y = "Volumen (m³/ha)"
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) +
  
  # plot data
  geom_point() +  # points
  geom_line()  # lines

# watch and save graph
graph_2
ggsave(filename = '../../graphs/SO/wood/SIERRA.png', device = 'png', units = 'mm', dpi = 300,
       width = 300, height = 300)


#-#-#-#-#-#-#-#-#-#

graph_2 <-
  ggplot(new_df, aes(x = T, y = SIERRA_GRUESA_LR_all, group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Volumen de madera total destinada a sierra gruesa (Life Rebollo) en la parcela SO02",
       subtitle = "incluye madera extraída en las cortas",
       x = "Edad de la masa (años)",
       y = "Volumen (m³/ha)"
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) +
  
  # plot data
  geom_point() +  # points
  geom_line()  # lines

# watch and save graph
graph_2
ggsave(filename = '../../graphs/SO/wood/SIERRA_GRUESA.png', device = 'png', units = 'mm', dpi = 300,
       width = 300, height = 300)


#-#-#-#-#-#-#-#-#-#

graph_2 <-
  ggplot(new_df, aes(x = T, y = DUELAS_INTONA_all, group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Volumen de madera total destinada a duelas (Intona - Life Rebollo) en la parcela SO02",
       subtitle = "incluye madera extraída en las cortas",
       x = "Edad de la masa (años)",
       y = "Volumen (m³/ha)"
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) +
  
  # plot data
  geom_point() +  # points
  geom_line()  # lines

# watch and save graph
graph_2
ggsave(filename = '../../graphs/SO/wood/DUELAS.png', device = 'png', units = 'mm', dpi = 300,
       width = 300, height = 300)


#-#-#-#-#-#-#-#-#-#

graph_2 <-
  ggplot(new_df, aes(x = T, y = DUELAS_FONDO_INTONA_all, group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Volumen de madera total destinada a duelas de fondo (Intona - Life Rebollo) en la parcela SO02",
       subtitle = "incluye madera extraída en las cortas",
       x = "Edad de la masa (años)",
       y = "Volumen (m³/ha)"
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) +
  
  # plot data
  geom_point() +  # points
  geom_line()  # lines

# watch and save graph
graph_2
ggsave(filename = '../../graphs/SO/wood/DUELAS_FONDO.png', device = 'png', units = 'mm', dpi = 300,
       width = 300, height = 300)


#-#-#-#-#-#-#-#-#-#

graph_2 <-
  ggplot(new_df, aes(x = T, y = MADERA_LAMINADA_GAMIZ_all, group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Volumen de madera total destinada a madera laminada (Gamiz - Life Rebollo) en la parcela SO02",
       subtitle = "incluye madera extraída en las cortas",
       x = "Edad de la masa (años)",
       y = "Volumen (m³/ha)"
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) +
  
  # plot data
  geom_point() +  # points
  geom_line()  # lines

# watch and save graph
graph_2
ggsave(filename = '../../graphs/SO/wood/LAMINADA.png', device = 'png', units = 'mm', dpi = 300,
       width = 300, height = 300)


#-#-#-#-#-#-#-#-#-#

graph_2 <-
  ggplot(new_df, aes(x = T, y = VARIOS_GARCIA_VARONA_all, group = n_scnr, colour = n_scnr)) +  # group by scnr
  
  # text
  labs(title = "Volumen de madera total destinada a usos múltiples (García Varona - Life Rebollo) en la parcela SO02",
       subtitle = "incluye madera extraída en las cortas",
       x = "Edad de la masa (años)",
       y = "Volumen (m³/ha)"
  ) +
  
  # text position and size
  theme(plot.title = element_text(size = 20, hjust = 0.5), # title
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title = element_text(size = 15),  # axis
        legend.title = element_text(size = 15),  # legend title
        legend.text = element_text(size = 12)) +  # legend content
  
  # set colors and legend name manually
  scale_color_manual('Escenarios', values = c('lightgrey', 'darkgray', '#6A84E0', 'black')) +
  
  # plot data
  geom_point() +  # points
  geom_line()  # lines

# watch and save graph
graph_2
ggsave(filename = '../../graphs/SO/wood/VARIOS_GARCIA_VARONA.png', device = 'png', units = 'mm', dpi = 300,
       width = 300, height = 300)
