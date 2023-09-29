#------------------------------------------------------------------------------------------#
####                            Summarize SIMANFOR scenarios                            ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 16/08/2023                              #
#                              Last modification: 27/08/2023                               #
#------------------------------------------------------------------------------------------#



#### Summary ####

# Code developed to summarize a group of scenarios in a single df to compare them easily


#### Basic steps ####

# libraries
library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)

# set directory
general_dir <- "/media/aitor/WDE/iuFOR_trabajo/Proyectos/Life_Rebollo/0_Entregable_C11/2_simanfor/4_septiembre_23/output/"
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

# get scenario code
#df$n_scnr <- substr(df$Scenario_file_name, 17, 19)
df$n_scnr <- sub("LR_|\\.json", "", df$Nombre_archivo_escenario)
df$n_scnr <- sub(".json", "", df$n_scnr)

# change labels to a better understanding
#df$n_scnr <- ifelse(df$n_scnr == 'gal_QP2_modificado', 'QP2_modificado', df$n_scnr)
#df$n_scnr <- ifelse(df$n_scnr == 'guia_calidad', 'calidad', df$n_scnr)
#df$n_scnr <- ifelse(df$n_scnr == 'guia_multiple', 'obj_multiple', df$n_scnr)
#df$n_scnr <- ifelse(df$n_scnr == 'comite_expertos', 'c_expertos', df$n_scnr)

# skip QP2 (it has the same data as QP2_modificado) and control
#df <- df[df$n_scnr != 'gal_QP2',]
#df <- df[df$n_scnr != 'control',]

# select useful information about scenarios
df <- select(df, n_scnr, Edad_de_escenario, Accion, Tipo_de_corta, Criterio_de_corta, Intensidad_de_la_corta, Arboles_porvenir)
df <- df[!duplicated(df), ]

# filter management not applied to our stands due to age restriction
df <- df[df$Edad_de_escenario >= 50,]
df <- df[!(df$Edad_de_escenario == 50 & df$Accion == 'Ejecución'), ]

# ver resultados
view(df)

# exportar
write_csv(df, '../../scenario/summary_scenarios.csv')
