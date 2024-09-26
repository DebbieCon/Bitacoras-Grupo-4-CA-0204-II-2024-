install.packages("tidyverse")
library(tidyverse)
install.packages("cowplot")
library(cowplot)
library(dplyr)
install.packages("janitor")
library(janitor)

df <- read_csv2("C:/Users/andre/OneDrive/Escritorio/UCR-TEC/2024/Segundo Semestre 2024/Herramientas de Ciencia de Datos I/base_datos/universidades_europeas.csv")
view(df)
df <- df %>% clean_names()
glimpse(df)

df <- df %>%
  mutate(marital_status = case_when(
    marital_status == 1 ~ "Soltero",
    marital_status == 2 ~ "Casado",
    marital_status == 3 ~ "Viudo",
    marital_status == 4 ~ "Divorciado",
    marital_status == 5 ~ "Unión Libre",
    marital_status == 6 ~ "Separado",
    TRUE ~ NA
  ))

df <- df %>%
  mutate(daytime_evening_attendance = case_when(
    daytime_evening_attendance == 0 ~ "Tarde",
    daytime_evening_attendance == 1 ~ "Mañana",
    TRUE ~ NA
  ))
view(df)

df$curricular_units_1st_sem_grade <- as.integer(df$curricular_units_1st_sem_grade)
df$marital_status <- as.factor(df$marital_status)
glimpse(Base)

