install.packages("tidyverse")
library(tidyverse)
install.packages("cowplot")
library(cowplot)
library(dplyr)
install.packages("janitor")
library(janitor)
install.packages("hrbrthemes")
library(hrbrthemes)

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

df <- df %>%
  mutate(debtor = case_when(
    debtor == 0 ~ "No",
    debtor == 1 ~ "Si",
    TRUE ~ NA
  ))
df <- df %>% 
  mutate(target = case_when(
    target == "Dropout" ~ "Desertor",
    target == "Enrolled" ~ "Matriculado",
    target == "Graduate" ~ "Graduado",
    TRUE ~ NA
  ))
view(df)

df %>%count(debtor, target) %>% 
  ggplot(aes(x=target, y = n, fill = debtor))+
  geom_col(position = "dodge") + 
  theme_classic()+
  labs(
    x = "Tipo de Estudiante",
    y = "Frecuencia",
    fill = "Deudor",
    title = "Deudor o no Deudor según Categoría del Estudiante"
  ) +
  scale_fill_manual(values = c("Si" = "blue","No"="red"))+
  theme(legend.position = "bottom", 
        panel.border = element_rect(colour = "black", fill = NA),
        plot.title = element_text(hjust = 0.5))

df <- df %>%
  mutate(scholarship_holder = case_when(
    scholarship_holder == 0 ~ "No",
    scholarship_holder == 1 ~ "Si",
    TRUE ~ NA
  ))

df %>%count(scholarship_holder, target) %>% 
  ggplot(aes(x=scholarship_holder, y = target, fill = n))+
  geom_tile()+ 
  theme_classic() + 
  labs(x = "Poseedor de Beca Socioeconómica", y = "Tipo de Estudiante", fill = "Frecuencia", title = "Frecuencia de Estudiantes Graduados, Matriculados \n o Desertores según su categoría de beca") + 
  scale_fill_continuous(low = "skyblue", high = "darkblue")+
  theme(legend.position = "right",
        panel.border = element_rect(colour = "black", fill=NA),
        plot.title = element_text(hjust = 0.5))

df <- df %>%
  mutate(tuition_fees_up_to_date = case_when(
    tuition_fees_up_to_date == 0 ~ "No",
    tuition_fees_up_to_date == 1 ~ "Si",
    TRUE ~ NA
  ))


df %>%count(tuition_fees_up_to_date, target) %>% 
  ggplot(aes(x=target, y = n, fill = tuition_fees_up_to_date))+
  geom_col(position = "dodge") + 
  theme_classic()+
  labs(
    x = "Tipo de Estudiante",
    y = "Frecuencia",
    fill = "Cuotas al día",
    title = "Deudor en Cuotas Escolares según el tipo de estudiante"
  ) +
  scale_fill_manual(values = c("Si" = "#A2CD5A","No"="red"))+
  theme(legend.position = "bottom", 
        panel.border = element_rect(colour = "black", fill = NA),
        plot.title = element_text(hjust = 0.5))

df <- df %>%
  mutate(international = case_when(
    international == 0 ~ "No",
    international == 1 ~ "Si",
    TRUE ~ NA
  ))

df %>%count(international, target) %>% 
  ggplot(aes(x=target, y = n, fill = international))+
  geom_col(position = "stack") + 
  theme_classic()+
  labs(
    x = "Tipo de Estudiante",
    y = "Frecuencia",
    fill = "Internacional",
    title = "Tipo de Estudiante según Nacionalidad"
  ) +
  scale_fill_manual(values = c("Si" = "#CD5555","No"="#BFEFFF"))+
  theme(legend.position = "bottom", 
        panel.border = element_rect(colour = "black", fill = NA),
        plot.title = element_text(hjust = 0.5))

?geom_boxplot

df <- df %>% 
  mutate(nacionality = case_when(
    nacionality == 1 ~ "Portugal",
    nacionality ==2  ~ "Alemania",
    nacionality ==3  ~ "España",
    nacionality ==4  ~ "Italia",
    nacionality ==5  ~ "Países Bajos",
    nacionality ==6  ~ "Inglaterra",
    nacionality ==7  ~ "Lituania",
    nacionality ==8  ~ "Angola",
    nacionality ==9  ~ "Cabo Verde",
    nacionality ==10  ~ "Guinea",
    nacionality ==11  ~ "Mozambique",
    nacionality ==12  ~ "Santo Tomé",
    nacionality ==13  ~ "Turquía",
    nacionality ==14  ~ "Brasil",
    nacionality ==15  ~ "Rumania",
    nacionality ==16  ~ "Maldovia",
    nacionality ==17  ~ "México",
    nacionality ==18  ~ "Ucrania",
    nacionality ==19  ~ "Rusia",
    nacionality ==20  ~ "Cuba",
    nacionality ==21  ~ "Colombia",
    TRUE ~ "Otros"
  ))

df$curricular_units_1st_sem_grade <-
  ifelse(df$curricular_units_1st_sem_grade>25, NA, df$curricular_units_1st_sem_grade)

df %>% group_by(nacionality) %>% 
  ggplot(aes(x=nacionality,y=curricular_units_1st_sem_grade, fill = nacionality)) + 
  geom_boxplot(outlier.color = "red", outlier.size = 2) + labs(
    title = "Promedio del Primer Semestre en \nFunción de la Nacionalidad",
    x = "Nacionalidad",
    fill = "Nacionalidad",
    y = "Promedio Ponderado"
  ) + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5),legend.position="none",axis.text.x = element_text(angle=90,hjust=1))
?theme()
