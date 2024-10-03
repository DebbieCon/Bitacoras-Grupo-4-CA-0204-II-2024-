library(tidyverse)
library(cowplot)
library(dplyr)
library(janitor)
library(readr)
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(knitr)
library(babynames)
library(forcats)
library(waffle)
library(scales)
library(rmarkdown)

df <- reknitrdf <- read_csv2("base_datos/universidades_europeas.csv")
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

df$curricular_units_1st_sem_grade <- as.integer(df$curricular_units_1st_sem_grade)
df$marital_status <- as.factor(df$marital_status)


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


df <- df %>%
  mutate(scholarship_holder = case_when(
    scholarship_holder == 0 ~ "No",
    scholarship_holder == 1 ~ "Si",
    TRUE ~ NA
  ))

df <- df %>%
  mutate(tuition_fees_up_to_date = case_when(
    tuition_fees_up_to_date == 0 ~ "No",
    tuition_fees_up_to_date == 1 ~ "Si",
    TRUE ~ NA
  ))

df <- df %>%
  mutate(international = case_when(
    international == 0 ~ "No",
    international == 1 ~ "Si",
    TRUE ~ NA
  ))


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

#LIMPIEZA DE NIVEL EDUCATIVO PADRES 
df <- df %>% 
  mutate(mothers_qualification=case_when(
    mothers_qualification== 1 ~ "Educación Secundaria",
    mothers_qualification== 2 ~ "Licenciatura",
    mothers_qualification== 3 ~ "Grado",
    mothers_qualification== 4 ~ "Master",
    mothers_qualification== 5~ "Doctorado",
    mothers_qualification== 6 ~ "9 - 12mo Año de Escolaridad - No Completado",
    mothers_qualification== 9 ~ "12º Año de Escolaridad",
    mothers_qualification==10~ "11º Año de Escolaridad - No Completado",
    mothers_qualification==11~ "7º Año (Antiguo)",
    mothers_qualification==12~ "11º Año de Escolaridad",
    mothers_qualification==13~ "2º Año del curso complementario de secundaria",
    mothers_qualification==14~ "10º Año de Escolaridad",
    mothers_qualification==18 ~ "Curso de comercio general",
    mothers_qualification==19~ "Educación Básica 3º Ciclo (9º/10º/11º Año) o Equiv.",
    mothers_qualification==20~ "Curso Complementario de Secundaria",
    mothers_qualification==22~ "Curso técnico-profesional",
    mothers_qualification==25~ "Curso Complementario de Secundaria - No Concluido",
    mothers_qualification==27~ "7º Año de Escolaridad",
    mothers_qualification==29~ "9º Año de Escolaridad - No Completado",
    mothers_qualification==30~ "8º Año de Escolaridad",
    mothers_qualification==31~ "Curso General de Administración y Comercio",
    mothers_qualification==33~ "Contabilidad y Administración Suplementaria",
    mothers_qualification==34~ "Desconocido",
    mothers_qualification==35~ "No sabe leer ni escribir",
    mothers_qualification==36~ "Sabe leer sin tener el 4º Año de Escolaridad",
    mothers_qualification==37~ "Educación Básica 1º Ciclo (4º/5º Año) o Equiv.",
    mothers_qualification==38~ "Educación Básica 2º Ciclo (6º/7º/8º Año) o Equiv.",
    mothers_qualification==39~ "Curso de especialización tecnológica",
    mothers_qualification==40~ "Educación Superior - Grado (1º Ciclo)",
    mothers_qualification==41~ "Curso de estudios superiores especializados",
    mothers_qualification==42~ "Curso técnico superior profesional",
    mothers_qualification==43~ "Educación Superior - Máster (2º Ciclo)",
    mothers_qualification==44~ "Educación Superior - Doctorado (3º Ciclo)",
    TRUE ~ "Desconocido"
  ))

df <- df %>% 
  mutate(fathers_qualification=case_when(
    fathers_qualification == 1 ~ "Educación Secundaria",
    fathers_qualification  == 2 ~ "Licenciatura",
    fathers_qualification == 3 ~ "Grado",
    fathers_qualification == 4 ~ "Master",
    fathers_qualification == 5~"Doctorado",
    fathers_qualification == 6 ~ "9 - 12mo Año de Escolaridad - No Completado",
    fathers_qualification == 9 ~ "12º Año de Escolaridad",
    fathers_qualification==10~ "11º Año de Escolaridad - No Completado",
    fathers_qualification==11~ "7º Año (Antiguo)",
    fathers_qualification==12~ "11º Año de Escolaridad",
    fathers_qualification==13~ "2º Año del curso complementario de secundaria",
    fathers_qualification==14~ "10º Año de Escolaridad",
    fathers_qualification==18 ~ "Curso de comercio general",
    fathers_qualification==19~ "Educación Básica 3º Ciclo (9º/10º/11º Año) o Equiv.",
    fathers_qualification==20~ "Curso Complementario de Secundaria",
    fathers_qualification==22~ "Curso técnico-profesional",
    fathers_qualification==25~ "Curso Complementario de Secundaria - No Concluido",
    fathers_qualification==27~ "7º Año de Escolaridad",
    fathers_qualification==29~ "9º Año de Escolaridad - No Completado",
    fathers_qualification==30~ "8º Año de Escolaridad",
    fathers_qualification==31~ "Curso General de Administración y Comercio",
    fathers_qualification==33~ "Contabilidad y Administración Suplementaria",
    fathers_qualification==34~ "Desconocido",
    fathers_qualification==35~ "No sabe leer ni escribir",
    fathers_qualification==36~ "Sabe leer sin tener el 4º Año de Escolaridad",
    fathers_qualification==37~ "Educación Básica 1º Ciclo (4º/5º Año) o Equiv.",
    fathers_qualification==38~ "Educación Básica 2º Ciclo (6º/7º/8º Año) o Equiv.",
    fathers_qualification==39~ "Curso de especialización tecnológica",
    fathers_qualification==40~ "Educación Superior - Grado (1º Ciclo)",
    fathers_qualification==41~ "Curso de estudios superiores especializados",
    fathers_qualification==42~ "Curso técnico superior profesional",
    fathers_qualification==43~ "Educación Superior - Máster (2º Ciclo)",
    fathers_qualification==44~ "Educación Superior - Doctorado (3º Ciclo)",
    TRUE ~ "Desconocido"
  ))


df$mothers_qualification<-as.factor(df$mothers_qualification)
df$fathers_qualification<-as.factor(df$fathers_qualification)
glimpse(df)

sum_mothers_qualification<-df %>% 
  mutate(mothers_qualification=fct_collapse(mothers_qualification, 
   "Primaria completada" = c("7º Año (Antiguo)","7º Año de Escolaridad", 
         "Educación Básica 2º Ciclo (6º/7º/8º Año) o Equiv."),
    "Primaria no completado"=c("Sabe leer sin tener el 4º Año de Escolaridad", 
           "Educación Básica 1º Ciclo (4º/5º Año) o Equiv."),
           "Secundaria completada" =c("Educación Secundaria", "12º Año de Escolaridad", 
            "Curso Complementario de Secundaria"),
            "Secundaria no completado"=c("9 - 12mo Año de Escolaridad - No Completado",
                                                                         "11º Año de Escolaridad - No Completado",
                                                                         "11º Año de Escolaridad", 
                                                                         "2º Año del curso complementario de secundaria",
                                                                         "Educación Básica 3º Ciclo (9º/10º/11º Año) o Equiv.", 
                                                                         "Curso Complementario de Secundaria - No Concluido",
                                                                         "9º Año de Escolaridad - No Completado",
                                                                         "8º Año de Escolaridad","10º Año de Escolaridad"),
                                            "Universidad completada"=c("Licenciatura","Grado","Master","Doctorado", 
                                                                       "Educación Superior - Máster (2º Ciclo)","Educación Superior - Grado (1º Ciclo)",
                                                                       "Educación Superior - Doctorado (3º Ciclo)" ),
                                            "Cursos complementarios"= c("Curso de comercio general", "Curso técnico-profesional", 
                                                                        "Curso General de Administración y Comercio","Contabilidad y Administración Suplementaria", 
                                                                        "Curso de especialización tecnológica", "Curso de estudios superiores especializados", 
                                                                        "Curso técnico superior profesional"),
                                            "No sabe leer ni escribir" = c("No sabe leer ni escribir")
  )) %>% 
  mutate(mothers_qualification=fct_lump(mothers_qualification, n=9)) %>% 
  count(mothers_qualification, sort=TRUE) %>% 
  rename(nivel_educativo_madre=n)

sum_fathers_qualification<-df %>% 
  mutate(fathers_qualification=fct_collapse(fathers_qualification, 
                                            "Primaria completada" = c("7º Año (Antiguo)","7º Año de Escolaridad", 
                                                                      "Educación Básica 2º Ciclo (6º/7º/8º Año) o Equiv."),
                                            "Primaria no completado"=c("Sabe leer sin tener el 4º Año de Escolaridad",
                                                                       "Educación Básica 1º Ciclo (4º/5º Año) o Equiv."),
                                            "Secundaria completada" =c("Educación Secundaria", "12º Año de Escolaridad", 
                                                                       "Curso Complementario de Secundaria"  ),
                                            "Secundaria no completado"=c("9 - 12mo Año de Escolaridad - No Completado", 
                                                                         "11º Año de Escolaridad - No Completado","11º Año de Escolaridad", 
                                                                         "2º Año del curso complementario de secundaria", 
                                                                         "Educación Básica 3º Ciclo (9º/10º/11º Año) o Equiv.", 
                                                                         "Curso Complementario de Secundaria - No Concluido",
                                                                         "9º Año de Escolaridad - No Completado",
                                                                         "8º Año de Escolaridad","10º Año de Escolaridad"),
                                            "Universidad completada"=c("Licenciatura","Grado","Master","Doctorado", 
                                                                       "Educación Superior - Máster (2º Ciclo)","Educación Superior - Grado (1º Ciclo)",
                                                                       "Educación Superior - Doctorado (3º Ciclo)" ),
                                            "Cursos complementarios"= c("Curso de comercio general", "Curso técnico-profesional", 
                                                                        "Curso General de Administración y Comercio","Contabilidad y Administración Suplementaria",
                                                                        "Curso de especialización tecnológica", "Curso de estudios superiores especializados", 
                                                                        "Curso técnico superior profesional"),
                                            "No sabe leer ni escribir" = c("No sabe leer ni escribir")
  )) %>% 
  mutate(fathers_qualification=fct_lump(fathers_qualification, n=9)) %>% 
  count(fathers_qualification, sort = TRUE) %>% 
  rename(nivel_educativo_padre=n)

sum_fathers_qualification<-sum_fathers_qualification %>% 
  rename(nivel_de_estudios=fathers_qualification)

sum_mothers_qualification<-sum_mothers_qualification %>% 
  rename(nivel_de_estudios=mothers_qualification)


sum_mothers_qualification
sum_fathers_qualification

#TABLA NIVEL EDUCATIVO-NIVEL EDUCACION HIJOS 
sum_qualification<-sum_mothers_qualification %>% 
  left_join(sum_fathers_qualification, by="nivel_de_estudios") 

sum_qualification
kable(sum_qualification)


target_parents_qualification<-df %>% 
  mutate(mothers_qualification=fct_collapse(mothers_qualification,
                                            "Primaria completada" = c("7º Año (Antiguo)","7º Año de Escolaridad", 
                                                                      "Educación Básica 2º Ciclo (6º/7º/8º Año) o Equiv."),
                                            "Primaria no completado"=c("Sabe leer sin tener el 4º Año de Escolaridad",
                                                                       "Educación Básica 1º Ciclo (4º/5º Año) o Equiv."),
                                            "Secundaria completada" =c("Educación Secundaria", "12º Año de Escolaridad", 
                                                                       "Curso Complementario de Secundaria"  ),
                                            "Secundaria no completado"=c("9 - 12mo Año de Escolaridad - No Completado", 
                                                                         "11º Año de Escolaridad - No Completado","11º Año de Escolaridad", 
                                                                         "2º Año del curso complementario de secundaria", 
                                                                         "Educación Básica 3º Ciclo (9º/10º/11º Año) o Equiv.", 
                                                                         "Curso Complementario de Secundaria - No Concluido",
                                                                         "9º Año de Escolaridad - No Completado",
                                                                         "8º Año de Escolaridad","10º Año de Escolaridad"),
                                            "Universidad completada"=c("Licenciatura","Grado","Master","Doctorado", 
                                                                       "Educación Superior - Máster (2º Ciclo)","Educación Superior - Grado (1º Ciclo)",
                                                                       "Educación Superior - Doctorado (3º Ciclo)" ),
                                            "Cursos complementarios"= c("Curso de comercio general", "Curso técnico-profesional", 
                                                                        "Curso General de Administración y Comercio","Contabilidad y Administración Suplementaria",
                                                                        "Curso de especialización tecnológica", "Curso de estudios superiores especializados", 
                                                                        "Curso técnico superior profesional"),
                                            "No sabe leer ni escribir" = c("No sabe leer ni escribir")
  )) %>% 
  mutate(fathers_qualification=fct_collapse(fathers_qualification,
                                            "Primaria completada" = c("7º Año (Antiguo)","7º Año de Escolaridad", 
                                                                      "Educación Básica 2º Ciclo (6º/7º/8º Año) o Equiv."),
                                            "Primaria no completado"=c("Sabe leer sin tener el 4º Año de Escolaridad",
                                                                       "Educación Básica 1º Ciclo (4º/5º Año) o Equiv."),
                                            "Secundaria completada" =c("Educación Secundaria", "12º Año de Escolaridad", 
                                                                       "Curso Complementario de Secundaria"  ),
                                            "Secundaria no completado"=c("9 - 12mo Año de Escolaridad - No Completado", 
                                                                         "11º Año de Escolaridad - No Completado","11º Año de Escolaridad", 
                                                                         "2º Año del curso complementario de secundaria", 
                                                                         "Educación Básica 3º Ciclo (9º/10º/11º Año) o Equiv.", 
                                                                         "Curso Complementario de Secundaria - No Concluido",
                                                                         "9º Año de Escolaridad - No Completado",
                                                                         "8º Año de Escolaridad","10º Año de Escolaridad"),
                                            "Universidad completada"=c("Licenciatura","Grado","Master","Doctorado", 
                                                                       "Educación Superior - Máster (2º Ciclo)","Educación Superior - Grado (1º Ciclo)",
                                                                       "Educación Superior - Doctorado (3º Ciclo)" ),
                                            "Cursos complementarios"= c("Curso de comercio general", "Curso técnico-profesional", 
                                                                        "Curso General de Administración y Comercio","Contabilidad y Administración Suplementaria",
                                                                        "Curso de especialización tecnológica", "Curso de estudios superiores especializados", 
                                                                        "Curso técnico superior profesional"),
                                            "No sabe leer ni escribir" = c("No sabe leer ni escribir")
  )) %>% 
  group_by(target, mothers_qualification, fathers_qualification) %>% 
  summarise(count=n())

target_parents_qualification
print(target_parents_qualification,n=90)

target_parents_qualification$mothers_qualification <- str_wrap(target_parents_qualification$mothers_qualification, width = 10)

#GRAFICO NIVEL EDUCATIVO PADRES-TARGET
ggplot(target_parents_qualification, aes(x = mothers_qualification, y = fathers_qualification, fill = count)) + 
  geom_tile() + scale_fill_gradient(low="#AAE48D",
                                    high = "#2B7308",
                                    guide = "colorbar" )+
  facet_wrap(~ target) + labs(x = "Nivel educativo de la madre", y = "Nivel educativo del padre", fill = "Frecuencia")+
  theme_bw()+theme(
    axis.text.x = element_text(size = 7)
  )


#GRAFICO PROPORCION DE ESTUDIANTES 
total_estudiantes<-nrow(df)
porcentaje_graduados<-(sum(df$target=="Graduado")/total_estudiantes)*100
porcentaje_matriculados<-(sum(df$target=="Matriculado")/total_estudiantes)*100
porcentaje_desertores<-(sum(df$target=="Desertor")/total_estudiantes)*100

print(porcentaje_matriculados)
print(porcentaje_desertores)
print(porcentaje_graduados)

df_porcentajes<-data.frame(
  target=c("Graduado", "Matriculado", "Desertor"),
  porcentaje=c(porcentaje_graduados,porcentaje_matriculados ,porcentaje_desertores)
)


ggplot(df_porcentajes, aes(x=target, fill = porcentaje))+ geom_bar()+
  labs(x="Estado", y="Porcentaje", title = "Porcentaje de Estudiantes por Estado")+
  theme_minimal()

#ggplot(df_porcentajes,aes(filtargetggplot(df_porcentajes,aes(filporcentajeggplot(df_porcentajes,aes(fill = target, values=porcentaje))+
  ##geom_waffle(na.rm=TRUE, n_rows = 5, flip=FALSE,colour="white")+
  #facet_wrap(~reorder(target, porcentaje), ncol=1, strip.position = "left")+
  #coord_equal()+ guides(fill='none')+
  #labs(
    #title="Proporción de estudiantes por categoría" )+
 # scale_fill_manual(values = c('#f72585', '#4F0325', '#8BC34A'))+ scale_x_continuous(labels = function(x) paste0(round(x), "%"))+
 #theme(
   #axis.text.y = element_blank(),
   #axis.ticks.y = element_blank()
   #)

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
    TRUE ~ NA
  ))

 tabla_1 <- df %>% 
   group_by(nacionality) %>% 
   summarise(
     Media = mean(curricular_units_1st_sem_grade, na.rm=TRUE),
     Mediana = median(curricular_units_1st_sem_grade, na.rm=TRUE),
     DesviaciónEstándar = sd(curricular_units_1st_sem_grade, na.rm = TRUE),
     N = n()
   )
 tabla_1 <- tabla_1 %>%
   filter(!is.na(nacionality)) %>%
   arrange(desc(N))
 
 kable(tabla_1)
