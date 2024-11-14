
library(tidyverse)

library(cowplot)
library(dplyr)

library(janitor)
library(readr)
library(hrbrthemes)
df <- read_csv2("base_datos/universidades_europeas.csv")
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(knitr)
library(forcats)


df <- df %>% clean_names()
view(df)

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

#GRAFICO GENDER-TARGET (BIT 3)
df<- df %>%  
  mutate (gender = case_when(
    gender == "0" ~ "Mujer", 
    gender == "1" ~ "Hombre",
    TRUE ~ NA
  ))
  
df %>% ggplot(aes(fill=gender, x = target))+geom_bar(stat="count")+
  theme_classic()+labs(
    x="Tipo de estudiante",
    y= "Cantidad de estudiantes",
    fill="Género",
    title = "Tipo de estudiante por género"
)+scale_fill_manual(values = c("Mujer"="lightblue", "Hombre" = "forestgreen"))+
  scale_y_continuous(breaks = seq(0, max(table(df$target)), by = 200))
  
#GRAFICO BECAS-NOTAS (BIT 3)

df <- df %>%
  mutate(
    curricular_units_1st_sem_grade = as.numeric(gsub("(\\d+\\.\\d+).*", "\\1", 
    curricular_units_1st_sem_grade)) %>%
      round(2))


df <- df %>%
  mutate(
    curricular_units_2nd_sem_grade = as.numeric(gsub("(\\d+\\.\\d+).*", "\\1", 
    curricular_units_2nd_sem_grade)) %>%
      round(2))

df %>% 
  ggplot(aes(x=curricular_units_1st_sem_grade, y=target, color=target))+
  geom_violin()+theme_classic()+labs(
    x="Notas del primer semestre",
    y="Tipo de estudiante",
    title="Notas del primer semestre por tipo de estudiante",
    color="Tipo de estudiante"
  )+scale_x_continuous(limits = c(0, 20))+guides(color = "none") 


df %>% 
  ggplot(aes(x=curricular_units_2nd_sem_grade, y=target, color=target))+
  geom_violin()+theme_classic()+labs(
    x="Notas del segundo semestre",
    y="Tipo de estudiante",
    title="Notas del segundo semestre por tipo de estudiante",
    color="Tipo de estudiante"
  )+scale_x_continuous(limits = c(0, 20))+guides(color = "none") 

df %>% 
  filter(curricular_units_1st_sem_grade < 20,curricular_units_2nd_sem_grade > 10,  
        curricular_units_2nd_sem_grade < 20,curricular_units_1st_sem_grade > 10,
        target %in% c("Graduado", "Desertor")) %>% 
  ggplot(aes(x=curricular_units_1st_sem_grade, y=curricular_units_2nd_sem_grade,
             colour =target))+
  geom_point()+theme_classic()+labs(
    x="Nota del primer semestre",
    y="Nota del segundo semestre",
    title = "Notas de los primeros dos semestres por tipo de estudiante",
    color="Tipo de estudiante"
  )



df %>% 
  mutate(debtor = ifelse(debtor == 1, "Si", "No")) %>% 
  count(debtor, target) %>% 
  ggplot(aes(x = target, y = n, fill = debtor)) + 
  geom_col(position = "dodge") + 
  theme_classic() + 
  labs(
    x = "Tipo de Estudiante",
    y = "Frecuencia",
    fill = "Deudor",
    title = "Deudor o no Deudor según Categoría del Estudiante"
  ) + 
    scale_fill_manual(values = c("Si" = "blue", "No" = "red")) + 
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
  labs(x = "Poseedor de Beca Socioeconómica", y = "Tipo de Estudiante",
       fill = "Frecuencia", title = "Gráfica 3. Frecuencia de Estudiantes 
       Graduados, Matriculados \n o Desertores según su categoría de beca") + 
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
    title = "Gráfica 2. Relación entre persona deudora y resultado académico"
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
    title = "Gráfica 4. Comparación en la frecuencia del estado de los\nestudiantes según su nacionalidad"
  ) +
  scale_fill_manual(values = c("Si" = "#CD5555","No"="#BFEFFF"))+
  theme(legend.position = "bottom", 
        panel.border = element_rect(colour = "black", fill = NA),
        plot.title = element_text(hjust = 0.5))

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


sum_mothers_qualification<-df %>% 
  mutate(mothers_qualification=fct_collapse(mothers_qualification, 
   "Primaria completada" = c("7º Año (Antiguo)","7º Año de Escolaridad", 
         "Educación Básica 2º Ciclo (6º/7º/8º Año) o Equiv."),
   
   "Primaria no completado"=c("Sabe leer sin tener el 4º Año de Escolaridad", 
           "Educación Básica 1º Ciclo (4º/5º Año) o Equiv."),
   
   "Secundaria completada" =c("Educación Secundaria", 
            "12º Año de Escolaridad", "Curso Complementario de Secundaria"),
   
   "Secundaria no completado"=c("9 - 12mo Año de Escolaridad - No Completado",
            "11º Año de Escolaridad - No Completado","11º Año de Escolaridad", 
            "2º Año del curso complementario de secundaria",
            "Educación Básica 3º Ciclo (9º/10º/11º Año) o Equiv.", 
            "Curso Complementario de Secundaria - No Concluido",
            "9º Año de Escolaridad - No Completado",
            "8º Año de Escolaridad","10º Año de Escolaridad"),
   "Universidad completada"=c("Licenciatura","Grado","Master","Doctorado", 
             "Educación Superior - Máster (2º Ciclo)",
             "Educación Superior - Grado (1º Ciclo)",
             "Educación Superior - Doctorado (3º Ciclo)"),
   
     "Cursos complementarios"= c("Curso de comercio general",
             "Curso técnico-profesional", 
             "Curso General de Administración y Comercio",
             "Contabilidad y Administración Suplementaria", 
             "Curso de especialización tecnológica", 
             "Curso de estudios superiores especializados", 
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
   "Curso Complementario de Secundaria"),
   
   "Secundaria no completado"=c("9 - 12mo Año de Escolaridad - No Completado", 
   "11º Año de Escolaridad - No Completado","11º Año de Escolaridad", 
    "2º Año del curso complementario de secundaria", 
    "Educación Básica 3º Ciclo (9º/10º/11º Año) o Equiv.", 
    "Curso Complementario de Secundaria - No Concluido",
    "9º Año de Escolaridad - No Completado",
    "8º Año de Escolaridad","10º Año de Escolaridad"),
   
    "Universidad completada"=c("Licenciatura","Grado","Master","Doctorado", 
    "Educación Superior - Máster (2º Ciclo)",
    "Educación Superior - Grado (1º Ciclo)",
    "Educación Superior - Doctorado (3º Ciclo)"),
   
    "Cursos complementarios"= c("Curso de comercio general", 
    "Curso técnico-profesional", "Curso General de Administración y Comercio",
    "Contabilidad y Administración Suplementaria",
    "Curso de especialización tecnológica",
    "Curso de estudios superiores especializados", 
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

sum_qualification<-sum_mothers_qualification %>% 
  left_join(sum_fathers_qualification, by="nivel_de_estudios") 

target_parents_qualification<-df %>% 
  mutate(mothers_qualification=fct_collapse(mothers_qualification,
  "Educación Primaria" = c("7º Año (Antiguo)","7º Año de Escolaridad", 
           "Educación Básica 2º Ciclo (6º/7º/8º Año) o Equiv.", 
           "Sabe leer sin tener el 4º Año de Escolaridad",
           "Educación Básica 1º Ciclo (4º/5º Año) o Equiv."),
          
  "Educación Secundaria" =c("Educación Secundaria", 
           "12º Año de Escolaridad", 
           "Curso Complementario de Secundaria",
           "9 - 12mo Año de Escolaridad - No Completado", 
           "11º Año de Escolaridad - No Completado","11º Año de Escolaridad", 
           "2º Año del curso complementario de secundaria", 
           "Educación Básica 3º Ciclo (9º/10º/11º Año) o Equiv.", 
           "Curso Complementario de Secundaria - No Concluido",
           "9º Año de Escolaridad - No Completado",
           "8º Año de Escolaridad","10º Año de Escolaridad"),
          
  "Educación Universitaria"=c("Licenciatura","Grado","Master","Doctorado", 
           "Educación Superior - Máster (2º Ciclo)",
           "Educación Superior - Grado (1º Ciclo)", 
           "Educación Superior - Doctorado (3º Ciclo)"),
          
  "Educación Técnico-Profesional"= c("Curso de comercio general", 
            "Curso técnico-profesional",
            "Curso General de Administración y Comercio",
            "Contabilidad y Administración Suplementaria",
            "Curso de especialización tecnológica",
            "Curso de estudios superiores especializados", 
            "Curso técnico superior profesional"),
  "No sabe leer ni escribir" = c("No sabe leer ni escribir")
  )) %>% 
  mutate(fathers_qualification=fct_collapse(fathers_qualification,
     "Educación Primaria" = c("7º Año (Antiguo)","7º Año de Escolaridad", 
          "Educación Básica 2º Ciclo (6º/7º/8º Año) o Equiv.", 
          "Sabe leer sin tener el 4º Año de Escolaridad",
          "Educación Básica 1º Ciclo (4º/5º Año) o Equiv."),
     
    "Educación Secundaria" =c("Educación Secundaria", "12º Año de Escolaridad", 
          "Curso Complementario de Secundaria", 
          "9 - 12mo Año de Escolaridad - No Completado",
          "11º Año de Escolaridad - No Completado",
          "11º Año de Escolaridad", 
          "2º Año del curso complementario de secundaria", 
          "Educación Básica 3º Ciclo (9º/10º/11º Año) o Equiv.", 
          "Curso Complementario de Secundaria - No Concluido",
          "9º Año de Escolaridad - No Completado", "8º Año de Escolaridad",
          "10º Año de Escolaridad"),
    
     "Educación Universitaria"=c("Licenciatura","Grado","Master","Doctorado", 
          "Educación Superior - Máster (2º Ciclo)",
          "Educación Superior - Grado (1º Ciclo)",
          "Educación Superior - Doctorado (3º Ciclo)"),
    
     "Educación Técnico-Profesional"= c("Curso de comercio general", 
         "Curso técnico-profesional", 
         "Curso General de Administración y Comercio",
         "Contabilidad y Administración Suplementaria",
         "Curso de especialización tecnológica",
         "Curso de estudios superiores especializados", 
         "Curso técnico superior profesional"),
    
      "No sabe leer ni escribir" = c("No sabe leer ni escribir")
  )) %>% 
  group_by(target, mothers_qualification, fathers_qualification) %>% 
  summarise(count=n())

target_parents_qualification
print(target_parents_qualification,n=100)

target_parents_qualification$mothers_qualification <- str_wrap(target_parents_qualification$mothers_qualification, width = 10)

target_parents_qualification<- target_parents_qualification %>% 
  filter(target %in% c("Graduado", "Desertor"))
print(target_parents_qualification,n=90)

target_parents_qualification <- target_parents_qualification %>% 
  filter(mothers_qualification != "Desconocido", 
         fathers_qualification != "Desconocido")
print(target_parents_qualification,n=90)

target_parents_qualification$mothers_qualification
target_parents_qualification$fathers_qualification

target_parents_qualification <- target_parents_qualification %>% 
  mutate(mothers_qualification = str_replace_all(mothers_qualification, "\\n", " "))

target_parents_qualification <- target_parents_qualification %>% 
  mutate(mothers_qualification=fct_relevel(mothers_qualification, 
                                           "No sabe leer ni escribir",
                                           "Educación Primaria",
                                           "Educación Secundaria", 
                                           "Educación Técnico-Profesional",
                                           "Educación Universitaria" ))

target_parents_qualification <- target_parents_qualification %>% 
  mutate(fathers_qualification =fct_relevel(fathers_qualification, 
                                           "No sabe leer ni escribir",
                                           "Educación Primaria",
                                           "Educación Secundaria", 
                                           "Educación Técnico-Profesional",
                                           "Educación Universitaria" ))

levels(target_parents_qualification$mothers_qualification)
levels(target_parents_qualification$fathers_qualification)

view(target_parents_qualification)

##PARENTS QUALIFICATION GRAFICA 
ggplot(target_parents_qualification, 
    aes(x = mothers_qualification, y = fathers_qualification, fill = count)) + 
  geom_tile() + scale_fill_gradient(low="#AAE48D",
                                    high = "#2B7308",
                                    guide = "colorbar" )+
  facet_wrap(~ target) + labs(x = "Nivel educativo de la madre", 
                              y = "Nivel educativo del padre",
                              fill = "Frecuencia", 
                              title="Gráfica 1. Comparación del nivel educativo 
                              de los padres y el tipo de estudiante")+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5),
                   axis.text.x = element_text(size=8,angle=90,hjust=1))

##Hora de clase → target 

view(df)

df<- df %>% 
  mutate(daytime_evening_attendance = case_when(
    daytime_evening_attendance == 1 ~ "Mañana",
    daytime_evening_attendance == 0 ~ "Noche",
    TRUE ~ "Otros"
  ))
ggplot(df, aes(fill = daytime_evening_attendance, x = target))+geom_bar()+
  scale_fill_manual(values = c("Noche"= "#CD5555", "Mañana"= "#A2CD5A"))+
       labs(x="Estado del estudiante",y="Cantidad de estudiantes", 
       fill= "Horario de clases",
 title = "Gráfica #. Estado del estudiante y horario de clases")
+theme_bw()


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
df$curricular_units_2nd_sem_grade <-
  ifelse(df$curricular_units_2nd_sem_grade>25, NA, df$curricular_units_2nd_sem_grade)
df %>% group_by(international) %>% 
  summarise(
    N=n()
  )
df$curricular_units_1st_sem_grade <- as.integer(df$curricular_units_1st_sem_grade)
df$curricular_units_2nd_sem_grade <- as.integer(df$curricular_units_2nd_sem_grade)


df_gr <- df %>% 
  filter(nacionality != "Otros" & !is.na(nacionality))


df_gr %>% 
  mutate(nacionality = as.factor(nacionality)) %>% 
  mutate(nacionality = fct_lump(nacionality, n = 3)) %>% 
  mutate(nacionality = fct_collapse(nacionality, "Otros" = c(NA, "Other"))) %>% 
  mutate(nacionality = fct_reorder(nacionality, curricular_units_1st_sem_grade)) %>% 
  group_by(nacionality) %>% 
  ggplot(aes(x=nacionality,y=curricular_units_1st_sem_grade, fill = nacionality)) + 
  geom_boxplot(outlier.color = "red", outlier.size = 2) + 
  labs(
    title = "Promedio del Primer Semestre en \nFunción de la Nacionalidad",
    x = "Nacionalidad",
    fill = "Nacionalidad",
    y = "Promedio Ponderado"
  ) + 
  theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5),legend.position="bottom",axis.text.x = element_text(angle=90,hjust=1))


df_gr %>% 
  mutate(nacionality = as.factor(nacionality)) %>% 
  mutate(nacionality = fct_lump(nacionality, n = 3)) %>% 
  mutate(nacionality = fct_collapse(nacionality, "Otros" = c(NA, "Other"))) %>% 
  mutate(nacionality = fct_reorder(nacionality, curricular_units_1st_sem_grade)) %>% 
  group_by(nacionality) %>% 
  ggplot(aes(x=nacionality,y=curricular_units_1st_sem_grade, fill = nacionality)) + 
  geom_boxplot(outlier.color = "red", outlier.size = 2) + 
  labs(
    title = "Promedio del Primer Semestre en \nFunción de la Nacionalidad",
    x = "Nacionalidad",
    fill = "Nacionalidad",
    y = "Promedio Ponderado"
  ) + 
  theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5),legend.position="bottom",axis.text.x = element_text(angle=90,hjust=1)) +
  scale_x_discrete(drop = FALSE)

df_gr %>% 
  mutate(nacionality = as.factor(nacionality)) %>% 
  mutate(nacionality = fct_lump(nacionality, n = 3)) %>% 
  mutate(nacionality = fct_collapse(nacionality, "Otros" = c(NA, "Other"))) %>% 
  mutate(nacionality = fct_reorder(nacionality, curricular_units_1st_sem_grade)) %>% 
  group_by(nacionality) %>% 
  ggplot(aes(x=nacionality,y=curricular_units_1st_sem_grade, fill = nacionality)) + 
  geom_boxplot(outlier.color = "red", outlier.size = 2) + 
  labs(
    title = "Promedio del Primer Semestre en \nFunción de la Nacionalidad",
    x = "Nacionalidad",
    fill = "Nacionalidad",
    y = "Promedio Ponderado"
  ) + 
  theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5),legend.position="bottom",
        axis.text.x = element_text(angle=90,hjust=1)) +
  scale_x_discrete(labels = unique(df_gr$nacionality))

df$curricular_units_1st_sem_grade <- as.double(df$curricular_units_1st_sem_grade)
df$curricular_units_2nd_sem_grade <- as.double(df$curricular_units_2nd_sem_grade)



tabla_1 <- df %>% 
  group_by(nacionality) %>% 
  summarise(
    Media = mean(curricular_units_1st_sem_grade, na.rm=TRUE),
    Mediana = median(curricular_units_1st_sem_grade, na.rm=TRUE),
    DesviaciónEstándar = sd(curricular_units_1st_sem_grade, na.rm = TRUE),
    N = n()
  ) %>%
  filter(!is.na(nacionality)) %>%
  arrange(desc(N))

kable(tabla_1)
glimpse(df)



view(df)

tabla_2 <- df %>% group_by(target) %>% 
  summarise(
    cantidad = n(),
    Promedio_Nota_Primer_Semestre = mean(curricular_units_1st_sem_grade, na.rm=TRUE),
    Promedio_Nota_Segundo_Semestre = mean(curricular_units_2nd_sem_grade, na.rm=TRUE),
    Desviación_Estándar_Primer_Semestre = sd(curricular_units_1st_sem_credited, na.rm=TRUE),
    Desviación_Estándar_Segundo_Semestre = sd(curricular_units_2nd_sem_credited, na.rm=TRUE)        
          ) %>% 
  filter(!is.na(cantidad)) %>% 
  arrange(desc(cantidad))

kable(t(tabla_2))

ggplot(df, aes(x = age_at_enrollment, fill = target))+
  geom_density(alpha=0.4, color="grey")+
  labs(
    x = "Edad de ingreso a la universidad",
    y = "Densidad",
    fill = "Categoría del estudiante",
    title = "Gráfica 5. Distribución de edad de admisión por categoría de estudiante"
  )+theme_minimal()+theme(plot.title = element_text(hjust = 0,5))

ggplot(df, aes(x = curricular_units_1st_sem_approved, fill = target))+
  geom_density(alpha=0.5, color=NA)+
  labs(
    x = "Créditos aprobados en el primer semestre",
    y = "Densidad",
    fill = "Categoría del estudiante",
    title = "Gráfica 6. Distribución de créditos aprobados en primer \n 
    semestre por tipo de estudiante",
  ) +theme_minimal()

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
 
 sum_qualification<-sum_mothers_qualification %>% 
   left_join(sum_fathers_qualification, by="nivel_de_estudios") 
 
 kable(sum_qualification)

 df <- df %>% 
   mutate(course=case_when(
     course ==33  ~ "Biofuel Production Technologies",
     course ==171  ~ "Animation and Multimedia Design",
     course ==8014  ~ "Social Service (evening attendance)",
     course ==9003  ~ "Agronomy",
     course ==9070  ~ "Communication Design",
     course ==9085  ~ "Veterinary Nursing",
     course ==9119  ~ "Informatics Engineering",
     course ==9130  ~ "Equiniculture",
     course ==9147  ~ "Managment",
     course ==9238  ~ "Social Service",
     course ==9254  ~ "Tourism",
     course ==9500  ~ "Nursing",
     course ==9556~ "Oral Hygiene",
     course ==9670  ~ "Advertising and Marketing Managment",
     course ==9773  ~ "Journalism and Communication",
     course ==9853  ~ "Basic Education",
     course ==9991  ~ "Management (evening attendance)",
     TRUE ~ NA
   ))
 
 

tabla_4 <- df %>% group_by(course) %>% 
  summarise(cantidad = n(),
            desertor = sum(target == "Desertor"),
            graduado = sum(target == "Graduado"),
            matriculado = sum(target == "Matriculado"))
tabla_4 <- tabla_4 %>%
  filter(!is.na(course)) %>%
  arrange(desc(cantidad))



kable(tabla_4)
 
  
