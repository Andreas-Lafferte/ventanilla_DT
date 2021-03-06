
library(readxl)
library(tidyverse)
library(lubridate)

## cargar bases. Fecha 01 diciembre de 2020.
base<-read_excel("../INPUT/archivo_sindicatos.xlsx")
base2<-read_excel("../INPUT/archivo_func_centrales_conf.xlsx")



## Anidar base sindicatos por año

anual<-base %>% mutate(anio=year(FechaConstitucion)) %>% group_by(anio) %>% tally() %>% mutate(Sindicatos_constituidos=as.numeric(n)) %>% select(anio,Sindicatos_constituidos)

anual2<-base %>% mutate(anio=year(FechaConstitucion)) %>% group_by(anio) %>% summarise(Socias=sum(Socias),
                                                                                      Socios=sum(Socios)) %>% mutate(Socias=as.numeric(Socias),
                                                                                                                     Socios=as.numeric(Socios),
                                                                                                                     Socios_as=Socias+Socios)
anual3<-base %>% filter(glosa=="ACTIVO") %>% 
  mutate(anio=year(FechaConstitucion)) %>% group_by(anio) %>% tally() %>% mutate(Sindicatos_activos=as.numeric(n)) %>% select(anio,Sindicatos_activos)

anual4<-base %>% filter(glosa=="ACTIVO") %>% 
  mutate(anio=year(FechaConstitucion)) %>% group_by(anio) %>% summarise(Socias=sum(Socias),
                                                                                       Socios=sum(Socios)) %>% mutate(Socias=as.numeric(Socias),
                                                                                                                      Socios=as.numeric(Socios),
                                                                                                                      Socios_as=Socias+Socios)

a<-merge(anual,anual2,by="anio")
b<-merge(anual3,anual4,by="anio")
anual<-merge(a,b,by="anio",all.x = TRUE)
anual<-anual %>% filter(!is.na(anio))

## Exportar base anidada
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, sheetName = "base anual sindicatos", gridLines = TRUE)
writeData(wb = wb, sheet = "base anual sindicatos", x = anual)
saveWorkbook(wb = wb, file = "../INPUT/base anual sindicatos anidada.xlsx", 
             overwrite = TRUE)

## Gráficos

anual %>% gather(categoria,sindicatos,-anio) %>% 
      filter(categoria %in% c("Sindicatos_constituidos","Sindicatos_activos")) %>% 
  ggplot(aes(x=anio,y=sindicatos,fill=categoria,color=categoria))+geom_line() + 
  scale_x_continuous(limits = c(1900,2019),breaks = c(1900,1925,1950,1975,2000,2019)) +
  labs(title="Sindicatos constituidos y sindicatos activos (1900-2019)",
       subtitle = " 12.079 sindicatos activos y 38.278 sindicatos constituidos",y="sindicatos",x="Año",
       caption = "Observatorio Sindical") + 
       theme_bw() +
  scale_color_manual("Sindicatos",values=c("red","black"), labels=c("constituidos","activos")) + 
  theme(legend.position = "bottom")

sum(anual$Sindicatos_constituidos,na.rm = TRUE)
sum(anual$Sindicatos_activos,na.rm = TRUE)

ggsave(
  plot = last_plot(),
  filename = "../OUTPUT/Graficos/sindicatos constituidos y activos (1900-2019).png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)




anual %>% gather(categoria,sindicatos,-anio) %>% 
  filter(categoria %in% c("Socios_as.x","Socios_as.y")) %>% 
  ggplot(aes(x=anio,y=sindicatos,color=categoria))+geom_line() + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_x_continuous(limits = c(1900,2019),breaks = c(1900,1925,1950,1975,2000,2019)) +
  labs(title="Número de socios/as de sindicatos constituidos y sindicatos activos (1900-2019)",
       subtitle = "514.813 socios/as de sindicatos activos",y="Socios y socias",x="Año",
       caption = "Observatorio Sindical") + 
  theme_bw() +
  scale_color_manual("Socios/as",values=c("red","black"), labels=c("de sindicatos constituidos","de sindicatos activos")) + 
  theme(legend.position = "bottom")

sum(anual$Socios_as.x,na.rm = TRUE)
sum(anual$Socias.y,na.rm = TRUE)

ggsave(
  plot = last_plot(),
  filename = "../OUTPUT/Graficos/socios y socias de sindicatos constituidos y activos (1900-2019).png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)


## Se acorta periodo

anual %>% gather(categoria,sindicatos,-anio) %>% 
  filter(categoria %in% c("Socios_as.x","Socios_as.y")) %>% 
  ggplot(aes(x=anio,y=sindicatos,color=categoria))+geom_line() + geom_point() +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_x_continuous(limits = c(1990,2020),breaks = c(1990,1995,2000,2005,2010,2015,2020)) +
  labs(title="Número de socios/as de sindicatos constituidos y sindicatos activos (1900-2019)",
       y="Socios y socias",x="Año",
       caption = "Observatorio Sindical") + 
  theme_bw() +
  scale_color_manual("Socios/as",values=c("red","black"), labels=c("de sindicatos constituidos","de sindicatos activos")) + 
  theme(legend.position = "bottom")

ggsave(
  plot = last_plot(),
  filename = "../OUTPUT/Graficos/socios y socias de sindicatos constituidos y activos (1990-2019).png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)



## Se acorta periodo, solo activos y acumulativo

anual$Socios_as.y_acum<-anual$Socios_as.y
anual$Sindicatos_activos_acum<-anual$Sindicatos_activos


for (i in c(2:nrow(anual))){
  anual$Socios_as.y_acum[i]<-sum(anual$Socios_as.y_acum[i-1],anual$Socios_as.y_acum[i],na.rm = TRUE)
}

for (i in c(2:nrow(anual))){
  anual$Sindicatos_activos_acum[i]<-sum(anual$Sindicatos_activos_acum[i-1],anual$Sindicatos_activos_acum[i],na.rm = TRUE)
}

anual %>% select(anio,Socios_as.y_acum) %>% 
  ggplot(aes(x=anio,y=Socios_as.y_acum))+geom_bar(stat = "identity") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_x_continuous(limits = c(1900,2020),
                     breaks = c(1990,2000,2010,2020)
                     ) +
  labs(title="Número de socios/as de sindicatos activos, acumulado (1900-2019)",
       y="Socios y socias",x="Año",
       caption = "Observatorio Sindical") + 
  theme_bw()


ggsave(
  plot = last_plot(),
  filename = "../OUTPUT/Graficos/socios y socias de sindicatos activos acumulado (1990-2019).png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)



anual %>% select(anio,Sindicatos_activos_acum) %>% 
  ggplot(aes(x=anio,y=Sindicatos_activos_acum))+geom_bar(stat = "identity") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_x_continuous(limits = c(1900,2020),
                       breaks = c(1900,1930,1960,1990,2020)
  ) +
  labs(title="Sindicatos activos, acumulado (1900-2019)",
       y="Sindicatos",x="Año",
       caption = "Observatorio Sindical") + 
  theme_bw()

anual %>% gather(categoria,sindicatos,-anio) %>% 
  filter(categoria %in% c("Sindicatos_activos_acum","Sindicatos_constituidos")) %>% 
  ggplot(aes(x=anio,y=sindicatos,fill=categoria,color=categoria))+geom_line() + 
  labs(title="Sindicatos constituidos y sindicatos activos anualmente (1900-2020)",
       subtitle = " 12.079 sindicatos activos y 38.278 sindicatos constituidos",y="sindicatos",x="Año",
       caption = "Observatorio Sindical") + 
  theme_bw() +
  scale_x_continuous(limits = c(1900,2020),breaks = c(1900,1925,1950,1975,2000,2020))+
  scale_color_manual("Sindicatos",values=c("red","black"), labels=c("activos acumulados","constituidos")) + 
  theme(legend.position = "bottom")


ggsave(
  plot = last_plot(),
  filename = "../OUTPUT/Graficos/sindicatos activos acumulado (1900-2020).png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)


anual %>% gather(categoria,sindicatos,-anio) %>% 
  filter(categoria %in% c("Socios_as.y_acum")) %>% 
  ggplot(aes(x=anio,y=sindicatos,fill=categoria,color=categoria))+geom_line() + 
  labs(title="Socios/as sindicatos activos anualmente (1900-2020)",
       y="sindicatos",x="Año",
       caption = "Observatorio Sindical") + 
  theme_bw() +
  scale_x_continuous(limits = c(1900,2020),
                     breaks = c(1900,1925,1950,1975,2000,2020))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_color_manual("Sindicatos",values=c("red","black"), labels=c("activos acumulados","constituidos")) + 
  theme(legend.position = "bottom")

ggsave(
  plot = last_plot(),
  filename = "../OUTPUT/Graficos/socios sindicatos activos acumulado (1900-2020).png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)

  

#### Datos mensuales ####

anual_mes<-base %>% 
  mutate(anio=year(FechaConstitucion),
         mes=month(FechaConstitucion)) %>% 
  group_by(anio,mes) %>% 
  tally() %>% 
  mutate(Sindicatos_constituidos=as.numeric(n)) %>% select(anio,mes,Sindicatos_constituidos)

anual_mes2<-base %>% 
        mutate(anio=year(FechaConstitucion),
               mes=month(FechaConstitucion)) %>% 
  group_by(anio,mes) %>% summarise(Socias=sum(Socias),
                                   Socios=sum(Socios)) %>% mutate(Socias=as.numeric(Socias),
                                                                  Socios=as.numeric(Socios),
                                                                  Socios_as=Socias+Socios)
anual_mes3<-base %>% filter(glosa=="ACTIVO") %>% 
  mutate(anio=year(FechaConstitucion),
         mes=month(FechaConstitucion)) %>% group_by(anio,mes) %>% tally() %>% 
  mutate(Sindicatos_activos=as.numeric(n)) %>% select(anio,mes,Sindicatos_activos)

anual_mes4<-base %>% filter(glosa=="ACTIVO") %>% 
  mutate(anio=year(FechaConstitucion),
         mes=month(FechaConstitucion)) %>% group_by(anio,mes) %>% summarise(Socias=sum(Socias),
                                                                        Socios=sum(Socios)) %>% mutate(Socias=as.numeric(Socias),
                                                                                                       Socios=as.numeric(Socios),
                                                                                                       Socios_as=Socias+Socios)

a<-merge(anual_mes,anual_mes2,by=c("anio","mes"))
b<-merge(anual_mes3,anual_mes4,by=c("anio","mes"))
anual_mes<-merge(a,b,by=c("anio","mes"),all.x = TRUE)
anual_mes<-anual_mes %>% filter(!is.na(anio))

## Exportar base anidada
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, sheetName = "base mensual sindicatos", gridLines = TRUE)
writeData(wb = wb, sheet = "base mensual sindicatos", x = anual_mes)
saveWorkbook(wb = wb, file = "../INPUT/base mensual sindicatos anidada.xlsx", 
             overwrite = TRUE)



## Gráficos

anual_mes$Sindicatos_activos_acum<-anual_mes$Sindicatos_activos

for (i in c(2:nrow(anual_mes))){
  anual_mes$Sindicatos_activos_acum[i]<-sum(anual_mes$Sindicatos_activos_acum[i-1],anual_mes$Sindicatos_activos_acum[i],na.rm = TRUE)
}

anual_mes %>% gather(categoria,sindicatos,-anio,-mes) %>% 
  mutate(fecha=paste(anio,mes,"01",sep="-"),
         fecha=ymd(fecha)) %>% 
  filter(anio>2015) %>% 
  filter(categoria %in% c("Sindicatos_constituidos","Sindicatos_activos")) %>% 
  ggplot(aes(x=fecha,y=sindicatos,fill=categoria,color=categoria))+geom_line() + 
  labs(title="Sindicatos constituidos y sindicatos activos mensualmente (2016-2020)",
       y="sindicatos",x="Año",
       caption = "Observatorio Sindical") + 
  scale_x_date(date_labels = "%Y %b",date_breaks = "7 month") +  theme_bw() +
  scale_color_manual("Sindicatos",values=c("red","black"), labels=c("constituidos","activos")) + 
  theme(legend.position = "bottom")

ggsave(
  plot = last_plot(),
  filename = "../OUTPUT/Graficos/sindicatos constiudios y activos mensualmente (2016-2019).png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)


anual_mes %>% gather(categoria,sindicatos,-anio,-mes) %>% 
  mutate(fecha=paste(anio,mes,"01",sep="-"),
         fecha=ymd(fecha)) %>% 
  filter(categoria %in% c("Sindicatos_activos_acum")) %>% 
  ggplot(aes(x=fecha,y=sindicatos,fill=categoria,color=categoria))+geom_line() + 
  labs(title="Sindicatos constituidos y sindicatos activos mensualmente (2016-2020)",
       subtitle = " 12.079 sindicatos activos al 01 de diciembre de 2020",y="sindicatos",x="Año",
       caption = "Observatorio Sindical") + 
  scale_x_date(date_labels = "%Y",date_breaks = "7 year",limits = as.Date(c("1900-01-01","2020-01-01"))) +  theme_bw() +
  scale_color_manual("Sindicatos",values=c("red","black"), labels=c("constituidos","activos")) + 
  theme(legend.position = "bottom")


ggsave(
  plot = last_plot(),
  filename = "../OUTPUT/Graficos/sindicatos activos mensuales acumulado 1900-2020.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)
