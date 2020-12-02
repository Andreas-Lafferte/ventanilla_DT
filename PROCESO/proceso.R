
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



