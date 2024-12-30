library(tidyverse)
library(ggthemes)
library(gganimate)
library(ggplot2)
library(plotly)
library(patchwork)
library(gt)
library(mapSpain)
library(sf)
library(scales)
library(ggradar)

p + theme_economist()
p + theme_wsj()
p + theme_fivethirtyeight() 
p + theme_gdocs()
p + theme_stata() 
p + theme_excel()


my_url <- "https://www.ine.es/jaxiT3/files/t/es/px/10822.px?nocab=1"

my_destino <- "./datos/23988.px"

curl::curl_download(my_url, my_destino)

df <- pxR::read.px(my_destino) %>% as.tibble()
df<- janitor::clean_names(df)



df <- df %>% 
  mutate(fecha = lubridate::ym(periodo)) %>% 
  mutate(anyo = lubridate::year(fecha)) %>% 
  mutate(trimestre = lubridate::quarter(fecha))  %>% 
  select(-periodo)  


df_wide <- df %>% pivot_wider(names_from = tipo_de_dato, values_from = value) 

df <- df_wide %>%
  select(-`Tasa de variación anual`,
         -`Acumulado en lo que va de año`,
         -`Tasa de variación acumulada`)

df <- df %>%
  mutate(total_personas = `Dato base`) %>%
  select(-`Dato base`) %>% 
  filter(pais_de_residencia != "Total")



p <- ggplot(data = df, mapping = aes(x = fecha, y = total_personas, colour = pais_de_residencia)) + 
  geom_line(size = 1) +
  labs(title = "Evolución llegada de turistas según nacionalidad",
       x = "Fecha",
       y = "Número de personas",
       color = "Nacionalidad") + 

  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) 



p <- ggplotly(p)

p


library(tidyverse)

my_url1 <- "https://www.ine.es/jaxiT3/files/t/es/px/13864.px?nocab=1"

my_destino1 <- "./datos/por_motivo1.px"

curl::curl_download(my_url1, my_destino1)

df1 <- pxR::read.px(my_destino1) %>% as.tibble()
df1<- janitor::clean_names(df1)


df1 <- df1%>%
  filter(tipo_de_dato == "Dato base") %>%
  filter(motivo_del_viaje != "Total")

df_motivos <- df1 %>%
  group_by(motivo_del_viaje) %>%
  summarise(total_turistas = sum(value, na.rm = TRUE)) %>%
  mutate(percentage = total_turistas / sum(total_turistas) * 100)  




p1 <- ggplot(df_motivos, aes(x = "", y = total_turistas, fill = motivo_del_viaje)) + 
  geom_bar(stat = "identity", width = 1) +  
  coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5),  
            color = "black", size = 5, fontface = "bold") +  
  labs(title = "Motivo de Viaje de los Turistas") + 
  theme_void() +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 30),  
        legend.title = element_text(face = "bold"), 
        legend.position = "right") +
  scale_fill_manual(values =c ("#5CACEE", "#EE5C42", "#EE3A8C"))

p1


df1_anyos <- df1 %>% 
  mutate(fecha = lubridate::ym(periodo)) %>% 
  mutate(anyo = lubridate::year(fecha)) %>% 
  mutate(trimestre = lubridate::quarter(fecha))  %>% 
  select(-periodo)  


df_motivos1 <- df1_anyos %>%
  group_by(anyo, motivo_del_viaje) %>%
  summarise(total_turistas = sum(value, na.rm = TRUE)) %>%
  mutate(percentage = total_turistas / sum(total_turistas) * 100)  


df_motivos1 <- df1_anyos %>%
  group_by(anyo, motivo_del_viaje) %>%
  summarise(total_turistas = sum(value, na.rm = TRUE)) %>%
  mutate(percentage = 100 * total_turistas / sum(total_turistas)) %>%
  ungroup()




p1_anyos <- ggplot(df_motivos1, aes(x = as.factor(anyo), y = percentage, fill = motivo_del_viaje)) +
  geom_bar(stat = "identity") +  
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")), 
    position = position_stack(vjust = 0.5),  
    color = "black", size = 3  
  ) +
  labs(
    title = "Motivo de Viaje de los Turistas por Año", 
    x = "Año", 
    y = "%", 
    fill = "Motivo del Viaje"  
  ) +
  scale_fill_manual(values = c("#FFD39B", "#CDAA7D", "#8B7355")) +  
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  )

p1_anyos






mi_link_gasto <- "https://www.ine.es/jaxiT3/files/t/es/px/10838.px?nocab=1"


my_destino_gasto <- "./datos/10838.px"

curl::curl_download(mi_link_gasto, my_destino)

df_gasto <- pxR::read.px(my_destino) %>% as.tibble()
df_gasto<- janitor::clean_names(df_gasto)


df_gasto <- df_gasto %>% 
  mutate(fecha = lubridate::ym(periodo)) %>% 
  mutate(anyo = lubridate::year(fecha)) %>% 
  mutate(trimestre = lubridate::quarter(fecha))  %>% 
  select(-periodo)  

df_wide_gasto <- df_gasto %>% pivot_wider(names_from = tipo_de_dato, values_from = value) 

df_wide_gasto <- df_wide_gasto %>%
  select(-`Tasa de variación anual`,
         -`Acumulado en lo que va de año`,
         -`Tasa de variación acumulada`)


df_wide_gasto <- df_wide_gasto %>% pivot_wider(names_from = gastos_y_duracion_media_de_los_viajes, values_from = `Dato base`)


df_wide_gasto <- df_wide_gasto %>% select(-`Gasto total`,
       -`Duración media de los viajes`, -`Gasto medio diario por persona`)



p_gasto <- ggplot(df_wide_gasto, aes(x = pais_de_residencia, y = `Gasto medio por persona`)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  coord_flip() +
  labs(
    title = "Gasto Medio por Persona según País de Residencia",
    x = "País de Residencia",
    y = "Gasto Medio por Persona (€)"
  ) +
  theme_minimal()


p_gasto1 <- p_gasto + geom_jitter(width = 0.2, alpha = 1/2, color = "brown")

p_gasto1

my_link_alojamiento <- "https://www.ine.es/jaxiT3/files/t/es/px/3152.px?nocab=1"

my_destino_alojamiento <- "./datos/3152.px"

curl::curl_download(my_link_alojamiento, my_destino_alojamiento)

df_alojamiento <- pxR::read.px(my_destino_alojamiento) %>% as.tibble()
df_alojamiento <- janitor::clean_names(df_alojamiento)

df_dicc <- pjpv.curso.R.2022::pjp_dicc(df_alojamiento)
df_uniques <- pjpv.curso.R.2022::pjp_valores_unicos(df_alojamiento)

rm(df_dicc, df_uniques)

df_alojamiento <- df_alojamiento %>% 
  mutate(fecha = lubridate::ym(periodo)) %>% 
  mutate(anyo = lubridate::year(fecha)) %>% 
  mutate(trimestre = lubridate::quarter(fecha))  %>% 
  select(-periodo)  


df_alojamiento <- df_alojamiento %>%
  filter(anyo >= 2015) %>%
  filter(residencia != "Total") %>%
  filter(comunidades_y_ciudades_autonomas != "Total Nacional") %>%
  filter(viajeros_y_pernoctaciones != "Pernoctaciones") %>%
  select(-viajeros_y_pernoctaciones)




p_alojamiento <- ggplot(df_alojamiento, aes(x = comunidades_y_ciudades_autonomas, y = value, fill = residencia)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + 
  facet_wrap(~ tipo_de_alojamiento, scales = "free_y") + 
  labs( title = "Total Viajeros según tipo de alojamiento y residencia desde 2015 hasta octubre 2024",
        x = "Comunidad Autónoma",
        y = "Número de viajeros",
        fill = "Residencia") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p_alojamiento

install.packages("fmsb")

library(fmsb)

my_link_via_acceso <- "https://www.ine.es/jaxiT3/files/t/es/px/10835.px?nocab=1"

my_destino_via_acceso <- "./datos/10835.px"

curl::curl_download(my_link_via_acceso, my_destino_via_acceso)

df_via_acceso <- pxR::read.px(my_destino_via_acceso) %>% as.tibble()
df_via_acceso <- janitor::clean_names(df_via_acceso)

df_via_acceso <- df_via_acceso %>% 
  mutate(fecha = lubridate::ym(periodo)) %>% 
  mutate(anyo = lubridate::year(fecha)) %>% 
  mutate(trimestre = lubridate::quarter(fecha))  %>% 
  select(-periodo)  


df_via_acceso <- df_via_acceso %>% 
  pivot_wider(names_from = tipo_de_dato, values_from = value) %>%
  select(-`Tasa de variación anual`,
         -`Acumulado en lo que va de año`,
         -`Tasa de variación acumulada`,
         -`trimestre`,
         -`fecha`) %>%
  filter(via_de_acceso != "Total") %>%
  rename(num_personas = `Dato base`) %>%
  filter(!is.na(num_personas))



df_via_acceso_perc <- df_via_acceso %>%
  group_by(via_de_acceso) %>%
  summarise(num_personas = sum(num_personas, na.rm = TRUE)) %>%
  mutate(porcentaje = num_personas / sum(num_personas) ) 


df_radar <- df_via_acceso_perc %>%
  select(via_de_acceso, porcentaje) %>%
  spread(via_de_acceso, porcentaje)  


df_radar <- rbind(rep(1, ncol(df_radar)), rep(0, ncol(df_radar)), df_radar)


radar_plot <- radarchart(df_radar, axistype = 1,
                         pcol = "blue", pfcol = rgb(0.2, 0.6, 0.3, 0.5), plwd = 4,
                         cglcol = "gray", cglty = 1, axislabcol = "gray",
                         caxislabels = seq(0, 1, by = 0.2), vlcex = 0.8)


title(main = "Distribución de Turistas por Tipo de Acceso (Normalizado entre 0 y 1)")




library(geojsonsf)
library(tidyverse)




provincia <- pjpv.curso.R.2022::LAU2_prov_2020_canarias


library(gt)
library(tidyverse)




my_link_pib <- "https://www.ine.es/jaxi/files/_px/es/px/t35/p011/rev19/serie/l0/03001.px?nocab=1"

my_destino_pib <- "./datos/03001.px"

curl::curl_download(my_link_pib, my_destino_pib)

df_pib <- pxR::read.px(my_destino_pib) %>% as.tibble()
df_pib <- janitor::clean_names(df_pib)

df_pib <- df_pib %>% drop_na()

df_pib_wide <- df_pib %>% pivot_wider(names_from = periodo, values_from = value)
df_pib_wide <- df_pib_wide %>% select( - `2022(A)`,
                                       - `2015`) 

tabla_pib <- df_pib_wide %>%
  DT::datatable(filter = 'top', options = list( autoWidth = TRUE))

tabla_pib



my_link_turistas_por_comunidad <- "https://www.ine.es/jaxiT3/files/t/es/px/23988.px?nocab=1"

my_destino_turistas_por_comunidad <- "./datos/23988.px"

curl::curl_download(my_link_turistas_por_comunidad, my_destino_turistas_por_comunidad)

df_turistas_por_comunidad <- pxR::read.px(my_destino_turistas_por_comunidad) %>% as.tibble()
df_turistas_por_comunidad <- janitor::clean_names(df_turistas_por_comunidad)

df_turistas_por_comunidad <- df_turistas_por_comunidad %>% 
  mutate(anyo = periodo)%>% 
  select(-periodo) %>%
  drop_na()
df_turistas_por_comunidad$comunidades_autonomas <- gsub("^\\d+\\s", "", df_turistas_por_comunidad$comunidades_autonomas)


df_wide_turistas_por_comunidad <- df_turistas_por_comunidad %>% pivot_wider(names_from = tipo_de_dato, values_from = value) %>%
  select(-`Tasa de variación anual`) %>%
          filter(comunidades_autonomas != "Total") 

df_turistas_por_comunidad <- df_wide_turistas_por_comunidad %>%
  filter(anyo == "2023") %>%
  rename(total_personas = `Dato base`) %>%
  select(- anyo)



df_ccaa <- esp_get_ccaa() %>%
  select(ine.ccaa.name, geometry) %>%
  rename("comunidades_autonomas" = ine.ccaa.name) 

df_mapa_turistas <- left_join(df_ccaa, df_turistas_por_comunidad, by = "comunidades_autonomas")


p_turistas <- ggplot(df_mapa_turistas) +
  geom_sf(aes(geometry = geometry, fill = `total_personas`)) + 
  scale_fill_viridis_c(option = "warp", name = "Turistas recibidos", 
                       labels = scales::comma) +
  labs(title = "Turistas recibidos por Comunidad Autónoma en 2023",
       caption = "Elaboración propia a partir de datos extraídos del INE") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(hjust = 0.5),
        panel.grid = element_blank())





p_turistas_inter <- ggplotly(p_turistas)

p_turistas_inter

