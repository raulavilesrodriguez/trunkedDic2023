library(tidyverse)
library(leaflet)
library(scales)
library(leaflet.extras)
library(hexbin)
library(leaflethex)
library(readxl)
library(here)
library(writexl)
library(stringr) # to regex

source(here::here('grades_to_decimal.R'))

#__________Wrangling__________
db.c <- read_excel('02 Abonados_Troncalizado.xlsx',
                   sheet = 'TR-UBICACION-Dic2023'
                   )

latitud <- str_replace(db.c$LATITUD, "\\?", "°")

# fix error in ESPINDOLA Y JUVENAL JARAMILLO. EDIF. DE LA CAE radiobase of MULTICOM
latitud <- str_replace(latitud, "4°22\"37.6'N", "4°22\"37.6'S")
db.c$LATITUD <- latitud
longitud <- str_replace(db.c$LONGITUD, "\\?", "°")
db.c$LONGITUD <- longitud



#---Change LATITUDE---
db.c <- db.c |> mutate(LATITUD = strsplit(LATITUD, "[°\"']"))
latitud <- grades_to_decimal(db.c$LATITUD)
db.c$LATITUD <- latitud


#---Change LONGUITUDE---
db.c <- db.c |> mutate(LONGITUD = strsplit(LONGITUD, "[°\"']"))
longitud <- grades_to_decimal(db.c$LONGITUD)
db.c$LONGITUD <- longitud

# sub data bases by operator
db.multicom <- db.c |> filter(NOMBRES != "RACOMDES S.A.")
db.racomdes <- db.c |> filter(NOMBRES == "RACOMDES S.A.")

#----Ploting-----
coverage <- 15 #medium power systems

map <- leaflet() %>% addTiles()

map <- map %>%
  addCircles(
    data = db.multicom,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "#FF9209",
    fillOpacity = 0.4,
    group = "MULTICOM",
    weight = 0,  # Set weight to 0 to remove the border
    label = ~RADIOBASE
  )

map <- map %>%
  addCircles(
    data = db.racomdes,
    lng = ~LONGITUD,
    lat = ~LATITUD,
    radius = ~coverage * 1000, # Convert diameter from km to meters
    color = "blue",
    fillOpacity = 0.4,
    group = "RACOMDES",
    weight = 0,  # Set weight to 0 to remove the border
    label = ~RADIOBASE
  )

# Add the layers control
map %>% addLayersControl(
  overlayGroups = c("MULTICOM", "RACOMDES"),
  options = layersControlOptions(collapsed = FALSE)
)


# number of provincias for each operator
db.multicom |> group_by(PROVINCIA) |> summarise()

db.racomdes |> group_by(PROVINCIA) |> summarise()




