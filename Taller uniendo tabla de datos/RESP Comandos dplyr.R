# Cargar paquetes ---------------------------------------------------------
# install.packages("tidyverse")
library(tidyverse)
library(readr)
library(dplyr)
# cmd+shift + r para agregar seccion
# cmd+shift + m para agregar el pipe %>% 


# Definicion de base ------------------------------------------------------
X <- tibble('id' = c(1,2,3), 'value_x' = c("x1", "x2", "x3"))
Y <- tibble('id' = c(1,2,4), 'value_y' = c("y1", "y2", "y3"))


# Comandos en dplyr -------------------------------------------------------
# Explicar operador Pipe

# Hacer un inner join de X con Y sin poner el argumento by

# Hacer un full join de X con Y explicitando el argumento
# Denotar a esta nueva tabla como Z
Z <- X %>% full_join(Y, by = "id")



#Notemos que podemos usar el Tab para ayudarnos en el nombre del comando. 

# Trabajar con NA usando replace_na()
Z %>% replace_na(list(value_x= "Sin Información", value_y = "Sin Información"))

# Seleccionar a los dos id más grandes con slice_max() 
# utilizando  n y prop como argumentos.
Z %>%  slice_max(id, n = 2)
Z %>% slice_max(id, prop = 0.5)

# Filtrar si es que el id = 1 o el id 2
Z %>% filter((id == 2) | (id == 1)) 

## Ordernar lo anterior de manera descendiente
Z %>% filter((id == 2) | (id == 1)) %>% 
  arrange(desc(id))
# Podemos agregar un - para que sea ascendente
# Notemos que el "==" por ejemplo 2==3 devuelve False, x= 1 es una asignacion

# Ir al PPT para ver con algunas bases que trabajarás.

