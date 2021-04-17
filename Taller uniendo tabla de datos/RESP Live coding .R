# Cargar paquetes ---------------------------------------------------------
# install.packages("tidyverse")
library(tidyverse)
library(readr)
library(dplyr)


# Datasets ----------------------------------------------------------------
# Primero vamos a cargar nuestros datos
sets <- read_csv("datos/sets.csv")
themes <- read_csv("datos/themes.csv")
parts <- read_csv("datos/parts.csv")
part_categories <- read_csv("datos/part_categories.csv")
inventories <- read_csv("datos/inventories.csv")
inventories_parts <- read_csv("datos/inventory_parts.csv")
colors <- read_csv("datos/colors.csv")

# Vamos a visualizar la base inventories
inventories
# Vamos a visualizar la base inventory_parts
inventory_parts

# Defina la tabla inventory_parts_joined como la interseccion entre:
# inventories e inventory_parts
# seleccione todas las columnas menos el id y version
# ordene la tabla segun la columna quantity de forma descendiente
inventory_parts_joined <- inventories %>%
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>%
  select(-id, -version) %>%
  arrange(desc(quantity))

inventory_parts_joined
# En inventory_parts_joined tenemos set_num, color_id como llaves
# Esta tabla nos muestra cantidad del lego para cada color

# Seleccion de Personajes  ---------------------------------------------------------------

cod_lego_1 = "71012-17"
cod_lego_2 = "71012-6"
# ¿cual es el nombre asociado a esos codigos?
# Hint: la tabla sets tiene: set_num como "llave" y la columna name

sets %>% filter(set_num == cod_lego_1) %>% select(name)
sets %>% filter(set_num == cod_lego_2) %>% select(name)

# De este modo definimos los nombres como:
nombre_1 = "Ursula"
nombre_2 = "Maleficient"

# Definimos el Lego 1 -----------------------------------------------------
# defina lego_1 como 
lego_1 <- inventory_parts_joined %>%
  filter(set_num == "71012-17")
  select(-set_num)
# Ahora mas que en las piezas nos fijamos en los colores
# defina lego_1_colors como la cantidad total para cada uno de los colores
# A dicha cantidad llamela total  
  
lego_1_colors <- lego_1 %>%
  group_by(color_id) %>%
  summarize(total = sum(quantity))
lego_1_colors 

# Definimos Lego 2 --------------------------------------------------------
sets %>% filter(set_num == "71012-6")

lego_2 <- inventory_parts_joined %>%
  filter(set_num == "71012-6")
  select(-set_num)

lego_2_colors <- lego_2 %>%
  group_by(color_id) %>%
  summarize(total = sum(quantity))
lego_2_colors

# Defina la variable colors_joined como la union de:
# lego_1_colors y lego_2_colors usando la llave "color_id"
# ¿Que ocurre si omitimos la llave?
# ¿Que ocurre con las columnas total? esto lo podemos arreglar con suffix
# Adicionalmente:
## agregue la columna total_lego_1 como la fraccion de color del lego 1
## agregue la columna total_lego_2 como la fraccion de color del lego 2
## defina la varibale difference como la diferecia entre el lego 1 y 2.
colors_joined <- lego_1_colors %>%
  full_join(lego_2_colors, by = "color_id", suffix = c("_lego_1", "_lego_2")) %>% replace_na(list(total_lego_1 = 0, total_lego_2 = 0)) %>%
  inner_join(colors, by = c("color_id" = "id")) %>%
  mutate(total_lego_1 = total_lego_1 / sum(total_lego_1),
         total_lego_2 = total_lego_2 / sum(total_lego_2),
         difference = total_lego_1 - total_lego_2)

# Vea como queda la variable colors_joined
colors_joined


# Grafico -----------------------------------------------------------------

library(ggplot2)
library(forcats)

colors_joined$rgb = paste("#", colors_joined$rgb , sep ="")

color_palette <- setNames(colors_joined$rgb, colors_joined$name)

# Aqui estamos ordenando los nombres segun la diferencia
colors_joined <- colors_joined %>%
  mutate(name = fct_reorder(name, difference))

# Cree un grafico que columnas que tenga coloreado segun el nombre del color
# Las columnas deben ir asociadas a la diferencia calculado de los dos legos.
ggplot(colors_joined, aes(x=name, y=difference, fill = name)) +
geom_col(color = "black") +
coord_flip() +
scale_fill_manual(values =  color_palette) +
theme_bw() +
  labs(title = paste(nombre_1, nombre_2, sep = " vs ") )
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = 34))

# ¿Y si quisieramos el mismo análisis para otros Legos? Por ejemplo para:
# Batmonible "7784-1"
# Batwing "70916-1"

# ¿Es posible optimizar el código? Tal vez pueden probar haciendo una función.
  

