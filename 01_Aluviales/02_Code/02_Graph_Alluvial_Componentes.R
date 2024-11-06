rm(list = ls())
# Librerías 
library(ggalluvial)
library(ggplot2)
library(readxl)

# Directorio
file_path <- "C:\\Users\\CAROLINA\\Documents\\03_Work\\06_The Why Hub\\02_GitHub\\01_Gráficos_GIZ_Informe_Final\\01_Databases\\02_Componentes.xlsx"
hoja <- "Hoja3"  # Las hojas de excel están numeradas según componente 

datos <- read_excel(file_path, sheet = hoja)
datos$Fase <- factor(datos$Fase, levels = unique(datos$Fase))
datos$Preguntas <- factor(datos$Preguntas) # Convertir Preguntas a factor

# Paleta de colores personalizada
custom_colors <- c("#06aed5", "#000000", "#b4b4b4", "#086788", "#f0c808",
                   "#fff1d0", "#dd1c1a", "#ebf8fb", "#dadada", "#f0f0f0",
                   "#ebf3f5", "#ffebeb", "#fff7ea", "#fbeded", "#9bff17", "#ebffd1")

# Gráfico aluvial
grafico <- ggplot(data = datos,
                  aes(axis1 = Preguntas, axis2 = Fase, y = Freq)) +
  geom_alluvium(aes(fill = Preguntas), width = 0.25) +
  geom_stratum(width = 0.25, color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Preguntas", "Fase"), expand = c(0.15, 0.05)) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    text = element_text(family = "sans"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),  # Elimina los valores del eje y
    legend.position = "bottom",  # Colocar la leyenda en la parte inferior
    plot.title = element_text(face = "bold", size = 14),  # Título en negrita y tamaño más pequeño
    plot.caption = element_text(face = "italic", size = 10, hjust = 0.5)  # Caption en cursiva, tamaño más pequeño y centrado
  ) +
  labs(title = "Redistribución de preguntas - Componente 3",
       x = "Categorías",
       y = "Cantidad",
       caption = "Fuente: Informe final de la Guía de Evaluación del Estándar de Integridad - Etapa I, II y III realizado por The Why Hub") +
  guides(fill = guide_legend(title="N°"))  # Cambiar título de la leyenda a Preguntas

# Guardar el gráfico en un archivo JPG
ggsave("C:\\Users\\CAROLINA\\Documents\\03_Work\\06_The Why Hub\\02_GitHub\\01_Gráficos_GIZ_Informe_Final\\03_Output\\Graph_Aluvial_3.jpg", plot = grafico, width = 10, height = 8, dpi = 300)
