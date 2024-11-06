# Antes de empezar se necesita instalar ggtex para resaltar los subcomponentes en negrita
#install.packages("ggtext")

rm(list = ls())

#Librerías 
library(ggtext)
library(ggplot2)
library(readxl)

# Directorio (Cambiar aquí)
file_path <- "C:\\Users\\CAROLINA\\Documents\\03_Work\\06_The Why Hub\\02_GitHub\\01_Gráficos_GIZ_Informe_Final\\01_Databases\\01_Index.xlsx"
data <- read_excel(file_path, sheet = "Hoja1")

data$Index <- as.numeric(gsub(",", ".", data$Index))

# Paleta de colores personalizada
custom_colors <- c("#06aed5", "#000000", "#b4b4b4", "#086788", "#f0c808")

data$Color <- cut(data$Index,
                  breaks = c(-Inf, 0.4, 0.6, 0.7, 0.9, 1),
                  labels = custom_colors,
                  include.lowest = TRUE)

# Subcomponentes a resaltar
subcomponentes_resaltados <- c(
  "3.3. Política de regalos, cortesías, atenciones y otros",
  "3.5. Política de incentivos y reconocimiento al personal",
  "4.3. Clasificación de información",
  "5.4 Auditoría interna (para empresas públicas)",
  "6.1. Inducción en integridad a personal entrante",
  "6.3. Comunicación de política de integridad a stakeholders",
  "7.2. Denuncias anónimas",
  "7.6. Marco normativo interno de infracciones y medidas disciplinarias"
)

data$label <- ifelse(data$Subcomponentes %in% subcomponentes_resaltados,
                     paste0("<b>", data$Subcomponentes, "</b>"),
                     data$Subcomponentes)

# Gráfico de barras 
ggplot(data, aes(x = Index, y = reorder(label, Index), fill = Color)) +
  geom_bar(stat = "identity") +
  labs(x = "Index", y = "Subcomponentes", title = "Índice por Subcomponente", caption = "Fuente: Informe final de la Guía de Evaluación del Estándar de Integridad - Etapa I, II y III 
       realizado por The Why Hub") + # Añadir la fuente del gráfico
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(size = 7), # Ajustar el tamaño y permitir markdown para negrita
    axis.title = element_text(size = 10), # Ajustar el tamaño del título de los ejes
    plot.title = element_text(size = 12), # Ajustar el tamaño del título del gráfico
    plot.caption = element_text(size = 8, hjust = 0), # Ajustar el tamaño de la fuente
    legend.text = element_text(size = 8), # Ajustar el tamaño del texto de la leyenda
    legend.title = element_text(size = 10), # Ajustar el tamaño del título de la leyenda
    legend.position = "none", # Retirar la leyenda
    panel.grid.major = element_blank(), # Quitar la cuadrícula mayor
    panel.grid.minor = element_blank() # Quitar la cuadrícula menor
  ) +
  scale_fill_identity() 

# Guardar el gráfico
ggsave("C:\\Users\\CAROLINA\\Documents\\03_Work\\06_The Why Hub\\02_GitHub\\01_Gráficos_GIZ_Informe_Final\\03_Output\\Indice_por_Subcomponente.jpg", width = 10, height = 8)
