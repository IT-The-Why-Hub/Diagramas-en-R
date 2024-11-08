# Antes de empezar se necesita instalar ggtex para resaltar los subcomponentes en negrita
#install.packages("ggtext")

rm(list = ls())

#Librer�as 
library(ggtext)
library(ggplot2)
library(readxl)

# Directorio (Cambiar aqu�)
file_path <- "C:\\Users\\CAROLINA\\Documents\\03_Work\\06_The Why Hub\\02_GitHub\\01_Gr�ficos_GIZ_Informe_Final\\01_Databases\\01_Index.xlsx"
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
  "3.3. Pol�tica de regalos, cortes�as, atenciones y otros",
  "3.5. Pol�tica de incentivos y reconocimiento al personal",
  "4.3. Clasificaci�n de informaci�n",
  "5.4 Auditor�a interna (para empresas p�blicas)",
  "6.1. Inducci�n en integridad a personal entrante",
  "6.3. Comunicaci�n de pol�tica de integridad a stakeholders",
  "7.2. Denuncias an�nimas",
  "7.6. Marco normativo interno de infracciones y medidas disciplinarias"
)

data$label <- ifelse(data$Subcomponentes %in% subcomponentes_resaltados,
                     paste0("<b>", data$Subcomponentes, "</b>"),
                     data$Subcomponentes)

# Gr�fico de barras 
ggplot(data, aes(x = Index, y = reorder(label, Index), fill = Color)) +
  geom_bar(stat = "identity") +
  labs(x = "Index", y = "Subcomponentes", title = "�ndice por Subcomponente", caption = "Fuente: Informe final de la Gu�a de Evaluaci�n del Est�ndar de Integridad - Etapa I, II y III 
       realizado por The Why Hub") + # A�adir la fuente del gr�fico
  theme_minimal() +
  theme(
    axis.text.y = element_markdown(size = 7), # Ajustar el tama�o y permitir markdown para negrita
    axis.title = element_text(size = 10), # Ajustar el tama�o del t�tulo de los ejes
    plot.title = element_text(size = 12), # Ajustar el tama�o del t�tulo del gr�fico
    plot.caption = element_text(size = 8, hjust = 0), # Ajustar el tama�o de la fuente
    legend.text = element_text(size = 8), # Ajustar el tama�o del texto de la leyenda
    legend.title = element_text(size = 10), # Ajustar el tama�o del t�tulo de la leyenda
    legend.position = "none", # Retirar la leyenda
    panel.grid.major = element_blank(), # Quitar la cuadr�cula mayor
    panel.grid.minor = element_blank() # Quitar la cuadr�cula menor
  ) +
  scale_fill_identity() 

# Guardar el gr�fico
ggsave("C:\\Users\\CAROLINA\\Documents\\03_Work\\06_The Why Hub\\02_GitHub\\01_Gr�ficos_GIZ_Informe_Final\\03_Output\\Indice_por_Subcomponente.jpg", width = 10, height = 8)
