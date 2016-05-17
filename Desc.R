# Lectura de datos.

install.packages("XLConnect")
library("XLConnect")

setwd("C:/Users/Personal/Documents/WorkdirR")

BD <- readWorksheetFromFile(
  "Guadua.xlsx", sheet=1)

View(BD)

# Análisis descriptivo.

## Univariado.

Resumen.uni <- t(c(summary(BD$Tallo_mm)))
Resumen.uni <- data.frame(Resumen.uni)
sd.uni <- sd(BD$Tallo_mm)
iqr.uni <- IQR(BD$Tallo_mm)

install.packages("moments")
library("moments")

skew.uni <- skewness(BD$Tallo_mm)
kurt.uni <- kurtosis(BD$Tallo_mm) - 3

Resumen.uni <- data.frame(Resumen.uni,
                          sd = sd.uni,
                          iqr = iqr.uni,
                          skew = skew.uni,
                          kurt = kurt.uni)

View(Resumen.uni)

boxplot(BD$Tallo_mm)
nn <- length(BD$Tallo_mm)
identify(rep(1,nn), BD$Tallo_mm, n = 2)

BD[c(1,4),]

hist(BD$Tallo_mm, main = "Histograma",
     xlab = "Tallo (mm)", ylab = "Frecuencia")
plot(density(BD$Tallo_mm), main = "Densidad",
     ylab = "", xlab = "Tallo (mm)")

## Multivariado.

Form <- Tallo_mm ~ interaction(Hongo,
                               Fertilizante)

Resumen <- aggregate(Form,
            data = BD, FUN = summary)

View(Resumen)

nombres <- as.character(Resumen[[1]])
Resumen <- Resumen[[2]]
Resumen <- data.frame(Tratamiento = nombres,
                      Resumen)

View(Resumen)

sd.data <- aggregate(Form, data = BD,
                     FUN = sd)[[2]]
iqr.data <- aggregate(Form, data = BD,
                      FUN = IQR)[[2]]
skew.data <- aggregate(Form, data = BD,
                    FUN = skewness)[[2]]
kurt.data <- aggregate(Form, data = BD,
                    FUN = function(x){
                    kurtosis(x)-3})[[2]]

Resumen <- data.frame(Resumen,
                      sd = sd.data,
                      iqr = iqr.data,
                      skew = skew.data,
                      kurt = kurt.data)

View(Resumen)

boxplot(Form, data = BD)
boxplot(Tallo_mm ~ Hongo, data = BD)
boxplot(Tallo_mm ~ Fertilizante, data = BD)

# Exportación de datos.

writeWorksheetToFile("Resumen_uni_Guadua.xlsx",
                     data = Resumen.uni,
                     sheet = "1")

writeWorksheetToFile("Resumen_mul_Guadua.xlsx",
               data = Resumen, sheet = "1")

# Exportación de gráficos.

png("Box_mul.png", units = "in", width = 10,
    height = 10, res = 72)
par(cex.axis = 2, cex.main = 3)
boxplot(Form, data = BD, main = "Boxplot")
dev.off()
