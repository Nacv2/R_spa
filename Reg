# Simulación.

x1 <- runif(300)
x2 <- rbinom(300, size = 1, prob = 0.4)

y <- -4 + 2*x1 + 2*x2 + rnorm(300, sd = 0.2)

plot(x1[x2 == 0], y[x2 == 0], xlim = c(0,1),
     ylim = c(min(y), max(y)),
     xlab = "x1", ylab = "y")
points(x1[x2 != 0], y[x2 != 0], col = "red")
legend(0.75, -3.3,
       legend = c("x2 = 0", "x2 = 1"),
       col = c(1,2), pch = 1)

model <- lm(y ~ x1 + x2)
(beta <- coef(model))
summary(model)$sigma
abline(beta[1], beta[2])
abline(beta[1] + beta[3], beta[2], col = 2)

y <- -4 + 2*x1 + 2*x2 -3*x1*x2 +
  + rnorm(300, sd = 0.2)

plot(x1[x2 == 0], y[x2 == 0], xlim = c(0,1),
     ylim = c(min(y), max(y)),
     xlab = "x1", ylab = "y")
points(x1[x2 != 0], y[x2 != 0], col = "red")
legend(0.75, -3.5,
       legend = c("x2 = 0", "x2 = 1"),
       col = c(1,2), pch = 1)

model <- lm(y ~ x1 * x2)
(beta <- coef(model))
summary(model)$sigma
abline(beta[1], beta[2])
abline(beta[1] + beta[3],
       beta[2] + beta[4], col = 2)

y <- 1 + 3*x1 - 7*x1^2 - 7*x1^3 +
  + rnorm(300, sd = 0.2)

plot(x1, y, xlim = c(0,1),
     ylim = c(min(y), max(y)),
     xlab = "x1", ylab = "y")

x1.2 <- x1^2
x1.3 <- x1^3
x1.4 <- x1^4

model1 <- lm(y ~ x1 + x1.2 + x1.3)
summary(model1)

model2 <- lm(y ~ x1*x1.2)
summary(model2)

X <- data.frame(x1, x1.2, x1.3)

model3 <- lm(y ~ ., data = X)
summary(model3)

model4 <- lm(y ~ x1 + x1.2 + x1.3 + x1.4)
summary(model4)

BIC(model3, model4)

yfit <- fitted(model3)[order(x1)]
lines(sort(x1), yfit, col = 2, lwd = 2)

# Lectura de datos.

library("XLConnect")

setwd("C:/Users/Personal/Documents/WorkdirR")

BD <- readWorksheetFromFile(
  "Guadua.xlsx", sheet=1)

# Ajuste del modelo.

model1 <- lm(Tallo_mm ~ Hongo*Fertilizante,
             data = BD)

# Verificación del modelo.

install.packages("MASS")
install.packages("car")
install.packages("gvlma")
library("MASS")
library("car")
library("gvlma")

## No multicolinealidad.

fHongo <- factor(BD$Hongo, labels = c(0,1))
iHongo <- as.numeric(levels(fHongo))[fHongo]
fFertilizante <- factor(BD$Fertilizante,
                        labels = c(0,1))
iFertilizante <- as.numeric(levels(
  fFertilizante))[fFertilizante]

cor(iHongo, iFertilizante)

## Independencia.

durbinWatsonTest(model1)

## Homocedasticidad.

bartlett.test(Tallo_mm ~ interaction(Hongo,
                               Fertilizante),
              data = BD)

## Normalidad.

shapiro.test(studres(model1))

## Otras pruebas.

gvlma(model1, alphalevel = 0.05)

# Transformación de datos. 
# log(y), sqrt(y), y^(-1).

BD$log_Tallo <- log(BD$Tallo_mm)

##############################

model1 <- lm(log_Tallo ~ Hongo*Fertilizante,
             data = BD)

durbinWatsonTest(model1)
bartlett.test(log_Tallo ~ interaction(Hongo,
                                  Fertilizante),
              data = BD)
shapiro.test(studres(model1))
gvlma(model1, alphalevel = 0.05)


model2 <- lm(log_Tallo ~ Hongo+Fertilizante,
             data = BD)
anova(model1, model2)

durbinWatsonTest(model2)
shapiro.test(studres(model2))
gvlma(model2, alphalevel = 0.05)

model3 <- lm(log_Tallo ~ Hongo, data = BD)
anova(model2, model3)

durbinWatsonTest(model3)
bartlett.test(log_Tallo ~ Hongo,
              data = BD)
shapiro.test(studres(model3))
gvlma(model3, alphalevel = 0.05)

model4 <- lm(log_Tallo ~ 1, data = BD)
anova(model3, model4)

BIC(model1, model2, model3, model4)

summary(model3)

# Prueba no paramétrica.

kruskal.test(Tallo_mm ~ interaction(Hongo,
                               Fertilizante),
             data = BD)
kruskal.test(Tallo_mm ~ factor(Hongo),
             data = BD)
kruskal.test(Tallo_mm ~ factor(Fertilizante),
             data = BD)
