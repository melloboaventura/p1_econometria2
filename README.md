# p1_econometria2
Série temporal  dos empregos formais de MS (script rstudio)
library(rbcb); library(forecast)
library(fpp2); library(tseries)
library(ggplot2); library(hrbrthemes)
library(seasonal);library(seasonalview)
library(dplyr); library(slider)

#obtendo os dados, ajustando, e modificando para série temporal----
df <- get_series(21990)
colnames(df) <- c("data",
                  "serie")

serie <- ts(df$serie, 
            start=c(1996,1), 
            end =c(2023,12),  
            frequency = 12)

# Estatísticas básicas----
print(summary(serie))
print(c('Desvio Padrão:',
        sd(serie)))
print(c('variância: ',
        var(serie)))
indice_max <- which.max(serie)
indice_min <- which.min(serie)

# Análise gráficado comportamento da série----
ggplot(df, aes(x=data, 
               y=serie)) +
  geom_line(color="blue") +
  labs(title = "Empregos Formais, MS - 1996 a 2023",
       x = "Mês",
       y = "Empregos Formais",
       caption = "Elaboração própria com dados do BCB") +
  theme_ipsum() +
  theme(plot.background = element_rect(fill = "gray90"),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5))
# Tendência
media_movel_3m <- stats::filter(serie, 
                                rep(1/3,3), sides=2)
media_movel_12m <- stats::filter(serie, 
                                 rep(1/12,12), sides=2)
media_movel_24m <- stats::filter(serie, 
                                 rep(1/24,24), sides=2)
par(bg="gray90")
plot(serie, type="l", 
     col="blue", 
     main="Empregos Formais com Médias Móveis", 
     xlab="Mês", 
     ylab="Empregos Formais")
lines(media_movel_3m, col="red")
lines(media_movel_12m, col="green")
lines(media_movel_24m, col="black")
legend("bottomleft", 
       legend=c("Série Temporal", 
                "Média Móvel 3 meses", 
                "Média Móvel 12 meses", 
                "Média Móvel 24 meses"),
       col=c("blue", "red", "green", "black"), 
       lty=1, cex=0.4)

print(media_movel_3m)


# Ciclo----
tend <- ma(serie, 
           12)
adj <- stl(serie, 
           s.window = 'periodic')
autoplot(serie, series = "Dados") +
  autolayer(seasadj(adj), 
            series = "Saz. Ajustada") +
  autolayer(tend, 
            series = "Tendência") + 
  xlab("Mês") + ylab("Empregos Formais") + 
  ggtitle("Ciclos, Tendência e Sazonalidade") + 
  scale_colour_manual(values = c("black", "blue", "red"), 
                      breaks = c("Dados", "Saz. Ajustada", "Tendência")) +
  theme(panel.background = element_rect(fill = "gray90"), 
        plot.background = element_rect(fill = "gray90"))


# Sazonalidade----
monthplot(serie)

# ETS----
treino <- window(serie, start = c(1996, 1), end = c(2021, 4))
teste <- window(serie, start = c(2021, 5))
modelo.ets <- ets(treino, model="ZZZ")
summary(modelo.ets)

prev_ets <- forecast(modelo.ets, h=32)
autoplot(prev_ets) + 
  xlab("Mês") + ylab("Empregos Formais") +
  ggtitle("Análise Preditiva - ETS") +
  autolayer(teste)



# ARIMA----
modelo.x13as <- seas(x=treino)
summary(modelo.x13as)
prev_x13as <- series(modelo.x13as, c('forecast.forecasts'))
prev_x13as <- prev_x13as[1:32]
