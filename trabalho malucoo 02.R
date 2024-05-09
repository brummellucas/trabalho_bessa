# Carregar o pacote ggplot2
library(ggplot2)

# Carregar o conjunto de dados mpg
data(mpg)

# Calcular o consumo médio de combustível na cidade e na estrada por classe de veículo
consumo_medio <- aggregate(cbind(cty, hwy) ~ class, data = mpg, FUN = mean)

# Ordenar as classes de veículo pelo consumo médio na cidade
consumo_medio <- consumo_medio[order(consumo_medio$cty), ]

# Criar o gráfico de barras com cores coloridas e rótulos de valor
ggplot(data = consumo_medio, aes(x = class)) +
  geom_bar(aes(y = cty, fill = class), stat = "identity", width = 0.4) +
  geom_text(aes(y = cty, label = round(cty, 1)), vjust = -0.5, size = 3, color = "black") +  # Rótulos para cty
  geom_bar(aes(y = hwy, fill = class), stat = "identity", width = 0.4) +
  geom_text(aes(y = hwy, label = round(hwy, 1)), vjust = -0.5, size = 3, color = "black") +  # Rótulos para hwy
  labs(title = "Consumo Médio de Combustível por Classe de Veículo",
       x = "Classe de Veículo",
       y = "Consumo Médio de Combustível (mpg)",
       fill = "Classe de Veículo") +
  scale_fill_manual(values = rainbow(nrow(consumo_medio))) +  # Usar cores do arco-íris
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
