# Carregar a biblioteca ggplot2
library(ggplot2)

# Carregar o conjunto de dados mpg
data(mpg)

# Contar o número de veículos por classe de veículo
class_count <- as.data.frame(table(mpg$class))

# Renomear as colunas
colnames(class_count) <- c("class", "count")

# Substituir os números pelos nomes das classes de veículo
class_count$class <- ifelse(class_count$class == "2seater", "Two Seaters",
                            ifelse(class_count$class == "compact", "Compact",
                                   ifelse(class_count$class == "midsize", "Midsize",
                                          ifelse(class_count$class == "minivan", "Minivan",
                                                 ifelse(class_count$class == "pickup", "Pickup",
                                                        ifelse(class_count$class == "subcompact", "Subcompact",
                                                               ifelse(class_count$class == "suv", "SUV", "Outro")))))))

# Agrupar classes com menos de 30 veículos e chamá-las de "Outro"
class_count$class <- ifelse(class_count$count < 30, "Outro", class_count$class)

# Somar as contagens por classe de veículo
class_count <- aggregate(count ~ class, data = class_count, FUN = sum)

# Reordenar as classes de veículo com base nas contagens decrescentes
class_count$class <- factor(class_count$class, levels = class_count$class[order(-class_count$count)])

# Calcular as porcentagens
class_count$percent <- round((class_count$count / sum(class_count$count)) * 100, 1)

# Criar o gráfico de barras colorido com ggplot2
grafico_barras <- ggplot(data = class_count, aes(x = class, y = count, fill = class)) +
  geom_bar(stat = "identity") +
  labs(title = "Consumo de Combustível por Classe de Veículo",
       x = "Classe de Veículo",
       y = "Total de Carros",
       fill = "Classe de Veículo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotacionar rótulos do eixo x para facilitar a leitura

# Criar o gráfico de pizza com porcentagens
grafico_pizza <- ggplot(data = class_count, aes(x = "", y = count, fill = class, label = paste0(percent, "%"))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), color = "white", size = 3) +
  coord_polar("y") +
  labs(title = "Proporção de Carros por Classe de Veículo",
       x = NULL,
       y = NULL,
       fill = "Classe de Veículo") +
  theme_void()  # Remover elementos de tema para o gráfico de pizza

# Exibir os gráficos lado a lado
library(gridExtra)
grid.arrange(grafico_barras, grafico_pizza, ncol = 2)
