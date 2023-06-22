# Carregar bibliotecas necessárias
library(ggpubr)
library(dplyr)
library(ggplot2)

# Carregar o conjunto de dados
data <- read.csv("data.csv")

# Converter códigos e valores para numéricos
data$Code <- as.numeric(data$Code)
data$Value <- as.numeric(data$Value)
data <- na.omit(data)


# Pacientes de interesse
pacientes_interesse <- c("data-03", "data-04", "data-22", "data-44", "data-45", "data-51", "data-52", "data-54", "data-55", "data-56", "data-57", "data-58", "data-59", "data-60", "data-61", "data-62", "data-63", "data-65", "data-66")

# Filtrar dados de insulina antes do café da manhã (Code 58) para os pacientes de interesse
insulina_cafe_manha1 <- data %>%
  filter(paciente %in% pacientes_interesse & Code == 58)

# Visualizar os dados
insulina_cafe_manha1



# Selecionar 100 dados da coluna "Value"
insulina_cafe_manha1 <- insulina_cafe_manha1 %>%
  sample_n(500) %>%
  select(Value)

# Visualizar os dados
insulina_cafe_manha1

# Converter coluna "Value" para formato numérico
insulina_cafe_manha1$Value <- as.numeric(insulina_cafe_manha1$Value)

# Plotar o histograma
hist(insulina_cafe_manha1$Value)




# Filtrar pacientes que utilizaram a insulina regular (Code 33) e não utilizaram a insulina 35
pacientes_regular <- data %>%
  filter(Code == 33 & !paciente %in% insulin_data$Code[insulin_data$Code == 35]) %>%
  distinct(paciente)

# Visualizar os pacientes
pacientes_regular



# Lista de pacientes
pacientes <- c("data-01", "data-02", "data-03", "data-04", "data-05", "data-06", "data-07", "data-08", "data-09", "data-10", "data-11", "data-12", "data-13", "data-14", "data-15", "data-16", "data-17", "data-18", "data-19", "data-20", "data-21", "data-22", "data-23", "data-24", "data-25", "data-26", "data-27", "data-28", "data-29", "data-30", "data-31", "data-32", "data-33", "data-34", "data-35", "data-36", "data-37", "data-38", "data-39", "data-40", "data-41", "data-42", "data-43", "data-44", "data-45", "data-46", "data-47", "data-48", "data-51", "data-52", "data-53", "data-54", "data-55", "data-56", "data-57", "data-58", "data-59", "data-60", "data-61", "data-62", "data-63", "data-65", "data-66", "data-67", "data-70")

# Filtrar pacientes e medida de insulina antes do café da manhã (Code 58)
insulina_cafe_manha <- data %>%
  filter(paciente %in% pacientes & Code == 58) %>%
  select(Value) %>%
  sample_n(500)

# Visualizar as medidas de insulina antes do café da manhã
insulina_cafe_manha
# Convertendo a coluna "Value" para numérico
insulina_cafe_manha$Value <- as.numeric(insulina_cafe_manha$Value)

# Plotar o histograma
hist(insulina_cafe_manha$Value)



# Teste t
t.test(insulina_cafe_manha$Value, insulina_cafe_manha1$Value)



group1 <-insulina_cafe_manha1$Value
group2 <- insulina_cafe_manha$Value

#quantidade
group1 <- na.omit(group1)
group2 <-na.omit(group2)

num_gr1 <- length(group1)
num_gr2 <- length(group2)
num_gr1
num_gr2
media_gr1 <- mean(group1)
media_gr2 <- mean(group2)
media_gr1
media_gr2

s1_gr1 <- var(group1)/num_gr1
s1_gr1
s1_gr2 <- var(group2)/num_gr2
media_diff <- media_gr1 - media_gr2
media_diff
s1_gr2
#estatistica
t0 <- (media_diff - 0)/(sqrt(s1_gr1+s1_gr2))
t0
#grau de liberdade
g1_cima = (s1_gr1 + s1_gr2)**2
g1_baixo = (s1_gr1**2)/(num_gr1-1) + (s1_gr2**2)/(num_gr2-1)
g1_total = g1_cima/g1_baixo
g1_total
#pvalor
ate_t0 <- pt(t0,df=g1_total)
ate_t0
2*(1-ate_t0)

# Realizar o t-teste
t.test(group1, group2)
hist(group1)
hist(group2)



# Criar um data frame combinando as duas variáveis
data <- data.frame(
  Grupo = c(rep("glicose_cafe_manha", length(insulina_cafe_manha$Value)),
            rep("glicose_cafe_manha1", length(insulina_cafe_manha1$Value))),
  Valor = c(insulina_cafe_manha$Value, insulina_cafe_manha1$Value)
)

# Criar o boxplot
ggplot(data, aes(x = Grupo, y = Valor)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Grupo", y = "Valor", title = "Boxplot das Insulinas") +
  theme_minimal()

# Criar um data frame combinando as duas variáveis
data <- data.frame(
  Grupo = c(rep("insulina_cafe_manha", length(insulina_cafe_manha$Value))),
  Valor = insulina_cafe_manha$Value
)
data2 <- data.frame(
  Grupo = c(rep("insulina_cafe_manha1", length(insulina_cafe_manha1$Value))),
  Valor = insulina_cafe_manha1$Value
)

p <- ggplot() +
  geom_density(data = data, aes(x = Valor, color = "Insulina NPH"), size = 1) +
  geom_density(data = data2, aes(x = Valor, color = "Insulina Ultralente"), size = 1) +
  labs(x = "Glicose", y = "Densidade", title = "Densidade das Insulinas") +
  scale_color_manual(values = c("Insulina NPH" = "blue", "Insulina Ultralente" = "red")) +
  theme_minimal()

# Mostrar o gráfico
print(p)
