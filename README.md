
# Análise Estatística
![image](https://github.com/thiagolealg/trabalho_R/assets/113521516/498a9daa-9338-4a45-8d07-9dc9dffe520f)

![image](https://github.com/thiagolealg/trabalho_R/assets/113521516/ecd8a760-40a9-4e14-b6d5-cf9e6b279c55)

![image](https://github.com/thiagolealg/trabalho_R/assets/113521516/371d0fdb-5d96-4baa-bd76-a453f0d64c61)


# Conjunto de Dados de Diabetes

Este repositório contém o conjunto de dados de diabetes disponível publicamente no UCI Machine Learning Repository.

## Descrição do conjunto de dados

O conjunto de dados de Diabetes é originado do Instituto Nacional de Diabetes e Doenças Digestivas e Renais. O objetivo do conjunto de dados é prever se um paciente tem diabetes, com base em certas medidas diagnósticas incluídas no conjunto de dados. Vários constrangimentos foram colocados na seleção dessas instâncias de uma base de dados maior. Em particular, todos os pacientes aqui são mulheres com pelo menos 21 anos de idade.

O conjunto de dados consiste em várias variáveis preditoras médicas e uma variável de destino, 'Outcome'. As variáveis preditoras incluem o número de gestações que a paciente teve, seu IMC, nível de insulina, idade e assim por diante.

## Características do conjunto de dados

1. **Número de instâncias**: 768

2. **Número de atributos**: 8 mais a classe de destino

3. **Atributos** (todos são numéricos):
    - Número de vezes grávida
    - Concentração de glicose plasmática a 2 horas em um teste oral de tolerância à glicose
    - Pressão arterial diastólica (mm Hg)
    - Espessura da dobra da pele do tríceps (mm)
    - Insulina sérica de 2 horas (mu U/ml)
    - Índice de massa corporal (peso em kg/(altura em m)^2)
    - Função de pedigree de diabetes
    - Idade (anos)
    - Classe de destino (0 ou 1)

4. **Valores faltantes**: Sim

5. **Distribuição de classe** (classe valor 1 é interpretado como "teste positivo para diabetes"):
    - Classe valor 0: 500 instâncias
    - Classe valor 1: 268 instâncias

## Uso do conjunto de dados

Este conjunto de dados pode ser usado para resolver problemas de classificação em Machine Learning e Data Science. Pode ser utilizado para treinar modelos de Machine Learning para prever se um paciente tem diabetes com base nas características fornecidas.

Nota: As informações acima são uma interpretação do conjunto de dados de diabetes do UCI Machine Learning Repository. Para mais informações, visite a [página oficial do conjunto de dados](https://archive.ics.uci.edu/dataset/34/diabetes).

## Carregar bibliotecas necessárias
library(ggpubr)
library(dplyr)
library(ggplot2)

## Carregar o conjunto de dados
data <- read.csv("data.csv")

## Converter códigos e valores para numéricos
data$Code <- as.numeric(data$Code)
data$Value <- as.numeric(data$Value)
data <- na.omit(data)


## Pacientes de interesse
pacientes_interesse <- c("data-03", "data-04", "data-22", "data-44", "data-45", "data-51", "data-52", "data-54", "data-55", "data-56", "data-57", "data-58", "data-59", "data-60", "data-61", "data-62", "data-63", "data-65", "data-66")

insulina_cafe_manha1 <- data %>%
  filter(paciente %in% pacientes_interesse & Code == 58)

## Visualizar os dados
insulina_cafe_manha1

Neste trecho, carregamos as bibliotecas necessárias e o conjunto de dados. Em seguida, convertemos os códigos e valores para numéricos e removemos as observações com valores ausentes. Depois, filtramos os pacientes de interesse e as medidas de insulina antes do café da manhã.

## Selecionar 500 dados da coluna "Value"
insulina_cafe_manha1 <- insulina_cafe_manha1 %>%
  sample_n(500) %>%
  select(Value)

## Visualizar os dados
insulina_cafe_manha1

Aqui selecionamos aleatoriamente 500 observações da coluna "Value" e visualizamos os dados.

## Converter coluna "Value" para formato numérico
insulina_cafe_manha1$Value <- as.numeric(insulina_cafe_manha1$Value)

## Plotar o histograma
hist(insulina_cafe_manha$Value)


![image](https://github.com/thiagolealg/trabalho_R/assets/113521516/29b10492-d69f-4b34-a3ba-729dc1c1c14c)


hist(insulina_cafe_manha1$Value)


![image](https://github.com/thiagolealg/trabalho_R/assets/113521516/ff0a0160-4d9e-4a5d-b9c2-d9861bec1e5c)

# Gráfico de Densidade

ggplot() +
  geom_density(data = data, aes(x = Valor, color = "Insulina NPH"), size = 1) +
  geom_density(data = data2, aes(x = Valor, color = "Insulina Ultralente"), size = 1) +
  labs(x = "Valor", y = "Densidade", title = "Densidade das Insulinas") +
  scale_color_manual(values = c("Insulina NPH" = "blue", "Insulina Ultralente" = "red")) +
  theme_minimal()


![image](https://github.com/thiagolealg/trabalho_R/assets/113521516/74e9dad9-bfd1-4671-8d76-c5f6415feb2c)


# Criar um data frame combinando as duas variáveis
data <- data.frame(
  Grupo = c(rep("insulina_cafe_manha1", length(insulina_cafe_manha1$Value)),
            rep("insulina_cafe_manha", length(insulina_cafe_manha$Value))),
  Valor = c(insulina_cafe_manha1$Value, insulina_cafe_manha$Value)
)

# Criar o boxplot
ggplot(data, aes(x = Grupo, y = Valor)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Grupo", y = "Valor", title = "Boxplot das Insulinas") +
  theme_minimal()

  
![image](https://github.com/thiagolealg/trabalho_R/assets/113521516/67eb9091-cd81-4f9b-bf99-f40d6bb01016)



Nestas etapas, convertemos a coluna "Value" para o formato numérico e plotamos o histograma e boxplot para visualizar a distribuição dos dados.

## Filtrar pacientes que utilizaram a insulina regular (Code 33) e não utilizaram a insulina 35
pacientes_regular <- data %>%
  filter(Code == 33 & !paciente %in% insulin_data$Code[insulin_data$Code == 35]) %>%
  distinct(paciente)

## Visualizar os pacientes
pacientes_regular


Aqui filtramos os pacientes que utilizaram a insulina regular (Code 33) e não utilizaram a insulina 35. Em seguida, visualizamos os pacientes selecionados.


## Lista de pacientes
pacientes <- c("data-01", "data-02", "data-03", "data-04", "data-05", "data-06", "data-07", "data-08", "data-09", "data-10", "data-11", "data-12", "data-13", "data-14", "data-15", "data-16", "data-17", "data-18", "data-19", "data-20", "data-21", "data-22", "data-23", "data-24", "data-25", "data-26", "data-27", "data-28", "data-29", "data-30", "data-31", "data-32", "data-33", "data-34", "data-35", "data-36", "data-37", "data-38", "data-39", "data-40", "data-41", "data-42", "data-43", "data-44", "data-45", "data-46", "data-47", "data-48", "data-51", "data-52", "data-53", "data-54", "data-55", "data-56", "data-57", "data-58", "data-59", "data-60", "data-61", "data-62", "data-63", "data-65", "data-66", "data-67", "data-70")

## Filtrar pacientes e medida de insulina antes do café da manhã (Code 58)
insulina_cafe_manha <- data %>%
  filter(paciente %in% pacientes & Code == 58) %>%
  select(Value) %>%
  sample_n(500)

## Visualizar as medidas de insulina antes do café da manhã
insulina_cafe_manha

Aqui, definimos a lista de pacientes e filtramos as medidas de insulina antes do café da manhã para esses pacientes. Em seguida, visualizamos as medidas selecionadas.


## Teste t
t.test(insulina_cafe_manha$Value, insulina_cafe_manha1$Value)
	Welch Two Sample t-test

data:  group1 and group2
t = 3.1551, df = 986.07, p-value = 0.001653
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  6.219986 26.688014
sample estimates:
mean of x mean of y 
  191.224   174.770 


Realizamos o teste t de Welch para comparar as médias das duas amostras.

group1 <-insulina_cafe_manha1$Value
group2 <- insulina_cafe_manha$Value

## Operações estatísticas
group1 <- na.omit(group1)
group2 <-na.omit(group2)

num_gr1 <- length(group1)

num_gr2 <- length(group2)

media_gr1 <- mean(group1)

media_gr2 <- mean(group2)

s1_gr1 <- var(group1)/num_gr1

s1_gr2 <- var(group2)/num_gr2

media_diff <- media_gr1 - media_gr2

## Estatística t
t0 <- (media_diff - 0)/(sqrt(s1_gr1+s1_gr2))
t0
Neste trecho, definimos as variáveis para realizar o cálculo da estatística t.

## Graus de liberdade
g1_cima = (s1_gr1 + s1_gr2)^2
g1_baixo = (s1_gr1^2)/(num_gr1-1) + (s1_gr2^2)/(num_gr2-1)
g1_total = g1_cima/g1_baixo
g1_total

Aqui, calculamos os graus de liberdade para o teste t de Welch.

## Valor-p
ate_t0 <- pt(t0,df=g1_total)
2*(1-ate_t0)



