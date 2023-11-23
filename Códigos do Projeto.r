# Instale o tidyverse se ainda não tiver instalado
# install.packages("tidyverse")
install.packages("tidyverse")
library(tidyverse)

# Carregar dados
casos_humanos <- fa_casoshumanos_1994_2021_1_
class(casos_humanos)
epizootias <- fa_epizpnh_1999_2021

str(casos_humanos[2154,])

str(epizootias)

# Remover linhas com valores ausentes
casos_humanos <- casos_humanos %>% drop_na()
epizootias <- epizootias %>% drop_na()

str(casos_humanos)

str(epizootias)

library(ggplot2)
library(dplyr)
casos_humanos <- head(casos_humanos, 550)
epizootias <- head(epizootias,550)

# Substitua 'Assintom�tico' por NA em 'DT_IS'
casos_humanos$DT_IS[casos_humanos$DT_IS == 'Assintom�tico'] <- NA

# Filtra linhas onde 'DT_IS' e 'DATA_OCOR' são datas válidas
dados_combinados_comb2 <- inner_join(
  casos_humanos %>% filter(!is.na(DT_IS)),
  epizootias %>% filter(!is.na(DATA_OCOR)),
  by = c("COD_MUN_LPI" = "COD_MUN_OCOR")
)

# Crie um gráfico de barras empilhadas
ggplot(dados_combinados_comb2, aes(x = DATA_OCOR, fill = factor(COD_MUN_LPI))) +
  geom_bar(position = "stack") +
  labs(x = "Data de Ocorrência", y = "Quantidade de Casos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Município LPI")



# Excluindo linhas com 'OBITO' igual a 'IGN'
casos_humanos <- casos_humanos[casos_humanos$OBITO != "IGN", ]

# Convertendo 'OBITO' para fator
casos_humanos$OBITO <- factor(casos_humanos$OBITO, levels = c("SIM", "NÃO"))

# Histograma
histograma_idade <- ggplot(casos_humanos, aes(x = IDADE, fill = OBITO)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  labs(x = "Idade", y = "Contagem", fill = "Óbito") +
  ggtitle("Distribuição de Idade por Óbito")

# Barplot
barplot_idade <- ggplot(casos_humanos, aes(x = OBITO, fill = OBITO)) +
  geom_bar() +
  labs(x = "Óbito", y = "Contagem", fill = "Óbito") +
  ggtitle("Contagem de Óbito e Sobrevivências") +
  theme_minimal()

# Visualizando os gráficos
print(histograma_idade)
print(barplot_idade)


# Suponha que você tenha um dataframe chamado 'seu_dataset'
# com colunas 'SEXO' e 'OBITO'

# Excluindo linhas com 'OBITO' igual a 'IGN'
casos_humanos <- casos_humanos[casos_humanos$OBITO != "IGN", ]

# Criando um histograma
histograma <- table(casos_humanos$SEXO, casos_humanos$OBITO)

# Plotando o histograma
barplot(histograma, beside = TRUE, legend = rownames(histograma), col = c("blue", "red"), main = "Relação Sexo x Óbito", xlab = "Óbitos", ylab = "Contagem")


# Localização x Óbito
casos_humanos %>%
  ggplot(aes(x = UF_LPI, fill = OBITO)) +
  geom_bar(position = "dodge") +
  labs(x = "UF Completo (Mortes e sobrevivencias)", y = "Contagem") +
  scale_fill_manual(values = c("blue", "red"))

# Histogramas
# Histograma por Idade
ggplot(casos_humanos, aes(x = IDADE)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(x = "Idade", y = "Número de Casos")

# Histograma por Estado
ggplot(casos_humanos, aes(x = UF_LPI)) +
  geom_bar(fill = "blue", color = "black") +
  labs(x = "UF", y = "Contagem")

# Histograma por Semana Epidemiológica
ggplot(casos_humanos, aes(x = SE_IS)) +
  geom_bar(fill = "blue", color = "black") +
  labs(x = "Semana Epidemiológica", y = "Contagem")

# Histograma por Semana Epidemiológica
ggplot(casos_humanos, aes(x = MACRORREG_LPI)) +
  geom_bar(fill = "blue", color = "black") +
  labs(x = "MACRO REGIÃO", y = "Contagem")

# Instale o pacote GGally se ainda não estiver instalado
# install.packages("GGally")

install.packages("GGally")

# Instale os pacotes se ainda não estiverem instalados
# install.packages(c("GGally", "dplyr"))

library(GGally)
library(dplyr)
casos_humanos <- head(casos_humanos, 2399)

# Selecione as variáveis relevantes nos dataframes
variaveis_casos_humanos <- casos_humanos %>%
  select(COD_UF_LPI, COD_MUN_LPI)

variaveis_epizootias <- epizootias %>%
  select(COD_UF_OCOR, COD_MUN_OCOR)

# Combine os dataframes
dados_combinados <- cbind(variaveis_casos_humanos, variaveis_epizootias)

# Crie a matriz de dispersão
scatter_matrix <- ggpairs(dados_combinados)

# Visualize a matriz de dispersão
print(scatter_matrix)

'''
library(GGally)
library(dplyr)
casos_humanos <- head(casos_humanos, 2399)
# Substitua 'Assintom�tico' por NA em 'DT_IS'
casos_humanos$DT_IS[casos_humanos$DT_IS == 'Assintom�tico'] <- NA

library(GGally)
library(dplyr)

# Substitua 'Assintom�tico' por NA em 'DT_IS'
casos_humanos$DT_IS[casos_humanos$DT_IS == 'Assintom�tico'] <- NA

# Filtra linhas onde 'DT_IS' e 'DATA_OCOR' são datas válidas
dados_combinados_comb2 <- inner_join(
  casos_humanos %>% filter(!is.na(DT_IS)),
  epizootias %>% filter(!is.na(DATA_OCOR)),
  by = c("COD_MUN_LPI" = "COD_MUN_OCOR")
)

# Crie a matriz de dispersão
scatter_matrix_comb2 <- ggpairs(dados_combinados_comb2, cardinality_threshold = 1000)

# Visualize a matriz de dispersão
print(scatter_matrix_comb2)


# Crie a matriz de dispersão
scatter_matrix_comb2 <- ggpairs(dados_combinados_comb2)

# Visualize a matriz de dispersão
print(scatter_matrix_comb2)
'''

# Exemplo de teste t para amostras independentes
resultado_teste_idade <- t.test(casos_humanos$IDADE, casos_humanos$OBITO, na.action = na.omit)

# Exibir os resultados do teste
print(resultado_teste_idade)

# Exemplo de teste Qui-Quadrado
tabela_contingencia <- table(casos_humanos$SEXO, casos_humanos$OBITO)
resultado_teste_qui_quadrado <- chisq.test(tabela_contingencia)

# Exibir os resultados do teste
print(resultado_teste_qui_quadrado)

# Exemplo de teste de Kruskal-Wallis
resultado_teste_kruskal_wallis <- kruskal.test(IDADE ~ OBITO, data = casos_humanos)

# Exibir os resultados do teste
print(resultado_teste_kruskal_wallis)

# Exemplo de teste de Kruskal-Wallis
resultado_teste_kruskal_wallis <- kruskal.test(SE_IS  ~ OBITO, data = casos_humanos)
print(resultado_teste_kruskal_wallis)

# Supondo que você tenha os conjuntos de dados casos_humanos e epizootias
# Certifique-se de substituir isso pelos nomes reais dos seus conjuntos de dados

# Instalando e carregando os pacotes, caso ainda não tenham sido instalados/carregados
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# Exemplo de dados (substitua pelos seus dados reais)
casos_humanos <- head(fa_casoshumanos_1994_2021_1_, 2399)
epizootias <- fa_epizpnh_1999_2021

# Realizando o teste de Kruskal-Wallis
resultado_teste_kruskal_wallis <- kruskal.test(SE_IS ~ factor(epizootias$DATA_OCOR), data = casos_humanos)
print(resultado_teste_kruskal_wallis)


# Supondo que você tenha os conjuntos de dados casos_humanos e epizootias
# Certifique-se de substituir isso pelos nomes reais dos seus conjuntos de dados

# Exemplo de teste de Kruskal-Wallis
resultado_teste_kruskal_wallis <- kruskal.test(UF_LPI ~ OBITO, data = casos_humanos)

# Exibir os resultados do teste
print(resultado_teste_kruskal_wallis)

# Exemplo de teste de Kruskal-Wallis
resultado_teste_kruskal_wallis <- kruskal.test(MACRORREG_LPI ~ OBITO, data = casos_humanos)

# Exibir os resultados do teste
print(resultado_teste_kruskal_wallis)