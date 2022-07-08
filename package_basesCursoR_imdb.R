# Base de dados - Curso R ------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 07/07/22 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes ----------------------------------------------------------------------------------------------------------------------------

library(basesCursoR)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(forcats)
library(gridExtra)

# Identificar bases disponíveis ------------------------------------------------------------------------------------------------------------

basesCursoR::bases_disponiveis()

# Carregar base de dados -------------------------------------------------------------------------------------------------------------------

imdb <- basesCursoR::pegar_base("imdb_completa")
View(imdb)

# Selecionar dados -------------------------------------------------------------------------------------------------------------------------

imdb1 <- imdb %>%
  select(genero, pais, duracao, nota_imdb, ano) %>%
  filter(ano %in% c("1906", "1914", "1949", "1976", "1980", "2004")) %>%
  filter(genero %in% c("Romance", "Drama", "Comedy", "Horror", "Western")) %>%
  filter(pais %in% c("USA", "France", "Russia", "Germany", "Spanish"))
View(imdb1)  
glimpse(imdb1)
imdb1$genero <- as.factor(imdb1$genero)
imdb1$pais <- as.factor(imdb1$pais)
imdb1$ano <- as.factor(imdb1$ano)

# Análises ---------------------------------------------------------------------------------------------------------------------------------

imdb2 <- imdb1 %>%
  group_by(genero) %>%
  summarise(med = mean(nota_imdb),
            sd = sd(nota_imdb),n = n(),
            se = sd/sqrt(n))
View(imdb2)

p1 <- ggplot(imdb2, aes(x = fct_reorder(genero, med), y = med)) +
  geom_col(fill = "#7fc97f", color = "black") +
  geom_errorbar(aes(x = genero, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Gêneros", y = "Notas IMDB")
p1

imdb3 <- imdb1 %>%
  group_by(genero) %>%
  summarise(med = mean(duracao),
            sd = sd(duracao),n = n(),
            se = sd/sqrt(n))
View(imdb3)

p2 <- ggplot(imdb3, aes(x = fct_reorder(genero, med), y = med)) +
  geom_col(fill = "#7fc97f", color = "black") +
  geom_errorbar(aes(x = genero, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Gêneros", y = "Duração dos filmes")
p2

imdb4 <- imdb1 %>%
  group_by(pais) %>%
  summarise(med = mean(nota_imdb),
            sd = sd(nota_imdb),n = n(),
            se = sd/sqrt(n))
View(imdb4)

p3 <- ggplot(imdb4, aes(x = fct_reorder(pais, med), y = med)) +
  geom_col(fill = "#7fc97f", color = "black") +
  geom_errorbar(aes(x = pais, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Países", y = "Notas IMDB")
p3

imdb5 <- imdb1 %>%
  group_by(pais) %>%
  summarise(med = mean(duracao),
            sd = sd(duracao),n = n(),
            se = sd/sqrt(n))
View(imdb5)

p4 <- ggplot(imdb5, aes(x = fct_reorder(pais, med), y = med)) +
  geom_col(fill = "#7fc97f", color = "black") +
  geom_errorbar(aes(x = pais, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Países", y = "Duração dos filmes")
p4

imdb5 <- imdb1 %>%
  group_by(ano) %>%
  summarise(med = mean(nota_imdb),
            sd = sd(nota_imdb),n = n(),
            se = sd/sqrt(n))
View(imdb5)

p5 <- ggplot(imdb4, aes(x = fct_reorder(pais, med), y = med)) +
  geom_col(fill = "#7fc97f", color = "black") +
  geom_errorbar(aes(x = pais, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Países", y = "Notas IMDB")
p5

imdb6 <- imdb1 %>%
  group_by(ano) %>%
  summarise(med = mean(duracao),
            sd = sd(duracao),n = n(),
            se = sd/sqrt(n))
View(imdb6)

p4 <- ggplot(imdb5, aes(x = fct_reorder(pais, med), y = med)) +
  geom_col(fill = "#7fc97f", color = "black") +
  geom_errorbar(aes(x = pais, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Países", y = "Duração dos filmes")
p4
