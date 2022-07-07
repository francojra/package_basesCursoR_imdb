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
  select(genero, pais, duracao, nota_imdb) %>%
  filter(genero %in% c("Romance", "Drama", "Comedy", "Horror", "Western")) %>%
  filter(pais %in% c("USA", "France", "Russia", "Germany", "Spanish"))
View(imdb1)  
glimpse(imdb1)
imdb1$genero <- as.factor(imdb1$genero)
imdb1$pais <- as.factor(imdb1$pais)

# Análises ---------------------------------------------------------------------------------------------------------------------------------

imdb2 <- imdb1 %>%
  group_by(genero) %>%
  summarise(med = mean(nota_imdb),
            sd = sd(nota_imdb),n = n(),
            se = sd/sqrt(n))
View(imdb2)

p1 <- ggplot(imdb2, aes(x = genero, y = med)) +
  geom_col(fill = "#7fc97f", color = "black") +
  geom_errorbar(aes(x = genero, y = med, ymin = med - se,
                    ymax = med + se), width = 0.3, size = 0.9) +
  labs(x = "Gêneros", y = "Notas IMDB")
p1
