#Pacotes e bibliotecas
if(!require("pacman")) install.packages

pacman::p_load(tidyverse)

install.packages("ggplot2")

library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(vcd)
library(plyr)
library(dplyr)
library(readr)
library(purrr)
library(tidyverse)
library(xtable)

#Workspace e leitura de bancos de dados
setwd("C:/Users/edusc/Desktop/ESTAT")

vendas <- read.csv("vendas.csv")

summary(vendas)

dev_att <- read.csv("devolução_atualizado.csv")

#Resumos
summary(dev_att)

head(vendas)

vendas %>% distinct(vendas$Category)

#Cores ESTAT
cores_estat <- c("# A11D21 ", " #003366 ", "# CC9900 ", " #663333 ", "# FF6600", "# CC9966 ", " #999966 ", " #006606 ", " #008091 ", " #041835 ", " #666666")

theme_estat <- function (...) {
  theme <- ggplot2 :: theme_bw () +
    ggplot2 :: theme (
      axis.title.y = ggplot2 :: element_text( colour = "black ",
                                                size = 12),
      axis.title.x = ggplot2 :: element_text( colour = "black ",
                                                size = 12),
      axis.text = ggplot2 :: element_text( colour = " black", size
                                            = 9.5),
      panel.border = ggplot2 :: element_blank () ,
      axis.line = ggplot2 :: element_line( colour = " black"),
      legend.position = "top",
      ...
    )
  return (
    list(
      theme ,
      scale_fill_manual ( values = cores_estat ),
      scale_colour_manual ( values = cores_estat )
    )
  )
}

#Análise 1
mens_sales <- vendas$Category == "Men's Fashion"
homens <- vendas[mens_sales,]

homens_fat <- sum(homens$Price, na.rm = TRUE)
homens_fat

womens_sales <- vendas$Category == "Women's Fashion"
mulheres <- vendas[womens_sales,]

mulheres_fat <- sum(mulheres$Price, na.rm = TRUE)
mulheres_fat

kids_sales <- vendas$Category == "Kids' Fashion"
criancas <- vendas[kids_sales,]

criancas_fat <- sum(criancas$Price, na.rm = TRUE)
criancas_fat

faturamentos <- c(homens_fat, mulheres_fat, criancas_fat)
names(faturamentos) <- c('Moda masculina', 'Moda feminina', 'Moda infantil')
faturamentos

coul <- brewer.pal(5, "Set2") 

barplot(faturamentos, main = "Faturamento", xlab = "Category",ylab = "$", ylim = c(0, 20000),col = "#A11D21") +
geom_text(
  aes(label = scales::percent(..count../sum(..count..))),
  stat = "count",
  position = position_dodge(width = 0.5),
  vjust = 5
)


#Análise 2
unique(vendas$Brand)

adidas_sales <- vendas$Brand == 'Adidas'
adidas <- vendas[adidas_sales,]
adidas_media <- mean(adidas$Price, na.rm = TRUE)
adidas_dp <- sd(adidas$Price, na.rm = TRUE)
adidas_cv <- (adidas_dp / adidas_media)*100
adidas_min <- min(adidas$Price, na.rm = TRUE)
adidas_max <- max(adidas$Price, na.rm = TRUE)
adidas_quartil_1 <- quantile(adidas$Price, probs = c(0.25), na.rm = TRUE)
adidas_quartil_3 <- quantile(adidas$Price, probs = c(0.75), na.rm = TRUE)
adidas_median <- median(adidas$Price, na.rm = TRUE)

hm_sales <- vendas$Brand == 'H&M'
hm <- vendas[hm_sales,]
hm_media <- mean(hm$Price, na.rm = TRUE)
hm_dp <- sd(hm$Price, na.rm = TRUE)
hm_cv <- (hm_dp / hm_media)*100
hm_min <- min(hm$Price, na.rm = TRUE)
hm_max <- max(hm$Price, na.rm = TRUE)
hm_quartil_1 <- quantile(hm$Price, probs = c(0.25), na.rm = TRUE)
hm_quartil_3 <- quantile(hm$Price, probs = c(0.75), na.rm = TRUE)
hm_median <- median(hm$Price, na.rm = TRUE)

zara_sales <- vendas$Brand == 'Zara'
zara <- vendas[zara_sales,]
zara_media <- mean(zara$Price, na.rm = TRUE)
zara_dp <- sd(zara$Price, na.rm = TRUE)
zara_cv <- (zara_dp / zara_media)*100
zara_min <- min(zara$Price, na.rm = TRUE)
zara_max <- max(zara$Price, na.rm = TRUE)
zara_quartil_1 <- quantile(zara$Price, probs = c(0.25), na.rm = TRUE)
zara_quartil_3 <- quantile(zara$Price, probs = c(0.75), na.rm = TRUE)
zara_median <- median(zara$Price, na.rm = TRUE)

gucci_sales <- vendas$Brand == 'Gucci'
gucci <- vendas[gucci_sales,]
gucci_media <- mean(gucci$Price, na.rm = TRUE)
gucci_dp <- sd(gucci$Price, na.rm = TRUE)
gucci_cv <- (gucci_dp / gucci_media)*100
gucci_min <- min(gucci$Price, na.rm = TRUE)
gucci_max <- max(gucci$Price, na.rm = TRUE)
gucci_quartil_1 <- quantile(gucci$Price, probs = c(0.25), na.rm = TRUE)
gucci_quartil_3 <- quantile(gucci$Price, probs = c(0.75), na.rm = TRUE)
gucci_median <- median(gucci$Price, na.rm = TRUE)

nike_sales <- vendas$Brand == 'Nike'
nike <- vendas[nike_sales,]
nike_media <- mean(nike$Price, na.rm = TRUE)
nike_dp <- sd(nike$Price, na.rm = TRUE)
nike_cv <- (nike_dp / nike_media)*100
nike_min <- min(nike$Price, na.rm = TRUE)
nike_max <- max(nike$Price, na.rm = TRUE)
nike_quartil_1 <- quantile(nike$Price, probs = c(0.25), na.rm = TRUE)
nike_quartil_3 <- quantile(nike$Price, probs = c(0.75), na.rm = TRUE)
nike_median <- median(nike$Price, na.rm = TRUE)

variacao_precos_marca <- data.frame(Marca = c("Adidas", "H&M", "Zara", "Gucci", "Nike"),
                                    Média = c(adidas_media, hm_media, zara_media, gucci_media, nike_media),
                                    DP = c(adidas_dp, hm_dp, zara_dp, gucci_dp, nike_dp),
                                    Mediana = c(adidas_median, hm_median, zara_median, gucci_median, nike_median),
                                    CV = c(adidas_cv, hm_cv, zara_cv, gucci_cv, nike_cv),
                                    Mínimo = c(adidas_min, hm_min, zara_min, gucci_min, nike_min),
                                    Q1 = c(adidas_quartil_1, hm_quartil_1, zara_quartil_1, gucci_quartil_1, nike_quartil_1),
                                    Q3 = c(adidas_quartil_3, hm_quartil_3, zara_quartil_3, gucci_quartil_3, nike_quartil_3),
                                    Máximo = c(adidas_max, hm_max, zara_max, gucci_max, nike_max))
variacao_precos_marca

#Análise 3 - Relação entre catagorias e cor
isbrand <- !is.na(vendas$Brand)
box_new_vendas <- vendas[isbrand,]

isprice <- !is.na(box_new_vendas$Price)
box_vendas <- box_new_vendas[isprice,]

ggplot (box_vendas, aes(x = Brand, y = Price)) + geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") + 
  labs(x = "Marca", y = "Preço")

iscategory <- !is.na(vendas$Category)
vendas_cat <- vendas[iscategory,]

nokids <- vendas_cat$Category != "Kids' Fashion"
nokids_vendas <- vendas_cat[nokids,]

nonas <- !is.na(nokids_vendas$Color)
nonkids_vendas <- nokids_vendas[nonas,]

ggplot(nonkids_vendas, aes(x = Color) ) +
  geom_bar(aes(fill = Category), position = "dodge" ) +
  scale_fill_manual(values=c("#A11D21", 
                             "#003366")) +
  geom_text(
    aes(label = scales::percent(..count../sum(..count..))),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5
  )

cat_cor <- table(nonkids_vendas$Category, nonkids_vendas$Color)
cat_cor

cat_cor_prop <- prop.table(cat_cor)
cat_cor_prop

cat_cor_linha <- prop.table(cat_cor, 1)
cat_cor_linha

cat_cor_coluna <- prop.table(cat_cor, 2)
cat_cor_coluna

test <- chisq.test(cat_cor)
test

contingencia <- assocstats(cat_cor)
contingencia
contigencia_alterada <- (0.076/sqrt(0.5))
contigencia_alterada
#Coeficiente de contengência alterada = 0,1075 (C* = C/Cmax, Cmax = ((t-1)/t)^1/2)
#Coeficiente de contingência baixo = 0,076

#Análise 4
isprice2 <- !is.na(vendas$Price)
vendas2_ <- vendas[isprice2,]

isavaliacao <- !is.na(vendas2_$Rating)
vendas2 <- vendas2_[isavaliacao,]
vendas2

ggplot(vendas2, aes(x = Rating, y = Price) ) +
  geom_point( aes(color = Rating) )

ggplot(vendas2, aes(x = Rating, y = Price) ) + 
  geom_smooth( aes(color = Rating), se = F, method = lm )

ggplot(vendas2, aes(x = Rating, y = Price)) +
  geom_smooth(aes(color = "#A11D21"))

cor_a4 <- cor(vendas2$Rating, vendas2$Price, method = c("pearson"))
cor_a4
#Coeficiente de correlação de Pearson alto = 0,9134. Quanto maior o preço, mais alta a avaliação.

#Análise 5 Frequência de cada tipo de devolução por marca

tudo <- list.files(path = "C:/Users/edusc/Desktop/ESTAT", pattern = "*.csv",
                   full.names = TRUE) %>% lapply(read.csv) %>% bind_rows()

tudo_join <- list.files(path = "C:/Users/edusc/Desktop/ESTAT", pattern = "*.csv",
                        full.names = TRUE) %>% lapply(read.csv) %>%
  purrr::reduce(full_join, by = "Unique.ID")
is_marca <- !is.na(tudo_join$Brand)
tj_marca <- tudo_join[is_marca,]

is_dev <- !is.na(tj_marca$Motivo.devolução.x)
tudo_a5 <- tj_marca[is_dev,]

dev_marca <- table(tudo_a5$Motivo.devolução.x, tudo_a5$Brand)
cont_a5 <- assocstats(dev_marca)
cont_a5
#t = 0,8165
cont_a5_alt <- (0.199/sqrt(2/3))
cont_a5_alt
#Coeficiente de contingência alterado = 0,2437242

tablea_a5 <- prop.table(dev_marca, 1)
tablea_a5

ggplot(tudo_a5, aes(x = Brand) ) +
  geom_bar(aes(fill = Motivo.devolução.x), position = "dodge") +
  scale_fill_manual(values=c("#A11D21", 
                             "#003366",
                             "#CC9900")) +
  geom_text(
    aes(label = scales::percent(..count../sum(..count..))),
    stat = "count",
    position = position_dodge(width = 0.5),
    vjust = 5
  ) +
  labs(x = 'Marca')

# Análise 6: avaliação média por marca


marca1 <- !is.na(vendas$Brand)
marcas <- vendas[marca1,]

aval <- !is.na(marcas$Rating)
marca_aval <- marcas[aval,]


m_a_dupla <- unique(marca_aval$Unique.ID)
tab <- marca_aval[m_a_dupla,]

df_unique_specific <- marca_aval[!duplicated(marca_aval[c("Unique.ID")]), ]



marca_factor <- as.factor(df_unique_specific$Brand)

tab_a6 <- df_unique_specific %>%
  group_by(as.factor(Brand)) %>%
  summarise(media_rating <- mean(Rating))
names(tab_a6) <- c('Marca', 'Média')
tab_a6 <- tab_a6 %>% arrange(desc(Média))
tab_a6_tex <- xtable(tab_a6)
tab_a6_tex

ggplot (df_unique_specific, aes(x = Brand, y = Rating)) + geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") + 
  labs(x = "Marca", y = "Avaliação")

