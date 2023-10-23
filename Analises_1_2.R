if(!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse)

install.packages("ggplot2")

library(RColorBrewer)

vendas <- read.delim( "vendas.csv")

summary(vendas)

head(vendas)

vendas %>% distinct(vendas$Category)

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

barplot(faturamentos, main = "Faturamento", xlab = "Category",ylab = "$", ylim = c(0, 20000),col = coul)

unique(vendas$Brand)

adidas_sales <- vendas$Brand == 'Adidas'
adidas <- vendas[adidas_sales,]
adidas_media <- mean(adidas$Price, na.rm = TRUE)
adidas_dp <- sd(adidas$Price, na.rm = TRUE)
adidas_cv <- (adidas_dp / adidas_media)*100

hm_sales <- vendas$Brand == 'H&M'
hm <- vendas[hm_sales,]
hm_media <- mean(hm$Price, na.rm = TRUE)
hm_dp <- sd(hm$Price, na.rm = TRUE)
hm_cv <- (hm_dp / hm_media)*100

zara_sales <- vendas$Brand == 'Zara'
zara <- vendas[zara_sales,]
zara_media <- mean(zara$Price, na.rm = TRUE)
zara_dp <- sd(zara$Price, na.rm = TRUE)
zara_cv <- (zara_dp / zara_media)*100

gucci_sales <- vendas$Brand == 'Gucci'
gucci <- vendas[gucci_sales,]
gucci_media <- mean(gucci$Price, na.rm = TRUE)
gucci_dp <- sd(gucci$Price, na.rm = TRUE)
gucci_cv <- (gucci_dp / gucci_media)*100

nike_sales <- vendas$Brand == 'Nike'
nike <- vendas[nike_sales,]
nike_media <- mean(nike$Price, na.rm = TRUE)
nike_dp <- sd(nike$Price, na.rm = TRUE)
nike_cv <- (nike_dp / nike_media)*100

variacao_precos_marca <- data.frame(Marca = c("Adidas", "H&M", "Zara", "Gucci", "Nike"),
                                    Média = c(adidas_media, hm_media, zara_media, gucci_media, nike_media),
                                    DP = c(adidas_dp, hm_dp, zara_dp, gucci_dp, nike_dp),
                                    CV = c(adidas_cv, hm_cv, zara_cv, gucci_cv, nike_cv))
variacao_precos_marca
