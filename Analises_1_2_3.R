if(!require("pacman")) install.packages

pacman::p_load(tidyverse)

install.packages("ggplot2")

library(RColorBrewer)

library(ggplot2)

library(tidyr)

devolução_att <- read.delim("devolução_atualizado.csv")

vendas <- read.delim("vendas.csv")

summary(vendas)

setwd("C:/Users/edusc/Desktop/ESTAT")

dev_att <- read.csv("devolução_atualizado.csv")

summary(dev_att)

head(vendas)

vendas %>% distinct(vendas$Category)

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

barplot(faturamentos, main = "Faturamento", xlab = "Category",ylab = "$", ylim = c(0, 20000),col = "#A11D21")

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
adidas_quartil

hm_sales <- vendas$Brand == 'H&M'
hm <- vendas[hm_sales,]
hm_media <- mean(hm$Price, na.rm = TRUE)
hm_dp <- sd(hm$Price, na.rm = TRUE)
hm_cv <- (hm_dp / hm_media)*100
hm_min <- min(hm$Price, na.rm = TRUE)
hm_max <- max(hm$Price, na.rm = TRUE)
hm_quartil_1 <- quantile(hm$Price, probs = c(0.25), na.rm = TRUE)
hm_quartil_3 <- quantile(hm$Price, probs = c(0.75), na.rm = TRUE)
hm_quartil

zara_sales <- vendas$Brand == 'Zara'
zara <- vendas[zara_sales,]
zara_media <- mean(zara$Price, na.rm = TRUE)
zara_dp <- sd(zara$Price, na.rm = TRUE)
zara_cv <- (zara_dp / zara_media)*100
zara_min <- min(zara$Price, na.rm = TRUE)
zara_max <- max(zara$Price, na.rm = TRUE)
zara_quartil_1 <- quantile(zara$Price, probs = c(0.25), na.rm = TRUE)
zara_quartil_3 <- quantile(zara$Price, probs = c(0.75), na.rm = TRUE)
zara_quartil

gucci_sales <- vendas$Brand == 'Gucci'
gucci <- vendas[gucci_sales,]
gucci_media <- mean(gucci$Price, na.rm = TRUE)
gucci_dp <- sd(gucci$Price, na.rm = TRUE)
gucci_cv <- (gucci_dp / gucci_media)*100
gucci_min <- min(gucci$Price, na.rm = TRUE)
gucci_max <- max(gucci$Price, na.rm = TRUE)
gucci_quartil_1 <- quantile(gucci$Price, probs = c(0.25), na.rm = TRUE)
gucci_quartil_3 <- quantile(gucci$Price, probs = c(0.75), na.rm = TRUE)
gucci_quartil

nike_sales <- vendas$Brand == 'Nike'
nike <- vendas[nike_sales,]
nike_media <- mean(nike$Price, na.rm = TRUE)
nike_dp <- sd(nike$Price, na.rm = TRUE)
nike_cv <- (nike_dp / nike_media)*100
nike_min <- min(nike$Price, na.rm = TRUE)
nike_max <- max(nike$Price, na.rm = TRUE)
nike_quartil_1 <- quantile(nike$Price, probs = c(0.25), na.rm = TRUE)
nike_quartil_3 <- quantile(nike$Price, probs = c(0.75), na.rm = TRUE)
nike_quartil

variacao_precos_marca <- data.frame(Marca = c("Adidas", "H&M", "Zara", "Gucci", "Nike"),
                                    Média = c(adidas_media, hm_media, zara_media, gucci_media, nike_media),
                                    DP = c(adidas_dp, hm_dp, zara_dp, gucci_dp, nike_dp),
                                    CV = c(adidas_cv, hm_cv, zara_cv, gucci_cv, nike_cv),
                                    Mínimo = c(adidas_min, hm_min, zara_min, gucci_min, nike_min),
                                    Q1 = c(adidas_quartil_1, hm_quartil_1, zara_quartil_1, gucci_quartil_1, nike_quartil_1),
                                    Q3 = c(adidas_quartil_3, hm_quartil_3, zara_quartil_3, gucci_quartil_3, nike_quartil_3),
                                    Máximo = c(adidas_max, hm_max, zara_max, gucci_max, nike_max))
variacao_precos_marca

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
                             "#003366"))

fac_cat <- factor(nonkids_vendas$Category)
fac_cat
fac_cor <- factor(nonkids_vendas$Color)
fac_cor

fac_cat_n <- as.numeric(fac_cat)
fac_cor_n <- as.numeric(fac_cor)

cat_cor <- cor(fac_cat_n, fac_cor_n)
cat_cor
