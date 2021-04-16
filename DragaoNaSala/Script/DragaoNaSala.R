####################################################################
####################################################################
####################################################################
#################       O DRAGÃO NA SALA            ################
################       Material de replicação       ################
################           do TCC de                ################
################                                    ################
################       DANILLO RAFAEL BATISTA       ################
################          DO NASCIMENTO             ################
################                                    ################
#################  ORIENTAÇÃO: IAN REBOUÇAS BATISTA ################
####################################################################
####################################################################
####################################################################


#chamando os pacotes necessários

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyselect)
library(ggplot2)
options(scipen = 999) ## desativando notação científica
setwd() ##Defina aqui o diretório de trabalho onde estará a pasta matriz do material de replicação

### Dados do comércio dos países da América do Sul-China
## os dados de comércio mundial baixados do UN Comtrade Database são baixados em partes, as pastas comtrade_n

#abrindo bancos

comtrade_1 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_1.csv")
comtrade_2 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_2.csv")

#selecionando as variaveis no banco 
comtrade_1 <- select(comtrade_1, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

comtrade_2 <- select(comtrade_2, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

#juntando os bancos

BANCO <- rbind(comtrade_1, comtrade_2)

#removendo os dois bancos originais (pesados e poluem o environment---> )

rm(comtrade_1, comtrade_2)

#código para filtrar o banco:

BANCO <- filter(BANCO, `Trade Flow` ==  "Export" |               #Selecionando somente Exportação e Importação
                     `Trade Flow` == "Import")            
BANCO <- filter(BANCO, `Reporter` != "Venezuela")                #Selecionando tudo que não for Venezuela 
BANCO <- filter(BANCO, `Partner` == "China")                     #Selecionando somente China

BANCO$Year <- as.numeric(BANCO$Year)
BANCO <- filter(BANCO, `Year` > 1997)                           #Selecionando o períod 1998-2018
BANCO <- filter(BANCO, `Year` < 2019)


# Plotando o comércio da Regiao com a China ao longo dos anos (GRÁFICO 1)

ggplot(data = BANCO) + 
  geom_smooth(mapping = aes(x = Year, y = `Trade Value (US$)`), se = FALSE) + xlab("Ano") + ylab("US$")

##### O BANCO será o banco com dados do comércio dos países da região com a China


### Dados do comércio dos países da América do Sul com países da América do Sul

#abrindo bancos

comtrade_3 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_3.csv")
comtrade_4 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_4.csv")
comtrade_5 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_5.csv")
comtrade_6 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_6.csv")
comtrade_7 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_7.csv")
comtrade_8 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtrade_8.csv")

#selecionando as variaveis no banco 

comtrade_3 <- select(comtrade_3, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

comtrade_4 <- select(comtrade_4, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

comtrade_5 <- select(comtrade_5, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

comtrade_6 <- select(comtrade_6, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

comtrade_7 <- select(comtrade_7, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))

comtrade_8 <- select(comtrade_8, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                     -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                     -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                     -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                     -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                     -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                     -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                     -("Flag"), -("Classification"))


BANCO2 <- rbind(comtrade_3, comtrade_4, comtrade_5, comtrade_6) ## Banco com comércio intrarregional

BANCO3 <- rbind(comtrade_7, comtrade_8) ## Banco com total de comércio dos países

rm(comtrade_3)    #removendo os bancos maiores e originais 
rm(comtrade_4)
rm(comtrade_5)
rm(comtrade_6)
rm(comtrade_7)
rm(comtrade_8)

BANCO2$Year <- as.numeric(BANCO2$Year) #filtrando o período
BANCO2 <- filter(BANCO2, `Year` > 1997)
BANCO2 <- filter(BANCO2, `Year` < 2019)

BANCO3$Year <- as.numeric(BANCO3$Year) #filtrando o período
BANCO3 <- filter(BANCO3, `Year` > 1997)
BANCO3 <- filter(BANCO3, `Year` < 2019)

BANCO3 <- BANCO3 %>% 
  rename(`Total` = "Trade Value (US$)") ## Total é o nome da variável "Total de Comércio do País"

#### O BANCO2 é o banco com os dados do comércio dos países da região com os países da região
#### O BANCO3 é o banco com dados do comércio total que os países da Am do Sul realizaram naquele ano

BANCO4 <- merge(BANCO2, BANCO3, by=c("Trade Flow", "Year", "Reporter")) ##Juntanto os dois bancos

BANCO4$`Trade Value (US$)` <- as.numeric(BANCO4$`Trade Value (US$)`) #tornando as variaveis numéricas
BANCO4$Total <- as.numeric(BANCO4$Total)

BANCO4 <- filter(BANCO4, `Reporter` != `Partner.x`)  ## Excluindo casos onde o país importa/exporta para ele mesmo

BANCO4$`Trade Value (US$)` <- NULL    #Excluidno essa variavel pra nao confundir com a do BANCO, quando juntar no BANCOFINAL

BANCO5 <- merge(BANCO, BANCO3, by=c("Trade Flow", "Year", "Reporter")) ##Juntando o banco com dados com a China (BANCO) com o banco que tem os dados do Total do comércio dos paísses da região (BANCO3)

BANCO5$`Trade Value (US$)` <- as.numeric(BANCO5$`Trade Value (US$)`) ## transformando as variáveis em numéricas
BANCO5$Total <- as.numeric(BANCO5$Total)

BANCO5$prop_chi <- BANCO5$`Trade Value (US$)`/ BANCO5$Total ##Criando a variável prop_chi, dividindo comercio com a china / Total

ggplot(data = BANCO5) + 
  geom_smooth(mapping = aes(x = Year, y = prop_chi), se= FALSE)     #evolução ao longo do tempo do peso do regional no comércio total da região

###### TOTAL DE COMÉRCIO DO MUNDO 
##caregando os bancos
comtradeW_1 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtradeW_1.csv")
comtradeW_2 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtradeW_2.csv")
comtradeW_3 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtradeW_3.csv")
comtradeW_4 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtradeW_4.csv")
comtradeW_5 <- read_csv("./DragaoNaSala/Dados/DadosOriginais/comtradeW_5.csv")

#selecionando as variaveis
comtradeW_1 <- select(comtradeW_1, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                      -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                      -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                      -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                      -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                      -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                      -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                      -("Flag"), -("Classification"))

comtradeW_2 <- select(comtradeW_2, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                      -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                      -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                      -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                      -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                      -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                      -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                      -("Flag"), -("Classification"))

comtradeW_3 <- select(comtradeW_3, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                      -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                      -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                      -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                      -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                      -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                      -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                      -("Flag"), -("Classification"))

comtradeW_4 <- select(comtradeW_4, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                      -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                      -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                      -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                      -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                      -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                      -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                      -("Flag"), -("Classification"))

comtradeW_5 <- select(comtradeW_5, -("Aggregate Level"), -("Is Leaf Code"), -("Reporter Code"), 
                      -("2nd Partner Code"), -("2nd Partner"), -("Period"), -("Period Desc."), 
                      -("Trade Flow Code"), -("Partner Code"), -("2nd Partner Code"), -("2nd Partner"),
                      -("2nd Partner ISO"), -("Customs Proc. Code"), -("Customs"), -("Mode of Transport Code"),
                      -("Mode of Transport"), -("Commodity"), -("Commodity Code"), -("Qty Unit Code"), 
                      -("Qty Unit"), -("Qty"), -("Alt Qty Unit Code"), -("Alt Qty Unit"), -("Alt Qty"),
                      -("Netweight (kg)"), -("Gross weight (kg)"), -("CIF Trade Value (US$)"), -("FOB Trade Value (US$)"),
                      -("Flag"), -("Classification"))

#juntando os bancos
BANCOW <- rbind(comtradeW_1, comtradeW_2, comtradeW_3, comtradeW_4, comtradeW_5)

#removendo os bancos originais
rm(comtradeW_1, comtradeW_2, comtradeW_3, comtradeW_4, comtradeW_5)

#filtrando somente exportação e importação
BANCOW <- filter(BANCOW, `Trade Flow` ==  "Export" | 
                   `Trade Flow` == "Import")  

#filtrando o período
BANCOW$Year <- as.numeric(BANCOW$Year)
BANCOW <- filter(BANCOW, `Year` > 1997)                         
BANCOW <- filter(BANCOW, `Year` < 2019)

#agregando por ano (total de expo e impo realizadas no ano)
BANCOW <- aggregate(BANCOW$`Trade Value (US$)`, by = list(BANCOW$Year), FUN = sum)

#renomeando as variaveis após o agregate
BANCOW <- BANCOW %>% 
  rename(`world_trade` = "x",
         `year` = "Group.1") 

ggplot(data = BANCOW) + 
  geom_smooth(mapping = aes(x = year, y = world_trade), se=FALSE )    

######################
###### Agregando a região toda por ano

reg_chi_by_year <- aggregate(BANCO$`Trade Value (US$)`, by = list(BANCO$Year), FUN = sum) #comercio região-china por ano

reg_reg_by_year <- aggregate(BANCO2$`Trade Value (US$)`, by = list(BANCO2$Year), FUN = sum) #comercio região-regiao por ano

reg_chi_by_year <- reg_chi_by_year %>%  #renomeando após o aggregate 
  rename(`Reg-Chi` = "x") 

reg_reg_by_year <- reg_reg_by_year %>%  #renomeando após o aggregate
  rename(`Reg-Reg` = "x") 

data <- merge(reg_chi_by_year, reg_reg_by_year, by=c("Group.1")) #juntanto os dois ultimos bancos num banco para as análises

cor.test(data$`Reg-Chi`, data$`Reg-Reg`)   #essa relação é positiva pq faz sentido: ao longo dos anos vc tem mais comércio então o comércio regional sobe e o com a china tambem
plot(data$`Reg-Chi`, data$`Reg-Reg`)

### Agora, adicionaremos a variavel de total de comércio no ano (BANCO5)

datatotal <- aggregate(BANCO5$Total, by = list(BANCO5$Year), FUN = sum) #agregando o total por ano (datatotal)
data_prop <- merge(data, datatotal, by=c("Group.1"))  #criando um banco com o data + o datatotal
data_prop$propchi <- data_prop$`Reg-Chi`/data_prop$x  #criando a variavel com a proporção de comercio pra china
data_prop$propreg <- data_prop$`Reg-Reg`/data_prop$x  #criando a variavel com a proporção de comércio para região

cor.test(data_prop$propchi, data_prop$propreg)   #teste de correlação, -0.89, significativo

#Grafico 3, de correlação entre as proporções
plot(data_prop$propchi, data_prop$propreg, 
     xlab = "Proporção de comércio com a China", ylab="Proporção de comércio intrarregional",
     pch=20)

# Gráfico 2,queda da proporção do comércio da região realizado com a própria região
ggplot(data = data_prop) + 
  geom_smooth(mapping = aes(x = Group.1, y = propreg), se= FALSE) + xlab ("Ano") +
  ylab("Proporção de comércio intrarregional")     #evolução ao longo do tempo do peso do regional no comércio total da região

write.csv(data_prop, file = "AggregateTradeRegionChinaandWorld", fileEncoding = "WINDOWS-1252")

#################
##### Banco para regressão - será com os números totais, e não a proporção

datachi <- aggregate(BANCO$`Trade Value (US$)`, by = list(BANCO$Reporter, BANCO$Year), FUN = sum) #agregando por pais e por ano o comércio com a china

datareg <- aggregate(BANCO2$`Trade Value (US$)`, by = list(BANCO2$Reporter, BANCO2$Year), FUN = sum) #agregando por pais e por ano o comercio com a região

datachi <- datachi %>%                     #renomeando as variaveis 
  rename(`china_trade` = "x",
         `country` = "Group.1",
         `year` = "Group.2") 

datareg <- datareg %>%                    #renomeando as variaveis 
  rename(`region_trade` = "x",
         `country` = "Group.1",
         `year` = "Group.2") 

dataset <- merge(datachi, datareg, by=c("country", "year"))   #juntando em um só banco o comércio com a china e com a região

dataset <- merge(dataset, BANCOW, by=c("year"))       #juntando agora o total do comércio do mundo por ano (variavel de controle)

plot(dataset$china_trade, dataset$region_trade) ## a relação entre as variáveis é linear, então podemos fazer uma regressão
cor.test(dataset$china_trade, dataset$region_trade)

### Criando as variáveis em log que serão utilizadas na regressão

dataset$china_trade_log <- log(dataset$china_trade) 
dataset$region_trade_log <- log(dataset$region_trade)
dataset$world_trade_log <- log(dataset$world_trade)

write.csv(dataset, file = "TradePerCountryToCountryandToChina", fileEncoding = "WINDOWS-1252")

## Criando os modelos da regressão 
model1 <- lm(dataset$region_trade_log ~ dataset$china_trade_log)    # somente VD ~ VI
summary(model1)

model2 <- lm(dataset$region_trade_log ~ dataset$china_trade_log + factor(dataset$year) - 1)  #efeitos fixos por ano
summary(model2)

model3 <- lm(dataset$region_trade_log ~ dataset$china_trade_log + factor(dataset$country) - 1)  #efeitos fixos por país
summary(model3)

model4 <- lm(dataset$region_trade_log ~ dataset$china_trade_log + dataset$world_trade_log)  #com controle 
summary(model4)

model5 <- lm(dataset$region_trade_log ~ dataset$china_trade_log + dataset$world_trade_log + factor(dataset$year) - 1)  #com controle e efeitos fixos por ano
summary(model5)

model6 <- lm(dataset$region_trade_log ~ dataset$china_trade_log + dataset$world_trade_log + factor(dataset$country) - 1) #com controle e efeitos fixos por país
summary(model6)

####Salvando a tabela da regressão e exportando
library(stargazer)
setwd("./DragaoNaSala/Output")   
stargazer(model1, model3, model4, model6, type = "html",
          add.lines = list(c("Fixed effects?", "No", "Yes", "No", "Yes")),
          out = "reg.doc", title = "Efeito do Comércio com a China no Comércio Regional") 

#Tabelas do Apêndice

#A1
stargazer(model2, type = "html",
          out = "reg.doc", title = "Efeito do Comércio com a China no Comércio Regional") 
#A2
stargazer(model5, type = "html",
          out = "reg.doc", title = "Efeito do Comércio com a China no Comércio Regional") 

## Os histogramas das variáveis utilizadas na regressão, distribuições normais (APÊNDICE)

histograma <- ggplot(data = dataset) +
  geom_histogram(mapping = aes(x = china_trade_log), binwidth = 0.5)+
  labs(x = "Distribuição da variável china_trade", y = " ")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12))
print(histograma)

histograma <- ggplot(data = dataset) +
  geom_histogram(mapping = aes(x = region_trade_log), binwidth = 0.5)+
  labs(x = "Distribuição da variável region_trade", y = " ")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12))
print(histograma)


 