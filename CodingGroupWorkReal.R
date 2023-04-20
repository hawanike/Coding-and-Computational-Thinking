#-------------------------------------------------------------------------------
#for the purpose of class activity, please run each block of code by 
#copy, paste, and run all lines from each chapter in the console chronologically
#-------------------------------------------------------------------------------

#PS. full detailed explanation is on the way and will be posted soon
#it's 4AM now and I have 9AM class
#-------------------------------------------------------------------------------
#Chapter 0.0 SETTING UP
#for first time user(s), please undo the comment of the lines below 
#and install the required packages
#install.packages("tibble")
#install.packages("tidyverse")
#install.packages("ggpubr")

#-------------------------------------------------------------------------------
#Chapter 0.1
#setup
library(package = "tibble")
library(package = "tidyverse")
library(ggpubr)
set.seed(1)

#-------------------------------------------------------------------------------
#Chapter 1.0 DATA PREPARATION
#generate data
MercutarIncome = rnorm      (50, 185, 52.5)
MercutarPrice = rnorm       (50, 600, 63.3)
Sea_and_TreeIncome = rnorm  (50, 175, 51.4)
Sea_and_TreePrice = rnorm   (50, 590, 62.2)
Kripp_AltstadtIncome = rnorm(50, 180, 52.3)
Kripp_AltstadtPrice = rnorm (50, 605, 63.2)
Kripp_NeustadtIncome = rnorm(50, 215, 51.8)
Kripp_NeustadtPrice = rnorm (50, 750, 62.5)
NordlethalIncome = rnorm    (50, 170, 53.9)
NordlethalPrice = rnorm     (50, 535, 61.5)

#-------------------------------------------------------------------------------
#Chapter 1.1
#generate data frame
House = c(rep("Mercutar", 50),rep("Sea and Tree", 50),rep("Kripp Altstadt", 50),
          rep("Kripp Neustadt", 50),rep("Nordlethal", 50))
Income = c(MercutarIncome, Sea_and_TreeIncome, 
         Kripp_AltstadtIncome, Kripp_NeustadtIncome,
         NordlethalIncome)
Price = c(MercutarPrice, Sea_and_TreePrice, Kripp_AltstadtPrice, 
          Kripp_NeustadtPrice, NordlethalPrice)
Sheet1 = tibble(House, Income, Price)
#View(Sheet1)

#-------------------------------------------------------------------------------
#Chapter 2.0 DATA VISUALIZATION
#visualize raw data "Figure1"
Figure1 = ggplot(Sheet1, aes(x = Income, y = Price, color = House))+geom_point()+
  labs( x = "Income per house (kEUR)", y = "House price (kEUR)",
        title = "Figure 1: general data visualization")+
  geom_smooth(method = "lm", se = FALSE)+geom_vline(xintercept = mean(Sheet1$Income))+
  geom_hline(yintercept = mean(Sheet1$Price))
Figure1

#-------------------------------------------------------------------------------
#Chapter 2.1
#interpret data
#how expensive and wealthy each of them should be
#generate mean of each house's data
gi = mean(Income)
imc = mean(MercutarIncome)
ic3 = mean(Sea_and_TreeIncome)
ika = mean(Kripp_AltstadtIncome)
ikn = mean(Kripp_NeustadtIncome)
inm = mean(NordlethalIncome)
gp = mean(Price)
pmc = mean(MercutarPrice)
pc3 = mean(Sea_and_TreePrice)
pka = mean(Kripp_AltstadtPrice)
pkn = mean(Kripp_NeustadtPrice)
pnm = mean(NordlethalPrice)

#generate another data frame
House_mean = c(rep("Mercutar", 1),rep("Sea and Tree", 1),rep("Kripp Altstadt", 1),
          rep("Kripp Neustadt", 1),rep("Nordlethal", 1))
Income_mean = c(imc, ic3, ika, ikn, inm)
Price_mean = c(pmc, pc3, pka, pkn, pnm)
Sheet2 = tibble(House_mean, Income_mean, Price_mean)
#View(Sheet2)

#-------------------------------------------------------------------------------
#Chapter 2.2 "Figure 2"
#visualize general interpreted data
Figure2 =ggplot(Sheet2, aes(x = Income_mean, y = Price_mean, color = House_mean))+geom_point()+
  labs( x = "Average income per house (kEUR)", y = "Average house price (kEUR)",
        title = "Figure 2: house average")+
  geom_vline(xintercept = mean(Sheet1$Income))+
  geom_hline(yintercept = mean(Sheet1$Price))
Figure2

#-------------------------------------------------------------------------------
#Chapter 2.3 "Figure 3"
#visualize interpreted data specifically focus on income comparison
Figure3 =ggplot(Sheet2, aes(x = House_mean, y = Income_mean, fill= House_mean))+
  stat_summary(fun.y = mean, geom = "bar")+
  ylim(0,250)+labs(x="Houses", y="Average income (kEUR)",
                   title="Figure3: average income comparison")+
  geom_hline(yintercept = mean(Sheet1$Income))
Figure3

#-------------------------------------------------------------------------------
#Chapter 2.4 "Figure 4"
#visualize interpreted data specifically focus on housing price comparison
Figure4 =ggplot(Sheet2, aes(x = House_mean, y = Price_mean, fill= House_mean))+
  stat_summary(fun.y = mean, geom = "bar")+
  ylim(0,850)+labs(x="Houses", y="Average housing price",
                   title="Figure4: average housing price comparison")+
  geom_hline(yintercept = mean(Sheet1$Price))
Figure4

#-------------------------------------------------------------------------------
#Chapter 3.0
#making assumption
#assuming that the more income in that area, the more expensive the houses are
#this could be made numerically by (Income1/Price1)=(Income2/Price2)
#if the data follows the assumption, the test should return a correlation of high p-values

t.test(Income/Price, MercutarIncome/MercutarPrice, mu = 0.025)

t.test(Income/Price, Sea_and_TreeIncome/Sea_and_TreePrice, mu = 0.025)

t.test(Income/Price, Kripp_AltstadtIncome/Kripp_AltstadtPrice, mu = 0.025)

t.test(Income/Price, Kripp_NeustadtIncome/Kripp_NeustadtPrice, mu = 0.025)

t.test(Income/Price, NordlethalIncome/NordlethalPrice, mu = 0.025)

#-------------------------------------------------------------------------------
#Chapter 3.1.1
#visualize correlation 
#based on the previous assumption, the mean of each house should be close enough to the mean of all houses
#the threshold was same as previously set to 0.025 away from the mean of all houses

House_cncl = c(rep("General",1),rep("Mercutar", 1),rep("Sea and Tree", 1),rep("Kripp Altstadt", 1),
               rep("Kripp Neustadt", 1),rep("Nordlethal", 1))
Correlation_cncl = c(mean(Income/Price)/mean(Income/Price), mean(MercutarIncome/MercutarPrice)/mean(Income/Price), mean(Sea_and_TreeIncome/Sea_and_TreePrice)/mean(Income/Price),
                mean(Kripp_AltstadtIncome/Kripp_AltstadtPrice)/mean(Income/Price), mean(Kripp_NeustadtIncome/Kripp_NeustadtPrice)/mean(Income/Price),
                mean(NordlethalIncome/NordlethalPrice)/mean(Income/Price))
Sheet3 = tibble(House_cncl, Correlation_cncl)
View(Sheet3)

Figure5 =ggplot(Sheet3, aes(x = House_cncl, y = Correlation_cncl, fill= House_cncl))+
  stat_summary(fun.y = mean, geom = "bar")+labs(x="Houses", y="Income per price compare to general mean",
                   title="Figure5: trend comparison")
Figure5

#-------------------------------------------------------------------------------
#Chapter 3.1.2
#visualize correlation 
#this method should theoretically display p-value but unfortunately, due to multiple 
#tested being made, the letters were all in one place making it impossible to see
#I tried the "label.sep" to separate the displayed p-values but unfortunately it didn't work

House_cn = c(rep("General",250),rep("Mercutar", 50),rep("Sea and Tree", 50),rep("Kripp Altstadt", 50),
               rep("Kripp Neustadt", 50),rep("Nordlethal", 50))
Correlation_cn = c(Income/Price, MercutarIncome/MercutarPrice, Sea_and_TreeIncome/Sea_and_TreePrice,
                     Kripp_AltstadtIncome/Kripp_AltstadtPrice, Kripp_NeustadtIncome/Kripp_NeustadtPrice,
                     NordlethalIncome/NordlethalPrice)
Sheet4 = data.frame(House_cn, Correlation_cn)
View(Sheet4)

Figure6 =ggboxplot(Sheet4,"House_cn","Correlation_cn", fill = "House_cn", add="jitter",
         palette = "jco")+stat_compare_means(label.x = 3)+
  labs(x="Houses", y="Correlarion coefficient",title="Figure6: correlation test")
Figure6

#-------------------------------------------------------------------------------
#Chapter 3.1.3
#visualize correlation 
#this method is from the internet and filled with my data, so I did not set the assumption myself
#however, since the Mercutar to General shows the highest p-value, I assume that the assumptions was
#"there is no correlation between income and housing price at all" since there was no tolerance (mu) set

Figure7 = ggscatter(Sheet1, x = "Income", y = "Price", color = "House", palette = "jco",
                    conf.int = TRUE)+stat_cor(aes(color = House), label.x =3)+
  labs( x = "Income per house (kEUR)", y = "House price (kEUR)",
        title = "Figure 7: general data visualization V2")

Figure7

#-------------------------------------------------------------------------------
#Summary
#Question 1: Which neighborhoods have the highest average income?
#Question 2: Is there a correlation between income and housing prices?
#Question 3: Are there any neighborhoods where housing prices are significantly lower than the 
#city average?

#Answer 1: from Figure 2 & 3, Kripp Neustadt has the highest average income
#Answer 2: from Figure 1, linearly modeled regression lines of each house's data
#majorly suggest a correlation between income and housing price,
#the richer, the more luxurious house. Mercutar is an exception where despite the,
#difference in income range, housing price remain nearly unchanged, one evidence being in Figure 7 where 
#it was the only house with extremely high p-value supporting the assumption of the un-related data
#altogether, most data supports the assumption and the scale of Mercatur's difference over others such as Nordlethal
#is minuscule according to Figure 5
#Answer 3: from Figure 2 & 4, Nordlethal has the lowest average housing price, marginally lower than the city average
#_______________________________________________________________________________