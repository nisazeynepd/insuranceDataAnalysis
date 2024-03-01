## ADIM 4

library(readxl)
insurance <- read_excel("C:/verianaliziR/insurance_data.xlsx", 
                        col_types = c("numeric", "text", "numeric", 
                                      "numeric", "text", "text", "numeric"))
View(insurance)

### kategorik deðiþkenleri faktör olarak tanýmlayalým:
insurance$smoker <- factor(insurance$smoker, levels=c("yes","no"))
insurance$region <- factor(insurance$region, levels=c("southwest","northwest","northeast","southeast"))
insurance$sex <- as.factor(insurance$sex)
insurance$bmi <- as.factor(insurance$bmi)
insurance$children<- as.factor(insurance$children)

### özet istatistikleri görelim:
summary(insurance)

### verinini ilk 6 satýrýný görelim:
head(insurance)

### eksik gözlem var mý?
rowSums(is.na(insurance))
colSums(is.na(insurance))

#### eksik gözlem yok. biz oluþturup nasýl doldurabiliriz görelim:
data_miss <- insurance
set.seed(123)
aa <- sample(1:nrow(data_miss),floor(nrow(data_miss)*0.02))
data_miss$age[aa] <- NA
colSums(is.na(data_miss))

### eksik gözlemlerimizin olduðu 26 satýrý yeni bir deðiþkene atayalým:
missing_data<- data_miss[!complete.cases(data_miss),]
missing_data

### eksik gözlemlerimizi görselle görelim.
library(mice)
md.pattern(data_miss[,c("age","smoker","bmi","charges","sex","region","children")])


### eksik gözlemin yüzdeliðini görelim:
library(VIM)
library(ISLR)
aggr(data_miss,col=c("navyblue","pink"),numbers=TRUE, sortVars=TRUE, labels=names(data_miss),cex.axis=.7,gap=3,ylab=c("Missing Ratio","Missing Pattern"))

#### eksik gözlem oranýmýzýn yaklaþýk %0.20 olduðunu görüyoruz.

### KNN (K-Nearest Neighbor) ile eksik gözlemleri dolduralým.
library(DMwR2)
data_knn <- data_miss
anyNA(data_knn) 
knn_imp <- knnImputation(data_knn, k=5, meth="median")
anyNA(knn_imp)

#### eksik gözlemimiz kalmadý. Orijinal veriden devam edelim.
set.seed(123456)
train_id<-sample(1:nrow(insurance),size=round(0.8*nrow(insurance)),replace=FALSE)
train<-insurance[train_id,]
test<-insurance[-train_id,]

library(readxl)
train <- read_excel("C:/verianaliziR/train.xlsx", 
                    col_types = c("numeric", "text", "numeric", 
                                  "numeric", "text", "text", "numeric"))
View(train)

#### kategorik deðiþkenlerimizi faktör olarak tanýmlayýp özet istatistiklerini görelim:
train$smoker <- factor(train$smoker, levels=c("yes","no"))
train$region <- factor(train$region, levels=c("southwest","northwest","northeast","southeast"))
train$sex <- as.factor(train$sex)

str(train)
summary(train)

### yaþ deðiþkenini,bmi deðiþkenini ve cocuk sayýsý deðiþkenini kategorize edelim:
train <- as.data.frame(train)
train$yas_kat <- ifelse(train$age <=29, "genc", ifelse(train$age >=30  & train$age <= 50 ,"yetiskin","yaslý"))
train$yas_kat <- factor(train$yas_kat, levels=c("genc","yetiskin","yaslý"), labels=c("genc","yetiskin","yaslý"))
train$bmi_kat<- ifelse(train$bmi < 18.5, "zayýf",
                       ifelse(train$bmi < 25, "normal",
                              ifelse(train$bmi < 30, "fazla kilolu",
                                     ifelse(train$bmi < 35, "obez (tip 1)",
                                            ifelse(train$bmi < 40, "obez (tip 2)",
                                                   "aþýrý obez (morbid obezite)")))))
train$bmi_kat<-factor(train$bmi_kat, levels=c("zayýf","normal","fazla kilolu","obez (tip 1)", "obez (tip 2)","aþýrý obez (morbid obezite)"), labels=c("zayýf","normal","fazla kilolu","obez (tip 1)", "obez(tip 2)","aþýrý obez (morbid obezite)"))

train$cocuk_kat <- ifelse(train$children ==0, "hic yok", ifelse(train$children > 0  & train$children < 4 ,"1-3 arasý","3'ten fazla"))
train$cocuk_kat<-factor(train$cocuk_kat, levels=c("hic yok","1-3 arasý","3'ten fazla"), labels=c("hic yok","1-3 arasý","3'ten fazla"))

### özet istatistiklerini görelim:
str(train)
summary(train)

## Adým 6
### nicel deðiþkenleri inceleyelim.
library(funModeling)
profiling_num(train)
plot_num(train)

### BAR-PLOT
#### sigara içme durumunda cinsiyet daðýlýmý:
library(ggplot2)
ggplot(train,aes(smoker, fill=sex))+
  geom_bar(position=position_dodge())+
  ggtitle("sigara içme durumunda cinsiyet daðýlýmý")+
  xlab("sigara içme durumu")+
  ylab("sýklýklar")+
  scale_fill_discrete(name = "cinsiyet")+
  theme(axis.title.x = element_text(color="black", face="bold", size=12),
        axis.title.y = element_text(color="black", face="bold",size=12),
        plot.title = element_text(hjust = 0.5,color="black", face="bold", size=14),
        legend.title = element_text(colour="black",face="bold",size=12))


### BOX-PLOT
#### yaþ kategorilerine karþýlýk masraflar kutu grafiði:
library(ggplot2)
ggplot(train, aes(x=yas_kat,y=charges,fill=yas_kat))+
  geom_boxplot()+
  stat_summary(fun=median,geom="line",group=1,color="yellow",size=1)


### NOKTA ÖLÇÜLERÝ
##### Charges (masraflar)

#### 3 Nokta Özeti
median(train$charges)
mean(train$charges)
hist(train$charges)


#### 5 Nokta Özeti
quantile(train$charges)
q1 <- as.vector(quantile(train$charges,0.25))
q3 <- as.vector(quantile(train$charges,0.75))
DAG <- q3-q1
DAG

genislik <- max(train$charges)-min(train$charges)
genislik


### Deðiþim Ölçüleri
#### masraflarýn standart sapmasýný görelim:

stdev<-sd(train$charges)
stdev

mean<-mean(train$charges)

degisim_kats_charges <- (stdev/mean)*100
degisim_kats_charges

sd_dk <- function(x) {c(std<-sd(x), dk<-(sd(x)/mean(x))*100)}
tapply(train$charges, train$sex, sd_dk)
quantile(train$charges)

sort <- train[order(train$charges),]
medians <- median(train$charges)
sort$imed <- abs(sort$charges-medians)
sort2 <- sort[order(sort$imed),]
mad <- median(sort2$imed)
mad 

sol <- function(x) {
  c(quantile(x, probs = 1/2) , 
    quantile(x, probs = 1/4),
    quantile(x, probs =1/8 ),
    quantile(x,probs=1/16),
    quantile(x,probs=1/32),
    quantile(x,probs=1/64)
  )
}

sag <- function(x) {
  c(quantile(x, probs = 1/2) , 
    quantile(x, probs = 3/4),
    quantile(x, probs = 7/8),
    quantile(x,probs=15/16),
    quantile(x,probs=31/32),
    quantile(x,probs=63/64)
  )
}

x_a <- sol(train$charges)
x_u <- sag(train$charges)
x_mrg <- as.data.frame(cbind(x_a,x_u))
rownames(x_mrg) <- c("1/2","1/4","1/8","1/16","1/32","1/64")
colnames(x_mrg) <- c("alt_kuyruk","ust_kuyruk")
x_mrg$orta_nokta <- (x_mrg$alt_kuyruk+x_mrg$ust_kuyruk)/2
x_mrg

hist(train$charges)

p <- 0.1
mean(train$charges, trim = p)

n <- nrow(train)
ks <-n-(as.integer(2*p*n)) 
ks

library("psych")
geometric.mean(train$charges)

freq <- as.data.frame(table(train$sex))
names(freq)[1] <- 'cinsiyet'
gini <- function(a,b) {
  a1 <- (a/(a+b))**2
  b1 <- (b/(a+b))**2
  x<-1-(a1 + b1)
  return(x)
}
gn <- gini(freq[1,2],freq[2,2])
k <- 2
gn/((k-1)/k)

entropy <- function(base,a,b) {
  var <-  abs(((a)/(a+b))*log(((a)/(a+b)),base))-(((b)/(a+b))*log(((b)/(a+b)),base))
  return(var)
}
ent <- entropy(10,freq[1,2],freq[2,2])
k <- 2
ent/(log(k,10)) 

crs <- table(train$sex, train$smoker)
addmargins(crs)

ggplot(train, aes(sex))+
  geom_bar(aes(fill=sex))+
  xlab("Cinsiyet")+ylab("Frekanslar")+
  ggtitle("Cinsiyet Daðýlýmý")+
  scale_fill_discrete(name="Cinsiyet")+
  xlab("Cinsiyet Kategorileri")+
  ylab("Sýklýklar")

ggplot(train,aes(yas_kat, fill=sex))+
  geom_bar(position=position_dodge())+
  ggtitle("yaþ kategorilerindeki cinsiyet daðýlýmý")+
  xlab("yaþ kategorileri")+
  ylab("sýklýklar")+
  scale_fill_discrete(name = "cinsiyet")+
  theme(axis.title.x = element_text(color="black", face="bold", size=12),
        axis.title.y = element_text(color="black", face="bold",size=12),
        plot.title = element_text(hjust = 0.5,color="black", face="bold", size=14),
        legend.title = element_text(colour="black",face="bold",size=12))

library(dplyr)
library(ggplot2)
std_pct <- train %>% group_by(yas_kat, sex) %>%
  dplyr::summarise(count=n(), .groups = "drop") %>%
  mutate(pct=round(count/sum(count),2))
std_pct


ggplot(std_pct, aes(yas_kat, pct, fill = sex)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = .5))+
  scale_y_continuous(labels = scales::percent)

library(dplyr)
library(Hmisc)
library(plotly)
cross<-as.data.frame(prop.table(table(train$yas_kat))) 
colnames(cross)[1] <- "yas_kat"
plot_ly(cross, labels = ~yas_kat, values = ~Freq, type = "pie")

ggplot(train, aes(x = charges, fill = sex)) +
  geom_histogram() +
  facet_grid(sex ~ .)

ggplot(train, aes(x = charges)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "white", color = "black") +
  geom_density(alpha = 0.4, fill = "violet")

library(ggpubr)
ggqqplot(train$charges)

library(psych)
library(rcompanion)
charges_tukey<-transformTukey(train$charges,plotit=FALSE)

library(MASS)
Box_charges <- boxcox(train$charges ~ 1,            
                      lambda = seq(-6,6,0.1))     
Cox_charges <- data.frame(Box_charges$x, Box_charges$y) 
Cox_charges <- Cox_charges[order(-Cox_charges$Box_charges.y),]  
Cox_charges[1,] 
lambda <- Cox_charges[1, "Box_charges.x"]
lambda

library(ggplot2)
ggplot(train,aes(yas_kat, fill=smoker))+
  geom_bar(position=position_dodge())+
  ggtitle(" yaþ kategorilerindeki sigara içme daðýlýmý")+
  xlab("yaþ kategorileri")+
  ylab("sýklýklar")+
  scale_fill_discrete(name = "sigara içme")+
  theme(axis.title.x = element_text(color="black", face="bold", size=12),
        axis.title.y = element_text(color="black", face="bold",size=12),
        plot.title = element_text(hjust = 0.5,color="black", face="bold", size=14),
        legend.title = element_text(colour="black",face="bold",size=12))


library(dplyr)
library(ggplot2)
std_pct <- train %>% group_by(yas_kat, smoker) %>%
  dplyr::summarise(count=n(), .groups = "drop") %>%
  mutate(pct=round(count/sum(count),2))
std_pct
ggplot(std_pct, aes(yas_kat, pct, fill = smoker)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = .5))+
  scale_y_continuous(labels = scales::percent)

library(ggplot2)
ggplot(train, aes(x=smoker,y=charges,fill=smoker))+
  geom_boxplot()+
  stat_summary(fun=median,geom="line",group=1,color="yellow",size=1)

library(tidyverse)
ggplot(train, aes(charges,age))+
  geom_point(size=2,shape=21,stroke=1,color="dodgerblue1", fill="white")+
  geom_smooth(method = "lm", col="darkblue",se = FALSE)

ggplot(train, aes(charges,age, color=sex, shape=sex))+
  geom_point(size=3,alpha=0.6)

library(ggExtra)
gr <- ggplot(train,aes(x=charges,y=age))+
  geom_point()+
  geom_text(size=3,label=rownames(train),nudge_x=0.25,
            nudge_y=0.25, check_overlap=T)+
  geom_smooth(method=lm,col="brown1", se=FALSE)

ggMarginal(gr,type="histogram",fill="darksalmon")

library(ggplot2)
ggplot(train, aes(age,charges, color=charges, size=charges))+
  geom_point(alpha=0.5, stroke=2)+
  scale_size(range = c(1, 8))+
  scale_color_gradient(low = "blue", high = "lightpink")

library(hexbin)
ggplot(train,aes(x=age,y=charges))+
  geom_hex(bins=20, color = "white")+
  scale_fill_gradient(low="mistyrose2", high="violetred3")

ggplot(train, aes(x=age, y=charges) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+
  scale_fill_distiller(palette = "Blues")

pl <- function(x, y, pch=20, col=1, ...) {
  points(x=x, y=y, col=col, pch=pch, type="p", ...)
  lines(lowess(x=x, y=y), col="blue")
}
coplot(charges~age|smoker, col="black", data=train,panel=pl) 
coplot(charges~age|bmi, col="black", data=train, panel=pl, rows=1)
coplot(charges~age|yas_kat*sex, data=train, col="black",panel=pl)

cor_train <- train[,c(1,7)]
library(GGally)
cor(cor_train)#Korelasyon degerleri
plot(cor_train)
ggpairs(cor_train,diag = list(continuous = "density"))

library(dplyr)
a <- train %>%group_by(region) %>%
  summarize(Q1=quantile (charges, probs=0.25), Median=quantile (charges, probs=0.50), Q3=quantile(charges, probs=0.75), DAG=Q3-Q1)
a

plot(a$region,a$Median, xlab="bölge kategori", ylab="Ortanca", main="Ortanca izi cizimi")

ggplot(a, aes(x=Median,y=DAG, color=region, group=1))+
  geom_point(size=4,alpha=0.6)+
  geom_line(color="black")

etk_train <- train%>%
  group_by(region,yas_kat)%>% 
  summarise(Median=median(age))
etk_train

ggplot(etk_train, aes(x =yas_kat, y = Median,color=region,group=region)) +
  geom_line() +
  geom_point()

library(funModeling)
plot_num(train)

library(ggplot2)
library(ggmosaic)
ggplot(train) +
  geom_mosaic(aes(x = product(sex, region), fill=sex)) +
  labs(x = "bölge ", title='f(yas Kategorileri, bölge| Cinsiyet)') + 
  facet_grid(yas_kat~.)

dt <- table(train$sex, train$smoker)
round(100*prop.table(dt,2), 2) 

library(DescTools)      
Assocs(dt)[1:3,1]

library("gplots")
balloonplot(t(dt), main ="cinsiyet ve sigara içme durumu ", xlab ="", ylab="",
            label = FALSE,show.margins = FALSE)

odds_ratio <- OddsRatio(dt, conf.level=0.95)
odds_ratio

dt2 <- xtabs(~ region+sex+smoker+yas_kat, data=train)
dt22 <- as.data.frame(ftable(dt2))
dt22

library(ggpubr)
ggballoonplot(
  dt22, x = "region", y = "sex",
  size = "Freq", fill = "Freq",
  facet.by = c("smoker","sex"),
  ggtheme = theme_bw())

library(dplyr)
library(Hmisc)
library(plotly)
cross<-as.data.frame(prop.table(table(train$bmi_kat))) 
colnames(cross)[1] <- "bmi_kat"
plot_ly(cross, labels = ~bmi_kat, values = ~Freq, type = "pie")

library(dplyr)
library(Hmisc)
library(plotly)
cross<-as.data.frame(prop.table(table(train$cocuk_kat))) 
colnames(cross)[1] <- "cocuk_kat"
plot_ly(cross, labels = ~cocuk_kat, values = ~Freq, type = "pie")

dt_c <- table(train$yas_kat,train$region)
dtc_exp <- chisq.test(dt_c)$expected
rowcs <- function(i, obs, exp) {
  sum(((obs[i,] - exp[i,])^2)/exp[i,])
}

chi_dtc <- as.matrix(lapply(seq_len(nrow(dt_c)), rowcs, obs = dt_c, exp = dtc_exp))
rownames(chi_dtc) <- rownames(dt_c)
chi_dtc

library(inspectdf)
library(dplyr)
train %>% inspect_types()
std_cat <- train %>% inspect_cat()
std_cat$levels$region

std_cat %>% show_plot()

charges<- train$charges
train$kareseld_charges<-(charges)^2
hist(train$kareseld_charges)

charges<- train$charges
train$log_charges<-log(charges)
hist(train$log_charges)

library(readxl)
test <- read_excel("C:/verianaliziR/test.xlsx", 
                   col_types = c("numeric", "text", "numeric", 
                                 "numeric", "text", "text", "numeric"))
View(test)

test$smoker <- factor(test$smoker, levels=c("yes","no"))
test$region <- factor(test$region, levels=c("southwest","northwest","northeast","southeast"))
test$sex <- as.factor(test$sex)

summary(test)

test <- as.data.frame(test)
test$yas_kat <- ifelse(test$age <=29, "genc", ifelse(test$age >=30  & test$age <= 50 ,"yetiskin","yaslý"))
test$yas_kat <- factor(test$yas_kat, levels=c("genc","yetiskin","yaslý"), labels=c("genc","yetiskin","yaslý"))
test$bmi_kat<- ifelse(test$bmi < 18.5, "zayýf",
                      ifelse(test$bmi < 25, "normal",
                             ifelse(test$bmi < 30, "fazla kilolu",
                                    ifelse(test$bmi < 35, "obez (tip 1)",
                                           ifelse(test$bmi < 40, "obez (tip 2)",
                                                  "aþýrý obez (morbid obezite)")))))
test$bmi_kat<-factor(test$bmi_kat, levels=c("zayýf","normal","fazla kilolu","obez (tip 1)", "obez (tip 2)","aþýrý obez (morbid obezite)"), labels=c("zayýf","normal","fazla kilolu","obez (tip 1)", "obez(tip 2)","aþýrý obez (morbid obezite)"))

test$cocuk_kat <- ifelse(test$children ==0, "hic yok", ifelse(test$children > 0  & test$children < 4 ,"1-3 arasý","3'ten fazla"))
test$cocuk_kat<-factor(test$cocuk_kat, levels=c("hic yok","1-3 arasý","3'ten fazla"), labels=c("hic yok","1-3 arasý","3'ten fazla"))

summary(test)

charges<- test$charges
test$log_charges<-log(charges)
hist(test$log_charges)

fit1<-lm(charges ~ age+ sex+smoker+region+bmi+children, data=train)
summary(fit1)

fit1_1<-lm(log_charges ~ age+sex+smoker+bmi+children, data=train)
summary(fit1_1)







