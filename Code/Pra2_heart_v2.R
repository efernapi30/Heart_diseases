# Carreguem les llibreries necessàries
if (!require('ggplot2'))install.packages('ggplot2'); library('ggplot2')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('plotly')) install.packages('plotly'); library('plotly')
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('corrplot')) install.packages('corrplot'); library('corrplot')
if (!require('Matrix')) install.packages('Matrix'); library('Matrix')
if(!require('reshape2')) install.packages('reshape2');library(reshape2)
if (!require('rpart')) install.packages('rpart'); library('rpart')
if (!require('rpart.plot')) install.packages('rpart.plot'); library('rpart.plot')
if (!require('ggplot'))install.packages('ggplot'); library('ggplot')
if (!require('gmodels'))install.packages('gmodels'); library('gmodels')
if (!require('vcd'))install.packages('vcd'); library('vcd')


#--------------------------------------------
# EXERCICI 2

# Carreguem el .csv heart
library(readr)
heart <- read.csv("heart.csv")
heart <- data.frame(heart)
heart$index <- 1:nrow(heart)
head(heart)

# Carreguem el .csv o2Saturation
o2 <- read.csv("o2Saturation.csv")
o2 <- data.frame(o2)
colnames(o2) <- c("O2_Saturation")
o2$index <- 1:nrow(o2)
head(o2)

# Unim els dos .csv en un mateix
all_data <- merge(heart, o2, by='index', all=TRUE)
all_data<- data.frame(all_data)
dim(all_data)
head(all_data)
tail(all_data)
summary(all_data)
str(all_data)



# Creem una funció per visualitzar alhora totes les columnes
plot_data <- function(data){
  # Creació d'una llista de gràfics
  plots <- list()
  
  names <- colnames(data[2:16])
  
  # Passem per totes les variables i guardem la representació 
  # en la llista creada anteriorment
  for (i in 1:length(names)){
    
    fig <- plot_ly(x = data[,i+1],
                   type = "histogram",
                   histnorm = "count",
                   name = names[i])
    
    plots[[i]] <- fig
  }
  
  # Representem els gràfics en 4 files 
  representacio <- subplot(plots, nrows = 4)
  representacio
}
plot_data(all_data)


#-----------------------------------------------------------------
# EXERCICI 3.1
all_data<- na.omit(all_data)
dim(all_data)
colSums(all_data=="")
colSums(is.na(all_data))

# EXERCICI 3.2
# Boxplot per a les variables numèriques
analisis_outliers <- function(variable, name){
  
  # Creació del gràfic
  fig <- plot_ly(type = 'box')
  
  # Representació de la variable
  fig <- fig %>% add_boxplot(y = variable,
                             jitter = 0.3, 
                             pointpos = -1.8, 
                             boxpoints = 'all',
                             marker = list(color = 'rgb(23,32,42)'),
                             line = list(color = 'rgb(23,32,42)'),
                             name = name)
  
  fig <- fig %>% layout(title = paste("Anàlisi d'outliers de la variable", name))
  
  # Obtenció dels possibles outliers
  outliers <- boxplot.stats(variable)$out
  
  return(list(outliers=outliers, fig=fig))
}
analisis = analisis_outliers(all_data$age,"Age")
analisis$fig

analisis = analisis_outliers(all_data$trtbps,"Resting blood pressure")
analisis$fig
analisis$outliers

analisis = analisis_outliers(all_data$chol,"Cholesterol")
analisis$fig
analisis$outliers

analisis = analisis_outliers(all_data$thalachh,"Maximum heart rate")
analisis$fig

analisis = analisis_outliers(all_data$oldpeak,"ST depression induced by exercise relative to rest")
analisis$fig
analisis$outliers

# Diagrames de barres per a les variables categòriques
library(ggplot2)

ggplot(all_data,
       aes(x=factor(sex)))+
  geom_bar()+
  labs(title="Representació de la variable sex",
       x="Sexe", y="Freqüència")+
  geom_text(aes(label=..count..),stat = 'count',
            position = position_dodge(0.9),
            vjust=-0.5,
            size=5.0)


ggplot(all_data,
       aes(x=factor(cp)))+
  geom_bar()+
  labs(title="Representació de la variable cp",
       x="Chest Pain Type", y="Freqüència")+
  geom_text(aes(label=after_stat(count)),stat = 'count',
            position = position_dodge(0.9),
            vjust=-0.5,
            size=5.0)

ggplot(all_data,
       aes(x=factor(fbs)))+
  geom_bar()+
  labs(title="Representació de la variable fbs",
       x="Fasting blood sugar", y="Freqüència")+
  geom_text(aes(label=after_stat(count)),stat = 'count',
            position = position_dodge(0.9),
            vjust=-0.5,
            size=5.0)

ggplot(all_data,
       aes(x=factor(restecg)))+
  geom_bar()+
  labs(title="Representació de la variable restecg",
       x="Resting electrocardiographic result", y="Freqüència")+
  geom_text(aes(label=after_stat(count)),stat = 'count',
            position = position_dodge(0.9),
            vjust=-0.5,
            size=5.0)

ggplot(all_data,
       aes(x=factor(exng)))+
  geom_bar()+
  labs(title="Representació de la variable exng",
       x="Exercise induced angina", y="Freqüència")+
  geom_text(aes(label=after_stat(count)),stat = 'count',
            position = position_dodge(0.9),
            vjust=-0.5,
            size=5.0)

ggplot(all_data,
       aes(x=factor(slp)))+
  geom_bar()+
  labs(title="Representació de la variable slp",
       x="Slope of the peak exercise ST segment", y="Freqüència")+
  geom_text(aes(label=after_stat(count)),stat = 'count',
            position = position_dodge(0.9),
            vjust=-0.5,
            size=5.0)

ggplot(all_data,
       aes(x=factor(caa)))+
  geom_bar()+
  labs(title="Representació de la variable caa",
       x="Number of major vessels", y="Freqüència")+
  geom_text(aes(label=after_stat(count)),stat = 'count',
            position = position_dodge(0.9),
            vjust=-0.5,
            size=5.0)

ggplot(all_data,
       aes(x=factor(thall)))+
  geom_bar()+
  labs(title="Representació de la variable thall",
       x="Thalassemia", y="Freqüència")+
  geom_text(aes(label=after_stat(count)),stat = 'count',
            position = position_dodge(0.9),
            vjust=-0.5,
            size=5.0)

ggplot(all_data,
       aes(x=factor(output)))+
  geom_bar()+
  labs(title="Representació de la variable output",
       x="Heart disease", y="Freqüència")+
  geom_text(aes(label=after_stat(count)),stat = 'count',
            position = position_dodge(0.9),
            vjust=-0.5,
            size=5.0)

#---------------------------------------------------
# EXERCICI 4

# OUTPUT vs. SEX
tabla1 <- table(all_data$sex, all_data$output, dnn = c("Sex", "Output"))
tabla1
prop.table(table(all_data$sex, all_data$output))

CrossTable(all_data$sex, all_data$output, prop.chisq = FALSE)

x1 <- all_data$output[all_data$sex == 0]
x2 <- all_data$output[all_data$sex == 1]
var.test(x1, x2)

barplot(table(all_data$sex, all_data$output),
        beside = T, 
        col = c("yellow", "lightblue"),
        names = c("No", "Yes"), 
        legend.text = c("Female", "Male"))

fisher.test(x = tabla1, alternative = "two.sided")
assocstats(x = tabla1)

# OUTPUT vs. DIABETES
tabla2 <- table(all_data$fbs, all_data$output, dnn = c("Diabetis", "Output"))
tabla2
prop.table(table(all_data$fbs, all_data$output))

CrossTable(all_data$fbs, all_data$output, prop.chisq = FALSE)

x1 <- all_data$output[all_data$fbs == 0]
x2 <- all_data$output[all_data$fbs == 1]
var.test(x1, x2)

barplot(table(all_data$fbs, all_data$output),
        beside = T, 
        col = c("yellow", "lightblue"),
        names = c("No", "Yes"), 
        legend.text = c("No diabetis", "Diabetis"))

fisher.test(x = tabla2, alternative = "two.sided")
assocstats(x = tabla2)

# REGRESSIÓ LINEAL

round(cor(all_data), 3)
plot(all_data)

mod <- lm(output ~ age + sex + cp + trtbps + chol + fbs + restecg + 
            thalachh + exng + oldpeak + slp + caa + thall + 
            O2_Saturation,
          data = all_data)
summary(mod)

# Treiem del model les variables que no són significatives: 
# age, trtbps, chol, fbs, restecg, thalachh
mod2 <- lm(output ~ sex + cp + exng + oldpeak + slp + caa + thall, data = all_data)
summary(mod2)

# Anàlisi dels residus
## Gràfic de residus enfront de valors estimats
plot(mod2$residuals  ~ mod2$fitted.values)

#Estandaritzem els residus
lm.stdres <- rstandard(mod2)
#Calculem la probabilitat normal i ho comparem amb els valors dels residus estandaritzats
qqnorm(lm.stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Normal Q-Q") 
qqline(lm.stdres, col = "red")

# Creació del .csv final
write.csv(all_data, "data_heart_final.csv")