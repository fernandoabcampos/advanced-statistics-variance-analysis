library(knitr) # kable
library(dplyr) # mutate / summarize / group_by
library(plyr) # ddploy
library(ggplot2) # ggplot
library(scales) #percent


df <- read.csv("data/Fumadores.csv")
head(df)

print(unlist(lapply(df, function(x) any(is.na(x)))))
res <- sapply(df, class)kable(data.frame(variables=names(res),clase=as.vector(res)))

summary(df) # se nota (Other) en Tipo, deberíamos tener solo 6 categorías

unique(df$Tipo) # Claramente el problema ha sido detectado - 2 letras minusculas
df <- as.data.frame(sapply(df, toupper))
# Aunque va enseñar solo 2 decimales, R mantiene el número como él era inicialmente
df$AE <- as.numeric(as.character(df$AE))

kable(summary(df)[,1], # 1  - posicion AE
      digits=2, align='l', caption="Estadística descriptiva de la variable AE", col.names = "AE")


category <-  as.data.frame(table(df$Tipo))
category
bp <- ggplot(df, aes(x="", y=category, fill=names(category)))
bp + coord_polar(null, start=0)




names(category)
ggplot(category, aes(x = factor(1), y=Freq,fill=factor(Var1)) ) + geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") +
geom_text(aes(y = Freq/3 + c(0, cumsum(Freq)[-length(Freq)]), 
              label = percent(Freq/100)), size=5)


df <- as.data.frame(table(mpg$class))


library(googleVis)
op <- options(gvis.plot.tag = "chart")
pie <- gvisPieChart(category, options = list(title = "Distribución por Tipo de Fumador", 
                                                 width = 1000, height = 500))
plot(pie)















r2 <- setNames(aggregate(df[, c("AE")], list(df$Tipo), mean), c("Tipo", "Media"))
r2 <- r2[order(r2$Media),]
r2

ggplot(r2, aes(x = reorder(Tipo, Media), y = Media), fill = Tipo) + 
  geom_bar(stat = "identity")
  #geom_point(aes(color = Tipo)) + xlab("Tipo de Fumadores") + 
  ggtitle("Tipo de fumadores")

bp <- ggplot(r2, aes(x= reorder(Tipo, Media), y=Media, fill=Tipo)) + geom_bar(stat="identity")

bp + ggtitle("Tipo de fumadores")
  
  
ggplot(r2, aes(x=reorder(categorias, -n), y=n)) +
  geom_bar(stat="identity")

df %>%
  group_by(Tipo) %>%
  summarize(mean_size = mean(AE, na.rm = TRUE))


r2 <- ddply(df, .(Tipo), summarize, 'Média'=mean(AE))
r2 <- r2[order(r2$Média),]
r2


r2 %>%
  group_by(Tipo) %>%
  mean(AE) %>%
  ggplot(., aes(x=reorder(Tipo, -n), y=n)) +
  geom_bar(stat="identity")





#########
n <- length(df$AE)
m <- mean(df$AE)

conf.level <- 0.95
z <- qt((1+conf.level)/2, df=n-1)
se <- sd(df$AE)/sqrt(n)
ci <- z * se

range_min <- m - ci
range_max <- m + ci

print(paste("Intervalo de confianza: ", range_min, " hasta ", range_max))

# Confirmando el valor
confirm <- t.test(df$AE, mu=5.0, conf.level = conf.level)
confirm$conf.int

###

colnames(df)
no_fumadores <- df[(df$Tipo =="NF" | df$Tipo == "FI"),] # NF - No Fumadores / FP - Fumadores pasivos
fumadores <- df[(df$Tipo !="NF" & df$Tipo != "FI"),] 
head(no_fumadores)
tail(no_fumadores)
head(fumadores)
tail(fumadores)
sum(nrow(no_fumadores), nrow(fumadores))
dif<- no_fumadores$AE - fumadores$AE
r2








z.test <- function( x, mu, pop.sd=NULL, cl=0.95 ){    #z test
  n<-length(x)
  mean.x <- mean(x)
  sd<-pop.sd
  if (is.null(pop.sd)) sd<-sd( x )  #estimamos la desv de la población a partir de la muestra.
  z.obs<- (mean.x - mu) / (sd/sqrt(n))   #z follows a normal distribution N(0,1)
  
  alfa <- 1-cl
  z.critical.one.tail <- qnorm( alfa, lower.tail=FALSE )
  z.critical.two.tail <- qnorm( alfa/2, lower.tail=FALSE  )
  one.tail.p_value <- pnorm( abs(z.obs), lower.tail=FALSE )
  two.tail.p_value <- one.tail.p_value * 2
  cat ("sample mean=", mean.x, "   sd=", sd, "  sample length=", n, "\n",
       "z obs= ", z.obs, "\n",
       "z critical one-tailed: ", z.critical.one.tail, "\n",
       "z critical two-tailed: ", z.critical.two.tail, "\n",
       "one-tailed probability", one.tail.p_value, "\n",
       "two-tailed probability", two.tail.p_value)
  
  #Guardamos los datos en una lista
  mylist <-list( mean=mean.x, sd=sd, n=n, zobs=z.obs, 
                 one.tailed= c(z.critical.one.tail, one.tail.p_value),
                 two.tailed=c(z.critical.two.tail, two.tail.p_value ))
  return (mylist)
}
