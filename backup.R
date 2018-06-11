library(knitr) # kable
library(dplyr) # mutate / summarize / group_by
library(plyr) # ddploy
library(ggplot2) # ggplot


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



?summarise
?summarize
df %>% 
  group_by(Tipo) %>% 
  summarise(...)

r2 <- setNames(aggregate(df[, c("AE")], list(df$Tipo), mean), c("Tipo", "Media"))
r2 <- r2[order(r2$Media),]
r2


ggplot(., aes(x=reorder(categorias, -n), y=n)) +
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