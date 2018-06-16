sum(result)
anova$df.residual

varNF <- sum ((NF - mean(NF)) ^ 2 )
varFP <- sum ((FP - mean(FP)) ^ 2 )
varNI <- sum ((NI - mean(NI)) ^ 2 )
varFL <- sum ((FL - mean(FL)) ^ 2 )
varFM <- sum ((FM - mean(FM)) ^ 2 )
varFI <- sum ((FI - mean(FI)) ^ 2 )

#SSW (Within Sum of Squares): varianza intra grupo
SSW <- varNF + varFP + varNI + varFL + varFM + varFI
SSW

#SST (Total Sum of Squares): varianza total
SST <- sum( (df$AE - mean(df$AE)) ^ 2 )
SST


# SSB (Between Sum of Squares): Varianza entre grupos
SSB <- SST - SSW
SSB

x <- data.frame("SSW" = c(format(round(SSW.aov, 2), nsmall = 2), format(round(SSW, 2), nsmall = 2))
              , "SSB" = c(format(round(SSB.aov, 2), nsmall = 2), format(round(SSB, 2), nsmall = 2))
              , "SST" = c(format(round(SST.aov, 2), nsmall = 2), format(round(SST, 2), nsmall = 2)))

kable(data.frame("SSW" = c(format(round(SSW.aov, 2), nsmall = 2), format(round(SSW, 2), nsmall = 2))
                 , "SSB" = c(format(round(SSB.aov, 2), nsmall = 2), format(round(SSB, 2), nsmall = 2))
                 , "SST" = c(format(round(SST.aov, 2), nsmall = 2), format(round(SST, 2), nsmall = 2))))

as.character()
god <- c(as.character(SSW.aov), as.character(SSB.aov), as.character(SST.aov))
calculado <- c(as.character(SSW), as.character(SSB), as.character(SST))

SSW_both <- c(SSW.aov, SSW)
SSB_both <- c(SSB.aov, SSB)
SST_both <- c(SST.aov, SST)

df.gen <- as.data.frame(god, calculado, stringsAsFactors=FALSE)
colnames(df.gen) <- c("regular", "calculado")


summary(anova)[[1]]$'Df'

mylm <- lm(wt~mpg, data = mtcars)
myanova <- anova(mylm)

grados_libertad <- summary(anova)[[1]]$'Df'
grados_libertad[1], grados_libertad[2]



myanova
myanova[1,1]
myanova[2,1]

alpha = .05
qf(0.95, 1, 30)

sumSq <- summary(anova)[[1]]$'Sum Sq'

#Manera 1
print(paste("SSW (Within Sum of Squares):",sumSq[2]))
#Manera 2
SSW.aov <- sum( anova$residuals ^ 2 )
SSW.aov

SSB.aov <- sumSq[1]
print(paste("SSB (Between Sum of Squares):", SSB.aov))

SST.aov <- sum(sumSq)
SST.aov

grados_libertad <- summary(anova)[[1]]$'Df'
grados_libertad



library(lsr)
etaS <- etaSquared(anova)
etaS[1]
summary.lm(anova)

k <- 6  #grupos
N <- length( df$AE )
N
F <- (SSB.aov / (k-1)) / (SSW.aov/ (N-k))
F

library(agricolae)
attributes(anova)
scheffe.test(anova,"df$Tipo", group=FALSE,console=TRUE, main="Capacidad Pulmonar de Fumadores y no fumadores")

plot(anova, 1)


######

df2 <- read.csv("data/Fumadores2.csv")
head(df2)

# Buscando por valores NA
print(unlist(lapply(df2, function(x) any(is.na(x)))))

# Reunindo los tipos de variables e imprimindo
res2 <- sapply(df2, class)
kable(data.frame(variables=names(res2),clase=as.vector(res2)))

summary(df2)

unique(df2$Tipo) 
unique(df2$Sex) 
df <- as.data.frame(sapply(df, toupper))
# Aunque va enseñar solo 2 decimales, R mantiene el número como él era inicialmente
df$AE <- as.numeric(as.character(df$AE))


library(dplyr) # mutate / summarize / group_by
attach(df2)
fum2 <- df2 %>%
  #group_by(df2$Tipo, df2$Sex) %>%
  group_by(df2$Tipo) %>%
  summarise(mean_size = mean(df2$AE, na.rm = TRUE))

fum2

attach(df2)
fum2 <- df2 %>%
  select(AE, Tipo, Sex) %>%
  group_by(Tipo, Sex) %>%
  summarise(AE = mean(AE))

fum2
