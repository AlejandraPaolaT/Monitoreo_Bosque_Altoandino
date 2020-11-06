library(readxl)
library(dplyr)
library(FactoMineR)
library(reshape)
library(ggthemes)
library(tidyverse)
library(reshape)
library(ggthemes)
library(nortest)
library(car)
library(BiodiversityR)
library(rafalib)
library(MASS)
library(pgirmess)
library(RVAideMemoire)
library(multcompView)
library(rcompanion)
library(ordinal)
library(PMCMR)
library(welchADF)
library(Rcmdr)
library(readxl)
library(rapportools)
library(ggplot2)

#Cedrela montana 

datos_cedro<-read.table("clipboard",header=TRUE)
summary(datos_cedro)

attach(datos_cedro)

lillie.test(Incremento_alt)
fligner.test(Incremento_alt~Cobertura,datos_cedro)
leveneTest(Incremento_alt, Cobertura)

#Prueba de Welch para diferencias entre tratamientos

resultado_alt<-welchADF.test(Incremento_alt~Cobertura, data=datos_cedro, 
                             contrast="omnibus", effect = "Cobertura",
                             trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_alt)

resultado_alt<-welchADF.test(Incremento_alt~Cobertura, data=datos_cedro, 
                             contrast="all.pairwise", effect = "Cobertura",
                             trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_alt)


lillie.test(Incremento_db)
fligner.test(Incremento_db~Cobertura,datos_cedro)

resultado_db<-welchADF.test(Incremento_db~Cobertura, data=datos_cedro, 
                            contrast="omnibus", effect = "Cobertura",
                            trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)
summary(resultado_db)

resultado_db<-welchADF.test(Incremento_db~Cobertura, data=datos_cedro, 
                            contrast="all.pairwise", effect = "Cobertura",
                            trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)
summary(resultado_db)


lillie.test(Incremento_dc)
levene.test(Incremento_dc,Tratamiento)
fligner.test(Incremento_dc~Cobertura,datos_cedro)
kruskal.test(Incremento_dc~Cobertura)
boxplot(Incremento_dc~Cobertura)

kruskal.test(Incremento_alt~Cobertura) 
kruskal.test(Incremento_db~Cobertura)
boxplot(Incremento_db~Cobertura)
kruskal.test(Incremento_dc~Cobertura)
boxplot(Incremento_dc~Cobertura)

#Prueba postHoc

pairwise.t.test(Incremento_alt,Cobertura,p.adj="bonf") 
pairwise.t.test(Incremento_db,Cobertura,p.adj="bonf")
pairwise.t.test(Incremento_dc,TCobertura,p.adj="bonf")


#Escallonia paniculata

datos_escallonia<-read.table("clipboard",header=TRUE)
attach(datos_escallonia)

lillie.test(Incremento_alt)
fligner.test(Incremento_alt~Cobertura,datos_escallonia)
kruskal.test(Incremento_alt~Cobertura)
pairwise.t.test(Incremento_alt,Cobertura,p.adj="bonf") 

lillie.test(Incremento_db)
fligner.test(Incremento_db~Cobertura,datos_escallonia)
kruskal.test(Incremento_db~cobertura)

lillie.test(Incremento_dc)
fligner.test(Incremento_dc~Cobertura,datos_escallonia)
kruskal.test(Incremento_dc~Cobertura)
hist(Incremento_dc)

kruskal.test(Incremento_alt~Cobertura)
kruskal.test(Incremento_db~Cobertura)
kruskal.test(Incremento_dc~Cobertura)

boxplot(Incremento_alt~Cobertura)
ggplot(data = datos_escallonia) + geom_boxplot(aes(x=factor(Cobertura), y=Incremento_alt))
pairwise.t.test(Incremento_alt,Cobertura,p.adj="bonf") 
boxplot(Incremento_alt~Cobertura)

leveneTest(Incremento_alt,Tratamiento)
leveneTest(Incremento_db,Tratamiento)
leveneTest(Incremento_dc,Tratamiento)

#Myrcianthes leucoxyla

datos_Myrcianthes<-read.table("clipboard",header=TRUE)
attach(datos_Myrcianthes)

lillie.test(Incremento_alt)
fligner.test(Incremento_alt~Cobertura,datos_Myrcianthes)

resultado_alt<-welchADF.test(Incremento_alt~Cobertura, data=datos_Myrcianthes, 
                             contrast="omnibus", effect = "Cobertura",
                             trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_alt)

resultado_alt<-welchADF.test(Incremento_alt~Cobertura, data=datos_Myrcianthes, 
                             contrast="all.pairwise", effect = "Cobertura",
                             trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)
summary(resultado_alt)

lillie.test(Incremento_db)
fligner.test(Incremento_db~Cobertura,datos_Myrcianthes)

resultado_db<-welchADF.test(Incremento_db~Cobertura, data=datos_Myrcianthes, 
                            contrast="omnibus", effect = "Cobertura",
                            trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_db)

resultado_db<-welchADF.test(Incremento_db~Cobertura, data=datos_Myrcianthes, 
                            contrast="all.pairwise", effect = "Cobertura",
                            trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_db)


lillie.test(Incremento_dc)
fligner.test(Incremento_dc~Cobertura,datos_Myrcianthes)
kruskal.test(Incremento_dc~Cobertura)

summary(resultado_dc)
boxplot(Incremento_alt~Cobertura)

kruskal.test(Incremento_alt~Cobertura) 
kruskal.test(Incremento_db~Cobertura)
boxplot(Incremento_db~Cobertura)
kruskal.test(Incremento_dc~Cobertura)
boxplot(Incremento_dc~Cobertura)

pairwise.t.test(Incremento_alt,Cobertura,p.adj="bonf")
pairwise.t.test(Incremento_db,Cobertura,p.adj="bonf")
pairwise.t.test(Incremento_dc,Cobertura,p.adj="bonf")

leveneTest(Incremento_alt,Cobertura)
leveneTest(Incremento_db,Cobertura)
leveneTest(Incremento_dc,Cobertura)

#Myrsine guianensis

datos_myrsine<-read.table("clipboard",header=TRUE)
attach(datos_myrsine)

lillie.test(Incremento_alt)
lillie.test(Incremento_db)
lillie.test(Incremento_dc)

fligner.test(Incremento_alt~Cobertura,datos_myrsine)
fligner.test(Incremento_db~Cobertura,datos_myrsine)
fligner.test(Incremento_dc~Cobertura,datos_myrsine)

kruskal.test(Incremento_alt~Cobertura)
kruskal.test(Incremento_db~Cobertura)
kruskal.test(Incremento_cober~Cobertura)

boxplot(Incremento_alt~Cobertura)
boxplot(Incremento_db~Cobertura)
boxplot(Incremento_dc~Cobertura)
pairwise.t.test(Incremento_alt,Cobertura,p.adj="bonf")

levene.test(Incremento_alt,Cobertura)
levene.test(Incremento_db,Cobertura)
levene.test(Incremento_dc,Cobertura)

#Oreopanax

datos_oreopanax<-read.table("clipboard", header=TRUE)
attach(datos_oreopanax)

lillie.test(Incremento_alt)
lillie.test(Incremento_db)
lillie.test(Incremento_dc)

fligner.test(Incremento_alt~Cobertura,datos_oreopanax)
fligner.test(Incremento_db~Cobertura,datos_oreopanax)
fligner.test(Incremento_dc~Cobertura,datos_oreopanax)

kruskal.test(Incremento_alt~Cobertura) 
kruskal.test(Incremento_db~Cobertura) 
kruskal.test(Incremento_dc~Cobertura) 

boxplot(Incremento_alt~Cobertura)
boxplot(Incremento_db~Cobertura)
boxplot(Incremento_dc~Cobertura)

leveneTest(Incremento_alt,Cobertura)
leveneTest(Incremento_db,Cobertura)
leveneTest(Incremento_dc,Cobertura)

#Retrophyllum 

datos_retrophyllum<-read.table("clipboard",header=TRUE)
attach(datos_retrophyllum)

lillie.test(Incremento_alt)
lillie.test(Incremento_db)
lillie.test(Incremento_dc)

kruskal.test(Incremento_alt~Cobertura)
kruskal.test(Incremento_db~Cobertura)
kruskal.test(Incremento_dc~Cobertura)

fligner.test(Incremento_alt~Cobertura, datos_retrophyllum)
fligner.test(Incremento_db~Cobertura, datos_retrophyllum)
fligner.test(Incremento_dc~Cobertura, datos_retrophyllum)
levene.test(Incremento_alt,Cobertura)
levene.test(Incremento_db,Cobertura)
levene.test(Incremento_dc,Cobertura)

boxplot(Incremento_alt~Cobertura)
boxplot(Incremento_db~Cobertura)
boxplot(Incremento_dc~Cobertura)
pairwise.t.test(Incremento_dap,Cobertura,p.adj="bonf") 

leveneTest(Incremento_alt,Cobertura)
leveneTest(Incremento_db,Cobertura)
leveneTest(Incremento_dc,Cobertura)

#Viburnum

datos_viburnum<-read.table("clipboard",header=TRUE)
attach(datos_viburnum)

lillie.test(Incremento_alt)
lillie.test(Incremento_db)
lillie.test(Incremento_dc)

fligner.test(Incremento_alt~Cobertura, datos_viburnum)
fligner.test(Incremento_db~Cobertura, datos_viburnum)
fligner.test(Incremento_dc~Cobertura, datos_viburnum)

kruskal.test(Incremento_alt~Cobertura)
kruskal.test(Incremento_db~Cobertura)
kruskal.test(Incremento_dc~Cobertura)

boxplot(Incremento_alt~Cobertura)
boxplot(Incremento_db~Cobertura)
boxplot(Incremento_dc~Cobertura)

levene.test(Incremento_alt,Cobertura)
levene.test(Incremento_db,Cobertura)
levene.test(Incremento_dc,Cobertura)

#plantacion

plantacion<-read.table("clipboard",header=TRUE)
attach(plantacion)
summary(plantacion)
lillie.test(Incremento_alt)
fligner.test(Incremento_alt~Cobertura)
leveneTest(Incremento_alt, Cobertura)
kruskal.test(Incremento_alt~Cobertura)

varianceTest(Incremento_alt,Cobertura)

resultado_alt<-welchADF.test(Incremento_alt~Cobertura, data=plantacion, 
                             contrast="omnibus", effect = "Cobertura",
                             trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)
summary(resultado_alt)

resultado_alt<-welchADF.test(Incremento_alt~Cobertura, data=plantacion, 
                             contrast="all.pairwise", effect = "Cobertura",
                             trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)
summary(resultado_alt)

kruskal.test(Incremento_alt~Cobertura)
pairwise.t.test(Incremento_alt,Cobertura,p.adj="bonf") 
boxplot(Incremento_alt~Cobertura)

#Boxplot

g.alt.esp<- ggplot(plantacion, aes(Cobertura, Incremento_alt, fill=Cobertura))
g.alt.esp <- g.alt.esp + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + 
  facet_wrap(~Especie, scales = "free" ) + theme(strip.text = element_text(size=40 , face = "bond")) + theme_bw() + xlab("COBERTURA") + 
  ylab("INCREMENTO EN ALTURA (cm)") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8)))
g.alt.esp


lillie.test(Incremento_db)
fligner.test(Incremento_db~Cobertura)
leveneTest(Incremento_db, Cobertura)

resultado_db<-welchADF.test(Incremento_db~Cobertura, data=plantacion, 
                            contrast="omnibus", effect = "Cobertura",
                            trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_db)


resultado_db<-welchADF.test(Incremento_db~Cobertura, data=plantacion, 
                            contrast="all.pairwise", effect = "Cobertura",
                            trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_db)

g.dap.esp<- ggplot(plantacion, aes(Cobertura, Incremento_db, fill=Cobertura))
g.dap.esp <- g.dap.esp + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + 
  facet_wrap(~Especie, scales = "free" ) + theme(strip.text = element_text(size=40 , face = "bond")) + theme_bw() + xlab("COBERTURA") + 
  ylab("INCREMENTO EN DIAMETRO (mm)") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8)))
g.dap.esp

lillie.test(Incremento_dc)
fligner.test(Incremento_dc~Cobertura)
leveneTest(Incremento_dc, Cobertura)
kruskal.test(Incremento_dc~Cobertura)
names(plantacion)

resultado_cop<-welchADF.test(Incremento_dc~Cobertura, data=plantacion, 
                             contrast="omnibus", effect = "Cobertura",
                             trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_cop)

resultado_cop<-welchADF.test(Incremento_dc~Cobertura, data=plantacion, 
                             contrast="all.pairwise", effect = "Cobertura",
                             trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_cop)

g.cop.esp<- ggplot(plantacion, aes(Cobertura, Incremento_dc, fill=Cobertura))
g.cop.esp <- g.cop.esp + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + 
  facet_wrap(~Especie, scales = "free" ) + theme(strip.text = element_text(size=40 , face = "bond")) + theme_bw() + xlab("COBERTURA") + 
  ylab("INCREMENTO EN DIAMETRO DE COPA (cm)") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8)))
g.cop.esp


lillie.test(DC_0.5)
lillie.test(DC_1.3)
lillie.test(Pendiente)
lillie.test(Necromasa)

fligner.test(DC_0.5~Cobertura, plantacion)
fligner.test(DC_1.3~Cobertura, plantacion)
fligner.test(Pendiente~Cobertura, plantacion)
fligner.test(Necromasa~Cobertura, plantacion)

kruskal.test(Incremento_alt~Cobertura)  
kruskal.test(Incremento_db~Cobertura)
kruskal.test(Incremento_dc~Cobertura)
pairwise.t.test(Incremento_db,Cobertura,p.adj="bonf") 
pairwise.t.test(Incremento_alt,Cobertura,p.adj="bonf")

kruskal.test(DC_1.3~Cobertura)
pairwise.t.test(DC_1.3,Cobertura,p.adj="bonf")
boxplot(DC_1.3~Cobertura)

kruskal.test(Pendiente~Cobertura)
pairwise.t.test(Pendiente,Cobertura,p.adj="bonf")
boxplot(Pendiente~Cobertura)

kruskal.test(Necromasa~Cobertura)
pairwise.t.test(Necromasa,Cobertura,p.adj="bonf") 
boxplot(Necromasa~Cobertura)

#Correlacion de Spearman

plantacion<-read.table("clipboard", header = TRUE)
attach(plantacion)
names(plantacion)

plantacion.cor<-cor(plantacion, method ="spearman")
round(plantacion.cor, digits=2)
corrplot(plantacion.cor)
corrplot(plantacion.cor,method="shade", shade.col= NA, tl.col="black",tl.srt=45)

col<-colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))

corrplot(plantacion.cor,method="shade", shade.col=NA, tl.col="black", tl.srt = 45, col=col(200),addCoef.col = "black", addcolorlabel="no", order="AOE",type="lower",diag=F,addshade = "all")

corrplot(plantacion.cor,method="shade", shade.col=NA, tl.col="black", tl.srt = 45, col=col(200), addcolorlabel="no", order="AOE",type="lower",diag=F,addshade = "all")

#Correlacion especies

datos_especies<-read.table("clipboard",header=TRUE)
attach(datos_especies)
head(datos_especies)

datos_especies.cor<-cor(datos_especies, method ="spearman")
round(datos_especies.cor, digits=2)
corrplot(datos_especies.cor)
corrplot(datos_especies.cor,method="shade", shade.col= NA, tl.col="black",tl.srt=45)

col<-colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))

corrplot(datos_especies.cor,method="shade", shade.col=NA, tl.col="black", tl.srt = 45, col=col(200),addCoef.col = "black", addcolorlabel="no", order="hclust",type="lower",diag=F,addshade = "all", size =8)

corrplot(datos_especies.cor,method="shade", shade.col=NA, tl.col="black", tl.srt = 45, col=col(200), addcolorlabel="no", order="AOE",type="lower",diag=F,addshade = "all")

#REGENERACION

reg <-read.table ("clipboard", header=TRUE)
attach(reg)
summary(reg)
kruskal.test(Regeneracion~Cobertura)
pairwise.t.test(Regeneracion,Cobertura,p.adj="bonf")
boxplot(Regeneracion~Cobertura)
fligner.test(Regeneracion~Cobertura, data=reg)
lillie.test(Regeneracion)

ggplot(data = reg )+ geom_boxplot(fill=c("yellow","orange","red"),aes(x=factor(Cobertura), y=PORCENTAJE))+ggtitle("PORCENTAJE DE REGENERACIÓN VS TRATAMIENTO",subtitle="Plantación")+
  theme(plot.title = element_text(hjust = 0.5))+theme(plot.subtitle=element_text(size=10, hjust=0.5, face="italic", color="black"))+xlab("Tratamiento")+ylab("Porcentaje (%)")+ 
  theme(axis.text.x = element_text(face="italic", hjust=0.5, vjust=0.5))


boxplot(Regeneracion~Cobertura)
levene.test(Regeneracion,Cobertura)
bartlett.test(Regeneracion,Cobertura, data=reg)
anova<-aov(Regeneracion~Cobertura)
summary(anova)
nuevoanova= unstack(anova,Regeneracion~Cobertura)
nuevoanova
TukeyHSD(anova)

#VARIABLES

variables<-read.table("clipboard", header=TRUE)
attach(variables)
names(variables)

lillie.test(Densidad_copa)
levene.test(Densidad_copa, Cobertura)
bartlett.test(Densidad_copa,Cobertura, data=variables)
anova_densidad<-aov(Densidad_copa~Cobertura)
summary(anova_densidad)
densidad= unstack(anova_densidad,Densidad_copa~Cobertura)

TukeyHSD(anova_densidad)
boxplot(Densidad_copa~Cobertura)

ggplot(data = variables )+ geom_boxplot(fill=c("yellow","orange","red"),aes(x=factor(Cobertura), y=Densidad_copa))+ggtitle("DENSIDAD DE COPA VS TRATAMIENTO",subtitle="Plantación")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.subtitle=element_text(size=10, hjust=0.5, face="italic", color="black"))+xlab("Tratamiento")+ylab("Porcentaje (%)")+ theme(axis.text.x = element_text(face="italic", hjust=0.5, vjust=0.5))

g.rg.esp<- ggplot(variables, aes(Cobertura, Regeneracion, fill=Cobertura))
g.rg.esp <- g.rg.esp + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + 
  theme(strip.text = element_text(size=40 , face = "bond")) + theme_bw() + xlab("COBERTURA") + 
  ylab("REGENERACIÓN (%)") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8)))
g.rg.esp

g.pen.esp<- ggplot(variables, aes(Cobertura, Pendiente, fill=Cobertura))
g.pen.esp <- g.pen.esp + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + 
  theme(strip.text = element_text(size=40 , face = "bond")) + theme_bw() + xlab("COBERTURA") + 
  ylab("PENDIENTE (%)") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8)))
g.pen.esp

g.cob.esp<- ggplot(variables, aes(Cobertura, Densidad_copa, fill=Cobertura))
g.cob.esp <- g.cob.esp + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + 
  theme(strip.text = element_text(size=40 , face = "bond")) + theme_bw() + xlab("COBERTURA") + 
  ylab("DENSIDAD DE COPA (%)") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8)))
g.cob.esp

g.nec.esp<- ggplot(variables, aes(Cobertura, Necromasa , fill=Cobertura))
g.nec.esp <- g.cmb.esp + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + 
  theme(strip.text = element_text(size=40 , face = "bond")) + theme_bw() + xlab("COBERTURA") + 
  ylab("NECROMASA FINA (Ton/ha)") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8)))
g.nec.esp

g.tmp.esp<- ggplot(variables, aes(Cobertura, Mortalidad , fill=Cobertura))
g.tmp.esp <- g.tmp.esp + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + 
  theme(strip.text = element_text(size=40 , face = "bond")) + theme_bw() + xlab("COBERTURA") + 
  ylab("TASA DE MORTALIDAD") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8)))
g.tmp.esp

lillie.test(Pendiente)
bartlett.test(Pendiente,Cobertura)
boxplot(Pendiente~Cobertura)

#Transformación para Pendiente
variables<-read.table ("clipboard", header=TRUE)
attach(variables)
lillie.test(Pendiente)
fligner.test(Pendiente, Cobertura)
library(fpp)
qqnorm(Pendiente)
lambda<- BoxCox.lambda(variables , method = c("loglik"), lower = -5, upper = 5 )
lambda

w<-BoxCox(variables$Pendiente, lambda)
as.matrix(w)
lillie.test(w)
qqnorm(w)
qqline(w)

#Anova de variables despues de la transformación

anova_pendiente<-aov(Pendiente~Cobertura)
summary(anova_pendiente)
pendiente= unstack(anova_pendiente,Pendiente~Cobertura)
TukeyHSD(anova_pendiente)

levene.test(Pendiente, Cobertura)
bartlett.test(Pendiente,Cobertura, data=variables)
boxplot(Pendiente~Cobertura)

ggplot(data = variables )+ geom_boxplot(fill=c("yellow","orange","red"),aes(x=factor(Cobertura), y=Pendiente))+ggtitle("PORCENTAJE DE COBERTURA VS TRATAMIENTO",subtitle="Plantación")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.subtitle=element_text(size=10, hjust=0.5, face="italic", color="black"))+xlab("Tratamiento")+ylab("Porcentaje (%)")+ theme(axis.text.x = element_text(face="italic", hjust=0.5, vjust=0.5))

lillie.test(Necromasa)
levene.test(Necromasa, Cobertura)
bartlett.test(Necromasa~Cobertura, data=variables)
anova_necromasa<-aov(Necromasa~Cobertura)
summary(anova_necromasa)
necromasa= unstack(anova_necromasa,Necromasa~Cobertura)

TukeyHSD(anova_necromasa)
boxplot(Necromasa~Cobertura)

ggplot(data = variables )+ geom_boxplot(fill=c("yellow","orange","red"),aes(x=factor(Cobertura), y=Necromasa))+ggtitle("CANTIDAD DE NECROMASA POR TRATAMIENTO",subtitle="Plantación")+theme(plot.title = element_text(hjust = 0.5))+theme(plot.subtitle=element_text(size=10, hjust=0.5, face="italic", color="black"))+xlab("Tratamiento")+ylab("Toneladas/ha")+ theme(axis.text.x = element_text(face="italic", hjust=0.5, vjust=0.5))


#Estadistica 

estadistica<-read.table("clipboard", header =TRUE)
attach(estadistica)
names(estadistica)

resultado_alt<-welchADF.test(Incremento_alt~Cobertura, data=estadistica, 
                             contrast="omnibus", effect = "Cobertura",
                             trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_alt)

resultado_alt<-welchADF.test(Incremento_alt~Cobertura, data=estadistica, 
                             contrast="all.pairwise", effect = "Cobertura",
                             trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

boxplot(Incremento_alt~Cobertura)

resultado_db<-welchADF.test(Incremento_db~Cobertura, data=estadistica, contrast="omnibus",
                            effect = "Cobertura",trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

boxplot(Incremento_db~Cobertura)
summary(resultado_db)

resultado_db<-welchADF.test(Incremento_db~Cobertura, data=estadistica, contrast="all.pairwise",
                            effect = "Cobertura",trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_db)

resultado_dc<-welchADF.test(Incremento_dc~Cobertura, data=estadistica, contrast="omnibus",
                            effect = "Cobertura",trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_dc)
boxplot(Incremento_dc~Cobertura)

resultado_dc<-welchADF.test(Incremento_dc~Cobertura, data=estadistica, contrast="all.pairwise",
                            effect = "Cobertura",trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_dc)
boxplot(Incremento_dc~Cobertura)


#PENDIENTE

lillie.test(Pendiente)
fligner.test(Pendiente,Cobertura)
kruskal.test(Pendiente~Cobertura)
pairwise.t.test(Pendiente,Cobertura,p.adj="bonf")
boxplot(Pendiente~Cobertura)

#DENSIDAD DE COPA

lillie.test(Densidad_copa)
fligner.test(Densidad_copa,Cobertura)  
leveneTest(Densidad_copa,Cobertura)
library(Rcmdr)

AnovaModel.2 <- aov(Densidad_copa ~ Cobertura, data=estadistica)
summary(AnovaModel.2)
with(estadistica, numSummary(Densidad_copa, groups=Cobertura, 
                             statistics=c("mean", "sd")))
oneway.test(Densidad_copa ~ Cobertura, data=estadistica)

TukeyHSD(AnovaModel.2)
boxplot(Densidad_copa~Cobertura)

#NECROMASA

lillie.test(Necromasa)
leveneTest(Necromasa, Cobertura)
AnovaModel.3 <- aov(Necromasa ~ Cobertura, data=estadistica)
summary(AnovaModel.3)
TukeyHSD(AnovaModel.3)
boxplot(Necromasa~Cobertura)

#BIPLOT

analisis<-read.table("clipboard", header=TRUE)
attach(analisis)
names(analisis)
local({
  PC <- princomp(~Densidad_copa+Necromasa+Incremento_alt+Incremento_db+Incremento_dc+Pendiente,
                 cor=TRUE, data=analisis)
  cat("\nComponent loadings:\n")
  print(unclass(loadings(.PC)))
  cat("\nComponent variances:\n")
  print(.PC$sd^2)
  cat("\n")
  print(summary(.PC))
  
  PC <- princomp(~Densidad_copa+Necromasa+Incremento_alt+Incremento_db+Incremento_dc+Pendiente,cor=TRUE, data=analisis)
})

library(devtools)
library(githubinstall)
install_github("ggbiplot","vqv")
install.packages("devtools")
install_github("ggbiplot")
githubinstall("ggbiplot")

especies<-read.table("clipboard", header=TRUE)
attach(especies)
names(especies)
PC1 <- princomp(~Incremento_alt+Incremento_db+Incremento_dc, cor=TRUE, data=especies)
library(ggbiplot)

#Biplot

ggbiplot(PC1, ellipse= FALSE, ellipe.prob= 0.95,
         circle =TRUE, circle.pron = 5, addEllipses = FALSE, choices = c(1,2))+ 
  theme_bw ()+ geom_point(aes(color = SP)) +  scale_x_continuous(breaks = seq(-5,5,by= 1)) +
  scale_y_continuous(breaks = seq(-5,5,by= 1)) +  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2)) +
  ggrepel::geom_label_repel(aes(label = especies$SP))

shapiro.test(Incremento_alt)
qqnorm(Incremento_alt)
qqline(Incremento_alt)

analisis<-read.table("clipboard", header = TRUE)
attach(analisis)
names(analisis)
library(ggbiplot)
PC<- princomp(~Densidad_copa+Necromasa+Pendiente, cor=TRUE, data=analisis)

ggbiplot(PC, groups = Cobertura , ellipse= TRUE, ellipe.prob= 0.95, circle =FALSE, circle.pron = 1, 
         repel = TRUE, col.ind = "blue", addEllipses = TRUE, choices = c(1,2))+ theme_bw ()

#REGENERACION NATIVA Y EXOTICA

lillie.test(REG_EX)
lillie.test(REG_NA)
fligner.test(REG_NA,Cobertura)
leveneTest(REG_NA, Cobertura)
kruskal.test(REG_NA~Cobertura)
pairwise.t.test(REG_NA,Cobertura,p.adj="bonf") 
boxplot(REG_NA~Cobertura)

g.rg.esp<- ggplot(analisis, aes(Cobertura, REG_EX, fill=Cobertura))
g.rg.esp <- g.rg.esp + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + 
  theme(strip.text = element_text(size=40 , face = "bond")) + theme_bw() + xlab("COBERTURA") + 
  ylab("REGENERACIÓN EXÓTICA (%)") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8)))
g.rg.esp

g.rg.esp<- ggplot(analisis, aes(Cobertura, REG_NA, fill=Cobertura))
g.rg.esp<- g.rg.esp + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + 
  theme(strip.text = element_text(size=40 , face = "bond")) + theme_bw() + xlab("COBERTURA") + 
  ylab("REGENERACIÓN NATIVA (%)") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8)))
g.rg.esp

lillie.test(REG_EX)
bartlett.test(REG_EX,Cobertura)
Anova_reg <- aov(REG_EX ~ Cobertura, data=analisis)
summary(Anova_reg)
TukeyHSD(Anova_reg)
boxplot(REG_EX~Cobertura)

#GRAFICOS

graficos<-read.table("clipboard", header=TRUE)
attach(graficos)
names(graficos)

g.dap.esp<- ggplot(graficos, aes(Cobertura, Regeneracion, fill=Cobertura))
g.dap.esp <- g.dap.esp + geom_boxplot(fill=c("olivedrab1", "olivedrab1", "olivedrab1")) + coord_flip() + 
  theme(strip.text = element_text(size=20 , face = "bond"))+ theme_base() + xlab("Cobertura") + 
  ylab("Porcentaje de regeneración (%)") + theme(legend.position = "none") + ggtitle(" ",subtitle="")+
  theme(plot.title = element_text(hjust = 0))+theme(plot.subtitle=element_text(size=11, hjust=0.5, face="bold.italic", color="black"))+
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 10, angle = 90)) + 
  theme(axis.title.y = element_text(face = "bold", size = rel(0.6)))  + theme(axis.title.x = element_text(face = "bold", size = rel(0.6)))
g.dap.esp

g.mt.esp<- ggplot(mortalidad, aes(Cobertura, Mortalidad, fill=Cobertura))
g.mt.esp <- g.mt.esp + geom_boxplot(fill=c("olivedrab1", "olivedrab1", "olivedrab1")) + coord_flip() + 
  theme(strip.text = element_text(size=40 , face = "bond")) + theme_base() + xlab("Cobertura") + 
  ylab("Tasa de Mortalidad") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8)))
g.mt.esp

g.db.esp<- ggplot(graficos, aes(Cobertura, Incremento_db, fill=Cobertura))
g.db.esp <- g.db.esp + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + 
  facet_wrap(~Especie) + theme_bw() + xlab("Cobertura") + 
  ylab("Incremento en diámetro (mm)") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5))
g.db.esp

pendiente<- ggplot(graficos, aes(Cobertura, Pendiente, fill= Cobertura))
pendiente <- pendiente + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + theme_bw() + xlab("Cobertura") + 
  ylab("Pendiente (%)") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5))
pendiente

rg<- ggplot(graficos, aes(Cobertura, Regeneracion , fill=Cobertura))
rg <- rg + geom_boxplot(fill=c("olivedrab1", "olivedrab1", "olivedrab1")) + coord_flip() + 
  theme(strip.text = element_text(size=20 , face = "bond"))+ theme_base()+ xlab("Cobertura") + 
  ylab("Porcentaje de regeneración") + theme(legend.position = "none") + ggtitle(" ",subtitle="")+
  theme(plot.title = element_text(hjust = 0))+theme(plot.subtitle=element_text(size=11, hjust=0.5, face="bold.italic", color="black"))+
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8))) 
rg

necromasa<- ggplot(graficos, aes(Cobertura, Necromasa , fill= Cobertura))
necromasa <- necromasa + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + theme_bw() + xlab("Cobertura") + 
  ylab("Necromasa Ton/ha") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5))
necromasa

rg<- ggplot(graficos, aes(Cobertura, Regeneracion , fill= Cobertura))
rg<- rg + geom_boxplot(fill=c("yellow", "orange", "red")) + coord_flip() + theme_bw() + xlab("Cobertura") + 
  ylab("Regeneración (%)") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5))
rg

mt<- ggplot(graficos, aes(Cobertura, Mortalidad , fill= Cobertura))
mt<- mt + geom_boxplot(fill=c("green", "green", "green")) + coord_flip() + theme_base() + xlab("Cobertura") + 
  ylab("Tasa de Mortalidad") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5))
mt


mortalidad<-read.table("clipboard", header = TRUE)
attach(mortalidad)
lillie.test(Mortalidad)
fligner.test(Mortalidad, Cobertura)

resultado_mortalidad<-welchADF.test(Mortalidad~Cobertura, data=mortalidad, contrast="omnibus",
                                    effect = "Cobertura",trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_mortalidad)

resultado_mortalidad<-welchADF.test(Mortalidad~Cobertura, data=mortalidad, contrast="all.pairwise",
                                    effect = "Cobertura",trimming = TRUE, effect.size=FALSE, bootstrap= FALSE, numsim_b=1000)

summary(resultado_mortalidad)

g.mt.esp<- ggplot(mortalidad, aes(Cobertura, Mortalidad, fill=Cobertura))
g.mt.esp <- g.mt.esp + geom_boxplot(fill=c("olivedrab1", "olivedrab1", "olivedrab1")) + coord_flip() + 
  theme(strip.text = element_text(size=40 , face = "bond")) + theme_base() + xlab("Cobertura") + 
  ylab("Tasa de Mortalidad") + theme(legend.position = "none") + 
  theme(axis.text.y = element_text(face="italic", hjust=0.5, vjust=0.5, size = 11, angle = 90 )) +
  theme(axis.title.y = element_text(face = "bold", size = rel(0.8))) + theme(axis.title.x = element_text(face = "bold", size = rel(0.8)))
g.mt.esp

