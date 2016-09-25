library(vegan)
All=read.table(file.choose(),header=T,sep=",",row.names=1)
dim(All)
T.All<-t(All)
dim(T.All) 
Y=T.All

All.group=read.table(file.choose(),header=T,sep=",",row.names=1)
dim(All.group)
Y=T.All
X=All.group
A=cca(Y~pH+waterpotential+Ca+K+Mg+Mn+P+Zn+NH4+NO3+CN,data=X)
#A=cca(Y~pH+Rainfall+waterpotential+Ca+K+Mg+Mn+P+Zn+NH4+NO3+N+TOC+CN,data=X)

vif.cca(A)
eig<-A$CCA$eig
eig
toeig<-A$CCA$tot.chi
toeig
cca1.percent = round(eig[1]/toeig,digits=3)*100 #DCA1 proportion
cca2.percent = round(eig[2]/toeig,digits=3)*100 #DCA2 proportion
x.label =paste("CCA1 ",cca1.percent,"%",sep="")
y.label =paste("CCA2 ",cca2.percent,"%",sep="")
plot(A,choice=c(1,2),dis=c('sites','cn'),type="points",cex=0.000000001,xlab=x.label,ylab=y.label,tck=0.01,mgp=c(1.5,0.3,0))
points(A, pch=c(rep(1,3),rep(2,3),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(8,4),rep(21,4),rep(22,4),rep(23,4),rep(24,4),rep(25,4)),col=c(rep("darkred",3),rep("red",3),rep("forestgreen",4),rep("darkmagenta",4),rep("goldenrod4",4),rep("deeppink3",4),rep("dodgerblue4",4),rep("olivedrab",4),rep("orange1",4),rep("saddlebrown",4),rep("yellow1",4),rep("wheat",4)), bg=c(c(rep("darkcyan",3),rep("red",3),rep("forestgreen",4),rep("darkmagenta",4),rep("goldenrod4",4),rep("deeppink3",4),rep("dodgerblue4",4),rep("olivedrab",4),rep("orange1",4),rep("saddlebrown",4),rep("yellow1",4),rep("wheat",4)), cex=c(rep(0.65,30),rep(1.2,16)),lwd=3))
legend(2.8,-1.9, c("P210","P270","P530","790","P930","P1060","P1260","P1500","P1800","P2500","PQ2500","P3550"), col = c("darkred","red","forestgreen","darkmagenta","goldenrod4","deeppink3","dodgerblue4","olivedrab","orange1","saddlebrown","yellow1","wheat"),
       border = "black",lty=0, lwd=0.5, pch=c(1,2,3,4,5,6,8,21,22,23,24,25),box.lty = 1,pt.lwd = 2,bty="o",ncol=2,cex=0.8)
#       angle = 45, density = NULL, bty = "o", bg = par("bg"),
#box.lwd = "none", box.lty = par("lty"), box.col = par("fg"),
#pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd,
#xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1,
#adj = c(0, 0.5), text.width = NULL, text.col = par("col"),
#text.font = NULL,  trace = FALSE,
#plot = TRUE, ncol = 1, horiz = FALSE, title = NULL,
#inset = 0, xpd=TRUE,, title.adj = 0.5,
#seg.len = 2


summary(A,display = c( "wa", "cn"))
scores(A,display = c("sites","bp"))
anova(A)
anova.cca(A)
anova.cca(A, by = "term",step = 200) 