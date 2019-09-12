##A script to create a plot that is a schematic describing forecast reconciliation.

library(latex2exp)
library(tikzDevice)
options(tikzLatexPackages=c(getOption("tikzLatexPackages"),"\\usepackage{amsfonts}","\\usepackage{bm}"),
        tikzFooter = "\\caption{a caption}")



##Point forecast reconciliation.

tikz('orth_pointforerec_schematic.tex',width=4)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5),asp=1)
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\huge $\\mathfrak{s}$}",pos=4)



yhat<-c(1.5,3)

points(1.5,3,pch=20,cex=2,col='blue')
text(1.5,3,"{\\large $\\color{blue}{\\hat{\\bm{y}}}$}",pos = 3,offset = 1.5)

SP<-matrix(c(0.8,0.4,0.4,0.2),2,2)

ytilde<-SP%*%yhat

points(ytilde[1],ytilde[2],pch=20,cex=2,col='red')
text(ytilde[1],ytilde[2],"{\\large $\\color{red}{\\tilde{\\bm{y}}}$}",pos=1,offset = 1.5)



#lines(c(0,2),c(-3,5),lty=2)
arrows(yhat[1],yhat[2],ytilde[1],ytilde[2],col='blue')


#text(1.25,2,"{\\large ${\\color{blue} S\\circ P}$}",pos=4)

points(6/7,3/7,pch=20,cex=2,col='black')
text(6/7,3/7,"{\\large $\\color{black}{{\\bm{y}}}$}",pos=1,offset = 1.5)


dev.off()


### In-sample scatter plot
tikz('insample.tex',height=5)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\huge $\\mathfrak{s}$}",pos=4)

#lines(c(-0.25,1),c(-1,4),lwd=2)

rmat<-matrix(c(1,4,-0.5,0.25),2,2,byrow = FALSE)
e<-t(rmat%*%matrix(rnorm(200,0,0.3),2,100))
points(e[,1],e[,2],pch=19,col='orange')
dev.off()

### In-sample scatter plot with dir
tikz('insampledir.tex',height=5)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\huge $\\mathfrak{s}$}",pos=4)
#lines(c(-0.25,1),c(-1,4),lwd=2)

points(e[,1],e[,2],pch=19,col='orange')
arrows(0,0,1,4,lwd=3)
text(1,4,"{\\Large ${\\bm R}$}",pos=3)
dev.off()

##Point Forecast Reconciliation
tikz('oblique_justification1.tex',height=5)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\huge $\\mathfrak{s}$}",pos=4)
#lines(c(-0.25,1),c(-1,4),lwd=2)
arrows(0,0,1,4,lwd=3)
text(1,4,"{\\Large ${\\bm R}$}",pos=3)

points(0.6,0.3,pch=20,cex=2,col='black')
text(0.6,0.3,"{\\huge $\\color{black}{\\bm{y}}$}",pos = 1,offset = 1)
dev.off()


tikz('oblique_justification2.tex',height=5)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
points(e[,1]+0.6,e[,2]+0.3,pch=19,col='gray')
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\huge $\\mathfrak{s}$}",pos=4)
#lines(c(-0.25,1),c(-1,4),lwd=2)
arrows(0,0,1,4,lwd=3)
text(1,4,"{\\Large ${\\bm R}$}",pos=3)

points(0.6,0.3,pch=20,cex=2,col='black')
text(0.6,0.3,"{\\huge $\\color{black}{\\bm{y}}$}",pos = 1,offset = 1)
points(1.5,3,pch=20,cex=2,col='blue')
text(1.5,3,"{\\huge $\\color{blue}{\\hat{\\bm{y}}}$}",pos = 3,offset = 1.5)
dev.off()


# lines(c(0,2),c(-3,5),lty=2)
# points(1.5,3,pch=20,cex=2,col='blue')
# text(1.5,3,"{\\huge $\\color{blue}{\\hat{\\bm{y}}}$}",pos = 3,offset = 1.5)
# 
# text(1.25,2,"{\\huge ${\\color{blue} s\\circ g}$}",pos=4)
# 
# points(6/7,3/7,pch=20,cex=2,col='red')
# text(6/7,3/7,"{\\huge $\\color{red}{\\tilde{\\bm{y}}}$}",pos=1,offset = 1.5)
# 
# arrows(1.5,3,6/7,3/7,col='blue')
# 
# dev.off()




##Point forecast reconciliation.

tikz('pointforerec_schematic.tex',height=5)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\huge $\\mathfrak{s}$}",pos=4)
arrows(0,0,1,4,lwd=3)
text(1,4,"{\\Large ${\\bm R}$}",pos=3)

lines(c(0,2),c(-3,5),lty=2)
points(1.5,3,pch=20,cex=2,col='blue')
text(1.5,3,"{\\huge $\\color{blue}{\\hat{\\bm{y}}}$}",pos = 3,offset = 1.5)

#text(1.25,2,"{\\huge ${\\color{blue} s\\circ g}$}",pos=4)

points(6/7,3/7,pch=20,cex=2,col='red')
text(6/7,3/7,"{\\huge $\\color{red}{\\tilde{\\bm{y}}}$}",pos=1,offset = 1.5)

arrows(1.5,3,6/7,3/7,col='blue')
points(0.6,0.3,pch=20,cex=2,col='black')
text(0.6,0.3,"{\\huge $\\color{black}{\\bm{y}}$}",pos = 1,offset = 1)
dev.off()



tikz('probforerec_schematic.tex',height=5,pointsize = 7)
plot.new()
plot.window(xlim = c(-0.25,4),ylim = c(-0.5,5))
lines(c(0,0),c(-1,5))
lines(c(-1,5),c(0,0))
#lines(c(-1,3.5),c(-0.5,1.75),lwd=2)
arrows(0,0,3.5,1.75,lwd=3)
text(3.5,1.75,"{\\huge $\\mathfrak{s}$}",pos=4)

#lines(c(-0.25,1),c(-1,4),lwd=2)
arrows(0,0,1,4,lwd=3)
text(1,4,"{\\Large ${\\bm R}$}",pos=3)

lines(c(0,1.5),c(-1,5),lty=2)

lines(c(1,2.5),c(-1,5),lty=2)

lines(c(0.25,1.25),c(0,0),lwd=4)
text(0.75,0,"{\\huge $\\mathcal{B}$}",pos = 1,offset = 1)
lines(c(2/7,10/7),c(1/7,5/7),lwd=4,col='red')
text(1.5,-0.5,"{\\huge $\\color{red}{s(\\mathcal{B})}$}",pos = 4,offset = 1.2)
arrows(1.5,-0.5,6/7,3/7,col='red')

cord.x<-c(-0.25, 0.75, 2.75, 1.75)
cord.y<-c(-2, -2, 6, 6)

polygon(cord.x,cord.y,col=adjustcolor('blue',alpha=0.1),border = NA)

text(3,3,"{\\huge $\\color{blue}{g^{-1}(\\mathcal{B}})$}",col='blue',pos = 3,offset=1.2)
arrows(3,3,1.5,2,col='blue')

dev.off()
