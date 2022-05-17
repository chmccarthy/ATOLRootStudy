# created 2018-10-22 by WRF
library(patchwork)
library(ggplot2)
args = commandArgs(trailingOnly=TRUE)

inputfile = "chang1.rf"
outputfile = "plot2.pdf"

chainrfd = read.table(inputfile,header=FALSE,sep="")

a <- ggplot(data=chainrfd, aes(x=V1, y=V4, group=1)) + geom_line(color="#01665e") + theme_bw() +
  labs(title="Chang2015_filtered.chain1.treelist", x="Iterations", y="Normalized RF distance")

inputfile = "chang2.rf"
chainrfd = read.table(inputfile,header=FALSE,sep="")

b <- ggplot(data=chainrfd, aes(x=V1, y=V4, group=1)) + geom_line(color="#01665e") + theme_bw() +
  labs(title="Chang2015_filtered.chain2.treelist", x="Iterations", y="Normalized RF distance")

inputfile = "whelan1.rf"
chainrfd = read.table(inputfile,header=FALSE,sep="")

c <- ggplot(data=chainrfd, aes(x=V1, y=V4, group=1)) + geom_line(color="#01665e") + theme_bw() +
  labs(title="Whelan2015D10_filtered.chain1.treelist", x="Iterations", y="Normalized RF distance")

inputfile = "whelan2.rf"
chainrfd = read.table(inputfile,header=FALSE,sep="")

d <- ggplot(data=chainrfd, aes(x=V1, y=V4, group=1)) + geom_line(color="#01665e") + theme_bw() +
  labs(title="Whelan2015D10_filtered.chain2.treelist", x="Iterations", y="Normalized RF distance")

inputfile = "sim171.rf"
chainrfd = read.table(inputfile,header=FALSE,sep="")

e <- ggplot(data=chainrfd, aes(x=V1, y=V4, group=1)) + geom_line(color="#01665e") + theme_bw() +
  labs(title="Simion2017_filtered.chain1.treelist", x="Iterations", y="Normalized RF distance")

inputfile = "sim172.rf"
chainrfd = read.table(inputfile,header=FALSE,sep="")

f <- ggplot(data=chainrfd, aes(x=V1, y=V4, group=1)) + geom_line(color="#01665e") + theme_bw() +
  labs(title="Simion2017_filtered.chain2.treelist", x="Iterations", y="Normalized RF distance")

inputfile = "whe171.rf"
chainrfd = read.table(inputfile,header=FALSE,sep="")

g <- ggplot(data=chainrfd, aes(x=V1, y=V4, group=1)) + geom_line(color="#01665e") + theme_bw() +
  labs(title="Whelan2017MCRS_filtered.chain1.treelist", x="Iterations", y="Normalized RF distance")

inputfile = "whe172.rf"
chainrfd = read.table(inputfile,header=FALSE,sep="")

h <- ggplot(data=chainrfd, aes(x=V1, y=V4, group=1)) + geom_line(color="#01665e") + theme_bw() +
  labs(title="Whelan2017MCRS_filtered.chain2.treelist", x="Iterations", y="Normalized RF distance")


p <- a + b + c + d + e + f + g + h + plot_layout(ncol = 2)
p


#
