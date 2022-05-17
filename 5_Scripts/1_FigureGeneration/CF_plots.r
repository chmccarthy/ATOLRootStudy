library(viridis)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(GGally)
library(entropy)
library(patchwork)
library(ggpmisc)
library(ggtree)
library(ggpubr)

myf <- y ~ x

d1 <- read.table("./pre/D20OG.cf.stat", header = T, fill = T)
d2 <- read.table("./post/D20fil.cf.stat", header = T, fill = T)
g1 <- read.tree("./pre/D20OG.cf.tree")
g2 <- read.tree("./post/D20fil.cf.tree")

c <- readLines("clans.txt")
l <- list()
l$Outgroups <- unlist(strsplit(c[1], " "))
l$Porifera <- unlist(strsplit(c[2], " "))
l$Ctenophora <- unlist(strsplit(c[3], " "))
#l$Placozoa <- unlist(strsplit(c[4], " "))
l$Cnidaria <- unlist(strsplit(c[5], " "))
l$Bilateria <- unlist(strsplit(c[6], " "))

scale <- c("0" = "black", "Outgroups" = "black", "Porifera" = "#d83131", "Ctenophora" = "#9b9b9b",
            "Cnidaria" = "#ad9300", "Bilateria" = "orange")

g1 <- groupOTU(g1, l)
g2 <- groupOTU(g2, l)

t1 <- ggtree(g1, aes(color=group), branch.length="none") + geom_tiplab() + geom_nodelab(hjust = 0.5) +
      hexpand(0.5) + scale_color_manual(values=scale) + theme(legend.position = "none")
t2 <- ggtree(g2, aes(color=group), branch.length="none") + geom_tiplab() + geom_nodelab(hjust = 0.5) +
      hexpand(0.5) + scale_color_manual(values=scale) + theme(legend.position = "none")

majornodes1 <- majornodes2 <- c()
for(i in 1:length(l)) {
  majornodes1[i] <- MRCA(g1, l[[i]])
  majornodes2[i] <- MRCA(g2, l[[i]])}

d1$group <- apply(d1, 1, function(x) {
  t1$data$group[x["ID"]]
})

d1$maj <- apply(d1, 1, function(x) {
 if (as.integer(x["ID"]) %in% majornodes1) {
   TRUE
 } else {
   FALSE
 }
})

d2$group <- apply(d2, 1, function(x) {
  t2$data$group[x["ID"]]
})

d2$maj <- apply(d2, 1, function(x) {
  if (as.integer(x["ID"]) %in% majornodes2) {
    TRUE
  } else {
    FALSE
  }
})

s1 <- ggplot(d1, aes(x = gCF, y = sCF)) + geom_point(aes(color=group, shape=maj, size=maj)) +
     geom_abline() + xlim(0, 100) + ylim(0, 100) + scale_color_manual(values=scale) +
     stat_regline_equation(formula = y ~ x, label.y = 90) + stat_cor(aes(label=..rr.label..), label.y = 80) +
     theme_bw() + theme(legend.position = "none")
s2 <- ggplot(d2, aes(x = gCF, y = sCF)) + geom_point(aes(color=group, shape=maj, size=maj)) +
      geom_abline() + xlim(0, 100) + ylim(0, 100) + scale_color_manual(values=scale) +
      stat_regline_equation(formula = y ~ x, label.y = 90) + stat_cor(aes(label=..rr.label..), label.y = 80) +
      theme_bw() + theme(legend.position = "none")


#s1 <- ggplot(d1, aes(x = gCF, y = sCF, label = PhyLabel)) + geom_point(aes(color=Phylum)) + xlim(0, 100) + ylim(0, 100) +
#      geom_label_repel(data = subset(d1, ID %in% majornodes1), min.segment.length = 0, max.overlaps = Inf) +
#                scale_color_identity() + geom_abline()

#s2 <- ggplot(d2, aes(x = gCF, y = sCF, label = ID)) + geom_point() + xlim(0, 100) + ylim(0, 100) +
#      geom_label_repel(data = subset(d2, ID %in% majornodes2), min.segment.length = 0, max.overlaps = Inf) +
#      scale_color_identity() + geom_abline()


#geom_text2(aes(subset=!isTip, label=node)) + hexpand(0.2)

plot(t1 + t2 + s1 + s2 + plot_layout(heights = c(4, 1)))