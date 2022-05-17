# cut -f 1,31 summarized freqs | sed 1,3d

library(ggplot2)
library(wesanderson)
library(ggpubr)

a <- read.table("Chang_All.txt", sep="\t")
b <- read.table("Chang_pass.txt", sep="\t")

c <- subset(a, !(V1 %in% b$V1))

b$V3 <- "Chang2015"
c$V3 <- "Chang2015"
b$V4 <- "Passed clan_check filter"
c$V4 <- "Failed clan_check filter"

d <- rbind(b, c)

a <- read.table("D10_All.txt", sep="\t")
b <- read.table("D10_pass.txt", sep="\t")

c <- subset(a, !(V1 %in% b$V1))

b$V3 <- "Whelan2015D10"
c$V3 <- "Whelan2015D10"
b$V4 <- "Passed clan_check filter"
c$V4 <- "Failed clan_check filter"

d <- rbind(d, b, c)

a <- read.table("D20_All.txt", sep="\t")
b <- read.table("D20_pass.txt", sep="\t")

c <- subset(a, !(V1 %in% b$V1))

b$V3 <- "Whelan2015D20"
c$V3 <- "Whelan2015D20"
b$V4 <- "Passed clan_check filter"
c$V4 <- "Failed clan_check filter"

d <- rbind(d, b, c)



a <- read.table("Sim_All.txt", sep="\t")
b <- read.table("Sim_pass.txt", sep="\t")

c <- subset(a, !(V1 %in% b$V1))

b$V3 <- "Simion2017"
c$V3 <- "Simion2017"
b$V4 <- "Passed clan_check filter"
c$V4 <- "Failed clan_check filter"

d <- rbind(d, b, c)

a <- read.table("Whe_All.txt", sep="\t")
b <- read.table("Whe_pass.txt", sep="\t")

c <- subset(a, !(V1 %in% b$V1))

b$V3 <- "Whelan2017MCRS"
c$V3 <- "Whelan2017MCRS"
b$V4 <- "Passed clan_check filter"
c$V4 <- "Failed clan_check filter"

d <- rbind(d, b, c)
d$V3 <- factor(d$V3, levels=c("Chang2015", "Whelan2015D10", "Whelan2015D20", "Simion2017", "Whelan2017MCRS"))

g <- ggplot(d, aes(x=V3, y=V2)) + geom_boxplot(aes(fill=V4)) + stat_compare_means(aes(group = V4), label="p.signif") +
  labs(x = "Dataset", y = "Orthogroup RCFV Value") + theme_bw() + scale_fill_manual(values=c("#ff9933", "#3399ff")) +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), axis.text.x=element_text(angle=45, hjust=1, size=10),axis.text.y=element_text(size=10))

plot(g)