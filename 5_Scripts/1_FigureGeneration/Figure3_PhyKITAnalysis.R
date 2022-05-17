# Check packages.
required <- c("ggplot2", "ggpubr", "patchwork")
missing <- required[!(required %in% installed.packages()[, "Package"])]
if(length(missing)) {
  install.packages(missing)
}

# Load packages.
library(ggplot2)
library(patchwork)
library(ggpubr)

# File to be read consists of seven lines per component orthogroup
# with results of seven PhyKIT analyses in following format:
# datasetname <tab> Pass or Fail <tab> PhyKIT analysis <tab> Value
phykit <- read.csv("filename_here", sep = "\t", header=FALSE)

# Tidy up values in table, do some factoring for ggplot.
phykit$V1[phykit$V1 == "chang"] <- "Chang2015"
phykit$V1[phykit$V1 == "whe15"] <- "Whelan2015_D10"
phykit$V1[phykit$V1 == "sim"] <- "Simion2017"
phykit$V1[phykit$V1 == "whe17"] <- "Whelan2017_MCRS"
phykit$V1[phykit$V1 == "whed20"] <- "Whelan2015_D20"
phykit$V2[phykit$V2 == "Pass"] <- "Passed clan_check filter"
phykit$V2[phykit$V2 == "Fail"] <- "Failed clan_check filter"
phykit$V1 <- factor(phykit$V1, levels = c("Chang2015", "Whelan2015_D10", "Whelan2015_D20",
                                      "Simion2017", "Whelan2017_MCRS"))
phykit$V2 <- factor(phykit$V2, levels = c("Passed clan_check filter", "Failed clan_check filter"))

# Draw boxplots for each PhyKIT analysis for passing/failing orthogroups
# in each dataset with significance included.
a <- ggplot(subset(pass, V3 == "Alignment length"), aes(x=V1, y=V4)) + geom_boxplot(aes(fill = V2)) +
     theme_classic() + ggtitle("Alignment length") + scale_fill_manual(values = alpha(c("#1c9099", "#fe9929"), 0.8)) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           plot.title = element_text(face = "bold"), legend.title = element_blank(), legend.position = "none") +
     stat_compare_means(aes(group = V2), label="p.signif")
b <- ggplot(subset(pass, V3 == "Mean bipartition support"), aes(x=V1, y=V4)) + geom_boxplot(aes(fill = V2)) +
     theme_classic() + ggtitle("Mean bipartition support") + scale_fill_manual(values = alpha(c("#1c9099", "#fe9929"), 0.8)) +
     theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           plot.title = element_text(face = "bold"), legend.title = element_blank(), legend.position = "none") +
     stat_compare_means(aes(group = V2), label="p.signif")
c <- ggplot(subset(pass, V3 == "Mean long branch score"), aes(x=V1, y=V4)) + geom_boxplot(aes(fill = V2)) +
  theme_classic() + ggtitle("Mean long branch score") + scale_fill_manual(values = alpha(c("#1c9099", "#fe9929"), 0.8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold"), legend.title = element_blank(), legend.position = "none") +
  stat_compare_means(aes(group = V2), label="p.signif")
d <- ggplot(subset(pass, V3 == "Parsimony informative sites"), aes(x=V1, y=V4)) + geom_boxplot(aes(fill = V2)) +
  theme_classic() + ggtitle("Parsimony informative sites") + scale_fill_manual(values = alpha(c("#1c9099", "#fe9929"), 0.8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold"), legend.title = element_blank(), legend.position = "none") +
  stat_compare_means(aes(group = V2), label="p.signif")
e <- ggplot(subset(pass, V3 == "Saturation"), aes(x=V1, y=V4)) + geom_boxplot(aes(fill = V2)) +
  theme_classic() + ggtitle("Saturation") + scale_fill_manual(values = alpha(c("#1c9099", "#fe9929"), 0.8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold"), legend.title = element_blank(), legend.position = "none") +
  stat_compare_means(aes(group = V2), label="p.signif")
f <- ggplot(subset(pass, V3 == "Treeness by RCV"), aes(x=V1, y=V4)) + geom_boxplot(aes(fill = V2)) +
  theme_classic() + ggtitle("Treeness by RCV") + scale_fill_manual(values = alpha(c("#1c9099", "#fe9929"), 0.8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold"), legend.title = element_blank(), legend.position = "none") +
  stat_compare_means(aes(group = V2), label="p.signif")
g <- ggplot(subset(pass, V3 == "Variable sites"), aes(x=V1, y=V4)) + geom_boxplot(aes(fill = V2)) +
  theme_classic() + ggtitle("Variable sites") + scale_fill_manual(values = alpha(c("#1c9099", "#fe9929"), 0.8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(face = "bold"), legend.position = "bottom") +
  stat_compare_means(aes(group = V2), label="p.signif", bracket.size = 0.5) +labs(fill = "Subset")

# Plot all seven boxplots together.
p <- a + b + c + d + e + f + g + plot_layout(ncol = 4)
plot(p)
