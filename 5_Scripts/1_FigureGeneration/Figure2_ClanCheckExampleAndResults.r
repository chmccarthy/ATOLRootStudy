# Check packages.
required <- c("ggplot2", "ggtree", "patchwork", "wesanderson")
missing <- required[!(required %in% installed.packages()[, "Package"])]
if(length(missing)) {
  install.packages(missing)
}

# Load packages.
library(ggplot2)
library(ggtree)
library(patchwork)
library(wesanderson)

# Load trees for example image: sp = Species tree, t1 = tree that recover clan,
# t2 = tree that can't recover clan, t3 = tree that only has 1 taxon from clan.
sp <- read.tree(text="((((A,B),(C,D)),((E,F), H)));")
t1 <- read.tree(text="(((A,B),D),E,F);")
t2 <- read.tree(text="(((A,B),C),(D,E),F);")
t3 <- read.tree(text="((E,F),H,A);")

# Highlight A+B+C+D clan in species tree and example trees.
sp <- groupOTU(sp, c("A", "B", "C", "D"), "Clan")
t1 <- groupOTU(t1, c("A", "B", "C", "D"), "Clan")
t2 <- groupOTU(t2, c("A", "B", "C", "D"), "Clan")
t3 <- groupOTU(t3, c("A"), "Clan")

# Draw species and example trees, define colour scheme for
# recovered / not recovered / <2 taxa as in bar chart below.
s1 <- ggtree(sp, aes(color=Clan)) + geom_tiplab() + theme(legend.position = "none", plot.title = element_text(hjust=0.5)) + ggtitle("Species tree") +
    xlab("\"Incontestable\" clan (A+B+C+D) to be assessed by clan_check.") + scale_color_manual(values=c("black", "#46ACC8"))
g1 <- ggtree(t1, aes(color=Clan), layout="daylight", branch.length = "none") + geom_tiplab() + hexpand(0.5) +
    theme(legend.position = "none") + scale_color_manual(values=c("black", "#46ACC8")) +
    xlab("Gene tree recovers clan (A+B+D) congruent with clan (A+B+C+D).") + ggtitle("Unrooted gene trees")
g2 <- ggtree(t2, aes(color=Clan), layout="daylight") + geom_tiplab() + hexpand(0.5) +
    xlab("Gene tree can't recover clan (A+B+C+D), no split exists where taxa A-D separate from taxa E-F.") +
    scale_color_manual(values=c("black", "#E2D200")) + theme(legend.position = "none")
g3 <- ggtree(t3, aes(color=Clan), layout="daylight") + geom_tiplab() + hexpand(0.5) +
    xlab("Gene tree can't recover clan (A+B+C+D), <2 member taxa present in gene tree.") +
    scale_color_manual(values=c("black", "#DD8D29")) + theme(legend.position = "none")

# Generate data for clan_check results in five AToL datasets.
sets <- c(rep("Chang2015", 15), rep("Whelan2015D10", 15), rep("Whelan2015D20", 15), rep("Simion2017", 15), rep("Whelan2017MCRS", 15))
clans <- c(rep("Outgroups" , 3) , rep("Porifera" , 3) , rep("Ctenophora" , 3) , rep("Cnidaria" , 3), rep("Bilateria", 3) )
status <- rep(c("Recovered" , "Not Recovered" , "<2 taxa") , 5)
value <- c(c(71,129, 0), c(8, 192, 0), c(197, 3, 0), c(17, 183, 0), c(38, 162, 0),
           c(58,152,0), c(13, 197, 0), c(181, 18, 11), c(41, 168, 1), c(46, 162, 2),
           c(44, 134, 0), c(15, 162, 1), c(143,15, 20), c(31, 147, 0), c(39, 137, 2),
           c(655, 1064, 0), c(98, 1621, 0), c(1635, 84, 0), c(502, 1216, 1), c(447, 1271, 1),
           c(92, 33, 2), c(11, 116, 0), c(40, 25, 62), c(53, 74, 0), c(57, 70, 0))
data <- data.frame(sets,clans,status,value)

# Add some factoring for ggplot and facet_wrap.
data$clans_f <- factor(data$clans, levels=c("Outgroups", "Porifera", "Ctenophora", "Cnidaria", "Bilateria"))
data$status_f <- factor(data$status, levels=c("<2 taxa", "Not Recovered", "Recovered"))
data$sets_f <- factor(data$sets, levels=c("Chang2015", "Whelan2015D10", "Whelan2015D20", "Simion2017", "Whelan2017MCRS"))
ogs <- c("Chang_2015 (200 OGs)", "Whelan2015_D10 (210 OGs)", "Whelan2015_D20 (178 OGs)", "Simion2017 (1719 OGs)", "Whelan2017_MCRS (127 OGs)")
names(ogs) <- levels(data$sets_f)

# Draw faceted, stacked barcharts for clan_check results across five datasets.
p <- ggplot(data, aes(fill=status_f, x=clans_f, y=value)) + 
  geom_bar(position="fill", stat="identity", colour = "black") + 
  geom_text(data=subset(data,value > 5),aes(x=clans_f, y=value, label=value), size=5, position=position_fill(vjust = 0.5)) + 
  facet_wrap(~sets_f, ncol=5, scales = "free_x", labeller=labeller(sets_f = ogs)) +
  scale_fill_manual(values=wes_palette(n=3, name="FantasticFox1")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), legend.position = "right",
        strip.text.x = element_text(size=10),
        axis.text.y=element_text(angle=25, hjust=1), axis.text.x=element_text(angle=25, hjust=1)) +
  labs(y = "% of OGs in Dataset", x = "Clans tested (excl. Placozoa)") + scale_y_continuous(labels = scales::percent)

# Plot clan_check example figures on top, clan_check results below.
plot((s1 | g1 + g2 + g3 + plot_layout(nrow=3, ncol=1)) / p)