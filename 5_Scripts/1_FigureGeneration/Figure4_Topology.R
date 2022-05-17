# Check packages.
required <- c("ggplot2", "wesanderson")
missing <- required[!(required %in% installed.packages()[, "Package"])]
if(length(missing)) {
  install.packages(missing)
}

# Load packages.
library(ggplot2)
library(wesanderson)

# Generate data for results of topological support test (AU results added in Inkscape).
sets <- c(rep("Chang2015", 9), rep("Whelan2015_D10", 9), rep("Whelan2015_D20", 9), rep("Simion2017", 9), rep("Whelan2017_MCRS", 9))
status <- c(rep("Original", 3), rep("Passed clan_check filter", 3), rep("Failed clan_check filter", 3))
tops <- rep(c("Porifera-sister", "Ctenophore-sister", "Paranimalia"), 5)
values <- c(c(39, 113, 48), c(6, 24, 4), c(39-6, 113-24, 48-4),
            c(37, 144, 29), c(6, 29, 5), c(31, 144-29, 29-5),
            c(39, 113, 26), c(7, 20, 2), c(32, 92, 25),
            c(397, 995, 327), c(93, 286, 79), c(397-93, 995-286, 327-79),
            c(31, 76, 20), c(11, 21, 10), c(31-11, 76-21, 20-10))

# Do some factoring for ggplot.
d <- data.frame(sets, status, tops, values)
d$top_f <- factor(d$tops, levels=c("Paranimalia", "Ctenophore-sister", "Porifera-sister"))
d$status_f <- factor(d$status, levels=c("Original", "Failed clan_check filter", "Passed clan_check filter"))
d$sets_f <- factor(d$sets, levels=c("Chang2015", "Whelan2015_D10", "Whelan2015_D20", "Simion2017", "Whelan2017_MCRS"))
e <- d

# Draw faceted, stacked barcharts for topology test results.
p <- ggplot(e, aes(fill=top_f, y=values, x=status_f)) + 
  geom_bar(position="fill", stat="identity", colour = "black") + facet_grid(cols = vars(sets_f)) +
  geom_text(data=subset(e,values > 0),aes(y=values, x=status_f, label=values), size=6, position=position_fill(vjust = 0.5)) +
  scale_fill_manual(values=c("#00A08A", "#FAD510", "#0883bf")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), axis.text.x=element_text(angle=45, hjust=1, size=10),axis.text.y=element_text(size=10)) +
  labs(x = "Subset", y = "% of OGs supporting AToL hypothesis") + scale_y_continuous(labels = scales::percent)

# Plot barcharts.
plot(p)