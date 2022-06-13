
# Load packages.
library(ggplot2)
library(ggridges)
library(patchwork)
library(reshape2)
library(wesanderson)

# Function to generate a random normal distribution based on
# predicted mean and standard deviation of replicates from PPAs.
genppa <- function(x){
    dist <- c(rnorm(500, mean=as.numeric(x[5]), sd=as.numeric(x[6])))
    return(list(dist))
}

# Function to convert tabular PPA data into long format for ggplot.
longify <- function(x){
  rl <- list()
  for(row in 1:nrow(x)){
      r <- x[row,]
      l <- unlist(x[row,][8])
      for (rep in l){
        n <- c(r[1:7], rep)
        names(n)[8] <- "Replicate"
        rl[[length(rl)+1]] <- n
      }
    }
  return(rl)
}

# File to be loaded in in the format:
# Dataset,Status,Statistic,Obs,Pred,SD,Replicates
# Chang2015,Original,PPA-DIV,4.07969,4.12583,0.00694512,500
p <- read.csv("ppas.csv", header=T)

# Generate normal distribution for each PPA results,
# then longify entire dataframe with individual replicates
# in distribution as values.
p$dist <- apply(p, 1, genppa)
d <- longify(p)

# Generate data frame, tidy up and factor columns.
f <- as.data.frame(do.call(rbind, d))
f$Obs <- as.numeric(f$Obs)
f$Pred <- as.numeric(f$Pred)
f$SD <- as.numeric(f$SD)
f$Replicates <- as.numeric(f$Replicates)
f$Replicate <- as.numeric(f$Replicate)
f$Status <- factor(f$Status, levels=(c("Original", "Filtered")))
f$Statistic <- factor(f$Statistic, levels=(c("PPA-DIV", "PPA-CONV", "PPA-VAR", "PPA-MAX", "PPA-MEAN")))
f$Dataset <- factor(f$Dataset, levels=c("Chang2015", "Whelan2015_D10", "Whelan2015_D20", "Simion2017", "Whelan2017_MCRS"))

# Generate Z-scores for each dataset, given observed and predicted mean and SD.
f$Z <- apply(f, 1, function(x){return((as.numeric(x[4]) - as.numeric(x[5]))/as.numeric(x[6]))})

# Generate PPA-DIV plots.
o <- subset(f, Statistic == "PPA-DIV")
g1 <- ggplot(o, aes(x=Replicate, y=Status, group=Status)) + geom_density_ridges(scale = 0.8, fill = "lightblue") +
  geom_segment(aes(x = Obs, y = as.numeric(Status) + 0.2, xend = Obs, yend = Status),
               colour = "red", stat = "unique", arrow = arrow(length = unit(0.2, "cm"))) +
  geom_label(aes(x = Obs, y = as.numeric(Status) + 0.4,
                label = paste0("|Z| = ", as.character(abs(round(Z, 2))))),
                stat = "unique", colour = "black", hjust="inward") +
  facet_grid(rows = vars(Statistic), cols = vars(Dataset), scales="free") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), axis.title.x=element_blank())

# Generate PPA-CONV plots.
o <- subset(f, Statistic == "PPA-CONV")
g2 <- ggplot(o, aes(x=Replicate, y=Status, group=Status)) + geom_density_ridges(scale = 0.8, fill = "lightblue") +
  geom_segment(aes(x = Obs, y = as.numeric(Status) + 0.2, xend = Obs, yend = Status),
               colour = "red", stat = "unique", arrow = arrow(length = unit(0.2, "cm"))) +
  geom_label(aes(x = Obs, y = as.numeric(Status) + 0.4,
                 label = paste0("|Z| = ", as.character(abs(round(Z, 2))))),
             stat = "unique", colour = "black", hjust="inward") +
  facet_grid(rows = vars(Statistic), cols = vars(Dataset), scales="free") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), axis.title.x=element_blank())

# Generate PPA-VAR plots.
o <- subset(f, Statistic == "PPA-VAR")
g3 <- ggplot(o, aes(x=Replicate, y=Status, group=Status)) + geom_density_ridges(scale = 0.8, fill = "lightblue") +
  geom_segment(aes(x = Obs, y = as.numeric(Status) + 0.2, xend = Obs, yend = Status),
               colour = "red", stat = "unique", arrow = arrow(length = unit(0.2, "cm"))) +
  geom_label(aes(x = Obs, y = as.numeric(Status) + 0.4,
                 label = paste0("|Z| = ", as.character(abs(round(Z, 2))))),
             stat = "unique", colour = "black", hjust="inward") +
  facet_grid(rows = vars(Statistic), cols = vars(Dataset), scales="free") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), axis.title.x=element_blank(),  strip.text.x = element_blank())

# Generate PPA-MAX plots.
o <- subset(f, Statistic == "PPA-MAX")
g4 <- ggplot(o, aes(x=Replicate, y=Status, group=Status)) + geom_density_ridges(scale = 0.8, fill = "lightblue") +
  geom_segment(aes(x = Obs, y = as.numeric(Status) + 0.2, xend = Obs, yend = Status),
               colour = "red", stat = "unique", arrow = arrow(length = unit(0.2, "cm"))) +
  geom_label(aes(x = Obs, y = as.numeric(Status) + 0.4,
                 label = paste0("|Z| = ", as.character(abs(round(Z, 2))))),
             stat = "unique", colour = "black", hjust="inward") +
  facet_grid(rows = vars(Statistic), cols = vars(Dataset), scales="free") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), strip.text.x = element_blank()) +
  labs(x = "Predicted range of simulated replicates")

# Generate PPA-MEAN plots.
o <- subset(f, Statistic == "PPA-MEAN")
g5 <- ggplot(o, aes(x=Replicate, y=Status, group=Status)) + geom_density_ridges(scale = 0.8, fill = "lightblue") +
  geom_segment(aes(x = Obs, y = as.numeric(Status) + 0.2, xend = Obs, yend = Status),
               colour = "red", stat = "unique", arrow = arrow(length = unit(0.2, "cm"))) +
  geom_label(aes(x = Obs, y = as.numeric(Status) + 0.4,
                 label = paste0("|Z| = ", as.character(abs(round(Z, 2))))),
             stat = "unique", colour = "black", hjust="inward") +
  facet_grid(rows = vars(Statistic), cols = vars(Dataset), scales="free") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(),  strip.text.x = element_blank()) +
  labs(x = "Predicted range of simulated replicates")

# Generate colour palette.
w <- wes_palette(name="Zissou1", n=5)[2:5]

# Data from QMaker analysis to be read in as:
# Dataset	Q	LG	JTT	WAG	Status
# Chang2015	94	6	0	0	Original
d <- read.table("QMaker.tab", header=T)

# Factoring dataset and melting on Q, LG, JTT and WAG columns.
d$Dataset <- factor(d$Dataset, levels=c("Chang2015", "Whelan2015_D10", "Whelan2015_D20", "Simion2017", "Whelan2017_MCRS"))
d$Status <- factor(d$Status, levels=c("Original", "Filtered"))
m <- melt(d)

# Generate QMaker plot as faceted, stacked barchart.
g <- ggplot(m, aes(x=Status, y=value, fill=variable)) + geom_bar(position="fill", stat="identity", colour = "black") +
  geom_text(data=subset(m, value > 1), aes(x=Status, y=value, label=value), size=6, colour = "black", position=position_fill(vjust = 0.5)) +
  facet_grid(cols = vars(Dataset)) + theme_bw() + scale_fill_manual(values=w) +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), axis.text.x=element_text(angle=45, hjust=1, size=10),axis.text.y=element_text(size=10)) +
  labs(x = "Subset", y = "% of test OGs with best-fit substitution model (BIC)") + scale_y_continuous(labels = scales::percent)

# Plot PPA-DIV and MAX plots alongside QMaker plot.
plot(g1 / g4 | g) + plot_layout(widths = c(1.5,1.25))

# Plot other PPA results.
plot(g2 / g3 / g5)