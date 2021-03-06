# Check packages.
required <- c("ggimage", "ggplot2", "ggtree", "patchwork")
missing <- required[!(required %in% installed.packages()[, "Package"])]
if(length(missing)) {
  install.packages(missing)
}

# Load packages.
library(ggimage)
library(ggplot2)
library(ggtree)
library(patchwork)


# Load three topologies: t1 = Porifera-sister, t2 = Ctenophora-sister, t3 = Paranimalia.
t1 <- read.tree(text="(((((Bilateria,Cnidaria),(Placozoa)),(Ctenophora)),Porifera),Outgroups);")
t2 <- read.tree(text="(((((Bilateria,Cnidaria),(Placozoa)),(Porifera)),Ctenophora),Outgroups);")
t3 <- read.tree(text="(((((Bilateria,Cnidaria),(Placozoa)),(Ctenophora,Porifera))),Outgroups);")

# Define matrix with names associated to PhyloPic UIDs, in order of the Paranimalia tree.
d <- data.frame(matrix(0,nrow=6,ncol=2))
rownames(d) <- t3$tip.label
colnames(d) <- c("name", "uid")
d$name <- t3$tip.label
d$uid <- c(1:length(t3$tip.label))
d[,2][1] <- "0f6af3d8-49d2-4d75-8edf-08598387afde"
d[,2][2] <- "839b9df7-c97f-444a-a611-95609e950960"
d[,2][3] <- "87e2d814-56f7-45bc-82e3-bed99c8c7f3a"
d[,2][4] <- "dfc37b82-ff17-4866-abf9-1c405a202a13"
d[,2][5] <- "3449d9ef-2900-4309-bf22-5262c909344b"
d[,2][6] <- "7ca79f27-b79b-4d24-8c22-c30a4c272749"
d$colour <- t3$tip.label

# Define colour scale for PhyloPic images.
scale <- c("Placozoa" = "black", "Outgroups" = "black", "Porifera" = "#d83131", "Ctenophora" = "#9b9b9b",
            "Cnidaria" = "#ad9300", "Bilateria" = "orange")

# Draw trees.
g1 <- ggtree(t1) + geom_tiplab() + hexpand(0.2) + xlab("Porifera-sister (Sponges first)")
g2 <- ggtree(t2) + geom_tiplab() + hexpand(0.2) + xlab("Ctenophora-sister (Comb jellies first)")
g3 <- ggtree(t3) %<+% d + geom_tiplab(aes(image=uid, colour=colour), geom="phylopic", offset = 1.8, size=0.1) + geom_tiplab() + hexpand(0.5) +
    xlab("Paranimalia (Sponges+Comb jellies first)") + scale_color_manual(values=scale) + theme(legend.position="none")

# Plot trees.
plot(g1 + g2 + g3)