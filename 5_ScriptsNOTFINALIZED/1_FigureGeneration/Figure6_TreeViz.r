
library(ggplot2)
library(patchwork)
library(ggpmisc)
library(ggtree)
library(ggpubr)
library(ape)
library(ggrepel)

chang2015f <- read.tree("Chang2015_filtered.bpcomp.con.tre")
wheland10f <- read.tree("Whelan2015D10_filtered.bpcomp.con.tre")
wheland20f <- read.tree("Whelan2015D20_filtered.bpcomp.con.tre")
simion2017f <- read.tree("Simion2017_filtered.bpcomp.con.tre")
whelan17f <- read.tree("Whelan2017MCRS_filtered.bpcomp.con.tre")
d10holo <- read.tree("D10_Holo.tre")
d20holo <- read.tree("D20_Holo.tre")
changcho <- read.tree("Chang2015Cho.tre")
d10cho <- read.tree("D10Cho.tre")
d20cho <- read.tree("D20Cho.tre")

fig6a <- function(tree, clans, rooting, title, endlabel=FALSE) {
    c <- readLines(clans)
    l <- list()
    l$Outgroups <- unlist(strsplit(c[1], " "))
    l$Porifera <- unlist(strsplit(c[2], " "))
    l$Ctenophora <- unlist(strsplit(c[3], " "))
    l$Placozoa <- unlist(strsplit(c[4], " "))
    l$Cnidaria <- unlist(strsplit(c[5], " "))
    l$Bilateria <- unlist(strsplit(c[6], " "))

    g1 <- groupOTU(tree, l)

    root_node <- MRCA(g1, rooting)
    g1 <- root(g1, node = root_node)

    clades <- c()
    for(i in 1:length(l)) {
        clades[i] <- MRCA(g1, l[[i]])
    }

    g1$node.label[g1$node.label == "1"] <- ""

    g <- ggtree(g1, aes(color=group)) + geom_tiplab() + scale_color_manual(values=scale) + theme(legend.position = "none")

    g$data$label[is.na(as.numeric(g$data$label))] <- ""

    g <- g + geom_label_repel(aes(label=label, color="0"), max.overlaps = Inf, box.padding = 1)

    if(endlabel){
        for(i in 1:length(clades)){
        clade <- clades[[i]]
        name <- names(l[i])
        color <- scale[name]
        if(name != "Outgroups"){
            g <- g + geom_cladelab(node=clade, label=name, textcolor=color, barcolor="white", size = 3)
            }
        }
    }
    g <- g + xlab(title)
    return(g)
}

scale <- c("0" = "black", "Outgroups" = "black", "Porifera" = "#d83131", "Ctenophora" = "#9b9b9b", "Placozoa" = "black",
            "Cnidaria" = "#ad9300", "Bilateria" = "orange")

pics <- data.frame(matrix(0,nrow=5,ncol=2))
rownames(pics) <- c("Porifera", "Ctenophora", "Placozoa", "Cnidaria", "Bilateria")
colnames(pics) <- c("name", "uid")
pics$name <- rownames(pics)
pics$uid <- c("839b9df7-c97f-444a-a611-95609e950960", "87e2d814-56f7-45bc-82e3-bed99c8c7f3a", "dfc37b82-ff17-4866-abf9-1c405a202a13",
            "3449d9ef-2900-4309-bf22-5262c909344b", "7ca79f27-b79b-4d24-8c22-c30a4c272749")

t1 <- fig6a(chang2015f, "chang15/clans.txt", c("Ministeria_vibrans", "Capsaspora_owczarzaki", "Sphaeroforma_arctica", "Amoebidium_parasiticum"), "Chang2015_filtered\n(Outgroups: Holozoa, Choanoflagellates)")
t2 <- fig6a(wheland10f, "whe15/clans.txt", c("Mortierella_verticillata", "Rhizopus_oryzae", "Spizellomyces_punctatus", "Allomyces_macrogynus"), "Whelan2015_D10_filtered\n(Outgroups: Fungi, Holozoa, Choanoflagellates)")
t3 <- fig6a(wheland20f, "d20/clans.txt", c("Mortierella_verticillata", "Rhizopus_oryzae", "Spizellomyces_punctatus", "Allomyces_macrogynus"), "Whelan2015_D20_filtered\n(Outgroups: Fungi, Holozoa, Choanoflagellates)")
t4 <- fig6a(simion2017f, "sim17/clans.txt", c("Pirum_gemmata", "Abeoforma_whisleri", "Amoebidium_parasiticum", "Creolimax_fragrantissima", "Sphaeroforma_arctica"), "Simion2017_filtered\n(Outgroups: Holozoa, Choanoflagellates)")
t5 <- fig6a(whelan17f, "whe17/clans.txt", c("Monosiga_ovata", "Salpingoeca_pyxidium", "Salpingoeca_rosetta", "Monosiga_brevicolis", "Acanthoeca_sp"), "Whelan2017_MCRS_filtered\n(Outgroups: Choanoflagellates)")

h1 <- fig6a(d10holo, "d10holocans.txt", c("Sphaeroforma_arctica", "Amoebidium_sp"), "Whelan2015_D10_filteredHolo")
h2 <- fig6a(d20holo, "d10holocans.txt", c("Sphaeroforma_arctica", "Amoebidium_sp"), "Whelan2015_D20_filteredHolo")

c1 <- fig6a(changcho, "changchoclans.txt", c("Acanthoeca_sp", "Stephanoeca_diplocostata", "Monosiga_brevicollis", "Monosiga_ovata", "Salpingoeca_sp"), "Chang2015_filteredChoano")
c2 <- fig6a(d10cho, "d10choclans.txt", c("Monosiga_ovata", "Salpingoeca_rosetta"), "Whelan2015_D10_filteredChoano")
c3 <- fig6a(d20cho, "d10choclans.txt", c("Monosiga_ovata", "Salpingoeca_rosetta"), "Whelan2015_D20_filteredChoano")

p1 <- (t1 + t2 + t3 + t4 + t5) + plot_layout(nrow=1, ncol=5) + plot_annotation(title="(a) CAT-GTR+G phylogenies of filtered AToL datasets.")
p2 <- (t1 + h1 + h2 + t4) + plot_layout(nrow=1, ncol=5) + plot_annotation(title="(b) CAT-GTR+G phylogenies of filtered AToL datasets with Holozoan+Choanoflagellate outgroups (excl. Whelan2017_MCRS_filtered).")
p3 <- (c1 + c2 + c3 + t4 + t5) + plot_layout(nrow=1, ncol=5) + plot_annotation(title="(c) CAT-GTR+G phylogenies of filtered AToL datasets with Choanoflagellate outgroups (note: yet to get Simion2017_filteredChoano tree).")

pp <- wrap_elements(p1) / wrap_elements(p2) / wrap_elements(p3)
plot(pp)