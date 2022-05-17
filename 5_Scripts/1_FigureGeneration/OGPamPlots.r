library(ggplot2)
library(dplyr)
library(forcats)
library(reshape)
library(wesanderson)
library(patchwork)

df1 <- read.table("d20.txt", sep=" ")
df1 <- df1[,1:2]
c1 <- strsplit(readLines("d20_clans.txt"), " ")
names(c1) <- c("Outgroup", "Porifera", "Ctenophora", "Placozoa", "Cnidaria", "Bilateria")

df1$clan <- apply(df1, 1, function(x) {
    if ( (as.character(x["V1"])) %in% c1[[1]]) {
        "Outgroup"
    } else if ( ( x["V1"]) %in% c1[[2]]) {
        "Porifera"
    } else if ( ( x["V1"]) %in% c1[[3]]) {
        "Ctenophora"
    } else if ( ( x["V1"]) %in% c1[[4]]) {
        ""
    } else if ( ( x["V1"]) %in% c1[[5]]) {
        "Cnidaria"
    } else if ( ( x["V1"]) %in% c1[[6]]) {
        "Bilateria"
    } else {
        "Other"
    }
})

coln <- as.numeric(ncol(df1)-1)

df1$total <- rowSums(df1[,2:coln])
df1$perc <- (df1$total / (coln - 1)) * 100
xtitle <- paste("Orthogroups (n = ", as.character(coln - 1),")", sep="")

mf1 <- melt(df1, id.vars=c("V1", "clan", "total", "perc"))
mf1$clan <- factor(mf1$clan,
            levels=c("Outgroup", "Porifera", "Ctenophora", "", "Cnidaria", "Bilateria"))

mf1 <- mf1 %>% arrange(variable, clan)
mf1$V1 <- factor(mf1$V1, levels = rev(unique(mf1$V1)))

g <- ggplot(mf1, aes(x=variable,y=V1,fill=value)) +
     geom_tile(color="white", lwd = 0.1, linetype = 1) +
     theme_bw() + theme(legend.position="none") + facet_grid(clan ~ ., scales="free_y", space="free_y") +
     theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), strip.text.y = element_blank(),
     axis.title.y = element_blank()) + xlab(xtitle)

g2 <- ggplot(mf1, aes(x=perc, y=V1)) + geom_bar(stat="identity", position="dodge", width=0.8) +
      theme_bw() + theme(legend.position="none") + facet_grid(clan ~ ., scales="free_y", space="free_y") +
      theme(strip.text.y = element_text(size = 8),
      axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y = element_blank()) +
      xlab("% in orthogroups")
plot(g + g2 + plot_layout(widths=c(3, 1)))