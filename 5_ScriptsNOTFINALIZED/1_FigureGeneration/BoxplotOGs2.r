library(ggplot2)
library(dplyr)
library(forcats)
library(reshape)
library(wesanderson)
library(patchwork)
library(data.table)
library(janitor)
library(ggpubr)

addClans <- function(df, c1){
    df$clan <- apply(df, 1, function(x) {
    if ( (as.character(x["V1"])) %in% c1[[1]]) {
        "Outgroup"
    } else if ( ( x["V1"]) %in% c1[[2]]) {
        "Porifera"
    } else if ( ( x["V1"]) %in% c1[[3]]) {
        "Ctenophora"
    } else if ( ( x["V1"]) %in% c1[[4]]) {
        "Placozoa"
    } else if ( ( x["V1"]) %in% c1[[5]]) {
        "Cnidaria"
    } else if ( ( x["V1"]) %in% c1[[6]]) {
        "Bilateria"
    } else {
        "Other"
    }
})
return(df)
}

getSums <- function(df, c1){
    sf <- data.frame(names(changclans))
    n <- c()
    for(v in 2:(length(df)-1))
    {
        for (c in (1:length(c1)))
        {
            n <- c(n, sum(df[v][df["clan"] == c1[c]]))
        }
        sf[v] <- n
        n <- c()
    }
    return(sf)
}

changpam <- read.table("chang.txt", sep=" ")
changclans <- strsplit(readLines("chang_clans.txt"), " ")
names(changclans) <- c("Outgroup", "Porifera", "Ctenophora", "Placozoa", "Cnidaria", "Bilateria")
changpam <- addClans(changpam, changclans)
changsums <- getSums(changpam, names(changclans))
changsums <- transpose(changsums) %>% row_to_names(row_number = 1) %>% "rownames<-" (NULL)
changpassorder <- scan("changpassorder.txt")
changsums$Subset <- "Fail"
changsums$Subset[changpassorder] <- "Pass"
changsums$name <- "Chang2015 (Fail: 166, Pass: 34)"
changsums <- melt(changsums, id.vars = c("name", "Subset"))
changsums$variable <- as.factor(changsums$variable)
changsums$value <- as.numeric(changsums$value)

d10pam <- read.table("d10.txt", sep=" ")
d10clans <- strsplit(readLines("d10_clans.txt"), " ")
names(d10clans) <- c("Outgroup", "Porifera", "Ctenophora", "Placozoa", "Cnidaria", "Bilateria")
d10pam <- addClans(d10pam, d10clans)
d10sums <- getSums(d10pam, names(d10clans))
d10sums <- transpose(d10sums) %>% row_to_names(row_number = 1) %>% "rownames<-" (NULL)
d10passorder <- scan("d10passorder.txt")
d10sums$Subset <- "Fail"
d10sums$Subset[d10passorder] <- "Pass"
d10sums$name <- "Whelan2015D10 (Fail: 170, Pass: 40)"
d10sums <- melt(d10sums, id.vars = c("name", "Subset"))
d10sums$variable <- as.factor(d10sums$variable)
d10sums$value <- as.numeric(d10sums$value)

simpam <- read.table("sim.txt", sep=" ")
simclans <- strsplit(readLines("sim_clans.txt"), " ")
names(simclans) <- c("Outgroup", "Porifera", "Ctenophora", "Placozoa", "Cnidaria", "Bilateria")
simpam <- addClans(simpam, simclans)
simsums <- getSums(simpam, names(simclans))
simsums <- transpose(simsums) %>% row_to_names(row_number = 1) %>% "rownames<-" (NULL)
simpassorder <- scan("simpassorder.txt")
simsums$Subset <- "Fail"
simsums$Subset[simpassorder] <- "Pass"
simsums$name <- "Simion2017 (Fail: 1262, Pass: 457)"
simsums <- melt(simsums, id.vars = c("name", "Subset"))
simsums$variable <- as.factor(simsums$variable)
simsums$value <- as.numeric(simsums$value)

whepam <- read.table("whe17.txt", sep=" ")
wheclans <- strsplit(readLines("whe17_clans.txt"), " ")
names(wheclans) <- c("Outgroup", "Porifera", "Ctenophora", "Placozoa", "Cnidaria", "Bilateria")
whepam <- addClans(whepam, wheclans)
whesums <- getSums(whepam, names(wheclans))
whesums <- transpose(whesums) %>% row_to_names(row_number = 1) %>% "rownames<-" (NULL)
whepassorder <- scan("whe17passorder.txt")
whesums$Subset <- "Fail"
whesums$Subset[whepassorder] <- "Pass"
whesums$name <- "Whelan2017 (Fail: 85, Pass: 42)"
whesums <- melt(whesums, id.vars = c("name", "Subset"))
whesums$variable <- as.factor(whesums$variable)
whesums$value <- as.numeric(whesums$value)

d20pam <- read.table("d20.txt", sep=" ")
d20clans <- strsplit(readLines("d20_clans.txt"), " ")
names(d20clans) <- c("Outgroup", "Porifera", "Ctenophora", "Placozoa", "Cnidaria", "Bilateria")
d20pam <- addClans(d20pam, d20clans)
d20sums <- getSums(d20pam, names(d20clans))
d20sums <- transpose(d20sums) %>% row_to_names(row_number = 1) %>% "rownames<-" (NULL)
d20passorder <- scan("d20passorder.txt")
d20sums$Subset <- "Fail"
d20sums$Subset[d20passorder] <- "Pass"
d20sums$name <- "Whelan2015D20 (Fail: 149, Pass: 29)"
d20sums <- melt(d20sums, id.vars = c("name", "Subset"))
d20sums$variable <- as.factor(d20sums$variable)
d20sums$value <- as.numeric(d20sums$value)

changsums <- rbind(changsums, d10sums, d20sums, simsums, whesums)
changsums$name <- factor(changsums$name,
    levels = c("Chang2015 (Fail: 166, Pass: 34)", "Whelan2015D10 (Fail: 170, Pass: 40)", "Whelan2015D20 (Fail: 149, Pass: 29)", "Simion2017 (Fail: 1262, Pass: 457)", "Whelan2017 (Fail: 85, Pass: 42)"))
p <- ggplot(changsums, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Subset)) + stat_compare_means(aes(group = Subset), label="p.signif") +
  scale_fill_manual(values=c("#3B9AB2", "#F21A00")) +facet_wrap(~name, scales = "free_y", nrow=2) +
  labs(x = "Clans", y = "# Taxa in OGs") + theme_bw() +
  scale_fill_manual(values=c("#ff9933", "#3399ff"), labels = c("Failed clan_check filter", "Passed clan_check filter")) +
  #stat_summary(fun=mean, aes(group=Subset), geom="point", shape=23, size=2, color="black", fill="white", position = position_dodge(0.75)) +
  theme(panel.grid.minor = element_blank(), legend.title = element_blank(), axis.text.x=element_text(angle=45, hjust=1, size=10),axis.text.y=element_text(size=10))
plot(p)

