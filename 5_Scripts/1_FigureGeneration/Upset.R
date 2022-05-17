library(UpSetR)

overlap <- read.table(file = "presence.txt", sep="\t", header = TRUE)
o <- as.data.frame(lapply(overlap[2:6], function(x) as.numeric(as.character(x))))
u <- upset(o, order.by = "freq", sets = rev(colnames(o)),
           mainbar.y.label = "# Human genes overlapping between datasets",
           sets.x.label = "# Human genes per dataset", keep.order = TRUE)
u