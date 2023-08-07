setwd("~/Downloads/")

library(dplyr)
library(data.table)

df <- as.data.frame(fread("metadata.tsv"))
#first we check if all the Angolan sequences match that on GISAID
Anogla <- subset(df, df$country=="Angola")
#okay so now we see that they dont so lets exclude Angola and grab them from GISAID directly
df <- subset(df, df$country!="Angola")

# grab all C.16 and B.1.1.275 which we will include from the whole world
C.16 <- subset(df, df$pango_lineage=="C.16")
B.1.1.275 <- subset(df, df$pango_lineage=="B.1.1.275")
include.all <- rbind(C.16, B.1.1.275)

# include random sample
Portugal <- subset(df, df$country=="Portugal")
Portugal <- Portugal[sample(nrow(Portugal), 2500),]
DRC <- subset(df, df$country=="Democratic Republic of the Congo")
Namibia <- subset(df, df$country=="Namibia")
Zambia <- subset(df, df$country=="Zambia")
Neighbouring <- rbind(DRC, Namibia, Zambia)
Neighbouring <- Neighbouring[sample(nrow(Neighbouring), 1250),]

include <- rbind(Neighbouring, Portugal, include.all)
write.table(include, file = "include.tsv", row.names = F, sep = "\t", quote = F)
