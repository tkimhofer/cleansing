
test=add_summary(ds1)
t1=as.data.frame(test)
library(WriteXLS)
WriteXLS("t1", "sums.xlsx", row.names = T, AdjWidth = F, BoldHeaderRow = T, FreezeRow = 1, FreezeCol = 1, AllText = T)
