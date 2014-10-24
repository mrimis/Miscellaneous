#Cell growth prediction program using multiple regression model
args <- commandArgs(TRUE)

parseArgs <- function(x) strsplit(sub("^-", "", x), "=")
argsDF <- as.data.frame(do.call("rbind", parseArgs(args)))
argsL <- as.list(as.character(argsDF$V2))

argsL <- argsDF$V1


if(is.null(argsL[2])) {print('Must enter -i')}
if(is.null(argsL[4])) {print('Must enter -o')}

filename = argsL[2]
outputfile = argsL[4]
data=read.table("/Users/mrinalmishra/Documents/thesis/regression/regression_data/Final_Data_RHI19.txt",header=TRUE,sep='')
fit=lm(log(Area)~Time+Cell_line+Drug+PC3_Nmber+LNCAP_Nmbr++Time*Cell_line+Time*Drug+Time*PC3_Nmber+Time*LNCAP_Nmbr+Cell_line*Drug+Cell_line*PC3_Nmber+Cell_line*LNCAP_Nmbr+Drug*PC3_Nmber+Drug*LNCAP_Nmbr+Time*Cell_line*Drug+Time*Cell_line*PC3_Nmber+Time*Cell_line*LNCAP_Nmbr+Time*Drug*PC3_Nmber+Time*Drug*LNCAP_Nmbr+Cell_line*Drug*PC3_Nmber+Cell_line*Drug*LNCAP_Nmbr,data=data)
print(summary(fit))

df = read.table(as.character(filename), header = TRUE,sep="")

output = predict(fit, df)
Area=exp(output)
Area=data.frame(log(Area))
PC3_LNCAP_Ratio=data.frame(df$PC3_LNCAP)
result=cbind(PC3_LNCAP_Ratio,Area)
write.table(result,as.character(outputfile),sep="\t",row.names = FALSE, col.names=TRUE)
