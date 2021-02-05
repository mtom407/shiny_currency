# HELPER SCRIPT FOR THE RMD

# just reading the data in 

path1 = "comparisonFrame.csv"
path2 = "seriesHelperFrame.csv"
path3 = "mapHelperFrame.csv"
path4 = "miscFrame.csv"

cF = read.table(file=path1,sep=";",dec=",")
sHF = read.table(file=path2,sep=";",dec=",")
mHF = read.table(file=path3,sep=";",dec=",")

miscFrame = read.table(file=path4,sep=";",dec=",")
params = list(curr1 = miscFrame$V1[2], curr2 = miscFrame$V2[2], rflag=miscFrame$V3[2])
