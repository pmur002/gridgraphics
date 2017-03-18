setwd('C:/Users/yeamin/Desktop/master/MasterProject/gridGraphics/gridgraphics/R')
filename = list.files()

for(i in 1:length(filename))
{
	source(filename[i])
}