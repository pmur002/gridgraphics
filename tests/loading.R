setwd('H:/New folder/gridgraphics/R')
filename = list.files()

for(i in 1:length(filename))
{
	source(filename[i])
}