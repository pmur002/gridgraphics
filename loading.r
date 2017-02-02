#path = 'C:/Users/yeamin/Desktop/master/gridgraphics-master/R/'
path = 'C:/Users/yeamin/Desktop/mproject/gridgraphics/R/'

files = list.files(path)
for(i in 1:length(files)){
	source(paste(path, files[i], sep = ''))
}

library(grid)