rm(list=ls())
filelist <- list.files(path=getwd(), pattern=".csv")
filename <- substr(filelist, 1, nchar(filelist)-4)
DB <- lapply(filelist, read.csv, header=T)

for ( i in 1:length(filename)) {
  assign(filename[i], DB[[i]])
}
