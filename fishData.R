## this is code to convert the names found in the fish data set to english

fish<-read.table("http://www.hi.is/~gunnar/kennsla/alsm/data/set103.dat",header=T)
nameCode <- data.frame(islansk=names(fish), english=c('number', 'spp', 'year', 'month', 'stat.rectangle', 'date', 'depth','gear', 'length', 'sex', 'maturation', 'age', 'live.wt','gutted.wt', 'liver.wt'))

if (english==T) {
    names(fish)[names(fish)==nameCode$islansk] <- nameCode$english
}