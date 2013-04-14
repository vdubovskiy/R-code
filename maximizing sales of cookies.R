require(lattice)
require(foreign)

source("~/R Tools/Stats.R")

source("develop.mixed.R")

## Read in and adjust Data ##
eg.df <- read.spss("Earthgrains Sales Revenue.sav",to.data.frame=TRUE,
	trim.factor.names=TRUE, max.value.labels=100)
eg.df$Store <- as.factor(eg.df$Store)
eg.df$Location <- as.factor(eg.df$Location)

# add ADA column (can do having done normality analysis for cells)
cell.mean.map <- with(eg.df,tapply(Sales,list(Location,Store),mean))
eg.df$SalesADA <- with(eg.df,abs(Sales - cell.mean.map[cbind(Location,Store)]))


## Descriptive Statistics ##
desc.list <- with (eg.df, tapply(1:nrow(eg.df),list(Store,Location),
	function(x,x.df) {
		data.frame(Location=x.df$Location[x[1]],Store=x.df$Store[x[1]],
			n=length(x),mean=mean(x.df$Sales[x]),sd=sd(x.df$Sales[x]),
			var=var(x.df$Sales[x]),min=min(x.df$Sales[x]),
			max=max(x.df$Sales[x]),range=diff(range(x.df$Sales[x]))
		)
	},eg.df)
)
write.table(do.call(rbind,desc.list),row.names=FALSE,quote=FALSE,sep=";",
	file="clipboard")

histogram(~Sales,data=eg.df,type="count",
	main="Histogram of Sales")

histogram(~Sales|Location,data=eg.df,type="count")

# Based on the histogram: location 4 is best, no interaction with store
histogram(~Sales|Location+Store,data=eg.df,type="count",
	par.strip.text=list(cex=0.6))


## Shape (normality) ##

# by cell mean

cell.mean <- with (eg.df,tapply(Sales,list(Store,Location),mean))
write.table(cell.mean,quote=TRUE,row.names=FALSE,sep="\t",
	file="store.mean.by.location")
cell.ada.mean <- with (eg.df,tapply(SalesADA,list(Store,Location),mean))
write.table(cell.ada.mean,quote=TRUE,row.names=FALSE,sep="\t",
	file="store.ada.mean.by.location")


store.means <- with (eg.df,tapply(1:nrow(eg.df),Store,function (x,x.df) {
		data.frame (
			loc.mean=mean(x.df$Sales[x]),
			loc.ada.mean=mean(x.df$SalesADA[x])
		)
	}, eg.df)
)
store.means.df <- do.call(rbind,store.means)
write.table(store.means.df,row.names=FALSE,quote=TRUE,sep="\t",
	file="store.means.x.location")


## Dispersion ##

disp.mod <- mixed.eff.aov (eg.df,response="SalesADA",fixed="Location",random="Store")
write.table(disp.mod$tw.tbl,quote=FALSE,sep=";")#,file="clipboard")

## Central Tendency ##

ct.mod <- mixed.eff.aov (eg.df,response="Sales",fixed="Location",random="Store")
write.table(ct.mod$tw.tbl,quote=FALSE,sep=";",file="clipboard")

# post hoc #
write.table(ct.mod$rnd.tbl,quote=FALSE,sep=";",file="clipboard")

# fixed effect post hoc - Location
dotplot(mean~fixed.levels,
	data=ct.mod$fixed.stats[order(ct.mod$fixed.stats$fixed.levels),],type=c("b","g"),
	ylab="Mean Sales ($)",xlab="Display Location")
write.table(ct.mod$var.equal.stats,quote=FALSE,sep=";",file="clipboard")
write.table(ct.mod$interact.stats,quote=FALSE,sep=";",file="clipboard")
write.table(ct.mod$fixed.stats,quote=FALSE,sep=";",file="clipboard")

