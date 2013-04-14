
require(foreign)
require(lattice)

source("~/Stats.R")
source("~/R Tools/MVP.R")


# read in data and adjust as necessary
crk.df <- read.spss(file="Humidity in Crackers.sav",use.value.labels=TRUE,
	to.data.frame=TRUE,trim.factor.names=TRUE,trim_values=TRUE)
crk.df$Box <- as.factor(crk.df$Box)

## Descriptive stats and graphs ##
desc.list <- with (crk.df,tapply(1:nrow(crk.df),list(Box,Package),function(x,x.df) {
		data.frame (
			Package=x.df$Package[x[1]],
			Box=x.df$Box[x[1]],
			Mean=mean(x.df$Moisture[x]),
			Median=median(x.df$Moisture[x]),
			Std.Dev=sd(x.df$Moisture[x]),
			Variance=var(x.df$Moisture[x]),
			Min=min(x.df$Moisture[x]),
			Max=max(x.df$Moisture[x]),
			Range=diff(range(x.df$Moisture[x])),
			N=length(x)
		)
	}, crk.df)
)
desc.df <- do.call(rbind,desc.list)
write.table(desc.df,row.names=FALSE,quote=FALSE,sep=";",file="clipboard")

histogram(~Moisture|Package,data=crk.df,layout=c(1,5),type="count")
densityplot(~Moisture|Package,data=crk.df,layout=c(1,5),par.strip.text=list(cex=0.7))

histogram(~Moisture|Box+Package,data=crk.df,layout=c(4,5),type="count",
	par.strip.text=list(cex=0.7))
densityplot(~Moisture|Box+Package,data=crk.df,layout=c(4,5),
	par.strip.text=list(cex=0.7))


## Shape analysis ##

# Use ADAs or ADMs?
# => ADAs

cell.mean.map <- with(crk.df,tapply(Moisture,list(Box,Package),mean))
crk.df$MoistureADA <- abs(crk.df$Moisture -
	cell.mean.map[cbind(crk.df$Box,crk.df$Package)])

means.df <- aggregate(cbind(Moisture,MoistureADA)~Box+Package,data=crk.df,FUN=mean)

raw.by.package <- unstack(means.df,Moisture~Package)
ADA.by.package <- unstack(means.df,MoistureADA~Package)
write.table(raw.by.package,row.names=FALSE,quote=FALSE,sep=",",file="raw.by.package")
write.table(ADA.by.package,row.names=FALSE,quote=FALSE,sep=",",file="ADA.by.package")


## Dispersion analysis ##

disp.aov <- nest.mixed.eff.aov(crk.df,response="MoistureADA",fixed="Package",
	nested="Box",alpha=0.05)
disp.aov$within.tbl[,"omega^2/Rho"] <- NA
full.tbl <- rbind (
	disp.aov$main.tbl[1:2,],
	disp.aov$within.tbl,
	disp.aov$main.tbl[3:4,]
)
write.table(full.tbl,quote=FALSE,sep=";",file="clipboard")

# Dispersion Post-hoc for Package (fixed effect) #

stripplot(MoistureADA~Package,data=crk.df,type=c("p","a","g"),jitter=TRUE)
xyplot(MoistureADA~Package,data=aggregate(MoistureADA~Package,data=crk.df,FUN=mean),
	type=c("b","g"),ylab="Mean MoistureADA")

write.table(disp.aov$eq.var.df,row.names=FALSE,quote=FALSE,sep=";",file="clipboard")
write.table(disp.aov$fixed.stats,row.names=FALSE,quote=FALSE,sep=";",file="clipboard")

# for point estimates - need to pool variances for Packages
cell.var <- aggregate(Moisture~Package+Box,data=crk.df,FUN=var)
package.var <- aggregate(Moisture~Package,data=cell.var,FUN=mean)


## Central Tendency ##

(ct.aov <- nest.mixed.eff.aov(crk.df,response="Moisture",fixed="Package",
	nested="Box",alpha=0.05))

(ct.full.tbl <- rbind (
	ct.aov$main.tbl[1:2,],
	ct.aov$within.tbl,
	ct.aov$main.tbl[3:4,]
))
write.table(ct.full.tbl,quote=FALSE,sep=";",file="clipboard")


# Post-hoc for Package - fixed effect #

stripplot(Moisture~Package,data=crk.df,type=c("p","a","g"),jitter=TRUE)
xyplot(Moisture~Package,data=aggregate(Moisture~Package,data=crk.df,FUN=mean),
	type=c("b","g"),ylab="Mean Moisture")

write.table(ct.aov$eq.var.df,row.names=FALSE,quote=FALSE,sep=";",file="clipboard")
write.table(ct.aov$fixed.stats,row.names=FALSE,quote=FALSE,sep=";",file="clipboard")

# for point estimates - need to pool variances for Packages
cell.var <- aggregate(Moisture~Package+Box,data=crk.df,FUN=var)
package.var <- aggregate(Moisture~Package,data=cell.var,FUN=mean)



