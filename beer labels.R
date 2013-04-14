
library(lattice)

source("~/R Tools/MVP.R")
source("~/R Tools/Stats.R")

glue.df <- readMVPStats("Label Adhesion.mvp")
glue.df$Cold_End_Flow_Temperature <- factor(glue.df$Cold_End_Flow_Temperature,
	labels=c("Low","Medium","High")
)
glue.df$Glue_Type <- factor(glue.df$Glue_Type,labels=c("1310","2896","2937"))

# descriptive stats
write.table(aggregate(Proportion_with_Fiber~Cold_End_Flow_Temperature+Glue_Type,data=glue.df,
	FUN=function(x)
		c(mean=mean(x),std.dev=sd(x),var=var(x),min=min(x),max=max(x),
		range=diff(range(x)),n=length(x))
),row.names=FALSE,quote=FALSE,file="clipboard")


histogram(~Proportion_with_Fiber|Cold_End_Flow_Temperature+Glue_Type,data=glue.df,
	par.strip.text=list(cex=0.7),type="count"
)

as.trans <- function (x) {
	sq <- sqrt(x)
	phi <- ifelse (x<1, 2*atan(sq/sqrt(1-x)), pi)
}

glue.df$phi <- as.trans(glue.df$Proportion_with_Fiber)

histogram(~phi|Cold_End_Flow_Temperature+Glue_Type,data=glue.df,
	par.strip.text=list(cex=0.7),type="count"
)

prop.mean.var <- aggregate(Proportion_with_Fiber~Cold_End_Flow_Temperature+Glue_Type,
	data=glue.df,FUN=function(x)c(mean=mean(x),var=var(x))
)
cor(prop.mean.var$Proportion_with_Fiber)

phi.mean.var <- aggregate(phi~Cold_End_Flow_Temperature+Glue_Type,
	data=glue.df,FUN=function(x)c(mean=mean(x),var=var(x))
)

cor(phi.mean.var$phi)

as.trans2 <- function (p) 2*asin(sqrt(p))

glue.df$phi2 <- as.trans2(glue.df$Proportion_with_Fiber)

phi2.mean.var <- aggregate(phi2~Cold_End_Flow_Temperature+Glue_Type,
	data=glue.df,FUN=function(x)c(mean=mean(x),var=var(x),sd=sd(x))
)
cor(phi2.mean.var$phi2)
## phi == phi2 (within some level of rounding error)
xyplot(var~mean,data=as.data.frame(phi.mean.var$phi),main="Transformed values",
	type=c("p","r"))
xyplot(var~mean,data=as.data.frame(prop.mean.var$Proportion_with_Fiber),
	main="Raw values",type=c("p","r"))


write.table(glue.df$phi,row.names=FALSE,quote=FALSE,file="clipboard")

# Graph main effect

dotplot(phi~Glue_Type,data=aggregate(phi~Glue_Type,data=glue.df,FUN=mean),
	type=c("p","l"),xlab="Glue Type",
	ylab="Estimated Marginal Mean of\nTransformed Proportion with Fiber"
)
stripplot(Proportion_with_Fiber~Glue_Type,data=glue.df,xlab="Glue_Type",
	groups=Cold_End_Flow_Temperature,jitter=TRUE,type=c("p","a"),
	auto.key=list(lines=TRUE,columns=3,title="Cold End Flow (Temperature)",cex=0.8)
)

# main effect (glue) post-hoc
glue.stats <- aggregate(phi~Glue_Type,data=glue.df,FUN=function (x)
	c(Mean=mean(x), Variance=var(x),n=length(x))
)
names(glue.stats) <- c("Glue_Type","TrProp")
write.table(glue.stats[order(glue.stats$TrProp[,"Mean"]),],row.names=FALSE,quote=FALSE,
	sep=";",file="clipboard")

glue.means <- aggregate(Proportion_with_Fiber~Glue_Type,data=glue.df,FUN=mean)
names(glue.means)<-c("Glue Type","Mean (raw)")
write.table(glue.means[order(glue.means[,2]),],row.names=FALSE,quote=FALSE,sep=";",
	file="clipboard"
)


# Nested effects post-hoc

dotplot(phi~Cold_End_Flow_Temperature,data=aggregate(phi~Cold_End_Flow_Temperature+
	Glue_Type,data=glue.df,FUN=mean),groups=Glue_Type,type=c("p","l"),
	auto.key=list(lines=TRUE,columns=3,cex=0.8,title="Glue Type"),
	xlab="Cold End Flow Temperature",
	ylab="Estimated Marginal Mean of\nTransformed Proportion with Fiber"

)
dotplot(phi~Cold_End_Flow_Temperature|Glue_Type,
	data=aggregate(phi~Cold_End_Flow_Temperature+Glue_Type,data=glue.df,FUN=mean),
	type=c("p","l"),layout=c(1,3),scales=list(y=list(relation="free")),
	xlab="Cold End Flow Temperature",par.strip.text=list(cex=0.8),
	ylab="Estimated Marginal Mean of\nTransformed Proportion with Fiber"
)

aggregate(Proportion_with_Fiber~Cold_End_Flow_Temperature+Glue_Type,data=glue.df,
	FUN=function(x) c(mean=mean(x),n=length(x)))




