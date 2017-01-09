#rm(list = ls(all = TRUE)) # clear workspace.

setwd("~/git-repos/Kinds-of-Reasoning-Cognition-2015")
#data = read.csv("LG2015-data.csv", header=TRUE)







	
valid = d[d$trivialArgument == "true",]	
 #separate out the logically valid arguments
valid.nec = valid[valid$modal == "necessary",] 
valid.cert = valid[valid$modal == "certain",]  
valid.pl = valid[valid$modal == "plausible",]  

inclusion.nec=valid.nec[valid.nec$argtype == "inclusion",]
identity.nec=valid.nec[valid.nec$argtype == "identity",]
   
invalid = d[d$trivialArgument == "false",]
   #invalid arguments	
invalid.nec = invalid[invalid$modal == "necessary",] 
invalid.cert = invalid[invalid$modal == "certain",]  
invalid.pl = invalid[invalid$modal == "plausible",]  


valid.nec.prop = length(valid.nec[valid.nec$response == "accept",]$rt)/length(valid.nec$rt)
valid.pl.prop = length(valid.pl[valid.pl$response == "accept",]$rt)/length(valid.pl$rt)

inclusion.nec=valid.nec[valid.nec$argtype == "inclusion",]
inclusion.nec.prop = length(inclusion.nec[inclusion.nec$response == "accept",]$rt) / length(inclusion.nec$rt)

identity.nec=valid.nec[valid.nec$argtype == "identity",]
identity.nec.prop = length(identity.nec[identity.nec$response == "accept",]$rt) / length(identity.nec$rt)

inclusion.pl=valid.pl[valid.pl$argtype == "inclusion",]
inclusion.pl.prop = length(inclusion.pl[inclusion.pl$response == "accept",]$rt) / length(inclusion.pl$rt)

identity.pl=valid.pl[valid.pl$argtype == "identity",]
identity.pl.prop = length(identity.pl[identity.pl$response == "accept",]$rt) / length(identity.pl$rt)

invalid.nec.prop = length(invalid.nec[invalid.nec$response == "accept",]$rt)/length(invalid.nec$rt)
invalid.pl.prop = length(invalid.pl[invalid.pl$response == "accept",]$rt)/length(invalid.pl$rt)


valid.invalid = data.frame(dummy=c(0,0))
for (i in 1:length(levels(d$modal))) {
	modal = levels(d$modal)[i]
	modd = d[d$modal == modal,]
	modd.valid = modd[modd$trivialArgument == "true",]
	modd.invalid = modd[modd$trivialArgument == "false",]
	valid.prop = length(modd.valid[modd.valid$response=="accept",]$rt) / length(modd.valid$rt)
	invalid.prop = length(modd.invalid[modd.invalid$response=="accept",]$rt) / length(modd.invalid$rt)
	props = c(valid=valid.prop, invalid = invalid.prop)
	valid.invalid =cbind(valid.invalid,props)
}
valid.invalid= valid.invalid[,2:length(colnames(valid.invalid))]
colnames(valid.invalid) = levels(d$modal)

library(RColorBrewer)
# plot(valid.invalid$certain, type="b", lwd=1, xaxt="n", col="blue", xlab="Validity", ylab="Proportion of positive responses", ylim=c(0,1), main="Endorsement Rate by Validity and Modal Frame")
# axis(1, at=1:length(valid.invalid$necessary),labels=rownames(valid.invalid))
# colors = c("blue", "red", "green", "orange", "purple", "brown", "black", "yellow")
# for (i in 2:length(colnames(valid.invalid))) {
	# lines(valid.invalid[,i], col=colors[i],type="b",lwd=1)
# }
# legend("bottomleft",legend=colnames(valid.invalid), lty=1,lwd=2,pch=21,col=colors,ncol=2, bty="n", cex=.8, text.col=colors, inset=.1)
# separating identity and inclusion arguments

summ = data.frame(dummy=c(0,0,0))
for (i in 1:length(levels(d$modal))) {
	modal = levels(d$modal)[i]
	modd = d[d$modal == modal,]
	modd.id = modd[modd$argtype == "identity",]	
	modd.incl = modd[modd$argtype == "inclusion",]
	modd.cont = modd[modd$argtype == "contingent",]
	id.prop = length(modd.id[modd.id$response=="accept",]$rt) / length(modd.id$rt)
	incl.prop = length(modd.incl[modd.incl$response=="accept",]$rt) / length(modd.incl$rt)
	cont.prop = length(modd.cont[modd.cont$response=="accept",]$rt) / length(modd.cont$rt)
	props = c(identity=id.prop, inclusion = incl.prop, contingent=cont.prop)
	summ=cbind(summ,props)
}
summ=summ[,2:length(colnames(summ))]
colnames(summ) = levels(d$modal)

library(RColorBrewer)
plot(jitter(summ$necessary), type="b", lwd=1, xaxt="n", col="blue", xlab="Argument Type", ylab="Proportion of positive responses", ylim=c(min(summ$necessary),1), main="Endorsement Rate by Argument Type and Modal Frame")
axis(1, at=1:length(summ$necessary),labels=rownames(summ))
colors = c("red", "green", "orange", "purple", "brown", "black", "yellow")
for (i in 2:length(colnames(summ))) {
	lines(summ[,i], col=colors[i],type="b",lwd=1)
}
legend("bottomleft",legend=colnames(summ), lty=1,lwd=2,pch=21,col=colors,ncol=2, bty="n", cex=.8, text.col=colors, inset=.1)


nec.hit.prop = length(valid.nec[valid.nec$response == "accept",]$rt) / length(valid.nec$rt)
nec.fa.prop = length(invalid.nec[invalid.nec$response == "accept",]$rt) / length(invalid.nec$rt)

#pilot data, using table from p.375 of Macmillan's SDT User's Guide:
#nec hit rate .84 = z .994
#nec fa rate .44 = z -.151

pl.hit.prop = length(valid.pl[valid.pl$response == "accept",]$rt) / length(valid.pl$rt)
pl.fa.prop = length(invalid.pl[invalid.pl$response == "accept",]$rt) / length(invalid.pl$rt)

sdt.z = function (prop) {
	mu=.5
	sd=.15
	val = (qnorm(prop, mu, sd) - mu)/sd
	return(val)
}
d.prime = function(hit.rate, fa.rate) {
	return(sdt.z(hit.rate) - sdt.z(fa.rate))	
}
d.prime.variance = function(hit.rate, n.signal.trials, fa.rate, n.noise.trials) {
	return((hit.rate*(1-hit.rate))/(n.signal.trials * (phi(hit.rate))^2) + (fa.rate*(1-fa.rate))/(n.noise.trials * (phi(fa.rate))^2))
	}
phi = function(rate) {return((exp(- (sdt.z(rate))^2 /2))/sqrt(2*pi))}

d.prime.ci = function(hit.rate, n.signal.trials, fa.rate, n.noise.trials) {
	dpr = d.prime(hit.rate, fa.rate)
	se = sqrt(d.prime.variance(hit.rate, n.signal.trials, fa.rate, n.noise.trials))
	return(c(dpr, dpr + 1.96*se, dpr - 1.96*se))
}

pl.d.prime = d.prime(pl.hit.prop, pl.fa.prop) #  1.751 - .954 # = .797
pl.d.prime.ci = d.prime.ci(pl.hit.prop, length(valid.pl$rt), pl.fa.prop, length(invalid.pl$rt))

nec.d.prime = d.prime(nec.hit.prop, nec.fa.prop)
# pilot: nec d' = 1.145, pl d' = .86. To compare, H&R 2005 found d'(nec) = 1.69 and d'(pl) = .86, using identity arguments only.
# final: nec d' = 1.149, pl d' = .64. To compare, H&R 2005 found d'(nec) = 1.69 and d'(pl) = .86, using identity arguments only.
nec.d.prime.ci = d.prime.ci(nec.hit.prop, length(valid.nec$rt), nec.fa.prop, length(invalid.nec$rt))

nec.identity.hit.prop = length(which(identity.nec$response=="accept"))/length(identity.nec$response)
nec.inclusion.hit.prop = length(which(inclusion.nec$response=="accept"))/length(inclusion.nec$response)

pl.identity.hit.prop = length(which(identity.pl$response=="accept"))/length(identity.pl$response)
pl.inclusion.hit.prop = length(which(inclusion.pl$response=="accept"))/length(inclusion.pl$response)

identity.nec.d.prime = d.prime(nec.identity.hit.prop, nec.fa.prop)
identity.nec.d.prime.ci = d.prime.ci(nec.identity.hit.prop, length(valid.pl$rt), nec.fa.prop, length(invalid.pl$rt))



identity.nec.hit.prop = length(identity.nec[identity.nec$response == "accept",]$rt) / length(identity.nec$rt)
inclusion.nec.hit.prop = length(inclusion.nec[inclusion.nec$response == "accept",]$rt) / length(inclusion.nec$rt)
# not much different.

#for comparison, here's d' for the no-modal condition:
valid.no.mod = valid[valid$modal == "none",]
invalid.no.mod = invalid[invalid$modal == "none",]
no.mod.hit.prop = length(valid.no.mod[valid.no.mod$response == "accept",]$rt) / length(valid.no.mod$rt) # = .986, z=2.326
no.mod.fa.prop = length(invalid.no.mod[invalid.no.mod$response == "accept",]$rt) / length(invalid.no.mod$rt) # = .567, z=.176
no.mod.d.prime = d.prime(no.mod.hit.prop, no.mod.fa.prop)  
# in the pilot, d'(no.mod) = 2.15 -- much higher than for either nec or pl in our data. 
#in the final results, d'(no.mod) = 1.23, very close to necessary.

d.primes=data.frame(dummy=c(0,0,0))
for (i in 1:length(levels(d$modal))) {
	mod=levels(d$modal)[i]
	valid.dat=valid[valid$modal==mod,]
	invalid.dat=invalid[invalid$modal==mod,]
	hit.rate = length(valid.dat[valid.dat$response == "accept",]$rt)/length(valid.dat$rt)
	fa.rate = length(invalid.dat[invalid.dat$response == "accept",]$rt)/length(invalid.dat$rt)
	d.primes = cbind(d.primes,c(hit.rate, fa.rate, d.prime(hit.rate,fa.rate)))
	#mat <- matrix(c(length(valid.dat[valid.dat$response == "accept",]$rt), length(valid.dat[valid.dat$response == "reject",]$rt), length(invalid.dat[invalid.dat$response == "accept",]$rt), length(invalid.dat[invalid.dat$response == "reject",]$rt)), 2, byrow = TRUE)
	
#library(sensR)
#d.primes = rbind(d.primes,SDT(mat, "logit"))
#using the library function for SDT with probit link, same results I got by hand. With logic, very different! how to choose? Which are H&R using?

#  z(Hit rate) z(False alarm rate)  d-prime
#1    1.534347            -0.35794 1.892287
}
d.primes=d.primes[,2:length(d.primes[1,])]
colnames(d.primes) = levels(d$modal)
rownames(d.primes) = c("hit.rate", "fa.rate", "d.prime")

#plot hit and false alarm rates with CIs

cols=c("blue", "red", "green", "orange", "purple", "brown", "black")
hit.ci = data.frame(hit.rate = t(sort(d.primes[1,])), upper=c(0,0,0,0,0,0,0),lower=c(0,0,0,0,0,0,0))
hit.modals.sorted = c("certain", "necessary", "probable", "likely", "possible", "plausible", "none")
rownames(hit.ci) = hit.modals.sorted
for (i in 1:7) { #calculate confidence intervals for the hits
	p = hit.ci[i,1]
	n=length(valid[valid$modal == hit.modals.sorted[i],]$rt)
	up = p + 1.96*sqrt(p*(1-p)/n)
	low = p - 1.96*sqrt(p*(1-p)/n)
	hit.ci[i,2] = up
	hit.ci[i,3] = low
}

#there appear to be three groups -- certain/necessary, probable/likely, and possible/plausible/none, though more data is needed to be sure the three are all clasically significantly different. 
dpr=t(d.primes)
fa.ci = data.frame(fa.rate = c(dpr[1,2], dpr[3,2], dpr[7,2], dpr[2,2], dpr[6,2], dpr[5,2], dpr[4,2]), upper=c(0,0,0,0,0,0,0),lower=c(0,0,0,0,0,0,0)) #sorted in the same order as the hit rates
rownames(fa.ci) = hit.modals.sorted
for (i in 1:7) { #calculate confidence intervals for the "false alarms"
	p = fa.ci[i,1]
	n=length(valid[valid$modal == hit.modals.sorted[i],]$rt)
	up = p + 2*sqrt(p*(1-p)/n)
	low = p - 2*sqrt(p*(1-p)/n)
	fa.ci[i,2] = up
	fa.ci[i,3] = low
}
fa.ci.min = min(fa.ci$lower)

fa.sorted=rbind(fa.ci[5,],fa.ci[6,],fa.ci[3,],fa.ci[4,],fa.ci[7,],fa.ci[2,],fa.ci[1,])
hit.sorted=rbind(hit.ci[5,],hit.ci[6,],hit.ci[3,],hit.ci[4,],hit.ci[7,],hit.ci[2,],hit.ci[1,])

# d' difference: p = .0095135 

cols=c("blue","red","green","orange","purple","brown","black")

plot(hit.sorted[,1], ylim=c(fa.ci.min - .05,1),xlab="Modal frame", ylab="Proportion acceptance", main="",xaxt='n',xlim=c(1,7.5))
#main="Endorsement by validity and modal",
arrows(x0=c(1,2,3,4,5,6,7),x1=c(1,2,3,4,5,6,7),y0=hit.sorted[,3],y1=hit.sorted[,2], angle=90,code=3,length=.04,lwd=.4,col=cols)
text(1:7, hit.sorted[1:7,1], "valid", cex=1, pos=4, col=cols)

points(1:7, fa.sorted[,1])
#plot(fa.sorted[,1], ylim=c(0,1),main="Contingent arguments",xlab="Modal frame", ylab="Endorsement rate")
arrows(x0=c(1,2,3,4,5,6,7),x1=c(1,2,3,4,5,6,7),y0=fa.sorted[,3],y1=fa.sorted[,2], angle=90,code=3,length=.04,lwd=.4, col=cols)
#axis(1, at=1:7, labels = hit.modals.sorted)
#text(1:7, fa.ci$lower - .05, row.names(fa.ci), cex=1, pos=1, col=cols)
axis(side=1, at=1:7, labels=row.names(hit.sorted))
text(1:7, fa.sorted[1:7,1], "contingent", cex=1, pos=4, col=cols)


#### calculating relevant stats separately for all modal frames, comparing unmodified ones with Osherson's

osherson.mat = data.frame(cows=c(NA,.79,.75,.74,.72,.73,.73,.75,.77),chimps=c(.79,NA,.23,.42,.40,.40,.43,.59,.64),gorillas=c(.75,.23,NA,.48,.47,.38,.41,.61,.63),mice=c(0.74,0.42,0.48,NA,.17,.28,.25,.58,.62),squirrels=c(0.72,0.40,0.47,0.17,NA,.32,.26,.54,.61),dolphins=c(0.73,0.40,0.38,0.28,0.32,NA,.06,.54,.54),seals=c(0.73,0.43,0.41,0.25,0.26,0.06,NA,.51,.56),elephants=c(0.75,0.59,0.61,0.58,0.54,0.54,0.51,NA,.57),rhinos=c(0.77,0.64,0.63,0.62,0.61,0.54,0.56,0.57,NA))
rownames(osherson.mat) = colnames(osherson.mat)

animal.names = rownames(osherson.mat)
unmod.animals=data.frame(c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0))
rownames(unmod.animals) = animal.names
colnames(unmod.animals) = animal.names
unmod.animals.ns=data.frame(c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0))
rownames(unmod.animals.ns) = animal.names
colnames(unmod.animals.ns) = animal.names

pcodes = levels(invalid$premisecode)
allpremisenames = c()
prop.by.premises = data.frame(dummy=c(0,0,0,0,0,0,0))
mod = levels(invalid$modal)
rownames(prop.by.premises) = mod
n.by.premises = data.frame(dummy=c(0,0,0,0,0,0,0))
rownames(n.by.premises) = mod
for (i in 1:length(pcodes)) {
	pcnow = pcodes[i]
	props = c()
	ns = c()
	for (j in 1:length(mod)) {
		modnow = mod[j]
#		dat = invalid[invalid$modal == modnow,]  # to look at non-valid arguments only
		dat = d[d$modal == modnow,]
		dat = dat[dat$premisecode == pcnow,]
		n = length(dat$rt)
		if (n > 0) {
			prop = length(dat[dat$response == "accept",]$rt) / n
		} else {
			prop = 0	
		}
		props = c(props, prop)
		ns = c(ns, n)
	}
	animals = c()
	if (!is.na(dat[1,1])) {
		for (k in 14:25) {    #14:25 are the animal names, incl. "horses", "mammals", "animals" 
			if (dat[1,k] == 1) {
				animals = c(animals,colnames(dat)[k])
			}
		}
		unmod.animals[which(rownames(unmod.animals) == animals[1]),which(colnames(unmod.animals) == animals[2])] = props[4]
		unmod.animals[which(colnames(unmod.animals) == animals[2]),which(rownames(unmod.animals) == animals[1])] = props[4]
		unmod.animals.ns[which(rownames(unmod.animals) == animals[1]),which(colnames(unmod.animals) == animals[2])] = ns[4]
		unmod.animals.ns[which(colnames(unmod.animals) == animals[2]),which(rownames(unmod.animals) == animals[1])] = ns[4]
		thispremiseanimals = paste(animals[1], animals[2])
		allpremisenames = c(allpremisenames, thispremiseanimals)
		prop.by.premises = cbind(prop.by.premises, props)
		n.by.premises = cbind(n.by.premises, ns)
	}
}
prop.by.premises = prop.by.premises[,2:length(prop.by.premises)]
colnames(prop.by.premises) = allpremisenames

n.by.premises = n.by.premises[,2:length(n.by.premises)]
colnames(n.by.premises) = allpremisenames

## usable data frames with endorsement rate, n by argument and modal
all.modal.proportions = data.frame(t(prop.by.premises))
all.modal.proportions = all.modal.proportions[order(all.modal.proportions$none),]

all.modal.ns = data.frame(t(n.by.premises))
all.modal.ns = all.modal.ns[order(data.frame(t(prop.by.premises))$none),]

#unmod.animals is a data frame giving the proportion of arguments acceptance for each animal pair
#unmod.animals.ns is the corresponding df of ns for these observations.
for (i in 1:length(rownames(unmod.animals))) {
	for (j in 1:length(colnames(unmod.animals))){
		if (i == j) {
			unmod.animals[i,j] = NA	
			unmod.animals.ns[i,j] = NA
			osherson.mat[i,j] = NA	
		}	
	}	
}

#result resembles osherson et al 1990's in some respects, but is fairly different in others.
#ns range from 23 to 39.

# par(mfrow=c(3,3))
# for (i in 1:length(colnames(osherson.mat))) {
	# plot(osherson.mat[!(is.na(osherson.mat[,i])),i], unmod.animals[!(is.na(unmod.animals[,i])),i],xlab="Osherson data",ylab="Our data",xlim=c(0,1), ylim=c(0,1),main=colnames(osherson.mat)[i])
	# #lines(lowess(osherson.mat[!(is.na(osherson.mat[,i])),i], unmod.animals[!(is.na(unmod.animals[,i])),i]))
# }

## make alternate version of all.modal.proportions and all.modal.ns with "horses", "mammals", and "animals" arguments collapsed
not.valid.props = all.modal.proportions[1:35,]  # 36 and 37 are valid, 38 isn't
not.valid.props = rbind(not.valid.props, all.modal.proportions[38,])
not.valid.ns = all.modal.ns[1:35,]  # 36 and 37 are valid, 38 isn't
not.valid.ns = rbind(not.valid.ns, all.modal.ns[38,])

valid.props = all.modal.proportions[39:length(all.modal.proportions$certain),]
valid.props = rbind(valid.props, all.modal.proportions[36:37,])
valid.ns = all.modal.ns[39:length(all.modal.ns$certain),]
valid.ns = rbind(valid.ns, all.modal.ns[36:37,])

animal.props = valid.props[1,]
animal.props = rbind(animal.props, valid.props[7:9,], valid.props[11,], valid.props[15,],valid.props[17,],valid.props[19,],valid.props[23,])
mammal.props = valid.props[3:4,]  
mammal.props = rbind(mammal.props, valid.props[10,], valid.props[12,], valid.props[16,], valid.props[24:27,])
horses.props = valid.props[2,]
horses.props = rbind(horses.props, valid.props[5:6,], valid.props[13:14,], valid.props[18,], valid.props[20:22,])

animal.ns = valid.ns[1,]
animal.ns = rbind(animal.ns, valid.ns[7:9,], valid.ns[11,], valid.ns[15,], valid.ns[17,], valid.ns[19,], valid.ns[23,])
mammal.ns = valid.ns[3:4,]  
mammal.ns = rbind(mammal.ns, valid.ns[10,], valid.ns[12,], valid.ns[16,], valid.ns[24:27,])
horses.ns = valid.ns[2,]
horses.ns = rbind(horses.ns, valid.ns[5:6,], valid.ns[13:14,], valid.ns[18,], valid.ns[20:22,])

animals.collapsed = c()
mammals.collapsed = c()
horses.collapsed = c()
for (i in 1:length(colnames(animal.props))) {
	coll.an = 0
	coll.mam = 0
	coll.hor = 0	
	for (j in 1:length(rownames(animal.props))) {
		coll.an = coll.an + animal.props[j,i] * animal.ns[j,i]
		coll.mam = coll.mam + mammal.props[j,i] * mammal.ns[j,i]
		coll.hor = coll.hor + horses.props[j,i] * horses.ns[j,i]
	}
	animals.collapsed = c(animals.collapsed,coll.an/sum(animal.ns[,i]))
	mammals.collapsed = c(mammals.collapsed, coll.mam/sum(mammal.ns[,i]))
	horses.collapsed = c(horses.collapsed, coll.hor/sum(horses.ns[,i]))
}
collapsed.props = rbind(not.valid.props, animals= animals.collapsed, mamals= mammals.collapsed, horses=horses.collapsed)

########### plot by modal
# #modals = colnames(all.modal.proportions)
# modals = colnames(collapsed.props)
# modals = setdiff(modals, "none")
# colors = c("blue", "red", "yellow", "black",  "green", "orange")
# par(mfrow=c(1,1))
# plot(collapsed.props$none, collapsed.props[,1], type="p", lwd=1, xaxt="n", col="blue", xlab="Endorsement rate by argument, no modal", ylab="Endorsement rate by argument, with modal", xlim=c(min(collapsed.props$none),max(collapsed.props$none)), ylim=c(0,1), main="Endorsement Rate of arguments")



cors=c()
for (i in 1:length(all.modal.proportions)) {cors=c(cors,cor(all.modal.proportions$none, all.modal.proportions[,i]))}
#modals=  "certain" "likely" "necessary"   "none"    "plausible" "possible"  "probable"
#cors =   0.7791765 0.7291349 0.7630018   1.0000000   0.6401379   0.5350662   0.6298348

###### 2 versions: 
#### 1 - taking the results of the unmodalized condition as the input as described above.
#### 1.5 -- varying which one is on the x-axis
#### 2 - non-modalized are luce choice. modalized take the prob input to this ?

## relate to threshold semantics?
#if we assume that the distribution on thresholds is a power law we can easily compute the interval.

## Noahs' way of thinking about thresholds:
#### degree modifiers manipulate alpha, the parameter of exponentiation for the probability.
### modals are like deg mods for non-modalized
#### set some alpha for non-modalized

#1:
## d-primes: 
## CHECK THAT THERE ARE MORE THAN TWO CLUSTERS (pairwise sig diff acc to MacMillan)
## see above
d.prime.cis = data.frame(dummy=c(0,0,0))
mods = c("none","certain","necessary","probable","likely","plausible","possible")
for (i in 1:length(mods)) {
	dat = d[d$modal == mods[i],]
	dat.valid = dat[dat$trivialArgument == "true",] 
	hr = length(dat.valid[dat.valid$response == "accept",]$rt)/length(dat.valid$rt)
	hitns = length(dat$rt)
	dat.invalid = dat[dat$trivialArgument == "false",]
	far = length(dat.invalid[dat.invalid$response == "accept",]$rt)/length(dat.invalid$rt)
	fans = length(dat.invalid$rt)
	d.prime.cis = cbind(d.prime.cis, d.prime.ci(hr, hitns, far, fans))
}
d.prime.cis = d.prime.cis[,2:length(d.prime.cis)]
colnames(d.prime.cis) = mods
rownames(d.prime.cis) = c("d.prime.est", ".95upper", ".95lower")
d.prime.cis = d.prime.cis[order(d.prime.cis[1,])]

#              possible plausible  probable    likely necessary  certain     none
# d.prime.est 0.5374079 0.6350262 0.7503972 0.9434497  1.149274 1.181601 1.236514
# .95upper    0.6766754 0.7714122 0.8729581 1.0717176  1.255044 1.289442 1.355179
# .95lower    0.3981404 0.4986402 0.6278362 0.8151818  1.043504 1.073759 1.117848
    #sig:	 	possible ~ probable (almost), likely, necessary, certain, none
 	#			plausible ~ likely, necessary, certain, none
 	#			probable ~ necessary, certain, none
 	#			likely ~ necessary(almost), certain (barely), none
 	#			others nsig

### plot d-prime CIs (below, without unmodalized condition which is odd-man-out).

library(plotrix)
cols=c("blue", "red", "green", "orange", "purple", "brown", "black")
plotCI(c(1,2,3,4,5,6,7), d.prime.cis[1,1:7], uiw=.1, ui=d.prime.cis[2,1:7], li=d.prime.cis[3,1:7], err="y",scol=cols, xlab="Modal frame", ylab="d'",ylim=c(min(d.prime.cis[3,]) - .1, max(d.prime.cis[2,1:7])), xlim=c(.7,7.3), xaxt='n', main="")
#main="d' by modal, with 95% confidence interval", 
text(c(1,2,3,4,5,6,7), d.prime.cis[3,1:7] - .05, colnames(d.prime.cis[1:7]),col=cols, lty=1,lwd=2,pch=21, bty="n", cex=1.3)
#beautiful: even better than 3 clusters, there's a continuous gradient in d-prime values, with 3 (nearly 4) regions with non-overlapping CIs.


#2: fit simple power-law model to data, then compare to H&R's -- they have two parameters, we have 1. See if we can get as good or better fit.

epsilon = .1

ss.powerlaw = function (data, baseline, alpha, epsilon) {
#baseline is a vector of proportions of the same length as data (e.g., the unmodalized condition).
	dev = 0
	for (i in 1:length(baseline)) { 
		dev = dev + (data[i] - (baseline[i]^alpha * (1 - epsilon) + (epsilon/2)))^2
	}
	return(dev)
}

## modify to add in noise??
min.ss.powerlaw = function (baseline, data) {
	epsilon= .1
	alpha = 1
	step = .001
	ss.now = ss.powerlaw(data,baseline,alpha, epsilon)
	ss.expl = ss.powerlaw(data,baseline,alpha + step, epsilon)
	while (ss.now > ss.expl) {
		alpha = alpha + step
		ss.now = ss.powerlaw(data,baseline,alpha, epsilon)
		ss.expl = ss.powerlaw(data,baseline,alpha + step, epsilon)
	}
	ss.expl = ss.powerlaw(data,baseline,alpha - step, epsilon)
	while (ss.now > ss.expl) {
		alpha = alpha - step
		ss.now = ss.powerlaw(data,baseline,alpha, epsilon)
		ss.expl = ss.powerlaw(data,baseline,alpha - step, epsilon)
	}
	estep = .001
## then optimize epsilon
	ss.expl = ss.powerlaw(data,baseline,alpha, epsilon + estep)
	while (ss.now > ss.expl) {
		epsilon = epsilon + estep 
		ss.now = ss.powerlaw(data,baseline,alpha,epsilon)
		ss.expl = ss.powerlaw(data,baseline,alpha, epsilon + estep)
	}
	ss.expl = ss.powerlaw(data,baseline,alpha, epsilon - estep)
	while (ss.now > ss.expl) {
		epsilon = epsilon - estep 
		ss.now = ss.powerlaw(data,baseline,alpha, epsilon)
		ss.expl = ss.powerlaw(data,baseline,alpha, epsilon + estep)
	}
	return(c(alpha, epsilon, ss.now, sqrt(ss.now/(length(baseline) - 1))))
}

min.ss.epsilon = function (baseline, data, alpha) {
	epsilon=.15
	step = .001
	ss.now = ss.powerlaw(data,baseline, alpha, epsilon)
	estep = .001
## then optimize epsilon
	ss.expl = ss.powerlaw(data,baseline,alpha, epsilon + estep)
	while (ss.now > ss.expl) {
		epsilon = epsilon + estep 
		ss.now = ss.powerlaw(data,baseline,alpha,epsilon)
		ss.expl = ss.powerlaw(data,baseline,alpha, epsilon + estep)
	}
	ss.expl = ss.powerlaw(data,baseline,alpha, epsilon - estep)
	while (ss.now > ss.expl) {
		epsilon = epsilon - estep 
		ss.now = ss.powerlaw(data,baseline,alpha, epsilon)
		ss.expl = ss.powerlaw(data,baseline,alpha, epsilon + estep)
	}
	return(c(alpha, epsilon, ss.now, sqrt(ss.now/(length(baseline) - 1))))
}


par(mfrow=c(2,3))
plaw.mod.data = all.modal.proportions 
#plaw.mod.data = collapsed.props
#mod.baseline = "none"
#bsln.idx = which(colnames(plaw.mod.data) == mod.baseline)
bsln.idx = 4
mod.baseline = colnames(plaw.mod.data)[bsln.idx]
lms = data.frame(dummy=c(0,0,0,0,0)) 
plaw.model.results = data.frame(dummy=c(0,0,0,0,0)) 
for (i in 1:length(colnames(plaw.mod.data))) {
	if (!(i == bsln.idx)) {
		minss = min.ss.powerlaw(plaw.mod.data[, bsln.idx], plaw.mod.data[, i])
		alpha.i = minss[1]
		resid.i = minss[4] #(baseline[i]^alpha * (1 - epsilon) + (epsilon/2))
		## fit epsilon by condition
		#epsilon.i = minss[2]
		# fit epsilon for all
		epsilon.i = .1 # derived after-the-fact using the recompute.epsilon() function below
		
		r.sq.i=round((cor(plaw.mod.data[,i],(plaw.mod.data[, bsln.idx])^alpha.i * (1 - epsilon.i) + (epsilon.i/2)))^2, 2)
		plaw.model.results = cbind(plaw.model.results, c(minss[1],epsilon.i,minss[3], minss[4], r.sq.i))
		plot(plaw.mod.data[, bsln.idx], plaw.mod.data[,i], xlab= "acceptance rate: no modal", ylab=paste('acceptance rate:', colnames(plaw.mod.data)[i]), main=colnames(plaw.mod.data)[i], xlim=c(min(plaw.mod.data[, bsln.idx]), max(plaw.mod.data[, bsln.idx])), ylim=c(.1,1), pch=20, cex=1.5)
		x=sprintf("%.2f", round(alpha.i,2))
		text(max(plaw.mod.data[, bsln.idx]) -.2, .25, expression(alpha), lty=1,lwd=2,pch=21, bty="n", col="red", cex=1.6)
		text(max(plaw.mod.data[, bsln.idx]) -.11, .25, paste(" =", x), lty=1,lwd=2,pch=21, bty="n", col="red", cex=1.5)
		text(max(plaw.mod.data[, bsln.idx]) -.02, .24, expression(alpha[0]), lty=1,lwd=2,pch=21, bty="n", col="red", cex=1.5)
#		text(.8, .24, paste("resid.sd=", round(resid.i,3)), lty=1,lwd=2,pch=21, bty="n", cex=1)
# plot power-law model curve
	    curve(x^alpha.i * (1-epsilon) + (epsilon/2), col="red", add=TRUE)
	    #text(max(plaw.mod.data[, bsln.idx]) -.2, .15, "test7", adj=NULL, lty=1,lwd=2,pch=21, bty="n", col="red", cex=1.6)
		text(max(plaw.mod.data[, bsln.idx]) -.2, .16, expression(R^2), col="red",lty=1,lwd=2,pch=21, bty="n", cex=1.5)
		y=sprintf("%.2f", round(r.sq.i,2))
		text(max(plaw.mod.data[, bsln.idx]) -.11, .15, paste(" =", y), col="red",lty=1,lwd=2,pch=21, bty="n", cex=1.5) 
#		text(max(plaw.mod.data[, bsln.idx]) -.1, .12, expression(epsilon), col="red",lty=1,lwd=2,pch=21, bty="n", cex=1.2)
#		text(max(plaw.mod.data[, bsln.idx]) -.04, .12, paste(" = ", round(epsilon.i,2)), col="red",lty=1,lwd=2,pch=21, bty="n", cex=1.2)
	}
}
plaw.model.results = plaw.model.results[,2:length(plaw.model.results)]
colnames(plaw.model.results) = c("certain","likely","necessary","plausible","possible","probable")
rownames(plaw.model.results) = c("alpha", "epsilon", "residual ss", "residual sd", "r.squared")

### need new minss function for this.
recompute.epsilon = function(params, dat) {
	alphas=params[1,]
	best.eps = .15
	eps.step = .001
	best.ss = ss.epsilon(dat, alphas, best.eps)
	proposed.eps = best.eps + eps.step
	proposed.ss = ss.epsilon(dat, alphas, proposed.eps)
	while (proposed.ss < best.ss) {
		best.ss = proposed.ss
		best.eps = proposed.eps
		proposed.eps = best.eps + eps.step
		proposed.ss = ss.epsilon(dat, alphas, proposed.eps)	
	}
	proposed.eps = best.eps - eps.step
	proposed.ss = ss.epsilon(dat, alphas, proposed.eps)
		while (proposed.ss < best.ss) {
		best.ss = proposed.ss
		best.eps = proposed.eps
		proposed.eps = best.eps - eps.step
		proposed.ss = ss.epsilon(dat, alphas, proposed.eps)	
	}
	return(c(best.eps, best.ss))
}

ss.epsilon = function(dat, alphas, epsilon) {
	ss = 0
	baseline = dat[,4]
	for (i in c(1,2,3,5,6,7)) {
		if (i < 4) {alpha = alphas[i]} else {alpha = alphas[i-1]}
		for (j in 1:length(baseline)) {
			pred = baseline[j]^alpha * (1 - epsilon) + (epsilon/2)
			actual = dat[j,i]
			ss = ss + (pred - actual)^2 
		}
	}
	return(ss)
}

#best-fit unified epsilon:

plaw.results.one.epsilon = recompute.epsilon(plaw.model.results, plaw.mod.data)

#summary by modal in plaw.model.results: with alpha, residual ss, residual stdev
#compared to objects lms for the linear model computed above, the power-law modal has better r-squared in all but one case (possible), and does it with one less parameter (alpha vs. slope & intercept).

## simulate 1-dimensional SDT model 
sdt.z = function (prop) {
	mu=.5
	sd=.15  # values of mu, sd don't matter for the purpose of calculating d', but does for other stuff.
	val = (qnorm(prop, mu, sd) - mu)/sd
	return(val)
}
d.prime = function(hit.rate, fa.rate) {
	return(sdt.z(hit.rate) - sdt.z(fa.rate))	
}

#collect hit and fa rates for use in model
modals=c("none","certain","necessary","probable","likely","plausible","possible")
hits=c()
misses=c()
false.alarms=c()
correct.rejections=c()
for (i in 1:length(modals)) {
	mod=modals[i]
	valid.mod.d = valid[valid$modal==mod,]
	invalid.mod.d = invalid[invalid$modal==mod,]
	hits=c(hits, length(valid.mod.d[valid.mod.d$response=="accept",]$rt))
	misses=c(misses, length(valid.mod.d[valid.mod.d$response=="reject",]$rt))
	false.alarms=c(false.alarms,length(invalid.mod.d[invalid.mod.d$response=="accept",]$rt))
	correct.rejections=c(correct.rejections,length(invalid.mod.d[invalid.mod.d$response=="reject",]$rt))
}
sdt.data=data.frame(hits=hits, misses=misses, false.alarms=false.alarms,correct.rejections=correct.rejections)


split.half = function(dat) {	
	dat.nrows = length(dat[,1])
	d1.indices = sample(1:dat.nrows, dat.nrows/2, replace=F)
	d2.indices = setdiff(1:dat.nrows, d1.indices) 
#		if (odd(length(d2.indices))) {d2.indices = setdiff(d2.indices, sample(d2.indices, 1))}
# remove a random index so the data sets have equal size. not necessary?
	d1 = dat[d1.indices,]
	d2 = dat[d2.indices,]   
	pcodes = intersect(levels(factor(d1$premisecode)),levels(factor(d2$premisecode)))
	f = function(pcnow) {
		d1.now = subset(d1, premisecode == pcnow)
		d2.now = subset(d2, premisecode == pcnow)
		d1.now.prop = length(subset(d1.now, response == "accept")$rt) / length(d1.now$rt)
		d2.now.prop = length(subset(d2.now, response == "accept")$rt) / length(d2.now$rt)
		return(c(d1.now.prop, d2.now.prop))
	}
	props = sapply(pcodes, FUN=f)
	return(cor(props[1,], props[2,]))
}

modals = c("possible", "plausible", "likely", "probable", "certain", "necessary", "none")
nrep=10000
g = function(mod) {
	mod.dat = subset(d, modal == mod)
	f = function(n) {return(split.half(mod.dat))}
	this.mod.cors = sapply(1:nrep, FUN=f)
	return(this.mod.cors)
}

#t=proc.time()
#mods.split.half.cors = sapply(modals, g) #caution: this will take several hours to run with nrep=10000
#proc.time()-t
#return value will be a nrep x 6 data frame, with each column one modal's split half
modals.split.half = data.frame(mods.split.half.cors)

#1st time, split-half results:
#          av.cor, 10000 reps
#possible           0.3439880
#plausible          0.3707010
#likely             0.5683951
#probable           0.4745637
#certain            0.6652872
#necessary          0.6986537
#none               0.6402522

#2nd time, split-half results:
#          av.cor, 1000 reps			model rsq:
# certain           0.6971551			.6
# likely            0.6027082			.64
# necessary         0.7076703			.65
# plausible         0.3837012			.42
# possible          0.4405788   		.49
# probable          0.5629598			.49

# none              0.6217590			(NA)

# 10/17/13, after removing data from bad subjects:
    # possible         plausible           likely          probable         certain         necessary     
 # Min.   :0.03986   Min.   :0.01213   Min.   :0.2314   Min.   :0.1827   Min.   :0.4508   Min.   :0.4281  
 # 1st Qu.:0.28945   1st Qu.:0.27396   1st Qu.:0.5190   1st Qu.:0.4883   1st Qu.:0.6569   1st Qu.:0.6781  
 # Median :0.34724   Median :0.34191   Median :0.5787   Median :0.5427   Median :0.6992   Median :0.7167  
 # Mean   :0.34315   Mean   :0.34149   Mean   :0.5741   Mean   :0.5384   Mean   :0.6958   Mean   :0.7135  
 # 3rd Qu.:0.39949   3rd Qu.:0.40869   3rd Qu.:0.6327   3rd Qu.:0.5925   3rd Qu.:0.7384   3rd Qu.:0.7530  
 # Max.   :0.61383   Max.   :0.67578   Max.   :0.7959   Max.   :0.7668   Max.   :0.8773   Max.   :0.8831  
      # none       
 # Min.   :0.3641  
 # 1st Qu.:0.6011  
 # Median :0.6476  
 # Mean   :0.6410  
 # 3rd Qu.:0.6866  
 # Max.   :0.8217

######
###### permutation test

perm.test = function(n.sims) {
	cors=c()
	for (k in 1:n.sims) {
		perm = data.frame(dummy=c(0,0,0))
		arguments = levels(d$premisecode)
		for (i in 1:length(levels(d$modal))) {
			modal = levels(d$modal)[i]
			modd = d[d$modal == modal,]
			this.perm = data.frame(argument=sample(modd$premisecode), response=modd$response)
			this.perm.props = c()
			for (j in 1:length(arguments)) {
				pcnow = arguments[j]
				this.arg.data = this.perm[this.perm$argument == pcnow,]
				this.perm.props = c(this.perm.props, length(this.arg.data[this.arg.data$response == "accept",]$argument)/length(this.arg.data$argument))
			}
		perm =cbind(perm, this.perm.props)
	}
	perm = perm[,2:length(colnames(perm))]
	colnames(perm) = levels(d$modal)

	rank.cors=c()
	for (i in 1:7) {
		for (j in 1:7) {
			if (j > i) {
				rank.cors=c(rank.cors, cor(perm[,i], perm[,j], method="spearman"))
				}
			}
		}
	cors = c(cors, mean(rank.cors))
	}
	return(cors)
}
#permutations = perm.test(10000)
#perms.ten.thousand.summary = summary(permutations)


###rank-order correlation across modals for not.valid.props

rank.cors=c()
cnames=c()
for (i in 1:length(colnames(all.modal.proportions))) {
	for (j in 1:length(colnames(all.modal.proportions))) {
		#if (i != 4 && j != 4 && (j > i)) {
		if (j > i) {
			rank.cors=c(rank.cors, cor(all.modal.proportions[,i],all.modal.proportions[,j], method="spearman"))
			cnames=c(cnames, paste(colnames(all.modal.proportions)[i], colnames(all.modal.proportions)[j]))
		}
	}
}


prop.sim = function(baseline.vec, alphas.vec, epsilon, ns.mat, n.sims) {
#start by assuming that the model is correct: each true probability is the "none" proportion to the alpha, etc.
	predicted.proportions = data.frame(dummy=rep(0, length(baseline.vec)))
	for (i in 1:7) {
		pp.now = c()
		if (i < 4) {
			for (j in 1:length(baseline.vec)) {
				pp.now = c(pp.now, baseline.vec[j]^alphas.vec[i]*(1-epsilon) + epsilon/2)
			}
		} else if (i > 4) 
			for (j in 1:length(baseline.vec)) {
				pp.now = c(pp.now, baseline.vec[j]^alphas.vec[i-1]*(1-epsilon) + epsilon/2)
			} else {pp.now = baseline.vec}
		predicted.proportions = cbind(predicted.proportions, pp.now)
	}	
	predicted.proportions = predicted.proportions[,2:length(predicted.proportions[1,])]
#now, simulate the observed number of draws with this as the binomial proportions. 
#do this n.sims times and average the mean rank-order correlations
	cors = c()
	for (i in 1:n.sims) {
		sim.now = data.frame(dummy=c(rep("rem", length(baseline.vec))))
		for (j in 1:length(predicted.proportions[1,])) {
			sim.yes.counts = c()
			pred.vec.now = predicted.proportions[,j]
			ns.now = ns.mat[,j]
			for (k in 1:length(pred.vec.now)) {
				sim.yes.counts = c(sim.yes.counts, rbinom(1, ns.now[k], pred.vec.now[k]))
			}
			sim.prop = sim.yes.counts / ns.now
			sim.now = cbind(sim.now, sim.prop)
		}
		sim.now = sim.now[,2:length(sim.now[1,])]
# compute the rank correlations by column		
		rank.cors=c()
		for (i in 1:7) {
			for (j in 1:7) {
				if (j > i) {
				#if (i != 4 && j!= 4 && j > i) {
					rank.cors=c(rank.cors, cor(sim.now[,i], sim.now[,j], method="spearman"))
				}
			}
		}
		cors = c(cors, mean(rank.cors)) #only reporting the average rank correlation for this simulation.
	}
	return(cors)
} 

#t = proc.time()
eps=.1  # best-fit for entire data set, from above
rank.order.predicted.cors = prop.sim(all.modal.proportions[,4], plaw.model.results[1,], eps, all.modal.ns, 10000)
#proc.time() - t

summary(rank.order.predicted.cors)

# recording all rank cors, not just means per sim
full.prop.sim = function(baseline.vec, alphas.vec, epsilon, ns.mat, n.sims) {
#start by assuming that the model is correct: each true probability is the "none" proportion to the alpha, etc.
	predicted.proportions = data.frame(dummy=rep(0, length(baseline.vec)))
	for (i in 1:7) {
		pp.now = c()
		if (i < 4) {
			for (j in 1:length(baseline.vec)) {
				pp.now = c(pp.now, baseline.vec[j]^alphas.vec[i]*(1-epsilon) + epsilon/2)
			}
		} else if (i > 4) 
			for (j in 1:length(baseline.vec)) {
				pp.now = c(pp.now, baseline.vec[j]^alphas.vec[i-1]*(1-epsilon) + epsilon/2)
			} else {pp.now = baseline.vec}
		predicted.proportions = cbind(predicted.proportions, pp.now)
	}	
	predicted.proportions = predicted.proportions[,2:length(predicted.proportions[1,])]
#now, simulate the observed number of draws with this as the binomial proportions. 
#do this n.sims times and average the mean rank-order correlations
	cors = rep(NA, 7)
	for (i in 1:n.sims) {
		sim.now = data.frame(dummy=c(rep("rem", length(baseline.vec))))
		for (j in 1:length(predicted.proportions[1,])) {
			sim.yes.counts = c()
			pred.vec.now = predicted.proportions[,j]
			ns.now = ns.mat[,j]
			for (k in 1:length(pred.vec.now)) {
				sim.yes.counts = c(sim.yes.counts, rbinom(1, ns.now[k], pred.vec.now[k]))
			}
			sim.prop = sim.yes.counts / ns.now
			sim.now = cbind(sim.now, sim.prop)
		}
		sim.now = sim.now[,2:length(sim.now[1,])]
# compute the rank correlations by column		
		rank.cors=c()
		for (i in 1:7) {
			for (j in 1:7) {
				#if (i != 4 && j!= 4 && j > i) {
				if (j > i) {
					rank.cors=c(rank.cors, cor(sim.now[,i], sim.now[,j], method="spearman"))
				}
			}
		}
		cors = rbind(cors, rank.cors) #only reporting the average rank correlation for this simulation.
	}
	return(cors[2:length(cors[,1]),])
} 

t = proc.time()
eps=.1  # best-fit epsilon for entire data set, from above
full.matrix.rank.order.predicted.cors = full.prop.sim(all.modal.proportions[,4], plaw.model.results[1,], eps, all.modal.ns, 50000)
proc.time() - t

mean.by.pair.rank.cors = apply(full.matrix.rank.order.predicted.cors, MARGIN=2, FUN=mean)

#plot(rank.cors, mean.by.pair.rank.cors, pch=20, col='blue', xlab="Mean simulated rank-order correlation", ylab="Observed rank-order correlation", xlim=c(.35,.9), ylim=c(.35,.9))
plot(rank.cors, mean.by.pair.rank.cors, pch=20, cex=1.8, col='blue', xlab="Mean simulated rank-order correlation", ylab="Observed rank-order correlation", xlim=c(0,1), ylim=c(0,1))
abline(a=0,b=1,lty=3)

ymin=apply(full.matrix.rank.order.predicted.cors, MARGIN=2, FUN=function(x){
	return(quantile(x, .05))
})
ymax=apply(full.matrix.rank.order.predicted.cors, MARGIN=2, FUN=function(x){
	return(quantile(x, .95))
})
arrows(x0=rank.cors, y0=ymin, y1=ymax, code=3, angle=90)

g = function(p) {
	v = c()
	for (i in 1:21) {
		if (rank.cors[i] <= quantile(mean.by.pair.rank.cors, p) || rank.cors[i] >= quantile(mean.by.pair.rank.cors, 1-p)) {
			v = c(v, i)
		}
	}
	return(v)
}

cor(mean.by.pair.rank.cors, rank.cors) # R=.86, p < .001

#
# sim figures
#

library(grDevices)
plot(1,1, xlim=c(0,1), ylim=c(0,1), col="white", main="", xlab="Conditional probability", ylab="Predicted response probability", cex.lab=1.5)
alphas=c(7, 5, 3, 2, 3/2, 1, 2/3, 1/2, 1/3, 1/5, 1/7)
cols=c("black", "red", "orange", "blue", "brown", "black", "brown", "blue", "orange", "red", "black")
for (i in 1:length(alphas)) {
	epsilon=.1
	curve((1 - epsilon) * x^alphas[i] + epsilon/2, col=cols[i], add=TRUE)
}
text(.10, .78, expression(paste(alpha[M], "=7")), col=cols[1])
text(.12, .7, expression(paste(alpha[M], "=5")), col=cols[2])
text(.15, .6, expression(paste(alpha[M], "=3")), col=cols[3])
text(.18, .5, expression(paste(alpha[M], "=2")), col=cols[4])
text(.4, .48, expression(paste(alpha[M], "=3/2")), col=cols[5])
text(.34, .4, expression(paste(alpha[M], "=1")), col=cols[6])
text(.38, .32, expression(paste(alpha[M], "=2/3")), col=cols[7])
text(.52, .21, expression(paste(alpha[M], "=1/2")), col=cols[8])
text(.62, .16, expression(paste(alpha[M], "=1/3")), col=cols[9])
text(.73, .12, expression(paste(alpha[M], "=1/5")), col=cols[10])
text(.78, .07, expression(paste(alpha[M], "=1/7")), col=cols[11])
#text(.1,.9, expression(paste(epsilon, "=.1")), cex=1.2)

## z ~ alpha chart
z.alpha = function(alpha) {
	epsilon = .1
	hits = 1 - epsilon/2 #we're looking at logically valid arguments now
	false.alarms = .5^alpha*(1 - epsilon) + epsilon/2
	return(d.prime(hits, false.alarms))
}
z.alpha(1)
plot(1,1, xlim=c(0,7), ylim=c(0,3.5), col="white", main="", xlab=expression(alpha[M]), ylab=expression(paste("predicted d-prime, mean contingent strength=.5")), cex.lab=1.3)
curve(z.alpha(x), add=TRUE, col="blue",lwd=2)

duplicate = sapply(levels(d$workerid), function(y) {
	rd = subset(d, workerid==y)
	return(length(rd$premisecode)-length(unique(rd$premisecode)))
})


