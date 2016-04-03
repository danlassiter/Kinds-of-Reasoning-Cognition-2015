d=read.csv("LG2015-data.csv", header=TRUE)

length(levels(factor(d$workerid))) # subjects total: 507
length(d$workerid) # 10260 data points

d=subset(d, language %in% c("English", "english+(north+american)", "Englis", "English.+", "english+", "Rnglish"))
# only data from self-reported native English speakers

length(levels(factor(d$workerid))) # 484 remaining
length(d$workerid) # 9797 data points remaining

bad.workers = c()
workers = levels(factor(d$workerid))
for (i in 1:length(workers)) {
	wd = subset(d, workerid == workers[i])
	responses = wd$response
	if (all(responses == 'accept') || all(responses == 'reject')) {
		bad.workers = c(bad.workers, workers[i])
	}
}

d = subset(d, !(workerid %in% bad.workers))

d=d[d$rt >= 3500,]  # remove 196 trials with really fast RT

length(levels(factor(d$workerid))) # 440 remaining
length(d$workerid) # 8710 data points remaining

