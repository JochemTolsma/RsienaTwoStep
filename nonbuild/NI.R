#make sure only rateb can be positive and rate can be negative.

require(truncnorm)
cov <- rtruncnorm(n = 20, a = -1, b = 1)
groups <- sample(1:2, 20, replace =20)
ego <- 1
alters <- 2:20
H=1
cov[ego] <- -1
weight_alters <- 1 -  (abs(cov[ego] - cov[alters])*H + (1-H)*2*abs(groups[ego] != groups[alters]))
opinion_delta <- 0.5*(cov[alters] - cov[ego])*weight_alters
mean(opinion_delta)
maximize <- 1 - abs(mean(opinion_delta))
maximize
df <- data.frame(cov, groups, c(NA,weight_alters), c(NA, opinion_delta))


ts_eval(net=ts_net1, ego=6, statistics=list(ts_degree, ts_recip), parameters=c(-2,1))
ts_eval(net=ts_net1, ego=10, ccovar=df_ccovar1, statistics=list(ts_degree, ts_recip, ts_transTrip, ts_transMedTrip, list(ts_egoX, "cov1")), parameters=c(-2,2,7,7,1))



ts_avAlt(beh=df_ccovar1[,2], net=ts_net1, ego=2)

ts_eval_beh(beh= df_ccovar1[,2], net=ts_net1, ego=2, ccovar=df_ccovar1, statistics = list(ts_linear), parameters=c(1))
ts_eval_beh(beh= df_ccovar1[,2], net=ts_net1, ego=2, ccovar=df_ccovar1, statistics = list(ts_quad), parameters=c(1))

ts_eval_beh(beh= df_ccovar1[,2], net=ts_net1, ego=2, ccovar=df_ccovar1, statistics = list(list(ts_avAlt, "friendship")), parameters=c(1))
ts_eval_beh(beh= df_ccovar1[,1], net=ts_net1, ego=2, ccovar=df_ccovar1, statistics = list(list(ts_effFrom, "cov2")), parameters=c(1))


ts_avNiAlt(beh=df_ccovar1[,2], net=ts_net1, ego=3, cov=df_ccovar1[,1], parameter= 0.5)
ts_1NiAlt(beh=df_ccovar1[,2], net=ts_net1, ego=3, cov=df_ccovar1[,1], parameter= 0.5)
ts_avAlt(beh=df_ccovar1[,2], net=ts_net1, ego=2, cov=df_ccovar1[,1])






ccovar_prep <- ts_prepdata(df_ccovar1)

ts_avNiAlt(beh=ccovar_prep$cov2, net=ts_net1, ego=3, cov=ccovar_prep$cov1, parameter= 1)

ts_1NiAlt(beh=ccovar_prep$cov2, net=ts_net1, ego=3, cov=ccovar_prep$cov1, parameter= 0)

ts_eval_beh(beh= ccovar_prep$cov2, net=ts_net1, ego=3, ccovar=ccovar_prep, statistics = list(list(ts_avNiAlt, cov="cov1", parameter=0.5)), parameters=c(1))


ccovar_prep <- ts_prepdata(df_ccovar1)
attributes(ccovar)


df_ccovar1

net1 <- matrix(rep(1,100), nrow=10, ncol =10)
diag(net1) <- 0
group <- rep(c(-1,1), each=5)
#group <- rep(1, 10)
require(truncnorm)
#opinion <- round(rtruncnorm(n = 10, a = -10, b = 10, sd=6)) #need to be integers
opinion <- -5:4
df <- data.frame(opinion = opinion, group=group)
df <- ts_prepdata(df)
attributes(df$opinion)
df

ego1 <- 8
options <- ts_alternatives_ministep_beh(df$opinion, ego1)

sapply(options, as.numeric)[ego1,]

ts_avNiAlt(options[[1]], net=net1, ego=ego1, cov=df$group, parameter= 0)
ts_avNiAlt(options[[2]], net=net1, ego=ego1, cov=df$group, parameter= 0)
ts_avNiAlt(options[[3]], net=net1, ego=ego1, cov=df$group, parameter= 0)

ts_1NiAlt(beh = options[[1]], net=net1, ego=ego1, cov=df$group, parameter= 0, seed = 234)
ts_1NiAlt(options[[2]], net=net1, ego=ego1, cov=df$group, parameter= 0, seed = 234)
ts_1NiAlt(options[[3]], net=net1, ego=ego1, cov=df$group, parameter= 0, seed = 234)

eval <- NA

eval[1] <- ts_avNiAlt(beh = options[[1]], net=net1, ego=ego1, cov=df$group, parameter= 1)
eval[2] <- ts_avNiAlt(options[[2]], net=net1, ego=ego1, cov=df$group, parameter= 1)
eval[3] <- ts_avNiAlt(options[[3]], net=net1, ego=ego1, cov=df$group, parameter= 1)

eval2 <- eval * 100

hist(sample(1:3, 1000, prob=exp(eval2 - max(eval2)), replace = TRUE))
eval <- eval - max(eval)
exp(eval *100 )




#note, we always need a structural component. / we need to tweak if we want behavioral only part
#note, if behavioral part, we always need ts_linear
#only group matters!
test <- ts_sim(   #         nsims = 1,
                    startvalues = c(100, 0, 0, 200,0, 100),
                    net1 = net1,
                    ccovar = df,
                    statistics = list(ts_degree, ts_recip, ts_linear, list(ts_1NiAlt, cov="group", parameter=0)),
                    p2step = c(0, 0, 1),
                    dist1 = NULL,
                    dist2 = NULL,
                    modet1 = "degree",
                    modet2 = "degree",
                    chain = TRUE,
                    verbose = TRUE,
           #         parallel = FALSE

)


#Ik snap niet waarom geen consensus of polarisatie!! waarschijnlijk door teveel ruis.








mean(res[[1]])
mean(df$opinion)
as.numeric(test$final$beh_n)
res <- sapply(test$chain$behs, as.numeric)
ressd <- na.omit(sapply(res, sd))
resmean <- na.omit(sapply(res, mean))

plot(1:length(ressd), ressd)
plot(1:length(ressd), resmean)
plot(1:20, resmean[1:20])

res[1:20]
as.numeric(test$beh_n)
df$group

df$opinion

df
