function (m,n,p0,alpha,ps,delta,o) 
{
set.seed(0.1)
arl.ccc = 0
arl.op = 0
p.diff = 0
rl.ccc = rep(0,m)
rl.op = rep(0,m)

	ucl.ccc=(log(alpha/2)/log(1-p0))
	lcl.ccc=(log(1-(alpha/2))/log(1-p0))
	gam.al=(log(log(1-alpha/2)/log(alpha/2))/log((alpha/2)/(1-alpha/2)))
	ucl.op=(gam.al*log(alpha/2)/log(1-p0))
	lcl.op=(gam.al*log(1-(alpha/2))/log(1-p0))

cat('\n','ucl.ccc = ',ucl.ccc,' ','lcl.ccc = ',lcl.ccc,' ','ucl.op = ',ucl.op,' ','lcl.op = ',lcl.op,'\n') 

for(i in 1:o)
{
	for(i in 1:m)
	{
		x=rep(0,n)
      		x=rgeom(n,ps)

########### Compare CCC ##################

		in.control=TRUE
		j=0
		k=0

	while(in.control)
		{
			j=j+1
			k=k+1
		if((x[k]>ucl.ccc)||(x[k]<lcl.ccc))
			{
			in.control=FALSE
			}
		}
		rl.ccc[i]=j



########### Compare op ###################

		in.control=TRUE
		j=0
		k=0

	while(in.control)
		{
			j=j+1
			k=k+1
		if((x[k]>ucl.op)||(x[k]<lcl.op))
			{
			in.control=FALSE
			}
		}
		rl.op[i]=j
}

######### print ###########################
arl.ccc=mean(rl.ccc)
arl.op=mean(rl.op)

p.diff=(arl.op-arl.ccc)/arl.ccc*100

cat('\n','Ps = ',ps*1000000,'arl.ccc =',arl.ccc,'arl.op =',arl.op,'p.diff = ',p.diff,'%','\n')

		ps=ps+delta
	}
}


#command com(10000,7000,0.0001,0.0027,0.00006,0.00001,15)
