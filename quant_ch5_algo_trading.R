library('quantmod')
getSymbols("^DJI",src="yahoo")

dji<- DJI[,"DJI.Close"]
dji<- dji[(index(dji) >= "2010-01-01" & index(dji) <= "2015-12-31"),]
ret_dji<- Delt(dji,k=1)
ret_dji<- Delt(dji,k=1:3)

head(ret_dji)
