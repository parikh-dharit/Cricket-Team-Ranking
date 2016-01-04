md <- read.csv(file="C:/Users/Dharit/Desktop/data/matches_summary.csv", strip.white=TRUE, header=TRUE)
#sapply(md,class) 
#md[6] has Hong Kong
#str(md)
#x<-as.Date(md$Date[[1]],format="%m/%d/%Y")
indexes<-seq(1:19)
md$Date<-as.character(md$Date)
md$Date<-as.Date(md$Date, "%m/%d/%Y")
md$DateY <- format(md$Date, '%Y')
md$DateM <- format(md$Date, '%m')
curr_Date<- as.Date("2014-07-13") #(Y-mm-dd)
md$Team1=as.character(md$Team1)
md$Team2=as.character(md$Team2) 
win<-NULL
for(val in 1:NROW(md))
{
	if(md$Score_Runs1[[val]] > md$Score_Runs2[[val]])
	{
		win<-c(win, md$Team1[[val]])
	}
	else if(md$Score_Runs2[[val]] > md$Score_Runs1[[val]])
	{
		win<-c(win, md$Team2[[val]])
	}
	else
	{
		win<-c(win,"Tie")
	}
}
md$wTeam<-win
Teams<-unique(c(as.character(md$Team1),as.character(md$Team2)))
# for all the teams -> for team i whether it is team 1 or 2 if it has scored more more than 50 runs than their opponent then they will get +10 points for less than that +5 for losing same points in -. If they 
#md[(md$Team1=="India")|(md$Team2=="India"),]
TeamRanks<-c("Australia", "South Africa", "India", "England", "Pakistan", "Sri Lanka", "New Zealand", "West Indies", "Bangladesh", "Zimbabwe", "Ireland", "Afghanistan", "Scotland", "Netherlandss", "Bermuda", "Kenya", "Canada", "U.A.E.", "Hong Kong")
oldTRanks<-data.frame(TeamRanks,indexes)
points<-rep(100.0,19)
conf<-rep(0.0,19)
for(t in Teams)
{
	teamIndex=match(t,Teams)
	tempData=md[(md$Team1==t)|(md$Team2==t),]
	for(i in 1:NROW(tempData))
	{
		if(tempData$Team1[[i]]==t)
		{
			if(tempData$Inning_Team1[[i]]==1)
			{
				if(match(tempData$Team1[[i]],TeamRanks)<match(tempData$Team2[[i]],TeamRanks))
				{
					w = 2/5    # 2/3
					l = 3/5    #4/3
				}
				else
				{
					w = 3/5    # 4/3
					l = 2/5    #2/3
				}
				scoreDifference = tempData$Score_Runs1[[i]] - tempData$Score_Runs2[[i]]
				if(scoreDifference>50)
				{
					points[[teamIndex]]=points[[teamIndex]]+(10*w)
				}
				else if(scoreDifference>0)
				{
					points[[teamIndex]]=points[[teamIndex]]+(5*w)
				}
				else if(scoreDifference==0)
				{
					points[[teamIndex]]=points[[teamIndex]]
				}
				else
				{
					if(tempData$Score_Wickets2[[i]] < 5)
					{
						points[[teamIndex]]=points[[teamIndex]] - (10*l)
					}
					else
					{
						points[[teamIndex]]=points[[teamIndex]] - (5*l)
					}
				}
			}
			else if(tempData$Inning_Team1[[i]]==2)
			{
				scoreDifference = tempData$Score_Runs1[[i]] - tempData$Score_Runs2[[i]]
				if(scoreDifference>0)
				{	
					if(tempData$Score_Wickets1[[i]] < 5)
					{
						points[[teamIndex]]=points[[teamIndex]] + (10*w)
					}
					else
					{
						points[[teamIndex]]=points[[teamIndex]] + (5*w)
					}
				}
				else if(scoreDifference==0)
				{
					points[[teamIndex]]=points[[teamIndex]]
				}
				else
				{
					if(scoreDifference>(-50))
					{
						points[[teamIndex]]=points[[teamIndex]] - (5*l)
					}
					else
					{
						points[[teamIndex]]=points[[teamIndex]] - (10*l)
					}
				}
			}
		}
		else if(tempData$Team2[[i]]==t)
		{
			if(match(tempData$Team2[[i]],TeamRanks)<match(tempData$Team1[[i]],TeamRanks))
			{
				w = 2/5     #2/3
				l = 3/5      #4/3
			}
			else
			{
				w = 3/5    #4/3
				l = 2/5     #2/3
			}
			if(tempData$Inning_Team2[[i]]==1)
			{
				scoreDifference = tempData$Score_Runs2[[i]] - tempData$Score_Runs1[[i]]
				if(scoreDifference>50)
				{
					points[[teamIndex]]=points[[teamIndex]]+(10*w)
				}
				else if(scoreDifference>0)
				{
					points[[teamIndex]]=points[[teamIndex]]+(5*w)
				}
				else if(scoreDifference==0)
				{
					points[[teamIndex]]=points[[teamIndex]]
				}
				else
				{
					if(tempData$Score_Wickets1[[i]] < 5)
					{
						points[[teamIndex]]=points[[teamIndex]] - (10*l)
					}
					else
					{
						points[[teamIndex]]=points[[teamIndex]] - (5*l)
					}
				}
			}
			else
			{
				scoreDifference = tempData$Score_Runs2[[i]] - tempData$Score_Runs1[[i]]
				if(scoreDifference>0)
				{	
					if(tempData$Score_Wickets2[[i]] < 5)
					{
						points[[teamIndex]]=points[[teamIndex]] + (10*w)
					}
					else
					{
						points[[teamIndex]]=points[[teamIndex]] + (5*w)
					}
				}
				else if(scoreDifference==0)
				{
					points[[teamIndex]]=points[[teamIndex]]
				}
				else
				{
					if(scoreDifference>(-50))
					{
						points[[teamIndex]]=points[[teamIndex]] - (5*l)
					}
					else
					{
						points[[teamIndex]]=points[[teamIndex]] - (10*l)
					}
				}
			}
		}
		
	}
}
normPoints=(points-min(points))/(max(points)-min(points))
teamPoint<-data.frame(Teams,points)
newRanks<-teamPoint[order(teamPoint$points,decreasing=TRUE), ]
newTRanks<-data.frame(newRanks$Teams, indexes)
newTRanks$oldRanks<-match(newTRanks$newRanks.Teams, oldTRanks$TeamRanks)
colnames(newTRanks)<-c("Team","newRank","oldRank")

add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
timePoint1=add.months(curr_Date,-1)
timePoint2=add.months(curr_Date,-3)
timePoint3=add.months(curr_Date,-12)

for(t in Teams)
{
	teamIndex=match(t,Teams)
	tempData1 = subset(md, ((md$Team1==t)|(md$Team2==t)) & (md$Date<curr_Date) & (md$Date>timePoint1))
	if(NROW(tempData1)!=0)
	{
		conf[[teamIndex]] = conf[[teamIndex]] + ((NROW(tempData1[tempData1$wTeam==t,])*100)/NROW(tempData1))*(5/10)
	}
	tempData2 = subset(md, ((md$Team1==t)|(md$Team2==t)) & (md$Date<timePoint1) & (md$Date>timePoint2))
	#tempData2=subset(md,(((md$Team1==t)|(md$Team2==t))&(md$Date<timePoint1)&(md$Date>timePoint2))
	if(NROW(tempData2)!=0)
	{
		conf[[teamIndex]] = conf[[teamIndex]] + ((NROW(tempData2[tempData2$wTeam==t,])*100)/NROW(tempData2))*(3/10)
	}
	tempData3 = subset(md, ((md$Team1==t)|(md$Team2==t)) & (md$Date<timePoint2) & (md$Date>timePoint3))
	#tempData3=subset(md,(((md$Team1==t)|(md$Team2==t))&(md$Date<timePoint2)&(md$Date>timePoint3))
	if(NROW(tempData3)!=0)
	{
		conf[[teamIndex]] = conf[[teamIndex]] + ((NROW(tempData3[tempData3$wTeam==t,])*100)/NROW(tempData3))*(2/10)
	}
}
normConf=(conf-min(conf))/(max(conf)-min(conf))
teamConf<-data.frame(Teams,conf)

tcp<-data.frame(Teams,points,conf)
ntcp<-data.frame(Teams,normPoints,normConf)

library(dimple)

dimple(ntcp[1:10,],
       xMeasure="normPoints",
       yMeasure="normConf",
       series="Teams",
       legend=TRUE,
       chartType="bubble")
	   
dimple(tcp[1:10,],
       xCategory="Teams",
       yMeasure="points",
       zMeasure="conf",
	 series="Teams",
       legend=TRUE,
       chartType="bubble")
	   
dimple(newTRanks,
       xMeasure="oldRank",
       yMeasure="newRank",
       series="Team",
       legend=TRUE)
