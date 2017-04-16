goal_diff <- read.table(file="match_goal_differential.csv",sep = ",",header=TRUE);

seasons <- goal_diff$season
s08 <- which( seasons == "2008/2009" )
s09 <- which( seasons == "2009/2010" )
s10 <- which( seasons == "2010/2011" )
s11 <- which( seasons == "2011/2012" )
s12 <- which( seasons == "2012/2013" )
s13 <- which( seasons == "2013/2014" )
s14 <- which( seasons == "2014/2015" )
s15 <- which( seasons == "2015/2016" )

gd08 <- goal_diff[s08,]
gd09 <- goal_diff[s09,]
gd10 <- goal_diff[s10,]
gd11 <- goal_diff[s11,]
gd12 <- goal_diff[s12,]
gd13 <- goal_diff[s13,]
gd14 <- goal_diff[s14,]
gd15 <- goal_diff[s15,]

#
# Information from the database
# Teams by season
#
t08 <- c("ARS","AVL","BLB","BOL","CHE","EVE","FUL","HUL","LIV","MCI","MID","MUN","NEW","POR","STK","SUN","TOT","WBA","WHU","WIG")
t09 <- c("ARS","AVL","BIR","BLB","BOL","BUR","CHE","EVE","FUL","HUL","LIV","MCI","MUN","POR","STK","SUN","TOT","WHU","WIG","WOL")
t10 <- c("ARS","AVL","BIR","BLA","BLB","BOL","CHE","EVE","FUL","LIV","MCI","MUN","NEW","STK","SUN","TOT","WBA","WHU","WIG","WOL")
t11 <- c("ARS","AVL","BLB","BOL","CHE","EVE","FUL","LIV","MCI","MUN","NEW","NOR","QPR","STK","SUN","SWA","TOT","WBA","WIG","WOL")
t12 <- c("ARS","AVL","CHE","EVE","FUL","LIV","MCI","MUN","NEW","NOR","QPR","REA","SOU","STK","SUN","SWA","TOT","WBA","WHU","WIG")
t13 <- c("ARS","AVL","CAR","CHE","CRY","EVE","FUL","HUL","LIV","MCI","MUN","NEW","NOR","SOU","STK","SUN","SWA","TOT","WBA","WHU")
t14 <- c("ARS","AVL","BUR","CHE","CRY","EVE","HUL","LEI","LIV","MCI","MUN","NEW","QPR","SOU","STK","SUN","SWA","TOT","WBA","WHU")
t15 <- c("ARS","AVL","BOU","CHE","CRY","EVE","LEI","LIV","MCI","MUN","NEW","NOR","SOU","STK","SUN","SWA","TOT","WAT","WBA","WHU")

# Since we are doing cumulative counts within a season
# it is easier to process them individually.

#
# 2008/2009 season
#
for ( t in t08 ) {

   #
   # Home games for this team
   #
   htsn <- gd08$home_team_short_name
   gmh <- which( htsn == t )

   #
   # Single-game home-team goal differential
   #
   hgd0 <- gd08[gmh,]$home_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd08[gmh[2:19],]$home_goal_diff_1 <- hgd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd08[gmh[2],]$home_goal_diff_2 <- hgd0[1]
   for ( i in 1:17 ) {
      gd08[gmh[i+2],]$home_goal_diff_2 <- sum( hgd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd08[gmh[2],]$home_goal_diff_3 <- hgd0[1]
   gd08[gmh[3],]$home_goal_diff_3 <- sum( hgd0[1:2] )
   for ( i in 1:16 ) {
      gd08[gmh[i+3],]$home_goal_diff_3 <- sum( hgd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd08[gmh[2],]$home_goal_diff_5 <- hgd0[1]
   gd08[gmh[3],]$home_goal_diff_5 <- sum( hgd0[1:2] )
   gd08[gmh[4],]$home_goal_diff_5 <- sum( hgd0[1:3] )
   gd08[gmh[5],]$home_goal_diff_5 <- sum( hgd0[1:4] )
   for ( i in 1:14 ) {
      gd08[gmh[i+5],]$home_goal_diff_5 <- sum( hgd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd08[gmh[2],]$home_goal_diff_10 <- hgd0[1]
   for ( i in 2:9 ) {
      gd08[gmh[i+1],]$home_goal_diff_10 <- sum( hgd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd08[gmh[i+10],]$home_goal_diff_10 <- sum( hgd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd08[gmh[i+1],]$home_goal_diff_total <- sum( hgd0[1:i] )
   }


   #
   # Same for this team when away
   #
   atsn <- gd08$away_team_short_name
   gma <- which( atsn == t )
   agd0 <- gd08[gma,]$away_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd08[gma[2:19],]$away_goal_diff_1 <- agd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd08[gma[2],]$away_goal_diff_2 <- agd0[1]
   for ( i in 1:17 ) {
      gd08[gma[i+2],]$away_goal_diff_2 <- sum( agd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd08[gma[2],]$away_goal_diff_3 <- agd0[1]
   gd08[gma[3],]$away_goal_diff_3 <- sum( agd0[1:2] )
   for ( i in 1:16 ) {
      gd08[gma[i+3],]$away_goal_diff_3 <- sum( agd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd08[gma[2],]$away_goal_diff_5 <- agd0[1]
   gd08[gma[3],]$away_goal_diff_5 <- sum( agd0[1:2] )
   gd08[gma[4],]$away_goal_diff_5 <- sum( agd0[1:3] )
   gd08[gma[5],]$away_goal_diff_5 <- sum( agd0[1:4] )
   for ( i in 1:14 ) {
      gd08[gma[i+5],]$away_goal_diff_5 <- sum( agd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd08[gma[2],]$away_goal_diff_10 <- agd0[1]
   for ( i in 2:9 ) {
      gd08[gma[i+1],]$away_goal_diff_10 <- sum( agd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd08[gma[i+10],]$away_goal_diff_10 <- sum( agd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd08[gma[i+1],]$away_goal_diff_total <- sum( agd0[1:i] )
   }
}


#
# 2009/2010 season
#
for ( t in t09 ) {

   #
   # Home games for this team
   #
   htsn <- gd09$home_team_short_name
   gmh <- which( htsn == t )

   #
   # Single-game home-team goal differential
   #
   hgd0 <- gd09[gmh,]$home_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd09[gmh[2:19],]$home_goal_diff_1 <- hgd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd09[gmh[2],]$home_goal_diff_2 <- hgd0[1]
   for ( i in 1:17 ) {
      gd09[gmh[i+2],]$home_goal_diff_2 <- sum( hgd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd09[gmh[2],]$home_goal_diff_3 <- hgd0[1]
   gd09[gmh[3],]$home_goal_diff_3 <- sum( hgd0[1:2] )
   for ( i in 1:16 ) {
      gd09[gmh[i+3],]$home_goal_diff_3 <- sum( hgd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd09[gmh[2],]$home_goal_diff_5 <- hgd0[1]
   gd09[gmh[3],]$home_goal_diff_5 <- sum( hgd0[1:2] )
   gd09[gmh[4],]$home_goal_diff_5 <- sum( hgd0[1:3] )
   gd09[gmh[5],]$home_goal_diff_5 <- sum( hgd0[1:4] )
   for ( i in 1:14 ) {
      gd09[gmh[i+5],]$home_goal_diff_5 <- sum( hgd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd09[gmh[2],]$home_goal_diff_10 <- hgd0[1]
   for ( i in 2:9 ) {
      gd09[gmh[i+1],]$home_goal_diff_10 <- sum( hgd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd09[gmh[i+10],]$home_goal_diff_10 <- sum( hgd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd09[gmh[i+1],]$home_goal_diff_total <- sum( hgd0[1:i] )
   }


   #
   # Same for this team when away
   #
   atsn <- gd09$away_team_short_name
   gma <- which( atsn == t )
   agd0 <- gd09[gma,]$away_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd09[gma[2:19],]$away_goal_diff_1 <- agd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd09[gma[2],]$away_goal_diff_2 <- agd0[1]
   for ( i in 1:17 ) {
      gd09[gma[i+2],]$away_goal_diff_2 <- sum( agd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd09[gma[2],]$away_goal_diff_3 <- agd0[1]
   gd09[gma[3],]$away_goal_diff_3 <- sum( agd0[1:2] )
   for ( i in 1:16 ) {
      gd09[gma[i+3],]$away_goal_diff_3 <- sum( agd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd09[gma[2],]$away_goal_diff_5 <- agd0[1]
   gd09[gma[3],]$away_goal_diff_5 <- sum( agd0[1:2] )
   gd09[gma[4],]$away_goal_diff_5 <- sum( agd0[1:3] )
   gd09[gma[5],]$away_goal_diff_5 <- sum( agd0[1:4] )
   for ( i in 1:14 ) {
      gd09[gma[i+5],]$away_goal_diff_5 <- sum( agd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd09[gma[2],]$away_goal_diff_10 <- agd0[1]
   for ( i in 2:9 ) {
      gd09[gma[i+1],]$away_goal_diff_10 <- sum( agd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd09[gma[i+10],]$away_goal_diff_10 <- sum( agd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd09[gma[i+1],]$away_goal_diff_total <- sum( agd0[1:i] )
   }
}


#
# 2010/2011 season
#
for ( t in t10 ) {

   #
   # Home games for this team
   #
   htsn <- gd10$home_team_short_name
   gmh <- which( htsn == t )

   #
   # Single-game home-team goal differential
   #
   hgd0 <- gd10[gmh,]$home_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd10[gmh[2:19],]$home_goal_diff_1 <- hgd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd10[gmh[2],]$home_goal_diff_2 <- hgd0[1]
   for ( i in 1:17 ) {
      gd10[gmh[i+2],]$home_goal_diff_2 <- sum( hgd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd10[gmh[2],]$home_goal_diff_3 <- hgd0[1]
   gd10[gmh[3],]$home_goal_diff_3 <- sum( hgd0[1:2] )
   for ( i in 1:16 ) {
      gd10[gmh[i+3],]$home_goal_diff_3 <- sum( hgd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd10[gmh[2],]$home_goal_diff_5 <- hgd0[1]
   gd10[gmh[3],]$home_goal_diff_5 <- sum( hgd0[1:2] )
   gd10[gmh[4],]$home_goal_diff_5 <- sum( hgd0[1:3] )
   gd10[gmh[5],]$home_goal_diff_5 <- sum( hgd0[1:4] )
   for ( i in 1:14 ) {
      gd10[gmh[i+5],]$home_goal_diff_5 <- sum( hgd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd10[gmh[2],]$home_goal_diff_10 <- hgd0[1]
   for ( i in 2:9 ) {
      gd10[gmh[i+1],]$home_goal_diff_10 <- sum( hgd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd10[gmh[i+10],]$home_goal_diff_10 <- sum( hgd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd10[gmh[i+1],]$home_goal_diff_total <- sum( hgd0[1:i] )
   }


   #
   # Same for this team when away
   #
   atsn <- gd10$away_team_short_name
   gma <- which( atsn == t )
   agd0 <- gd10[gma,]$away_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd10[gma[2:19],]$away_goal_diff_1 <- agd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd10[gma[2],]$away_goal_diff_2 <- agd0[1]
   for ( i in 1:17 ) {
      gd10[gma[i+2],]$away_goal_diff_2 <- sum( agd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd10[gma[2],]$away_goal_diff_3 <- agd0[1]
   gd10[gma[3],]$away_goal_diff_3 <- sum( agd0[1:2] )
   for ( i in 1:16 ) {
      gd10[gma[i+3],]$away_goal_diff_3 <- sum( agd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd10[gma[2],]$away_goal_diff_5 <- agd0[1]
   gd10[gma[3],]$away_goal_diff_5 <- sum( agd0[1:2] )
   gd10[gma[4],]$away_goal_diff_5 <- sum( agd0[1:3] )
   gd10[gma[5],]$away_goal_diff_5 <- sum( agd0[1:4] )
   for ( i in 1:14 ) {
      gd10[gma[i+5],]$away_goal_diff_5 <- sum( agd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd10[gma[2],]$away_goal_diff_10 <- agd0[1]
   for ( i in 2:9 ) {
      gd10[gma[i+1],]$away_goal_diff_10 <- sum( agd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd10[gma[i+10],]$away_goal_diff_10 <- sum( agd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd10[gma[i+1],]$away_goal_diff_total <- sum( agd0[1:i] )
   }
}


#
# 2011/2012 season
#
for ( t in t11 ) {

   #
   # Home games for this team
   #
   htsn <- gd11$home_team_short_name
   gmh <- which( htsn == t )

   #
   # Single-game home-team goal differential
   #
   hgd0 <- gd11[gmh,]$home_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd11[gmh[2:19],]$home_goal_diff_1 <- hgd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd11[gmh[2],]$home_goal_diff_2 <- hgd0[1]
   for ( i in 1:17 ) {
      gd11[gmh[i+2],]$home_goal_diff_2 <- sum( hgd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd11[gmh[2],]$home_goal_diff_3 <- hgd0[1]
   gd11[gmh[3],]$home_goal_diff_3 <- sum( hgd0[1:2] )
   for ( i in 1:16 ) {
      gd11[gmh[i+3],]$home_goal_diff_3 <- sum( hgd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd11[gmh[2],]$home_goal_diff_5 <- hgd0[1]
   gd11[gmh[3],]$home_goal_diff_5 <- sum( hgd0[1:2] )
   gd11[gmh[4],]$home_goal_diff_5 <- sum( hgd0[1:3] )
   gd11[gmh[5],]$home_goal_diff_5 <- sum( hgd0[1:4] )
   for ( i in 1:14 ) {
      gd11[gmh[i+5],]$home_goal_diff_5 <- sum( hgd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd11[gmh[2],]$home_goal_diff_10 <- hgd0[1]
   for ( i in 2:9 ) {
      gd11[gmh[i+1],]$home_goal_diff_10 <- sum( hgd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd11[gmh[i+10],]$home_goal_diff_10 <- sum( hgd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd11[gmh[i+1],]$home_goal_diff_total <- sum( hgd0[1:i] )
   }


   #
   # Same for this team when away
   #
   atsn <- gd11$away_team_short_name
   gma <- which( atsn == t )
   agd0 <- gd11[gma,]$away_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd11[gma[2:19],]$away_goal_diff_1 <- agd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd11[gma[2],]$away_goal_diff_2 <- agd0[1]
   for ( i in 1:17 ) {
      gd11[gma[i+2],]$away_goal_diff_2 <- sum( agd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd11[gma[2],]$away_goal_diff_3 <- agd0[1]
   gd11[gma[3],]$away_goal_diff_3 <- sum( agd0[1:2] )
   for ( i in 1:16 ) {
      gd11[gma[i+3],]$away_goal_diff_3 <- sum( agd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd11[gma[2],]$away_goal_diff_5 <- agd0[1]
   gd11[gma[3],]$away_goal_diff_5 <- sum( agd0[1:2] )
   gd11[gma[4],]$away_goal_diff_5 <- sum( agd0[1:3] )
   gd11[gma[5],]$away_goal_diff_5 <- sum( agd0[1:4] )
   for ( i in 1:14 ) {
      gd11[gma[i+5],]$away_goal_diff_5 <- sum( agd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd11[gma[2],]$away_goal_diff_10 <- agd0[1]
   for ( i in 2:9 ) {
      gd11[gma[i+1],]$away_goal_diff_10 <- sum( agd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd11[gma[i+10],]$away_goal_diff_10 <- sum( agd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd11[gma[i+1],]$away_goal_diff_total <- sum( agd0[1:i] )
   }
}

#
# 2012/2013 season
#
for ( t in t12 ) {

   #
   # Home games for this team
   #
   htsn <- gd12$home_team_short_name
   gmh <- which( htsn == t )

   #
   # Single-game home-team goal differential
   #
   hgd0 <- gd12[gmh,]$home_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd12[gmh[2:19],]$home_goal_diff_1 <- hgd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd12[gmh[2],]$home_goal_diff_2 <- hgd0[1]
   for ( i in 1:17 ) {
      gd12[gmh[i+2],]$home_goal_diff_2 <- sum( hgd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd12[gmh[2],]$home_goal_diff_3 <- hgd0[1]
   gd12[gmh[3],]$home_goal_diff_3 <- sum( hgd0[1:2] )
   for ( i in 1:16 ) {
      gd12[gmh[i+3],]$home_goal_diff_3 <- sum( hgd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd12[gmh[2],]$home_goal_diff_5 <- hgd0[1]
   gd12[gmh[3],]$home_goal_diff_5 <- sum( hgd0[1:2] )
   gd12[gmh[4],]$home_goal_diff_5 <- sum( hgd0[1:3] )
   gd12[gmh[5],]$home_goal_diff_5 <- sum( hgd0[1:4] )
   for ( i in 1:14 ) {
      gd12[gmh[i+5],]$home_goal_diff_5 <- sum( hgd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd12[gmh[2],]$home_goal_diff_10 <- hgd0[1]
   for ( i in 2:9 ) {
      gd12[gmh[i+1],]$home_goal_diff_10 <- sum( hgd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd12[gmh[i+10],]$home_goal_diff_10 <- sum( hgd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd12[gmh[i+1],]$home_goal_diff_total <- sum( hgd0[1:i] )
   }


   #
   # Same for this team when away
   #
   atsn <- gd12$away_team_short_name
   gma <- which( atsn == t )
   agd0 <- gd12[gma,]$away_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd12[gma[2:19],]$away_goal_diff_1 <- agd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd12[gma[2],]$away_goal_diff_2 <- agd0[1]
   for ( i in 1:17 ) {
      gd12[gma[i+2],]$away_goal_diff_2 <- sum( agd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd12[gma[2],]$away_goal_diff_3 <- agd0[1]
   gd12[gma[3],]$away_goal_diff_3 <- sum( agd0[1:2] )
   for ( i in 1:16 ) {
      gd12[gma[i+3],]$away_goal_diff_3 <- sum( agd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd12[gma[2],]$away_goal_diff_5 <- agd0[1]
   gd12[gma[3],]$away_goal_diff_5 <- sum( agd0[1:2] )
   gd12[gma[4],]$away_goal_diff_5 <- sum( agd0[1:3] )
   gd12[gma[5],]$away_goal_diff_5 <- sum( agd0[1:4] )
   for ( i in 1:14 ) {
      gd12[gma[i+5],]$away_goal_diff_5 <- sum( agd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd12[gma[2],]$away_goal_diff_10 <- agd0[1]
   for ( i in 2:9 ) {
      gd12[gma[i+1],]$away_goal_diff_10 <- sum( agd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd12[gma[i+10],]$away_goal_diff_10 <- sum( agd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd12[gma[i+1],]$away_goal_diff_total <- sum( agd0[1:i] )
   }
}


#
# 2013/2014 season
#
for ( t in t13 ) {

   #
   # Home games for this team
   #
   htsn <- gd13$home_team_short_name
   gmh <- which( htsn == t )

   #
   # Single-game home-team goal differential
   #
   hgd0 <- gd13[gmh,]$home_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd13[gmh[2:19],]$home_goal_diff_1 <- hgd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd13[gmh[2],]$home_goal_diff_2 <- hgd0[1]
   for ( i in 1:17 ) {
      gd13[gmh[i+2],]$home_goal_diff_2 <- sum( hgd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd13[gmh[2],]$home_goal_diff_3 <- hgd0[1]
   gd13[gmh[3],]$home_goal_diff_3 <- sum( hgd0[1:2] )
   for ( i in 1:16 ) {
      gd13[gmh[i+3],]$home_goal_diff_3 <- sum( hgd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd13[gmh[2],]$home_goal_diff_5 <- hgd0[1]
   gd13[gmh[3],]$home_goal_diff_5 <- sum( hgd0[1:2] )
   gd13[gmh[4],]$home_goal_diff_5 <- sum( hgd0[1:3] )
   gd13[gmh[5],]$home_goal_diff_5 <- sum( hgd0[1:4] )
   for ( i in 1:14 ) {
      gd13[gmh[i+5],]$home_goal_diff_5 <- sum( hgd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd13[gmh[2],]$home_goal_diff_10 <- hgd0[1]
   for ( i in 2:9 ) {
      gd13[gmh[i+1],]$home_goal_diff_10 <- sum( hgd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd13[gmh[i+10],]$home_goal_diff_10 <- sum( hgd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd13[gmh[i+1],]$home_goal_diff_total <- sum( hgd0[1:i] )
   }


   #
   # Same for this team when away
   #
   atsn <- gd13$away_team_short_name
   gma <- which( atsn == t )
   agd0 <- gd13[gma,]$away_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd13[gma[2:19],]$away_goal_diff_1 <- agd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd13[gma[2],]$away_goal_diff_2 <- agd0[1]
   for ( i in 1:17 ) {
      gd13[gma[i+2],]$away_goal_diff_2 <- sum( agd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd13[gma[2],]$away_goal_diff_3 <- agd0[1]
   gd13[gma[3],]$away_goal_diff_3 <- sum( agd0[1:2] )
   for ( i in 1:16 ) {
      gd13[gma[i+3],]$away_goal_diff_3 <- sum( agd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd13[gma[2],]$away_goal_diff_5 <- agd0[1]
   gd13[gma[3],]$away_goal_diff_5 <- sum( agd0[1:2] )
   gd13[gma[4],]$away_goal_diff_5 <- sum( agd0[1:3] )
   gd13[gma[5],]$away_goal_diff_5 <- sum( agd0[1:4] )
   for ( i in 1:14 ) {
      gd13[gma[i+5],]$away_goal_diff_5 <- sum( agd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd13[gma[2],]$away_goal_diff_10 <- agd0[1]
   for ( i in 2:9 ) {
      gd13[gma[i+1],]$away_goal_diff_10 <- sum( agd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd13[gma[i+10],]$away_goal_diff_10 <- sum( agd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd13[gma[i+1],]$away_goal_diff_total <- sum( agd0[1:i] )
   }
}


#
# 2014/2015 season
#
for ( t in t14 ) {

   #
   # Home games for this team
   #
   htsn <- gd14$home_team_short_name
   gmh <- which( htsn == t )

   #
   # Single-game home-team goal differential
   #
   hgd0 <- gd14[gmh,]$home_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd14[gmh[2:19],]$home_goal_diff_1 <- hgd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd14[gmh[2],]$home_goal_diff_2 <- hgd0[1]
   for ( i in 1:17 ) {
      gd14[gmh[i+2],]$home_goal_diff_2 <- sum( hgd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd14[gmh[2],]$home_goal_diff_3 <- hgd0[1]
   gd14[gmh[3],]$home_goal_diff_3 <- sum( hgd0[1:2] )
   for ( i in 1:16 ) {
      gd14[gmh[i+3],]$home_goal_diff_3 <- sum( hgd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd14[gmh[2],]$home_goal_diff_5 <- hgd0[1]
   gd14[gmh[3],]$home_goal_diff_5 <- sum( hgd0[1:2] )
   gd14[gmh[4],]$home_goal_diff_5 <- sum( hgd0[1:3] )
   gd14[gmh[5],]$home_goal_diff_5 <- sum( hgd0[1:4] )
   for ( i in 1:14 ) {
      gd14[gmh[i+5],]$home_goal_diff_5 <- sum( hgd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd14[gmh[2],]$home_goal_diff_10 <- hgd0[1]
   for ( i in 2:9 ) {
      gd14[gmh[i+1],]$home_goal_diff_10 <- sum( hgd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd14[gmh[i+10],]$home_goal_diff_10 <- sum( hgd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd14[gmh[i+1],]$home_goal_diff_total <- sum( hgd0[1:i] )
   }


   #
   # Same for this team when away
   #
   atsn <- gd14$away_team_short_name
   gma <- which( atsn == t )
   agd0 <- gd14[gma,]$away_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd14[gma[2:19],]$away_goal_diff_1 <- agd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd14[gma[2],]$away_goal_diff_2 <- agd0[1]
   for ( i in 1:17 ) {
      gd14[gma[i+2],]$away_goal_diff_2 <- sum( agd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd14[gma[2],]$away_goal_diff_3 <- agd0[1]
   gd14[gma[3],]$away_goal_diff_3 <- sum( agd0[1:2] )
   for ( i in 1:16 ) {
      gd14[gma[i+3],]$away_goal_diff_3 <- sum( agd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd14[gma[2],]$away_goal_diff_5 <- agd0[1]
   gd14[gma[3],]$away_goal_diff_5 <- sum( agd0[1:2] )
   gd14[gma[4],]$away_goal_diff_5 <- sum( agd0[1:3] )
   gd14[gma[5],]$away_goal_diff_5 <- sum( agd0[1:4] )
   for ( i in 1:14 ) {
      gd14[gma[i+5],]$away_goal_diff_5 <- sum( agd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd14[gma[2],]$away_goal_diff_10 <- agd0[1]
   for ( i in 2:9 ) {
      gd14[gma[i+1],]$away_goal_diff_10 <- sum( agd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd14[gma[i+10],]$away_goal_diff_10 <- sum( agd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd14[gma[i+1],]$away_goal_diff_total <- sum( agd0[1:i] )
   }
}


#
# 2015/2016 season
#
for ( t in t15 ) {

   #
   # Home games for this team
   #
   htsn <- gd15$home_team_short_name
   gmh <- which( htsn == t )

   #
   # Single-game home-team goal differential
   #
   hgd0 <- gd15[gmh,]$home_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd15[gmh[2:19],]$home_goal_diff_1 <- hgd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd15[gmh[2],]$home_goal_diff_2 <- hgd0[1]
   for ( i in 1:17 ) {
      gd15[gmh[i+2],]$home_goal_diff_2 <- sum( hgd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd15[gmh[2],]$home_goal_diff_3 <- hgd0[1]
   gd15[gmh[3],]$home_goal_diff_3 <- sum( hgd0[1:2] )
   for ( i in 1:16 ) {
      gd15[gmh[i+3],]$home_goal_diff_3 <- sum( hgd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd15[gmh[2],]$home_goal_diff_5 <- hgd0[1]
   gd15[gmh[3],]$home_goal_diff_5 <- sum( hgd0[1:2] )
   gd15[gmh[4],]$home_goal_diff_5 <- sum( hgd0[1:3] )
   gd15[gmh[5],]$home_goal_diff_5 <- sum( hgd0[1:4] )
   for ( i in 1:14 ) {
      gd15[gmh[i+5],]$home_goal_diff_5 <- sum( hgd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd15[gmh[2],]$home_goal_diff_10 <- hgd0[1]
   for ( i in 2:9 ) {
      gd15[gmh[i+1],]$home_goal_diff_10 <- sum( hgd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd15[gmh[i+10],]$home_goal_diff_10 <- sum( hgd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd15[gmh[i+1],]$home_goal_diff_total <- sum( hgd0[1:i] )
   }


   #
   # Same for this team when away
   #
   atsn <- gd15$away_team_short_name
   gma <- which( atsn == t )
   agd0 <- gd15[gma,]$away_goal_diff_0

   #
   # Goal diff for last previous game only
   #
   gd15[gma[2:19],]$away_goal_diff_1 <- agd0[1:18]

   #
   # Goal diff for last 2 previous games
   #
   gd15[gma[2],]$away_goal_diff_2 <- agd0[1]
   for ( i in 1:17 ) {
      gd15[gma[i+2],]$away_goal_diff_2 <- sum( agd0[i:(i+1)] )
   }

   #
   # Goal diff for last 3 previous games
   #
   gd15[gma[2],]$away_goal_diff_3 <- agd0[1]
   gd15[gma[3],]$away_goal_diff_3 <- sum( agd0[1:2] )
   for ( i in 1:16 ) {
      gd15[gma[i+3],]$away_goal_diff_3 <- sum( agd0[i:(i+2)] )
   }

   #
   # Goal diff for last 5 previous games
   #
   gd15[gma[2],]$away_goal_diff_5 <- agd0[1]
   gd15[gma[3],]$away_goal_diff_5 <- sum( agd0[1:2] )
   gd15[gma[4],]$away_goal_diff_5 <- sum( agd0[1:3] )
   gd15[gma[5],]$away_goal_diff_5 <- sum( agd0[1:4] )
   for ( i in 1:14 ) {
      gd15[gma[i+5],]$away_goal_diff_5 <- sum( agd0[i:(i+4)] )
   }

   #
   # Goal diff for last 10 previous games
   #
   gd15[gma[2],]$away_goal_diff_10 <- agd0[1]
   for ( i in 2:9 ) {
      gd15[gma[i+1],]$away_goal_diff_10 <- sum( agd0[1:i] )
   }
   for ( i in 1:9 ) {
      gd15[gma[i+10],]$away_goal_diff_10 <- sum( agd0[i:(i+9)] )
   }

   #
   # Cumulative season goal diff
   #
   for ( i in 1:18 ) {
      gd15[gma[i+1],]$away_goal_diff_total <- sum( agd0[1:i] )
   }
}

# Save calculations
write.table( gd08, "goal_differential_info.csv", append = FALSE, sep = ",", row.names=FALSE, col.names=TRUE );
write.table( gd09, "goal_differential_info.csv", append = TRUE, sep = ",", row.names=FALSE, col.names=FALSE );
write.table( gd10, "goal_differential_info.csv", append = TRUE, sep = ",", row.names=FALSE, col.names=FALSE );
write.table( gd11, "goal_differential_info.csv", append = TRUE, sep = ",", row.names=FALSE, col.names=FALSE );
write.table( gd12, "goal_differential_info.csv", append = TRUE, sep = ",", row.names=FALSE, col.names=FALSE );
write.table( gd13, "goal_differential_info.csv", append = TRUE, sep = ",", row.names=FALSE, col.names=FALSE );
write.table( gd14, "goal_differential_info.csv", append = TRUE, sep = ",", row.names=FALSE, col.names=FALSE );
write.table( gd15, "goal_differential_info.csv", append = TRUE, sep = ",", row.names=FALSE, col.names=FALSE );

