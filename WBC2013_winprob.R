#------------------------------------------------------------------------------#
#                                                                              #
# WBC2013                                                                      #
#------------------------------------------------------------------------------#
rm( list=ls() )

send.table.to.excel <- function( tab, filename=paste(tempfile(),".csv",sep="") ){
  write.csv(tab, filename )
  shell.exec(filename)
}

#------------------------------------------------------------------------------#
# 状況別得点別確率                                                             #
#------------------------------------------------------------------------------#
( datRP <- read.csv( "data/rpiunprob_albert.csv" ) )   # albert tab7-4
n <- nrow(datRP)
datRP[,"run0"]
p0 <- t( sapply( 1:n, function(i) dpois( 3:10, lambda=datRP[i,"runs"] ) ) )
p012 <- apply( datRP[,c("run0","run1","run2")], 1, sum )
p345 <- 1 - p012
cbind( p345, apply( p0,1,sum) )
apply( p0/apply( p0,1,sum), 1, sum )
rp <- cbind( datRP[,c("run0","run1","run2")], p0/apply( p0,1,sum) * p345 )
apply( rp, 1, sum )
plot( c(0,10), c(0,1), type="n" )
apply( rp,  1, function(x) points(0:10, x, type="b" ) )
#--------------------------------------#
# 勝利確率：点差ｘイニング終了時       #
#--------------------------------------#
(tmp <- read.csv( "data/winprob_by_inning_and_rundifferential.csv" ))
( wp_ird <- tmp[ tmp$method=="Albert", ] )
( wp_H2 <- tmp[ tmp$method=="home", -1] )
( wp_V1 <- tmp[ tmp$method=="visitor",-1 ] )
( wp_H1 <- cbind( -wp_V1[11:1,1], 1-wp_V1[11:1,-1] ) )

plot( c(0,9), c(0,1), type="n", xlab="inning", ylab="home team win probability" )

cols <- c( 6:2,"white",2:6 )
pchs <- c(rep(16,5),5,rep(1,5))
sapply( 1:nrow(wp_H2), function(i) lines( 1:9, wp_H2[i,2:10], type="b", col=cols[i], lwd=3, 
                                          pch=pchs[i]  ) )
wp_H <- cbind( wp_H1[,2], wp_H2[,2], wp_H1[,3], wp_H2[,3],
               wp_H1[,4], wp_H2[,4], wp_H1[,5], wp_H2[,5],
               wp_H1[,6], wp_H2[,6], wp_H1[,7], wp_H2[,7],
               wp_H1[,8], wp_H2[,8], wp_H1[,9], wp_H2[,9],
               wp_H1[,10], wp_H2[,10] )
wp_H

plot( c(0,18), c(0,1), type="n", xlab="inning", ylab="home team winning probability" )
sapply( 1:nrow(wp_H), function(i) lines( 1:18, wp_H[i,], type="b", col=i ) )
#----------------------------#
# 攻撃中の勝率(攻撃側)       #
#----------------------------#
get_WP <- function( inn, out, base, rd0, half ){
  ok <- datRP$outs==out & datRP$bases==base
  RD <- (-8):8
  ( wp_rd <- wp_H2[ ,inn+1] )
  if( half==1 ) wp_rd <- wp_V1[ ,inn+1]
  ( wp_rd <- c(wp_rd[1]/8, wp_rd[1]/4, wp_rd[1]/2, wp_rd, (1+wp_rd[length(wp_rd)])/2, 
               (3+wp_rd[length(wp_rd)])/4, (7+wp_rd[length(wp_rd)])/8 ) )
  if( out==3 ){ if(rd0>max(RD)) return( max(wp_rd, na.rm=T)); 
    if(rd0<min(RD)) return(min(wp_rd, na.rm=T));
    return(wp_rd[ RD==rd0 ]) }
  runs <- 0:8
  ( p_runs <- unlist(rp[ok,(1:9)]) )
  ( rd1 <- rd0 + runs )
  ( WP <- sapply( rd1, function(rd){ if(rd>max(RD)) return( max(wp_rd)); 
    if(rd<min(RD)) return(min(wp_rd));
    wp_rd[ RD==rd ] } )  )
  cbind( runs, p_runs, rd1, WP );
  c(WP %*% p_runs)
}

get_WP( inn=9, out=0, base=0, rd0=-1, half=2 )
get_WP( inn=9, out=0, base=1, rd0=-1, half=2 )
get_WP( inn=9, out=0, base=3, rd0=-1, half=2 )
get_WP( inn=9, out=0, base=13, rd0=-1, half=2 )
get_WP( inn=9, out=0, base=23, rd0=-1, half=2 )
get_WP( inn=9, out=0, base=123, rd0=-1, half=2 )
get_WP( inn=9, out=1, base=123, rd0=-1, half=2 )
get_WP( inn=9, out=2, base=123, rd0=-1, half=2 )

i <- 1
out <- datRP$outs[i]; base <- datRP$bases[i]

get_WP( inn=10, out=0, base=0, rd0=0, half=2 )

get_WP( inn=9, out=2, base=1, rd0=0, half=2 )

get_WP( inn=9, out=2, base=2, rd0=0, half=2 ) -
  get_WP( inn=10, out=0, base=0, rd0=0, half=1 )

get_WP( inn=5, out=2, base=2, rd0=5, half=2 ) -
  get_WP( inn=6, out=0, base=0, rd0=5, half=1 )

get_WP( inn=1, out=2, base=2, rd0=0, half=1 ) -
  (1-get_WP( inn=1, out=0, base=0, rd0=0, half=2 ))

get_WP( inn=5, out=2, base=2, rd0=-2, half=1 ) -
  (1-get_WP( inn=5, out=0, base=0, rd0=2, half=2 ))


get_WP( inn=1, out=1, base=2, rd0=0, half=1 ) -
  get_WP( inn=1, out=2, base=0, rd0=0, half=1 )


get_WP( inn=2, out=2, base=23, rd0=0, half=2 ) -
  (1-get_WP( inn=3, out=0, base=0, rd0=0, half=1 ))

get_WP( inn=2, out=1, base=2, rd0=1, half=1 ) -
  (1-get_WP( inn=1, out=0, base=0, rd0=0, half=2 ))

get_WP( inn=3, out=2, base=13, rd0=2, half=1 ) -
  (1-get_WP( inn=3, out=0, base=0, rd0=-1, half=1 ))



###############################################################################
# WBC
###############################################################################
#-----------------------------------------------------#
dat7  <- read.csv("data/WBC2013_CT_vs_JP.csv")
dat7

get.gamepdat <- function(dat7){     
  ( nin <- length(dat7$inning) )
  ( cin <- dat7$inning[1])
  for( i in 2:nin ){
    if( is.na(dat7$inning[i]) ) dat7$inning[i] <- cin
    else cin <- dat7$inning[i]
  }
  ( nin <- length(dat7$homeaway) )
  (cin <- dat7$homeaway[1])
  for( i in 2:nin ){
    if(dat7$homeaway[i]=="" ) dat7$homeaway[i] <- cin
    else cin <- dat7$homeaway[i]
  }
  dat7$visiR[ is.na(dat7$visiR)] <- 0
  dat7$homeR[ is.na(dat7$homeR)] <- 0
  
  dat7$half <- 1
  dat7$half[ dat7$homeaway=="home" ] <- 2
  
  cnames <- c( "homeaway","inning","out","base", "batter", "result","half","homeR","visiR" )
  
  dat7$inning0 <- dat7$inning
  dat7$inning0[ dat7$homeaway=="home" ] <- dat7$inning0[ dat7$homeaway=="home" ] + .5
  
  gdat <- NULL
  for( inn in unique(dat7$inning0) ){
    a <- dat7[ dat7$inning0==inn, cnames ] 
    b <- a[1,]
    b$out <- 3; b$visiR <- 0; b$homeR <- 0; b$batter <- -1; # b$run <- 0
    gdat <- rbind( gdat, a, b )
  }
  ( N <- nrow(gdat) )
  gdat$visiCR <- c(0,cumsum( gdat$visiR )[-N] )
  gdat$homeCR <- c(0,cumsum( gdat$homeR )[-N] )
  
  gdat$rdV <- gdat$visiCR - gdat$homeCR    # visitorチームから見た点差
  gdat$rdH <- gdat$homeCR - gdat$visiCR    #    homeチームから見た点差
  
  gdat$rd0 <- gdat$rdV*(gdat$half==1)+gdat$rdH*(gdat$half==2)
  
  get_WP <- function( inn, out, base, rd0, half ){   # inn=1; out=0; base=0; rd0=0; half=1
    
    ok <- datRP$outs==out & datRP$bases==base
    RD <- (-8):8
    ( wp_rd <- wp_H2[ ,inn+1] )
    if( half==1 ) wp_rd <- wp_V1[ ,inn+1]
    ( wp_rd <- c(wp_rd[1]/8, wp_rd[1]/4, wp_rd[1]/2, wp_rd, (1+wp_rd[length(wp_rd)])/2, 
                 (3+wp_rd[length(wp_rd)])/4, (7+wp_rd[length(wp_rd)])/8 ) )
    
    if( out==3 ){ if(rd0>max(RD)) return( max(wp_rd)); 
      if(rd0<min(RD)) return(min(wp_rd));
      return(wp_rd[ RD==rd0 ]) }
    runs <- 0:8
    ( p_runs <- unlist(rp[ok,(1:9)]) )
    ( rd1 <- rd0 + runs )
    ( WP <- sapply( rd1, function(rd){ if(rd>max(RD)) return( max(wp_rd, na.rm=T)); 
      if(rd<min(RD)) return(min(wp_rd, na.rm=T));
      wp_rd[ RD==rd ] } )  )
    cbind( runs, p_runs, rd1, WP );
    c(WP %*% p_runs)
  }
  
  gdat$wp <- sapply( 1:nrow(gdat), function(i){   # i <- i+1; # i <-1
    #cat( i, "\n" )
    a <- get_WP( inn=gdat$inning[i], out=gdat$out[i], base=gdat$base[i], 
                 rd0=gdat$rd0[i], half=gdat$half[i] ) 
    a*(gdat$half[i]==1) + (1-a)*(gdat$half[i]==2)
  } )
  gdat$play <- 1:nrow(gdat)
  
  gdat$wp[1] <- 0.5
  ( start_inning <- gdat$play[ gdat$out==0 & gdat$base==0 & !(gdat$inning==1 & gdat$half==1)] )
  gdat$wp[ start_inning ] <- gdat$wp[ start_inning - 1 ]
  
  
  gdat$wp_diff <- c(diff(gdat$wp),0)
  gdat$wp_diff[ gdat$out==3 ] <- 0
  gdat
}

gdat <- get.gamepdat( dat7 )
gdat
#--------------------------------------#
# Plot of Winning Probability by inning#
#--------------------------------------#

plot( c(0,nrow(gdat)),c(0,1), type="n", xlab="play", ylab="win probability" )

abline( h=0:2*0.5, v=, lty=1 )  

tmp <- c(0,gdat$play[ gdat$out==3]) + 0.5
(inn_breaks <- cbind( tmp[-length(tmp)], tmp[-1], half=c(2,4) ))
sapply( 1:nrow(inn_breaks), function(i) rect( xleft=inn_breaks[i,1], xright=inn_breaks[i,2], ytop=1, ybottom=0, col=inn_breaks[i,3] ) )
abline( h=1:10/10, lty=2 )  
lines( 1:nrow(gdat), gdat$wp, type="b", lwd=3, pch=16, col=7 )

round( tapply( gdat$wp_diff, gdat[,c("batter","homeaway")], sum ), 3)
