jscrapeDirect <- function(id=NULL, sleep=1, data=NULL, getDataOnly=FALSE){

  require("stringr", quietly=TRUE, warn.conflicts=FALSE, character.only=TRUE)
  if(is.null(data)) {
  require("XML", quietly=TRUE, warn.conflicts=FALSE, character.only=TRUE)
  require("reshape", quietly=TRUE, warn.conflicts=FALSE, character.only=TRUE)
  # Final table with (eventually) all shows
  d.jpdy <- data.frame()

  # Temporary table which will hold single show's data
  d.temp <- as.data.frame(matrix(NA,nrow=3,ncol=7))
  colnames(d.temp) <- c("Title","2","3","4","5","6","WebId")

  if(is.null(id)) {
    m <- 1:4500
    id <- jgetid(m, sleep=sleep)
  }

  # THE GAME 
  # Loop that gets one show's data per iteration and aggregates it

  for ( k in id ) {

    url <- paste0("http://www.j-archive.com/showgame.php?game_id=",k)

    if(sleep > 0) Sys.sleep(sleep)
    doc <- try(htmlParse(url, error = function(...){}))

    r <- xmlRoot(doc)
    tables <- getNodeSet(doc, "//table")

    cat(k, "\n", file="k.txt", append=TRUE)
    
    # Create a vector of table lengths to help find the info we want
    vec <- rep(NA,length(llply(tables,names)))
    for (j in 1:length(llply(tables,names))){
      vec[j] <- length(names(tables[[j]]))
    }

    # Find the tables our info is in
    firstrd <- max(which(vec==6))-1
    secrd <- min(which(vec==3))
    finalrd <- length(vec)-1
    qcorr <- length(vec)

    # Scrape one show's data
    for (i in 0:2){
      d.temp[i+1,1] <- xmlValue(r[[1]][[1]])
      d.temp[i+1,2] <- xmlValue(tables[[1]][[1]][[3]][[2+2*i]])
      d.temp[i+1,3] <- xmlValue(tables[[firstrd]][[2]][[5-2*i]])
      d.temp[i+1,4] <- xmlValue(tables[[secrd]][[2]][[5-2*i]])
      d.temp[i+1,5] <- xmlValue(tables[[finalrd]][[2]][[5-2*i]])
      d.temp[i+1,6] <- xmlValue(tables[[qcorr]][[3]][[5-2*i]])
      d.temp[i+1,7] <- k
    }
    
    # Bind the show data to the final data frame
    d.jpdy <- rbind(d.jpdy, d.temp)

  }
  } else {
      d.jpdy <- data
  }
  if(getDataOnly) {
      return(d.jpdy)
  }

  # Extract and Format the Variables
  for (i in 1:dim(d.jpdy)[1]){

cat(i, d.jpdy[i,1], "WebId= ", d.jpdy[i,7], "\n", file="dinfo.txt", append=TRUE)

    personInfo <- d.jpdy[i,2]
    personInfoCommaIndex <- gregexpr(',', personInfo)
    # cat(i, personInfo, "\n", file="pi.txt", append=TRUE)
    occupationStart <- regexpr(' a ', personInfo)[1]
    nameEnd <- occupationStart - 2
    if(occupationStart == -1) {
        occupationStart <- regexpr(' an ', personInfo)[1] + 1
        if(occupationStart < 1) {
            nameEnd <- personInfoCommaIndex[[1]][1] - 1
        } else {
            nameEnd <- occupationStart - 3
        }
    }
    cityStart <- regexpr(' originally from ', personInfo)[1]
    fromLen <- 17
    if(cityStart == -1) {
        cityStart <- regexpr(' from ', personInfo)[1]
        fromLen <- 6
    }
    whoseStart <- regexpr('whose ', personInfo)[1]
    if(whoseStart == -1) {
        firstTime <- TRUE
        cityStateEnd <- nchar(personInfo)
    } else {
        cityStateEnd <- whoseStart - 3
    }
    d.jpdy$Name[i] <- substr(personInfo, 1, nameEnd)
    if(occupationStart < 1) {
        d.jpdy$Occupation[i] <- NA
    } else {
        d.jpdy$Occupation[i] <- substr(personInfo, occupationStart+3,
            cityStart-1)
    }
    cityState <- substr(personInfo, cityStart + fromLen, cityStateEnd)
    cityStateCommaIndex <- gregexpr(',', cityState)[[1]][1]
    if(cityStateCommaIndex == -1) {
        d.jpdy$City[i] <- NA
        d.jpdy$State[i] <- str_trim(cityState)
    } else {
        d.jpdy$City[i] <- str_trim(
            substr(cityState, 1, cityStateCommaIndex[[1]][1]-1))
        d.jpdy$State[i] <- str_trim(
            substr(cityState, cityStateCommaIndex[[1]][1]+1,
            nchar(cityState)))
    }
    if(nchar(d.jpdy$State[i]) == 0) {
        d.jpdy$State[i] <- NA
    }
    
    # Extract number of days on show
    e <- regexpr('whose', d.jpdy[i,2])[1]
    f <- regexpr('-day',d.jpdy[i,2])[1]
    if (e == -1) d.jpdy$num_times_on_show[i] <- 1 else{
      d.jpdy$num_times_on_show[i] <- as.numeric(substr(d.jpdy[i,2],e+6,f-1))+1
      }

    #######################
    ##Make numbers numeric
    ########################
    win1 <- gsub(',','', d.jpdy[i,3])
    d.jpdy$first_round_winnings[i] <- as.numeric(gsub('[$]','',win1))
    win2 <- gsub(',','', d.jpdy[i,4])
    d.jpdy$Winnings_2nd_Round[i] <- as.numeric(gsub('[$]','',win2))
    win3 <- gsub(',','', d.jpdy[i,5])
    d.jpdy$Final_Winnings[i] <- as.numeric(gsub('[$]','',win3))
    
    #################
    #Find Show
    #################
    place1 <- regexpr('#', d.jpdy[i,1])[1]
    place2 <- regexpr(',',d.jpdy[i,1])[1]
    show.temp <- substr(d.jpdy[i,1], place1, place2)
    show.temp <- gsub('#','',show.temp)
    d.jpdy$Show[i] <- as.numeric(gsub(',','',show.temp))

    #-----------------------------
    #Get Number Right
    d.jpdy$n.Right[i]<-as.numeric(strsplit(d.jpdy[i,6],' ')[[1]][1])
	
    #-----------------------------
    #Get Number Wrong
    w<-regexpr("W",d.jpdy[i,6])[1]-2
    c<-regexpr(",",d.jpdy[i,6])[1]+1
    d.jpdy$n.Wrong[i]<-as.numeric(substr(d.jpdy[i,6],c,w))

    #-----------------------------
    #Get Number DD Wrong
    str<-strsplit(d.jpdy[i,6],"W")[[1]][2]
    ddw<-regexpr("DD",str)[1]
    if (is.na(strsplit(d.jpdy[i,6],"W")[[1]][2])) {d.jpdy$DD.Wrong[i]<-0} 
    else {d.jpdy$DD.Wrong[i]<-as.numeric(substr(str,ddw-2,ddw-2))}

    #---------------------------
    #Get Number DD Right
    loc.c<-regexpr(",",d.jpdy[i,6])[1]
    loc.R<-regexpr("R",d.jpdy[i,6])[1]
    dds<-substr(d.jpdy[i,6],loc.R,loc.c)
    ddr<-regexpr("DD",dds)[1] 
    if (loc.c-loc.R < 2) {d.jpdy$DD.Right[i]<-0} 
    else {d.jpdy$DD.Right[i]<-as.numeric(substr(dds,ddr-2,ddr-2))}

  }

  d.jpdy <- d.jpdy[,-(2:6)]

  return(d.jpdy)

}
