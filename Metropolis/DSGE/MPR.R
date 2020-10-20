library(tidyverse)
library(tidytext)
library(lubridate)

downloadMPR <- 1

issues <- c("July 10, 2019 Monetary Policy Report - July 2019",
            "April 24, 2019 Monetary Policy Report - April 2019",
            "January 9, 2019 Monetary Policy Report - January 2019",
            "October 24, 2018 Monetary Policy Report - October 2018",
            "July 11, 2018 Monetary Policy Report - July 2018",
            "April 18, 2018 Monetary Policy Report - April 2018 ",
            "January 17, 2018 Monetary Policy Report - January 2018 ",
            "October 25, 2017 Monetary Policy Report - October 2017 ",
            "July 12, 2017 Monetary Policy Report - July 2017 ",
            "April 12, 2017 Monetary Policy Report - April 2017 ",
            "January 18, 2017 Monetary Policy Report - January 2017 ",
            "October 19, 2016 Monetary Policy Report - October 2016 ",
            "July 13, 2016 Monetary Policy Report - July 2016 ",
            "April 13, 2016 Monetary Policy Report - April 2016 ",
            "January 20, 2016 Monetary Policy Report - January 2016 ",
            "October 21, 2015 Monetary Policy Report - October 2015 ",
            "July 15, 2015 Monetary Policy Report - July 2015 ",
            "April 15, 2015 Monetary Policy Report - April 2015 ",
            "January 21, 2015 Monetary Policy Report - January 2015 ",
            "October 22, 2014 Monetary Policy Report - October 2014 ",
            "July 16, 2014 Monetary Policy Report - July 2014 ",
            "April 16, 2014 Monetary Policy Report - April 2014 ",
            "January 22, 2014 Monetary Policy Report - January 2014 ",
            "October 23, 2013 Monetary Policy Report - October 2013 ",
            "July 17, 2013 Monetary Policy Report - July 2013 ",
            "April 17, 2013 Monetary Policy Report - April 2013 ",
            "January 23, 2013 Monetary Policy Report - January 2013 ",
            "October 24, 2012 Monetary Policy Report - October 2012 ",
            "July 18, 2012 Monetary Policy Report - July 2012 ",
            "April 18, 2012 Monetary Policy Report - April 2012 ",
            "January 18, 2012 Monetary Policy Report - January 2012 ",
            "October 26, 2011 Monetary Policy Report - October 2011 ",
            "July 20, 2011 Monetary Policy Report - July 2011 ",
            "April 13, 2011 Monetary Policy Report - April 2011 ",
            "January 19, 2011 Monetary Policy Report - January 2011 ",
            "October 20, 2010 Monetary Policy Report - October 2010 ",
            "July 22, 2010 Monetary Policy Report - July 2010 ",
            "April 22, 2010 Monetary Policy Report - April 2010 ",
            "January 21, 2010 Monetary Policy Report - January 2010 ",
            "October 26, 2009 Monetary Policy Report - October 2009 ",
            "July 23, 2009 Monetary Policy Report - July 2009 ",
            "April 22, 2009 Monetary Policy Report - April 2009 ",
            "January 22, 2009 Monetary Policy Report Update - January 2009 ",
            "October 23, 2008 Monetary Policy Report - October 2008 ",
            "July 17, 2008 Monetary Policy Report Update - July 2008 ",
            "April 24, 2008 Monetary Policy Report - April 2008 ",
            "January 24, 2008 Monetary Policy Report Update - January 2008 ",
            "October 18, 2007 Monetary Policy Report - October 2007 ",
            "July 12, 2007 Monetary Policy Report Update - July 2007 ",
            "April 26, 2007 Monetary Policy Report - April 2007 ",
            "January 16, 2007 Monetary Policy Report Update - January 2007 ",
            "October 19, 2006 Monetary Policy Report - October 2006 ",
            "July 13, 2006 Monetary Policy Report Update - July 2006 ",
            "April 27, 2006 Monetary Policy Report - April 2006 ",
            "January 26, 2006 Monetary Policy Report Update - January 2006 ",
            "October 20, 2005 Monetary Policy Report - October 2005 ",
            "July 14, 2005 Monetary Policy Report Update - July 2005 ",
            "April 14, 2005 Monetary Policy Report - April 2005 ",
            "January 27, 2005 Monetary Policy Report Update - January 2005 ",
            "October 21, 2004 Monetary Policy Report - October 2004 ",
            "July 22, 2004 Monetary Policy Report Update - July 2004 ",
            "April 15, 2004 Monetary Policy Report - April 2004 ",
            "January 22, 2004 Monetary Policy Report Update - January 2004 ",
            "October 22, 2003 Monetary Policy Report - October 2003 ",
            "July 17, 2003 Monetary Policy Report Update - July 2003 ",
            "April 23, 2003 Monetary Policy Report - April 2003 ",
            "January 23, 2003 Monetary Policy Report Update - January 2003 ",
            "October 23, 2002 Monetary Policy Report - October 2002 ",
            "July 24, 2002 Monetary Policy Report Update - July 2002 ",
            "April 24, 2002 Monetary Policy Report - April 2002 ",
            "January 23, 2002 Monetary Policy Report Update - January 2002 ",
            "November 7, 2001 Monetary Policy Report - November 2001 ",
            "August 1, 2001 Monetary Policy Report Update - August 2001 ",
            "May 1, 2001 Monetary Policy Report - May 2001 ",
            "February 6, 2001 Monetary Policy Report Update - February 2001 ",
            "November 9, 2000 Monetary Policy Report - November 2000 ",
            "August 16, 2000 Monetary Policy Report Update - August 2000 ",
            "May 11, 2000 Monetary Policy Report - May 2000 ",
            "February 16, 2000 Monetary Policy Report Update - February 2000 ",
            "November 17, 1999 Monetary Policy Report - November 1999 ",
            "May 19, 1999 Monetary Policy Report - May 1999 ",
            "November 16, 1998 Monetary Policy Report - November 1998 ",
            "May 13, 1998 Monetary Policy Report - May 1998 ",
            "November 20, 1997 Monetary Policy Report - November 1997 ",
            "May 20, 1997 Monetary Policy Report - May 1997 ",
            "November 20, 1996 Monetary Policy Report - November 1996 ",
            "May 20, 1996 Monetary Policy Report - May 1996 ",
            "November 20, 1995 Monetary Policy Report - November 1995 ",
            "May 15, 1995 Monetary Policy Report - May 1995 ")

pub <- str_split(issues," Monetary Policy Report ", simplify = TRUE)
mth <- match(word(pub[,1], 1),month.name)
yr   <- as.integer(word(pub[,1], 3))
dpub <- as.Date(pub[,1], "%B %d, %Y")

root <- "https://www.bankofcanada.ca/wp-content/uploads/"

u <- paste0(root,yr,"/",substr(dpub,6,7),"/","mpr-",dpub,".pdf")

# 2013/mpr-october2013.pdf

v <- paste0(root,yr,"/","mpr-",tolower(word(pub[,1], 1)),yr,".pdf")
w <- paste0(root,yr,"/",substr(dpub,6,7),"/","mpr-",tolower(word(pub[,1], 1)),yr,".pdf")
x <- paste0(root,yr,"/",substr(dpub,6,7),"/","mpr",
            substr(tolower(word(pub[,1], 1)),1,3),substr(dpub,3,4),".pdf")

xx <- paste0(root,"2010/04/","mpr_",
             substr(tolower(word(pub[,1], 1)),1,3),"_",
             substr(dpub,1,4),".pdf")

# 2010/07/mprsumapr10.pdf
y <- paste0(root,yr,"/",substr(dpub,6,7),"/","mprsum",
            substr(tolower(word(pub[,1], 1)),1,3),substr(dpub,3,4),".pdf")

z <- paste0(root,yr,"/",substr(dpub,6,7),"/","mpr",
            paste0(substr(dpub,9,10),substr(dpub,6,7),substr(dpub,3,4)),
            ".pdf")

savepdf <- paste0("mpr-",dpub,".pdf")

if (downloadMPR >0) {
  
  for (i in 1:13){
    url <- u[i]
    download.file(url,savepdf[i],mode="wb")
  }
  
  # https://www.bankofcanada.ca/wp-content/uploads/
  # 2014/07/mpr-2015-01-21.pdf
  
  i   <- 14
  url <- "https://www.bankofcanada.ca/wp-content/uploads/2014/07/mpr-2015-01-21.pdf"
  download.file(url,savepdf[i],mode="wb")
  
  i   <- 15
  url <- "https://www.bankofcanada.ca/wp-content/uploads/2014/07/mpr-2014-10-22.pdf"
  download.file(url,savepdf[i],mode="wb")
  
  for (i in 16:17){
    url <- u[i]
    download.file(url,savepdf[i],mode="wb")
  }
  
  i <- 18
  url <- "https://www.bankofcanada.ca/wp-content/uploads/2013/11/mpr-2014-01-22.pdf"
  download.file(url,savepdf[i],mode="wb")
  
  i <- 19
  url <- v[i]
  download.file(url,savepdf[i],mode="wb")
  
  for (i in 20:20){
    url <- u[i]
    download.file(url,savepdf[i],mode="wb")
  }
  
  i <- 21
  url <- "https://www.bankofcanada.ca/wp-content/uploads/2013/01/mpr-2013-04-17.pdf"
  download.file(url,savepdf[i],mode="wb")
  
  i <- 22
  url <- u[i]
  download.file(url,savepdf[i],mode="wb")
  
  for (i in 23:28){
    url <- w[i]
    download.file(url,savepdf[i],mode="wb")
  }
  
  for (i in 29:31){
    url <- x[i]
    download.file(url,savepdf[i],mode="wb")
  }
  
  i <- 32
  url <- gsub("/07/mprjul","/09/mprjuly",x[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 33
  url <- gsub("2010/04","2010/07",z[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 34
  url <- gsub("2010/01","2010/04",z[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 35
  url <- gsub("2009/10","2010/04",z[i])
  url <- gsub("26","22",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 36
  url <- gsub("2009/07","2010/04",z[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 37
  url <- gsub("2009/04","2010/03",z[i])
  url <- gsub("22","23",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 38
  url <- gsub("2009/01","2010/03",z[i])
  url <- gsub("mpr","update",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 39
  url <- gsub("2008/10","2010/03",z[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 40
  url <- gsub("2008/07","2010/03",z[i])
  url <- gsub("mpr","update",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 41
  url <- gsub("2008/04","2010/03",z[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 42
  url <- gsub("2008/01","2010/03",z[i])
  url <- gsub("mpr","update",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 43
  url <- gsub("2007/10","2010/02",z[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 44
  url <- gsub("2007/07","2010/02",z[i])
  url <- gsub("mpr","update",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 45
  url <- gsub("2007/04","2010/02",z[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 46
  url <- gsub("2007/01","2010/02",z[i])
  url <- gsub("mpr","update",url)
  url <- gsub("16","18",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 47
  url <- gsub("2006/10","2010/02",x[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 48
  url <- gsub("2006/07","2010/02",z[i])
  url <- gsub("mpr","mpr_update",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 49
  url <- gsub("2006/04","2010/02",x[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 50
  url <- gsub("2006/01","2010/02",z[i])
  url <- gsub("mpr","mpr_update",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 51
  url <- gsub("2005/10","2010/04",x[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 52
  url <- gsub("2005/07","2010/02",z[i])
  url <- gsub("mpr","mpr_update",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 53
  url <- gsub("2005/04","2010/02",x[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 54
  url <- gsub("2005/01","2010/02",z[i])
  url <- gsub("mpr","mpr_update",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 55
  url <- gsub("2004/10","2010/02",x[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 56
  url <- gsub("2004/07","2010/02",z[i])
  url <- gsub("mpr","mpr_update",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 57
  url <- gsub("2004/04","2010/02",x[i])
  url <- gsub("apr","april",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 58
  url <- gsub("2004/01","2010/02",z[i])
  url <- gsub("22","",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 59
  url <- gsub("2003/10","2010/02",x[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 60
  url <- gsub("2003/07","2010/02",z[i])
  url <- gsub("mpr","mpr_update",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 61
  url <- gsub("2003/04","2010/02",x[i])
  url <- gsub("apr","april",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 62
  url <- gsub("2003/01","2010/02",z[i])
  url <- gsub("mpr","mpr_update",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 63
  url <- gsub("2002/10","2010/02",x[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 64
  url <- gsub("2002/07","2010/02",x[i])
  url <- gsub("mprjul","mpr-updatejuly",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 65
  url <- gsub("2002/04","2010/02",x[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 66
  url <- gsub("2002/01","2010/02",x[i])
  url <- gsub("mprjan","mpr-updatejan",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 67
  url <- gsub("2001/11","2010/02",x[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 68
  url <- gsub("2001/08","2010/02",z[i])
  url <- gsub("mpr01","mpr",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 69
  url <- gsub("2001/05","2010/02",x[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 70
  url <- gsub("2001/02","2010/04",x[i])
  url <- gsub("mprfeb","mpr_up_feb20",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 71
  url <- gsub("2000/11","2010/02",x[i])
  download.file(url,savepdf[i],mode="wb")
  
  i <- 72
  url <- gsub("2000/08","2010/04",x[i])
  url <- gsub("mpraug","mpr_up_aug20",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 73
  url <- gsub("2000/05","2010/04",x[i])
  url <- gsub("mprmay","mpr_may_20",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 74
  url <- gsub("2000/02","2010/04",x[i])
  url <- gsub("mprfeb00","mpr00e",url)
  download.file(url,savepdf[i],mode="wb")
  
  for (i in 75:77) {
    url <- xx[i]
    download.file(url,savepdf[i],mode="wb")
  }
  
  i <- 78
  url <- xx[i]
  url <- gsub("may","apr",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 79
  url <- xx[i]
  download.file(url,savepdf[i],mode="wb")
  
  i <- 80
  url <- xx[i]
  url <- gsub("may","apr",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 81
  url <- xx[i]
  download.file(url,savepdf[i],mode="wb")
  
  i <- 82
  url <- xx[i]
  url <- gsub("may","apr",url)
  download.file(url,savepdf[i],mode="wb")
  
  i <- 83
  url <- xx[i]
  download.file(url,savepdf[i],mode="wb")
  
  i <- 84
  url <- xx[i]
  url <- gsub("may","apr",url)
  url <- gsub("95","951",url)
  download.file(url,savepdf[i],mode="wb")
  
  
  download.file("https://www.bankofcanada.ca/wp-content/uploads/2019/04/mpr-2019-04-24.pdf",
                "mpr-2019-04-24.pdf", mode="wb")
  
  download.file("https://www.bankofcanada.ca/wp-content/uploads/2019/10/mpr-2019-10-30.pdf",
                "mpr-2019-10-30.pdf", mode="wb")
  
  download.file("https://www.bankofcanada.ca/wp-content/uploads/2020/01/mpr-2020-01-22.pdf",
                "mpr-2020-01-22.pdf", mode="wb")
  
  download.file("https://www.bankofcanada.ca/wp-content/uploads/2020/04/mpr-2020-04-15.pdf",
                "mpr-2020-04-15.pdf", mode="wb")
  
  download.file("https://www.bankofcanada.ca/wp-content/uploads/2020/07/mpr-2020-07-15.pdf",
                "mpr-2020-07-15.pdf", mode="wb")

}

