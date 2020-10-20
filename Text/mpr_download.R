library(dplyr)
library(stringr)

# Pub dates: Obtained as a copy/paste/edit of https://www.bankofcanada.ca/publications/mpr/

issues <- c("July 15, 2020 Monetary Policy Report - July 2020",
            "April 15, 2020 Monetary Policy Report - April 2020",
            "January 22, 2020 Monetary Policy Report - January 2020",
            "October 30, 2019 Monetary Policy Report - October 2019",
            "July 10, 2019 Monetary Policy Report - July 2019",
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

# Create consistent by date name series form dates

Dates <- str_split(issues," Monetary Policy Report ", simplify = TRUE) %>% 
  as_tibble(., .name_repair = "unique") %>% 
  select(Published = 1) %>% 
  mutate(Date = as.Date(Published, "%B %d, %Y")) %>% 
  mutate(fname = paste0("mpr-", as.character(Date), ".pdf"))

# Every address of the MPRs. 
# A bit erratic, now settled down, so could write a program to download since
# the last inconsistency, which was January 2015.

addr <- c(
  "https://www.bankofcanada.ca/wp-content/uploads/2020/07/mpr-2020-07-15.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2020/04/mpr-2020-04-15.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2020/01/mpr-2020-01-22.pdf",
  
  "https://www.bankofcanada.ca/wp-content/uploads/2019/10/mpr-2019-10-30.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2019/07/mpr-2019-07-10.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2019/04/mpr-2019-04-24.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2019/01/mpr-2019-01-09.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2018/10/mpr-2018-10-24.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2018/07/mpr-2018-07-11.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2018/04/mpr-2018-04-18.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2018/01/mpr-2018-01-17.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2017/10/mpr-2017-10-25.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2017/07/mpr-2017-07-12.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2017/04/mpr-2017-04-12.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2017/01/mpr-2017-01-18.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2016/10/mpr-2016-10-19.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2016/07/mpr-2016-07-13.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2016/04/mpr-2016-04-13.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2016/01/mpr-2016-01-20.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2015/10/mpr-2015-10-21.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2015/07/mpr-2015-07-15.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2015/04/mpr-2015-04-15.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2014/07/mpr-2015-01-21.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2014/07/mpr-2014-10-22.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2014/07/mpr-2014-07-16.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2014/04/mpr-2014-04-16.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2013/11/mpr-2014-01-22.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2013/mpr-october2013.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2013/07/mpr-2013-07-17.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2013/01/mpr-2013-04-17.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2013/01/mpr-2013-01-23.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2012/10/mpr-october2012.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2012/07/mpr-july2012.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2012/04/mpr-april2012.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2012/01/mpr-january2012.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2011/10/mpr-october2011.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2011/07/mpr-july2011.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2011/04/mprapr11.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2011/01/mprjan11.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/10/mproct10.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/09/mprjuly10.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/07/mpr220410.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr210110.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr221009.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr230709.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/03/mpr230409.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/03/update220109.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/03/mpr231008.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/03/update170708.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/03/mpr240408.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/03/update240108.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr181007.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/update120707.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr260407.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/update180107.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mproct06.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr_update130706.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mprapr06.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr_update260106.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mproct05.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr_update140705.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mprapr05.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr_update270105.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mproct04.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr_update220704.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mprapril04.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr0104.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mproct03.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr_update170703.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mprapril03.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr_update230103.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mproct02.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr-updatejuly02.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mprapr02.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr-updatejan02.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mprnov01.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mpr0801.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mprmay01.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_up_feb2001.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/02/mprnov00.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_up_aug2000.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_may_2000.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr00e.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_nov_1999.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_may_1999.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_nov_1998.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_apr_1998.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_nov_1997.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_apr_1997.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_nov_1996.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_apr_1996.pdf",

  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_nov_1995.pdf",
  "https://www.bankofcanada.ca/wp-content/uploads/2010/04/mpr_apr_19951.pdf")

# Download and save in format "mpr-YYYY-MM-DD.pdf". Note requires mode="wb".

for (i in 1:length(addr)) download.file(addr[i], destfile=Dates$fname[i], mode="wb")
