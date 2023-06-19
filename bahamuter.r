


wkDir = "E:\\AllRepo\\R_Crawler" ;   setwd(wkDir);
library( stringr ) 
library(RCurl)
library(XML)
library (plyr)
library( openxlsx )

#####=====(1) �]�w�ؼк���url =====#####

#### https://forum.gamer.com.tw/B.php?bsn=60561
# url = "https://forum.gamer.com.tw/" ; 
# php = "B.php?bsn=60561" ; 
# response2 = getURL( str_c( url , php ), .encoding = 'utf8' ) 
# print(response2)
# 
# 
# response2.parser = htmlParse(response2, asText=TRUE)
# print(response2.parser)


#####=====(2) �M��ؼ� xpath =====#####

# list <- xpathSApply(response2.parser, "//*[@id='BH-master']
#                                              //*[@class='b-list__row b-list-item b-imglist-item']
#                                              //*[@class='b-list__main']
#                                              //*[@class='imglist-text']
#                                              /div/p" )
# 
# author <- xpathSApply(response2.parser, "//*[@id='BH-master']
#                                              //*[@class='b-list__row b-list-item b-imglist-item']
#                                              //*[@class='b-list__count']
#                                              //*[@class='b-list__count__user']" 
#                                              , xmlValue )
# 
# author <- str_replace_all( author, "\n","") ; author
# 
# head( list, 3 )


# [[1]]
# <p data-gtm="B���峹�C��-�Y��" href="C.php?bsn=60561&amp;snA=22990&amp;tnum=2" class="b-list__main__title">�i�߱o�j�J��F��ڤ��������q</p> 
#   
#   [[2]]
# <p data-gtm="B���峹�C��-�Y��" href="C.php?bsn=60561&amp;snA=22975&amp;tnum=9" class="b-list__main__title">�i���D�j�n��u�{�v �n��� �~�����~�������@�I�����q</p> 
#   
#   [[3]]
# <p data-gtm="B���峹�C��-�Y��" href="C.php?bsn=60561&amp;snA=22988&amp;tnum=2" class="b-list__main__title">�i���D�j������u�@</p> 


#####=====(3) ���X title �M href =====#####

# title.df <- xmlToDataFrame(list)
# title.df$php <- sapply( list, function(x)xpathSApply( x, "attribute::href") ) 
# title.df$author <- author; title.df 
# 
#  
# write.xlsx( title.df, "title.xlsx") 

#####=====(4) ��X���� =====#####





GetContentList <- function( x )
{
  print( str_c("���D : ", x[1] ) ) 
  Sys.sleep( 5 )
  php = x[2]
  author = x[3] 
  print( str_c( url , php ) )

  ##### content #####
  content = getURL( str_c( url , php ), .encoding = 'utf8' ) ; # print( content )
  content.parser = htmlParse( content, asText=TRUE) ; # print( content.parser )
  content.list <- xpathSApply( content.parser,  "//*[@id='BH-master']
                                                 //*[@class='c-section']
                                                 //*[@class='c-section__main c-post ']
                                                 //*[@class='c-post__body']
                                                 //*[@class='c-article FM-P2']
                                                 //*[@class='c-article__content']", xmlValue )
  
  content.list <- lapply(content.list, function(x)str_replace_all( x, "\n","") )
  
  ##### date time #####
  dateTime = getURL( str_c( url , php ), .encoding = 'utf8' ) ; 
  dateTime.parser = htmlParse( dateTime, asText=TRUE) ; 
  dateTime.value <- xpathSApply( dateTime.parser, "/html/body/div[5]/div/div[2]/section[1]/div[2]/div[1]/div[3]/a/attribute::data-mtime" ) 
  
  # /html/body/div[5]/div/div[2]/section[2]/div[2]/div[1]/div[3]/a 
  
  if (  is.null(dateTime.value) ) 
  {
    print( "is null" ) 
    dateTime.value <- xpathSApply( dateTime.parser, "/html/body/div[5]/div/div[2]/section[2]/div[2]/div[1]/div[3]/a/attribute::data-mtime" )
    
  } # if 
  
  if( is.null(dateTime.value) )
  {
    dateTime.value = "Date Time Error" 
  } 
  print( dateTime.value )
  
  content.list <- append( content.list, dateTime.value, after = 0 )
  ##### bp & gp #####
  
  gp <- xpathSApply( content.parser,  "/html/body/div[5]/div/div[2]/section[2]/div[2]/div[1]/div[2]/div/span[1]/span", xmlValue )
  if ( is.null( gp ) )
  {
    gp <- xpathSApply( content.parser,  "/html/body/div[5]/div/div[2]/section[1]/div[2]/div[1]/div[2]/div/span[1]/span", xmlValue )
  } 

  bp <- xpathSApply( content.parser,  "/html/body/div[5]/div/div[2]/section[2]/div[2]/div[1]/div[2]/div/span[2]/span", xmlValue )
  if ( is.null(bp) )
  {
    bp <- xpathSApply( content.parser,  "/html/body/div[5]/div/div[2]/section[1]/div[2]/div[1]/div[2]/div/span[2]/span", xmlValue )
  }
  
  if ( is.null( gp ) )
  {
    gp = "GP Error"
  }
  
  if( is.null( bp ) ) 
  {
    bp = "BP Error"
  }
  
  print( c( str_c( "gp :", gp), str_c( "bp :", bp))  )
  content.list <- append( content.list, gp, after = 1 )
  content.list <- append( content.list, bp, after = 2 )
  #####

  
  
  return ( content.list  ) 
} 

# # apply : 1 -> row(��), 2 -> column(��)
# ll <- apply(title.df, 1, GetContentList)
# 
# print (ll[[1]][1])
# 
# wb <- createWorkbook(creator = 'grant', title = '�ګ��i�S����')
# addWorksheet(wb, sheetName = 'sheet1')
# writeData(wb,
#           sheet = 'sheet1',
#           x = list("���D", "�@��", "����"),
#           startCol = 1)
# # saveWorkbook(wb, "�ګ��i�S����.xlsx", overwrite = TRUE)
# 
# for (i in c(1:length(title.df$text)))
# {
#   print(i)
#   df <- list(c(title.df$text[i], title.df$author[i], ll[i]))
#   tryCatch({
#     # expr
#     writeData(wb,
#               sheet = 'sheet1',
#               x = df,
#               startRow = i + 1)
#   },
#   error = function(e) {
#     #error
#     print(e)
#     conditionMessage(e)
#     df <-
#       list(c(title.df$text[i - 1], title.df$author[i - 1], ll[i - 1]))
#     writeData(wb,
#               sheet = 'sheet1',
#               x = df,
#               startRow = i + 1)
#   }) # try catch
# }
# 
# saveWorkbook(wb, "�ګ��i�S����.xlsx", overwrite = TRUE)


#####=====(6) �o���� =====#####



#####=====(5) ��X20�����Ҧ���T =====#####
url = "https://forum.gamer.com.tw/" ; 
php = "B.php?page=1&bsn=60561" ; 
wb <- createWorkbook(creator = 'grant',title = '�ګ��i�S����_20��')
addWorksheet(wb,sheetName = 'sheet1')
writeData(wb,sheet = 'sheet1',x = list("���D", "�@��", "�o����", "gp", "bp", "����" , "�^��" ), startCol = 1 )
cnt = 1

for ( p in c(1:100) ) 
{
  p = 1 
  php = str_c( "B.php?page=", p, "&bsn=60561") 
  print( str_c( "�� ", p, " �� href : ", str_c( url , php ) )  ) 
  response2 = getURL( str_c( url , php ), .encoding = 'utf8' )
  response2.parser = htmlParse(response2, asText=TRUE)
  
  ## title 
  list <- xpathSApply(response2.parser, "//*[@id='BH-master']
                                             //*[@class='b-list__row b-list-item b-imglist-item']
                                             //*[@class='b-list__main']
                                             //*[@class='imglist-text']
                                             /div/p" )
  print( str_c("��", length(list), "�g��" ) ) 
  ### 
  
  ## author 
  author <- xpathSApply(response2.parser, "//*[@id='BH-master']
                                             //*[@class='b-list__row b-list-item b-imglist-item']
                                             //*[@class='b-list__count']
                                             //*[@class='b-list__count__user']" 
                        , xmlValue )
  
  author <- str_replace_all( author, "\n","") ; 
  ### 
  
  title.df <- xmlToDataFrame(list)
  title.df$php <- sapply( list, function(x)xpathSApply( x, "attribute::href") ) 
  title.df$author <- author; # head( title.df, 2 )  
  
  ## func : GetContentList ����C�g�峹������M�^�_�s�@�� ll (list)
  ll <- apply( title.df, 1, GetContentList ) ;
  
  for ( i in c( 1: length( title.df$text) ) ) 
  {
    print( i )
    df <- list( c( title.df$text[i], title.df$author[i], ll[i] ) )
    tryCatch( 
      { 
        # expr
        writeData( wb,sheet = 'sheet1',x = df, startRow = cnt+1 )
      }, 
      
      error = function(e) {  
        #error 
        print( e ) 
        conditionMessage(e)
        df <- list( c( title.df$text[i-1], title.df$author[i-1], ll[i-1] ) )
        writeData( wb,sheet = 'sheet1',x = df, startRow = cnt+1 )
        }
      
    ) # try catch 
    
    cnt = cnt + 1 
    
  } # for 
  
  
} # for 

saveWorkbook(wb, "�ګ��i�S����.xlsx", overwrite = TRUE)





#####=====(6) ���P�����ε��c =====#####

# ���Ĥ@�g�� author title time text 


url = "https://forum.gamer.com.tw/" ; 
php = "B.php?page=1&bsn=60561" ; 
wb <- createWorkbook(creator = 'grant',title = '�ګ��i�S����_20��')
addWorksheet(wb,sheetName = 'sheet1')
writeData(wb,sheet = 'sheet1',x = list("title", "author", "time", "gp", "bp", "maintext", "sub.Author", "sub.Time", "sub.gp", "sub.bp", "sub.text"), startCol = 1 )
cnt = 1

allData <- data.frame( title=c(NA), author=c(NA), time=c(NA), gp=c(NA), bp=c(NA),  mainText=c(NA), sub.author=c(NA), sub.time=c(NA), sub.gp=c(NA), sub.bp=c(NA), sub.Text=c(NA))
allData <- emptyDF[-1,]

for ( p in c(1:100) ) 
{
  php = str_c( "B.php?page=", p, "&bsn=60561") 
  print( str_c( "�� ", p, " �� href : ", str_c( url , php ) )  ) 
  response2 = getURL( str_c( url , php ), .encoding = 'utf8' )
  response2.parser = htmlParse(response2, asText=TRUE)
  
  ## title 
  list <- xpathSApply(response2.parser, "//*[@id='BH-master']
                                             //*[@class='b-list__row b-list-item b-imglist-item']
                                             //*[@class='b-list__main']
                                             //*[@class='imglist-text']
                                             /div/p" )
  print( str_c("��", length(list), "�g��" ) ) 
  ### 
  
  ## author 
  author <- xpathSApply(response2.parser, "//*[@id='BH-master']
                                             //*[@class='b-list__row b-list-item b-imglist-item']
                                             //*[@class='b-list__count']
                                             //*[@class='b-list__count__user']" 
                        , xmlValue )
  
  author <- str_replace_all( author, "\n","") ; 
  ### 
  
  title.df <- xmlToDataFrame(list)
  title.df$php <- sapply( list, function(x)xpathSApply( x, "attribute::href") ) 
  title.df$author <- author; # head( title.df, 2 )
  
  # head( title.df, 3 ) 
  #    text                              php                              author
  # 1 �i���D�j���b�˴��~�u�@�L���e���жi C.php?bsn=60561&snA=23073&tnum=6 Ropwso
  # 2           �i�Q�סj�̪񥿦Ҽ{���u�@ C.php?bsn=60561&snA=23052&tnum=9 di25247994
  # 3   �i�Q�סj���q�o��!!���K�����ް��D C.php?bsn=60561&snA=23054&tnum=2 love779779
  
  # �Ҧ��峹���e 
  ll <- apply( title.df, 1, GetData )
  ll.df <- ldply (ll, data.frame)
  
  allData <- rbind( allData, ll.df)
  
  print( str_c("�`�@��:", length( unique(allData$title) ) ) ) 


} # for 

writeData( wb ,sheet = 'sheet1',x = allData )
saveWorkbook( wb, "new.xlsx", overwrite = TRUE )


GetMainData <- function( sec )
{
  floor = xpathSApply( sec, "./div[@class='c-post__header']/div[@class='c-post__header__author']/a[@class='floor tippy-gpbp']/attribute::data-floor") ; 
  #print( str_c("��", floor, "��") ) 
  # c-section__main c-post c-post--feature
  author = xpathSApply( sec, "./div[@class='c-post__header']/div[@class='c-post__header__author']/a[@class='userid']", xmlValue ) ; print( str_c( floor, author ) )
  gp = xpathSApply( sec, "./div[@class='c-post__header']/div[@class='c-post__header__author']/div[@class='postcount']/span[@class='postgp']/span ", xmlValue ) ;# gp
  bp = xpathSApply( sec, "./div[@class='c-post__header']/div[@class='c-post__header__author']/div[@class='postcount']/span[@class='postbp']/span ", xmlValue ) ;# bp
  time = xpathSApply( sec, "./div[@class='c-post__header']/div[@class='c-post__header__info']/a[@class='edittime tippy-post-info']/attribute::data-mtime" ) ; #time 
  text = xpathSApply( sec, "./div[@class='c-post__body']/article[@class='c-article FM-P2']/div[@class='c-article__content']", xmlValue ) ; 
  text = str_replace_all( text, '\n', '' ) ; # print( text )
  data = data.frame( author = author, time = time, gp = gp, bp = bp, mainText = text )
 
 
  return ( data )
} 

GetSubData <- function( sec )
{
  floor = xpathSApply( sec, "./div[@class='c-post__header']/div[@class='c-post__header__author']/a[@class='floor tippy-gpbp']/attribute::data-floor") ; 
  # print( str_c("��", floor, "��") )
  author = xpathSApply( sec, "./div[@class='c-post__header']/div[@class='c-post__header__author']/a[@class='userid']", xmlValue ) ; print( str_c( floor, author ) )
  gp = xpathSApply( sec, "./div[@class='c-post__header']/div[@class='c-post__header__author']/div[@class='postcount']/span[@class='postgp']/span ", xmlValue ) ; #print(gp)
  bp = xpathSApply( sec, "./div[@class='c-post__header']/div[@class='c-post__header__author']/div[@class='postcount']/span[@class='postbp']/span ", xmlValue ) ; #print(bp)
  time = xpathSApply( sec, "./div[@class='c-post__header']/div[@class='c-post__header__info']/a[@class='edittime tippy-post-info']/attribute::data-mtime" ) ; #print(time)
  text = xpathSApply( sec, "./div[@class='c-post__body']/article[@class='c-article FM-P2']/div[@class='c-article__content']", xmlValue ) ; 
  text = str_replace_all( text, '\n', '' ) ; #print( text )
  data = data.frame( sub.author = author, sub.time = time, sub.gp = gp, sub.bp = bp, sub.Text = text )
  
  # print( "Sub data =========================================================================================================================================================================")
  # print( data ) 
  # print( "Sub data =========================================================================================================================================================================")
  return( data ) ; 
}


GetData <- function( df ) 
{
  Sys.sleep( 3 )
  emptyDF <- data.frame( title=c(NA), author=c(NA), time=c(NA), gp=c(NA), bp=c(NA),  mainText=c(NA), sub.author=c(NA), sub.time=c(NA), sub.gp=c(NA), sub.bp=c(NA), sub.Text=c(NA))
  emptyDF <- emptyDF[-1,]
  
  tt = data.frame( title = as.character(df[1]) ) ; 
  print( tt )
  php = df[2] 
  response = getURL( str_c( url , php ), .encoding = 'utf8' )
  print( str_c( url , php ) )
  parser = htmlParse(response, asText=TRUE)
  section = xpathSApply( parser, "//div[@id='BH-master']/section[@class='c-section']/div[@class='c-section__main c-post ' or @class='c-section__main c-post c-post--feature' ]" ) # or @class='c-section__main c-post c-post--feature'
  # /html/body/div[5]/div/div[2]/section[3]/div[2]/div[1]/div[1]
  print( length(section) )
  # /div[@class='c-post__header']/div[@class='c-post__header__author']/a[@class='tippy-gpbp']/attribute::data-floor" ) ; # floor 
  first = TRUE ; 
  for ( sec in section )
  {
    floor = xpathSApply( sec, "./div[@class='c-post__header']/div[@class='c-post__header__author']/a[@class='floor tippy-gpbp']/attribute::data-floor") ; # floor 
    # print( floor )
    if ( floor != "1" && first == TRUE )
    {
      print( "ERROR" )
    } 
    else if ( floor == "1" ) 
    {
      maindata <- GetMainData( sec )
      first = FALSE 
    }
    else if ( floor != "1" && first == FALSE ) 
    {
      
      subdata <- GetSubData( sec )  
      mergedata <- cbind( tt, maindata ,subdata)
      emptyDF<- rbind(emptyDF, mergedata)
    }
    
    if ( length(section) == 1 )
    {
      nulldata = data.frame( sub.author = c(NA), sub.time = c(NA), sub.gp = c(NA), sub.bp = c(NA), sub.Text = c(NA) )
      mergedata <- cbind( tt, maindata ,nulldata)
      emptyDF<- rbind(emptyDF, mergedata)
    }
    
  }
  # print( tt )
  # print( str_c("��",length(section), "��" ) ) 
  # print() 
  # print( head(emptyDF) )
  return( emptyDF ) 

}




