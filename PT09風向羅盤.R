# PT09風向羅盤.R: lexical analysis by wind compass 
# Jia-Sheng Heh, 05/22/2019, revised from P910.R

wkDir = "E:\\AllRepo\\R_Crawler\\文本分析\\";   setwd(wkDir)
dataDir = "E:\\AllRepo\\R_Crawler\\文本分析\\";
library(jiebaR); library(stringr);   library(text2vec);   library(data.table)
library( openxlsx )
library(readxl)
library( rio )
library(dplyr)
library(flextable)
library(ggplot2)
library(showtext)
font_add_google("Noto Sans TC", "NotoSansTC")
font_families
showtext_auto(enable=TRUE)
par(family = 'NotoSansTC')


##########===== (A).文件檔案到詞語 =====##########

#####===== (A1) (KDD1) 讀取數據 (RR....csv-->RR) =====#####
RR = read.csv('new.csv', header = TRUE, sep = ',', fileEncoding = "utf-8" ) 

RR <- filter( RR, RR$sub.author != "" )

RR$gp[ RR$gp == "-" ] = 0
RR$bp[ RR$bp == "-" ] = 0

RR$gp[ RR$gp == "爆" ] = as.integer( 1000 )
RR$bp[ RR$bp == "X" ] = as.integer( 1000 )

RR$sub.gp[ RR$sub.gp == "-" ] = 0
RR$sub.bp[ RR$sub.bp == "-" ] = 0

RR$gp[ is.na( RR$gp ) ] = 0
RR$bp[ is.na( RR$bp ) ] = 0
RR$sub.gp[ is.na( RR$sub.gp ) ] = 0
RR$sub.bp[ is.na( RR$sub.bp ) ] = 0

RR$gp = as.integer( RR$gp ) 
RR$bp = as.integer( RR$bp )
RR$sub.gp = as.integer( RR$sub.gp )
RR$sub.bp = as.integer( RR$sub.bp )

RR$sub.time[ RR$sub.time == ""  ] = NA

RR$nch   = nchar( RR$mainText )

RR$sub.nch   = nchar( RR$sub.Text )

RR$Rdate = as.Date(RR$sub.time)

dim(RR);   head(RR,2)   #-- [1] 13945    14

# title      author                time gp bp
# 1 20歲到30歲沒專長的年輕人都在做甚麼工作 shueh201237 2022-07-30 04:18:52 12  0
# 2 20歲到30歲沒專長的年輕人都在做甚麼工作 shueh201237 2022-07-30 04:18:52 12  0
# mainText
# 1 每年那麼多大學畢業生一些很專業的工作缺沒有那麼多也不是所有人都做相關科系的工作比較不需要門檻的像是很多工廠 工地也幾乎都沒有年輕人餐飲業也是店員幾乎都是老人的趨勢想問看看這種年紀區間沒特殊專長的人可以分享都在做甚麼樣的工作嗎
# 2 每年那麼多大學畢業生一些很專業的工作缺沒有那麼多也不是所有人都做相關科系的工作比較不需要門檻的像是很多工廠 工地也幾乎都沒有年輕人餐飲業也是店員幾乎都是老人的趨勢想問看看這種年紀區間沒特殊專長的人可以分享都在做甚麼樣的工作嗎
# sub.author            sub.time sub.gp sub.bp                                                                                           sub.Text nch sub.nch      Rdate
# 1  dh5987462 2022-07-30 04:50:36     20      0 輪班囉設備工程、產品工程、技術員。學校不會教你組裝業界機台、看良率分析啦！大多數文憑只是張面試門票 112      49 2022-07-30
# 2  qbgeneral 2022-07-30 06:50:47     18      0         就大叔我的觀察有幾種外送員貨運電子廠輪班作業員餐飲超商派遣直銷賣車房仲藍藍路差不多就這樣了 112      45 2022-07-30

#####===== (A2) 主題數據框到主題價值模型(Topic Value Model) (Pv->Pv$chCount0,Pv$nDay0) =====#####  
setDT(RR, key = "title") 
Pv = RR[, .(title=unique(title), text = unique(mainText), postNo=length(sub.time), D0=min(sub.time), Df=max(sub.time), Nauthor=length(unique(sub.author)), 
            chCount=sum(nch,na.rm=T)), by=title];   

Pv$postNo0 = cut(Pv$postNo, breaks=c(0,1,9,99,999,7999));   table(Pv$postNo0)
Pv$chCount0 = cut(Pv$chCount, breaks=c(-1,0,9,99,999,9999,99999,999999));    table(Pv$chCount0)
Pv$nDay = as.Date(Pv$Df)-as.Date(Pv$D0);   range(Pv$nDay)   #-- 0 4253
Pv$nDay0 = cut(as.numeric(Pv$nDay), breaks=c(-1,0,7,30,100,400,1200,4300));  table(Pv$nDay0)
dim(Pv);   head(Pv,3)   #-- [1] 2214   11
addmargins( table(Pv$nDay0, Pv$chCount0) )
# nDay0\chCount0    (-1,0] (0,9] (9,99] (99,999] (999,1e+04] (1e+04,1e+05] (1e+05,1e+06]  Sum
# (-1,0]                 0     1    119      414         111            12             0  657   #-- 一日話題
# (0,7]                  1     1     20      406         563            63             1 1055   #-- 一週
# (7,30]                 0     0      3       72         137            32             1  245   #-- 一月
# (30,100]               0     0      0       22          39            25             1   87   #-- 三月
# (100,400]              1     0      1       18          39            18             2   79   #-- 一年
# (400,1.2e+03]          0     0      0       12          41            26             1   80   #-- 三年
# (1.2e+03,4.3e+03]      0     0      0        1           4             6             0   11   #-- 逾三年
# Sum                    2     2    143      945         934           182             6 2214
#                            數十字話題  數百字..    數千字..     數萬字..      數十萬字..



#####===== (A3) (KDD-3) 數據轉換:將文本轉為詞語 (k-->RR$text[which(RR$title==RR$title[k]]-->Dtext) =====#####
kk = 400;   RR$title[kk]     #-- [1] "【問題】27歲 離職待業兩個月了 正在找尋方向 求巴友們指點迷津"
which(RR$title==RR$title[kk]);   length(which(RR$title==RR$title[kk]))   #-- 7
# [1] 395 396 397 398 399 400 401


Dtext = RR$sub.Text[which(RR$title==RR$title[kk])];   length(Dtext);   Dtext[1:3];  #-- [1] 7
# [1] "怎麼會有人覺得工程師想當就當的阿？真的是被網路廣告洗腦洗得很好呢你連一些基本的只要手腳正常的工作都沒法待久了工程師這種需要花腦力的肯定更撐不下去我建議樓主不當保全去當個小七員工或著連鎖餐飲打工仔就好了基本上測驗能被測出做保全，不適合作需要高度邏輯思考的工作"                                                                                                                                                                                                                      
# [2] "題目算蠻明確的，喜歡就喜歡不喜歡就不喜歡不用欺騙自己作答，結果其實算準確的，我現在在科學領域職場（電機），但也想過有朝一日當自家大樓的保全不用過得那麼辛苦，錢賺自己爽就好，也會覺得社會角落有許多弱勢需要幫助，滿足自己精神層面的需求，這3個剛好就蠻符合我自身的狀態。"                                                                                                                                                                                                               
# [3] "從您的學經歷來看，最好就繼續從事防治、環境相關的技術工作，這類行業我記得也是有證照可以考得，而且相對冷門等於你站穩了就可以一直做下去。沒有資工背景，大學也不是頂大，臨時要轉軟體工程師頂多就是去小公司，待遇不見得會比你在特定領域深耕好。除非真的對寫程式相關的工作有興趣，不然還是多考慮一下。雖然網路上常常在講什麼斜槓、轉行、轉職、自我挑戰、追求人生理想，但人資刷履歷的原則基本上還是歧視換跑道跟學經歷不好的人，包括連當一個技術員或作業員，都會優先聘用在大廠穩定工作過的人。"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            


#####===== (A4) 詞類分析 =====##### 
## 內容詞------
#-    實體詞："n" 名词  "nr" 人名 "nrt" "ns" 地名 "nt" 机构团体  "nz" 其他专名 ------
#             "r" 代词  
#              "v" 动词  "vn" 名动词
#-    修飾詞："a" 形容词  "ad"  副形词 -----------------> Emotional Analysis: 情緒分析--正向詞,負向詞
#             "d" 副词  "df" 
#             "m" 数词 "q" 量词  
#             "f" 方位词  "s" 处所词  "t" 时间词 
#             "x" 字符串  
## 功能詞------
#-    "c" 连词  
#-    "p" 介词  
#     "u" 助词  "ud" 的 底 "ug" 过 "uj" 所 "ul" 了 喽 "uv" 一样 一般 似的 般
#     "i" 成语  "l" 简称略语  
#     "y"  语气词 
#     "z" 状态词  "zg" 

#####===== (A5) 標題之文本分析--實體詞 標籤序列 (sportList/sportDegree/ sport0.txt --> spN --> Wkey) =====#####  
sportList = c( "nBoss", "nCoworker", "nSalary", "nLeave", "nBenefit", "nCareer", "nWorkplace", "nEquipment", 
               "nInterview", "nDepartment", "nAdvantage", "nLanguage", "nStudy", "nHealth", "nPL", "nInvestment"  )
sportDegree = c(0,25,50,75,100,  120,150,180,  210,240,270,  300,330)
wkr = worker( user="tag0.txt", type="tag")
term = segment(Pv$text, wkr)
unique(names(term)) 
length( term[names(term)=="nBoss"]) 
# [1] "r"           "p"           "ns"          "v"           "c"           "n"           "m"          
# [8] "x"           "nWorkplace"  "a"           "d"           "zg"          "nCoworker"   "f"          
# [15] "t"           "nDepartment" "eng"         "uj"          "q"           "nSalary"     "l"          
# [22] "ul"          "y"           "nr"          "nCareer"     "nHealth"     "nBenefit"    "ad"         
# [29] "s"           "nrt"         "b"           "df"          "u"           "nInvestment" "nBoss"      
# [36] "ud"          "vn"          "ng"          "i"           "z"           "rr"          "ug"         
# [43] "an"          "nz"          "nAdvantage"  "nLeave"      "nEquipment"  "h"           "nInterview" 
# [50] "o"           "k"           "vd"          "mq"          "e"           "j"           "ag"         
# [57] "nStudy"      "uv"          "vg"          "rz"          "nt"          "tg"          "vq"         
# [64] "uz"          "g"           "rg"          "vi"          "dg"          "mg"  

ajterm = term[grep( "a", names(term))]
write( unique(ajterm), "aj.txt" )

ntagterm = term[grep( "n", names(term))]
ntagterm.df = as.data.frame( ntagterm ) 
ntagterm.df = filter( ntagterm.df , nchar( ntagterm.df$ntagterm ) >= 2) 
length( ntagterm ); dim ( ntagterm.df)
ntagterm0 = as.character(ntagterm.df[1:dim(ntagterm.df)[1], ]);

remove_english_words <- function(text) {
  pattern <- "\\b[A-Za-z0-9[:punct:]]+\\b"
  cleaned_text <- gsub(pattern, "", text)
  cleaned_text <- gsub("\\s{2,}", " ", cleaned_text)  # 删除多余的空格
  cleaned_text <- trimws(cleaned_text)  # 删除首尾空
  return(cleaned_text)
}

re = remove_english_words( ntagterm0 ) ; 
ntagterm0 = re[ re != "" ]
ntagterm0 = unique( ntagterm0 )
length( ntagterm0 ) ; head( ntagterm0 )
kwKind = list()
kwKind$"nBoss" = c("老闆","機車","主管","政府","業主","廠商","店長","客戶","朋友") 
kwKind$"nCoworker" = c("老鳥","新人","朋友","學長","人員","同事","機車","員工","人員","對方","部門")

kwKind$"nSalary" = c("彈性","業績","價值","全額","薪水","天數","年資","月薪","夜班","薪資","加班費","輪班","日班","全職","行業")
kwKind$"nLeave" = c("事假","天數","小時")
kwKind$"nBenefit" = c("保險","投保","國民","年金","合約","契約","勞保","規定","勞基法","勞工","失業","獎金","福利")

kwKind$"nCareer" = c("公職","人生","待業","offer","創業","跳槽","運氣","機會","工作","大運","大學","內容","賽","球員","網站")
kwKind$"nWorkplace" = c("內場","單位","餐廳","中國","社區","業界","市場","疫苗","職場","環境","壓力","企業","發展","機構","國營","醫院","工廠","辦公室","加油站","疫情","日本","現場","銀行")

kwKind$"nEquipment" = c("cnc","軟體","裝潢","電腦","手機","設備","檢查","系統","品管")
kwKind$"nInterview" = c("面試","面試官","課程","會員","次數","姿勢","強度","感覺")
kwKind$"nDepartment" = c("學歷","老師","學生","學校","科系","老師","碩士","大學","畢業","感覺","興趣","經歷","讀書","當兵")
kwKind$"nAdvantage" = c("堆高機","程式","證照","乙級","課程","專長","考試")
# 公職/產業/行業/職業
kwKind$"nJob" = c("清潔隊","職業","產業","遊戲","公務員","船員","郵差","計程車","設計師","助理","外勤","職業","作業員","行銷","企劃","工程師",
                "司機","業務","uber","eat","技師","電機","熊貓","工程師","餐飲","line")
kwKind$"nLanguage" = c("英文","基礎","語言","日文" )
kwKind$"nStudy" = c("經驗","師傅","基礎","學徒","年輕人" )
kwKind$"nHealth" = c("身體","狀況" )
kwKind$"nPL" = c("java","php", "game" )
kwKind$"nInvestment" = c("投資","股票", "保險","情況","規劃" )

length(kwKind)  #-- 17
df = as.data.frame( unique( ntagterm0 ) ) ; dim( df )
df$tag = ""
for (k in 1:length(kwKind)) {
  for (j in 1:length(kwKind[[k]])) {
    print( names(kwKind)[k]) 
    df$tag[ grep(kwKind[[k]][j], ntagterm0 ) ] = names(kwKind)[k]
  }
}
# write( unique(ntagterm0) , "tag0.txt", sep = ",")
write.table(df, file = "df.txt",row.names = FALSE)
########## (B) 爬文的內容分析/文本分析(Content/Text Analysis)的語彙層次 ##########

#####===== (B1) 標題之文本分析--主題風向羅盤LIB =====#####  
plotWindCompass <- function (content,Wkey,pPower,nPower,topic,labelX,labelY,quadText) {
  library(plotrix);
  plot( x=NULL, y=NULL, xlim=c(-5,5), ylim=c(-5,5), 
        main=paste0(topic," 網路風向羅盤圖 ","(貼文數=",as.character(length(content)),")"), 
        xlab=labelX,  ylab=labelY)
  abline(v=0);         abline(h=0);
  draw.circle(0,0,  1,lty=2,border="blue");
  draw.circle(0,0,  2,lty=2,border="blue");
  # draw.circle(0,0, 2.5,lty=1,border="brown")
  draw.circle(0,0,  3,lty=1,border="brown");
  draw.circle(0,0,  4,lty=2,border="green")
  draw.circle(0,0,  5,lty=2,border="green")
  
  for (j in 1:8) text( 5.5*cos(2*pi*(j-0.5)/8), 5.5*sin(2*pi*(j-0.5)/8), quadText[j], col="black" )
  
  for (j in 1:8) {
    dt = 45/(length(Wkey[[j]])+1)
    for (i in 1:length(Wkey[[j]])) {
      theta = ((j-1)*45+dt*i) * pi / 180;
      # print( paste0( "Wkey:", Wkey[[j]][i]) )
      kContent = grep(Wkey[[j]][i], content)
      if (length(kContent)!=0) {
        # print( pPower[kContent] )
        rPos = max(as.numeric(pPower[kContent]),na.rm=T)
        # print( paste0("POS:", rPos))
        if (rPos > 0.5) {
          # print( "rPos > 0.5" )
          boxed.labels( (3+2*rPos)*cos(theta), (3+2*rPos)*sin(theta), labels=Wkey[[j]][i], bg="#000079", col="blue", cex=0.9, xpad=0.2, ypad=0.4, border=TRUE)
        } else {
          # print( "rPos <= 0.5" )
          boxed.labels( (3+2*rPos)*cos(theta), (3+2*rPos)*sin(theta), labels=Wkey[[j]][i], bg="#000079", col="green", cex=0.9, xpad=0.2, ypad=0.4, border=FALSE)
        }
        rNeg = max(as.numeric(nPower[kContent]),na.rm=T)
        # print( paste0("NEG:", rNeg))
        if (rNeg < 0.5) {
          # print( "rNeg < 0.5" )
          boxed.labels( (3-2*rNeg)*cos(theta), (3-2*rNeg)*sin(theta), labels=Wkey[[j]][i], bg="#000079", col="orange", cex=0.9, xpad=0.2, ypad=0.4, border=FALSE)
        } else {
          # print( "rNeg >= 0.5" )
          boxed.labels( (3-2*rNeg)*cos(theta), (3-2*rNeg)*sin(theta), labels=Wkey[[j]][i], bg="#000079", col="red", cex=0.9, xpad=0.2, ypad=0.4, border=FALSE)
        }
        lines( c( (2*(rPos+1)+1)*cos(theta), (2*(1-rNeg)+1)*cos(theta)), c( (2*(rPos+1)+1)*sin(theta), (2*(1-rNeg)+1)*sin(theta)))
      }
      
      # print("================================================================" )
    }
    
    
  }
}

#####===== (B2) 語言學的語詞分類 [C.C.Fries, 1952] =====#####
# --  功能詞(function words)
# ..    Words that have little lexical meaning or have ambiguous meaning
# ..    express grammatical relationships with other words within a sentence, or specify the attitude or mood of the speaker.
# ..    might be prepositions, pronouns, auxiliary verbs, conjunctions, grammatical articles or particles
# --  內容詞(content words or lexical words)
# ..    Words that are not function words
# ..    Dictionaries: define the specific meanings of content words, but can only describe the general usages of function words. 
# ..    include (實體詞) nouns, verbs, (修飾詞) adjectives, and most adverbs, although some adverbs are function words (e.g., then, and, why)

#####===== (B3) 標題之文本分析--實體詞 標籤序列 (sportList/sportDegree/ sport0.txt --> spN --> Wkey) =====#####  
# sportList = c("nBase","nBasket","nBalls","nField","nWater",  "nDevice","nClothes","nBrand",  "nMeasure","nEat","nBody",  "nFitness","nPlay")
ttlist = c( "nBoss", "nCoworker", "nSalary", "nLeave", "nBenefit", "nCareer", "nWorkplace", "nEquipment", 
               "nInterview", "nDepartment", "nAdvantage", "nJob", "nLanguage", "nStudy", "nHealth", "nPL", "nInvestment"  )
sportDegree = c(0,25,50,75,100,  120,150,180,  210,240,270,  300,330)

# wkr = worker(user=paste0(wkDir,"sport0.txt"), type="tag")

wkr = worker( user="tag0.txt", stop_word = "stop.txt", type="tag")

term = segment(Pv$text, wkr)

unique(names(term))

# [1] "m"           "zg"          "v"           "nAdvantage"  "uj"          "nStudy"      "d"           "p"          
# [9] "r"           "x"           "nWorkplace"  "n"           "t"           "y"           "a"           "ul"         
# [17] "mq"          "eng"         "nJob"        "nDepartment" "nCoworker"   "nBoss"       "nSalary"     "l"          
# [25] "s"           "c"           "nInterview"  "i"           "f"           "ad"          "nBenefit"    "nLeave"     
# [33] "k"           "z"           "nInvestment" "nCareer"     "ng"          "b"           "j"           "nEquipment" 
# [41] "nr"          "e"           "nLanguage"   "u"           "ud"          "ug"          "vn"          "ag"         
# [49] "nHealth"     "df"          "ns"          "q"           "rr"          "vg"          "h"           "nt"         
# [57] "vd"          "tg"          "nz"  

# for (k in 1:length(sportList)) { print(paste0(">>> ",sportList[k]));  print( as.vector( term[grep(sportList[k],names(term))] ) ) }
# spN = list();  for (k in 1:length(sportList)) spN[[k]] = unique(term[grep(sportList[k], names(term))])
# Wkey = list(c(spN[[1]],spN[[2]]), c(spN[[3]],spN[[4]],spN[[5]]), spN[[6]], c(spN[[7]],spN[[8]]), c(spN[[9]],spN[[10]]), spN[[11]], spN[[12]],spN[[13]])
# length(Wkey);   head(Wkey,2)

for (k in 1:length(ttlist)) { print(paste0(">>> ",ttlist[k]));  print( as.vector( term[grep(ttlist[k],names(term))] ) ) }

spN = list();  for (k in 1:length(ttlist)) spN[[k]] = unique(term[grep(ttlist[k], names(term))])

 
# "個人 ---動態--- 社會"
# 健康 教育 技術 投資 交際 社會福利 工作 雜項
# 健康 : "nHealth" 15
# 教育 : "nStudy","nDepartment" 14 10
# 技術 : "nAdvantage", "nLanguage", "nPL" 11 13 16
# 投資 : "nInvestment", "nInterview" 17 9
# 交際 : "nBoss", "nCoworker"  1 2 
# 社會福利 : "nSalary", "nLeave", "nBenefit"  3 4 5 
# 工作 : "nCareer", "nWorkplace", "nJob" 6 7 12 
# 雜項 : "nEquipment"  8
Wkey = list(  spN[[15]], 
              c( spN[[14]], spN[[10]]),
              c( spN[[11]], spN[[13]], spN[[16]]),
              c( spN[[17]], spN[[9]]),
              c( spN[[1]], spN[[2]]),
              c( spN[[3]], spN[[4]], spN[[5]]),
              c( spN[[6]], spN[[7]], spN[[12]]),
              spN[[8]]
           )

length(Wkey);   Wkey   #-- 8
# [[1]]
# [1] "政府"     "廠商"     "主管"     "老闆"     "業主"     "客戶"     "店長"     "當老闆"   "女主管"   "員工"    
# [11] "同事"     "新人"     "人員"     "朋友"     "對方"     "學長"     "工商部門" "機車"     "公務人員" "跨部門"  
# [21] "交朋友"   "部門"     "老鳥"     "機車行"   "保全人員" "女同事"   "普通員工"
# 
# [[2]]
# [1] "薪資"     "薪水"     "行業"     "夜班"     "全職"     "發薪水"   "年資"     "輪班"     "月薪"     "加班費"  
# [11] "日班"     "彈性"     "值夜班"   "價值"     "小時"     "勞工"     "失業"     "契約"     "勞基法"   "勞保"    
# [21] "國民"     "年金"     "合約"     "福利"     "勞工局"   "獎金"     "投保"     "規定"     "年終獎金"

#####===== (B4) 標題之文本分析--修飾詞 標籤序列 (adjList/ aterm.txt --> adjMat) =====#####  
adjList = c("ap","a0","an")
wkAdj = worker(user="newAterm.txt", stop_word = "stop.txt", type="tag")
adjMat   = matrix(0, nrow=dim(Pv)[1], ncol=length(adjList)) # initial adjMat
Pv = Pv[3: dim( Pv )[1]]
# head( Pv )
for (i in 1:length(Pv$text)) { 
  adjKW = segment(Pv$text[i], wkAdj)
  for (j in 1:length(adjList)) {
    IND = grep( adjList[j], names(adjKW) ); 
    adjMat[i,j] = paste(adjKW[IND],collapse="/") 
    }
}
colnames(adjMat) = adjList;     dim(adjMat);     head(adjMat,20)   #-- [1] 2210    3


#####===== (B5) 標題之文本分析--網路風向羅盤 (Pv$title/Wkey/adjMat --> plotWindCompass with topicG/labelX/labelY/quadText) =====#####  
# topicG = "mobile運動";   labelX = "配備 ---動態--- 運動";    labelY = "個動 ---參與者--- 群動"
topicG = "巴哈--職場版";   labelX = "個動 ---參與者--- 群動";   labelY = "社會 ---動態--- 個人"
## 健康 教育 技術 投資 交際 社會福利 工作 雜項
quadText <- c("健康","教育","技術","投資","交際","社會福利","工作","雜項")
IND = 1:5
plotWindCompass( 
                 Pv$text[IND], # [1] "20歲到30歲沒專長的年輕人都在做甚麼工作" [2] "4種低調賺大錢的工作，菜市場、擺小吃攤、有專長的工人、資源回收業" 
                 Wkey, 
                 as.numeric( adjMat[IND,1]!="" ), # ap 
                 as.numeric( adjMat[IND,3]!="" ), # an
                 topicG,   ## "巴哈--職場版"
                 labelX,   ## "個人 ---動態--- 社會"
                 labelY,   ## "個動 ---參與者--- 群動"
                 quadText  ## c("社會","資源","工作","服務業","教育","家庭","技術","雜項")
               )
IND = 1:2000
plotWindCompass(Pv$title[IND], Wkey, as.numeric(adjMat[IND,1]!=""), as.numeric(adjMat[IND,3]!=""), topicG, labelX,labelY,quadText)

#####===== (C1) 情緒字典 =====#####
##== (1)
library(tmcn)
data(NTUSD)  #--> 似乎有問題...

##== (2)
# https://github.com/EternalFeather/Chinese-Sentiment-Lexicon 
# --> Seedwords.txt

##== (3)
# 中文版LIWC詞典: https://cliwc.weebly.com/1997936617214503287932097.html
# --> 404 Error
# --> https://www.liwc.app/  要錢買

