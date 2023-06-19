# PT08知識架構: lexical analysis with vocabulary by jieaR and GloVe
# Jia-Sheng Heh, 05/17/2022, reviseded from P907.R

wkDir = "E:\\AllRepo\\R_Crawler\\文本分析/";   setwd(wkDir)
dataDir = wkDir;
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")
library(jiebaR);     library(xlsx);    library(stringr);   library(text2vec);   library(data.table); library(Rgraphviz) 
########## (A).知識空間理論(Knowledge Space Theory) ##########

#####===== (A1).幾個文獻 =====#####
#== Knowledge Space Theory (KST): a set concepts and structures that aims to assess and represent one’s knowledge state
library(kst)
### [Jean-Paul Doignon and Jean-Claude Falmagne. (1999). Knowledge Spaces. Springer] -- introduced by Doignon and Falmagne (1985)
### [Christina Stahl and Cord Hockemeyer. (2019). Knowledge Space Theory. https://cran.r-project.org/web/packages/kst/vignettes/kst.pdf]
### [ChriLucas Oliveira. (2020). Introduction to Knowledge Space. https://medium.com/adapted/introduction-to-knowledge-space-theory-ce4fd91ae1ae ]
### [Ali Unl and Anatol Sargin. (2010). DAKS: An R Package for Data Analysis Methods in Knowledge Space Theory. J.Statistical Software. https://www.jstatsoft.org/article/view/v037i02]

#####===== (A2).知識架構(Knowledge Structure) =====#####
#== DOMAIN, item set Q: m dichotomous items
#== KNOWLEDGE STATES: response patterns as subsets of Q
#== KNOWLEDGE STRUCTURE K, (Q,K): a collection of subsets of Q (elements of 2^Q, the power set of Q)
kst = kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), set("a","b","d","e"), set("a","c","d","e"), 
                     set("a","b","c","d","e")));    kst 
#-- {{}, {"a"}, {"a", "b"}, {"a", "c"}, {"d", "e"}, {"a", "b", "d", "e"}, {"a", "c", "d", "e"}, {"a", "b", "c", "d", "e"}}
kdomain(kst)   #-- {"a", "b", "c", "d", "e"}
plot(kst)

#####===== (A3).知識空間(Knowledge Space) =====#####
#== KNOWLEDGE SPACE: a knowledge structure that is closed under union
#== the entire knowledge structure by a combination of a small set of elements
ksp = kspace(kst);   ksp
#-- {{}, {"a"}, {"a", "b"}, {"a", "c"}, {"d", "e"}, {"a", "b", "c"}, {"a", "d", "e"}, {"a", "b", "d", "e"}, {"a", "c", "d", "e"}, {"a", "b", "c", "d", "e"}}
plot(ksp)

#####===== (A4).知識基底(Knowledge Base) =====#####
#== ATOMS: are the minimum states in which a given item q is present.
katm = katoms(kst, items=kdomain(kst));   katm
# $a   {{"a"}}
# $b   {{"a", "b"}}
# $c   {{"a", "c"}}
# $d   {{"d", "e"}}
# $e   {{"d", "e"}}

#####-----
K = set(set("d"), set("a","c"), set("e","f"), set("a","b","c"), set("d","e","f"), 
        set("a","b","c","d"), set("a","c","e","f"), set("a","c","d","e","f"), set("a","b","c","d","e","f"))
plot(kstructure(K))
plot(kspace(kstructure(K)))
katoms(kstructure(K),items=kdomain(kstructure(K)))

#####===== (A5).學習路徑(Learning Path) =====#####
#== LEARNING PATH: a maximal sequence of knowledge states, which allows learners to gradually traverse a knowledge structure 
#                  or space from the empty set {} (or any other bottom state) to the full set of domain problems Q
lpath(ksp)
# [[1]] {{}, {"a"}, {"a", "b"}, {"a", "b", "c"}, {"a", "b", "c", "d", "e"}}
# [[2]] {{}, {"a"}, {"a", "c"}, {"a", "b", "c"}, {"a", "b", "c", "d", "e"}}
# [[3]] {{}, {"a"}, {"a", "b"}, {"a", "b", "d", "e"}, {"a", "b", "c", "d", "e"}}
# [[4]] {{}, {"a"}, {"a", "d", "e"}, {"a", "b", "d", "e"}, {"a", "b", "c", "d", "e"}}
# [[5]] {{}, {"d", "e"}, {"a", "d", "e"}, {"a", "b", "d", "e"}, {"a", "b", "c", "d", "e"}}
# [[6]] {{}, {"a"}, {"a", "c"}, {"a", "c", "d", "e"}, {"a", "b", "c", "d", "e"}}
# [[7]] {{}, {"a"}, {"a", "d", "e"}, {"a", "c", "d", "e"}, {"a", "b", "c", "d", "e"}}
# [[8]] {{}, {"d", "e"}, {"a", "d", "e"}, {"a", "c", "d", "e"}, {"a", "b", "c", "d", "e"}}


########## (B).文本分析中的知識架構:數據準備 ##########

#####===== (B1) (KDD1) 讀取數據 (RR....csv-->RR) =====#####
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
# 1: 20歲到30歲沒專長的年輕人都在做甚麼工作 shueh201237 2022-07-30 04:18:52 12  -
# 2: 20歲到30歲沒專長的年輕人都在做甚麼工作 shueh201237 2022-07-30 04:18:52 12  -
#   mainText
# 1: 每年那麼多大學畢業生一些很專業的工作缺沒有那麼多也不是所有人都做相關科系的工作比較不需要門檻的像是很多工廠 工地也幾乎都沒有年輕人餐飲業也是店員幾乎都是老人的趨勢想問看看這種年紀區間沒特殊專長的人可以分享都在做甚麼樣的工作嗎
# 2: 每年那麼多大學畢業生一些很專業的工作缺沒有那麼多也不是所有人都做相關科系的工作比較不需要門檻的像是很多工廠 工地也幾乎都沒有年輕人餐飲業也是店員幾乎都是老人的趨勢想問看看這種年紀區間沒特殊專長的人可以分享都在做甚麼樣的工作嗎
# sub.author            sub.time sub.gp sub.bp
# 1:  dh5987462 2022-07-30 04:50:36     20      -
# 2:  qbgeneral 2022-07-30 06:50:47     18      -
# sub.Text
# 1: 輪班囉設備工程、產品工程、技術員。學校不會教你組裝業界機台、看良率分析啦！大多數文憑只是張面試門票
# 2:         就大叔我的觀察有幾種外送員貨運電子廠輪班作業員餐飲超商派遣直銷賣車房仲藍藍路差不多就這樣了                                                             \n\nvivavida7749 wrote:\r\n板上偶爾有人在討論背...(恕刪)\n\r\n感覺是貼錯了⋯⋯應該是下背痛那幾篇...Jeff和譯者都佛心來著\r\nhttps://youtu.be/Z3-mYrVjBEo\r\nhttps://youtu.be/gYsXOKAfxTs\r\nhttps://youtu.be/vw-LIovZ_Os\r\nhttps://youtu.be/oXj3O0quR0E

#####===== (B2) (KDD3) 數據轉換 (RR$title/RR$text-->content/Ncontent) =====##### ---> 只就 RR$title 關鍵字太少，難以分類
RRtitle = unique(RR$title);   length(RRtitle)  #-- 2214
content = NULL;   Ncontent = NULL
for (title in RRtitle) {
  content = c(content, paste(title, paste(unique(RR$mainText[which(RR$title==title)]),collapse="\n\n"),collapse="\n\n"))
  Ncontent = c(Ncontent, length(which(RR$title==title)))
}
length(content);   head(content)  #-- [1] 2821
# [1] "20歲到30歲沒專長的年輕人都在做甚麼工作 "       "【問題】［保全］哪種固定日班類型比較好 "      
# [3] "【閒聊】發現主管跟老闆偷情 "                   "【問題】怎麼判斷徵才缺人 "                    
# [5] "【問題】求助24歲人生進修轉職方向（職安相關） " "【閒聊】有些人的臉皮真的厚得很誇張 "
Ncontent[1:20]   #--  [1] 19  5  1  2  1  5  3  5 19 19 17  5  7  9  1  5  4  3  4  1

#####===== (B3) (KDD4) (use B7 method) 以Seg$title找Dword2B ** ,求dtm,作文本聚類(A)取關鍵n詞 (Seg$title-->Dword2-->dtm-->group) (GOOD!!!) =====#####
removeKW <- function(KWlist,KWvec) { 
  OUTlist = list()
  for (kl in 1:length(KWlist)) {
    ind = NULL
    for (k in 1:length(KWvec)) ind = c( ind, as.vector(which(KWvec[k]==KWlist[[kl]])) )
    OUTlist[[kl]] = KWlist[[kl]][setdiff( (1:length(KWlist[[kl]])), ind )]
  }
  return(OUTlist)
}
cutter = worker( stop_word = "stop.txt", type="tag" )   #-- 可以用 byliness=TRUE, 就可以轉成 list()
Dword = sapply(content, function(x)segment(tolower(as.character(x)),cutter));
Dword = sapply(content, function(x)segment(tolower(as.character(x)),cutter));  length(Dword);   head(Dword[[1]],15)  #-- [1] 1993
# m       zg        v        m       zg        v        n       uj        n        d        p        v        r        r        r 
# "20"     "歲"     "到"     "30"     "歲"     "沒"   "專長"     "的" "年輕人"     "都"     "在"     "做"   "甚麼"   "每年"   "那麼" 
nDword = lapply(Dword,nchar);    head(nDword[[1]],15)
# m zg  v  m zg  v  n uj  n  d  p  v  r  r  r 
# 2  1  1  2  1  1  2  1  3  1  1  1  2  2  2 
Dword2B = lapply(Dword, function(x){nx=sapply(x,nchar); xA=x[nx>=2]; return(xA[setdiff((1:length(x)),grep("[[:digit:]]+",xA))])} );   length(Dword2B);   head(Dword2B[[1]],15)  #-- [1] 1993
#     r          n          r          x          d          r          v          n          c          n          n          c         nz          n          n 
# "自己" "腰痠背痛"     "自己"     "板上"     "偶爾"     "有人"     "討論"     "背痛"     "或是"     "腰痛"     "問題"     "然而"   "上班族" "腰痠背痛"     "原因" 
Dword2 = lapply(Dword2B, function(xB) return(xB[grep("n",names(xB))]));   length(Dword2);   Dword2[1:3]
Dword3 = removeKW(Dword2, c("https","http","www","tw","com","htm","wrote","vs","運動","如題","樓主","有點","謝謝","小弟","前輩","個人","我會","問題","時間","感覺","建議","時候","活動","東西","習慣"))
Dword3B = removeKW(Dword2B, c("https","http","www","tw","com","htm","wrote","vs","運動","如題","樓主","有點","謝謝","小弟","前輩","個人","我會","問題","時間","感覺","建議","時候","活動","東西","習慣"))
keys = worker("keywords",topn=5)   #-- 上述應該可以用 stop_word=c("XX","XX",...) 來進行
Dword4B = lapply(Dword3B, FUN=function(x)vector_keywords(x,keys));   length(Dword3);   head(Dword3[[1]])  #-- 1993
#         n          n          n          n         nz          n 
# "腰痠背痛"     "背痛"     "腰痛"     "問題"   "上班族" "腰痠背痛" 
Dword4 = lapply(Dword3, FUN=function(x)vector_keywords(x,keys));   length(Dword3);   head(Dword3[[1]])  #-- 1993
#         n          n          n          n         nz          n 
# "腰痠背痛"     "背痛"     "腰痛"     "問題"   "上班族" "腰痠背痛" 

#####===== (B4) (KDD4) (use B7 method) 以Seg$title找Dword2B ** ,求dtm,作文本聚類(B)求dtm矩陣 (Seg$title-->Dword2B-->dtm-->group) (GOOD!!!) =====#####
tokens = Dword4
length(tokens);   length(tokens[[1]]);   head(tokens[[1]])  #-- 2214,  5,     
# 23.4784  23.4784  13.9007  11.7392  11.7392 
# "年輕人"   "專長"   "科系"   "區間"   "大學" 
it = itoken(tokens, progressbar = FALSE)  #-- 可在此處加上參數 preprocessor = tolower 可以轉換成小寫，也可以在此處自行設置 tokenizer 
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 3L);   dim(vocab);   head(vocab,3)   #-- 662  3
#    term term_count doc_count
# 1:  cal          3         3
# 2: 核心          3         3
# 3: 傳統          3         3
vectorizer = vocab_vectorizer(vocab)
dtm = create_dtm(it, vectorizer);   dim(dtm);    #-- [1] 2214  756
DTM = as.matrix(dtm);   rownames(DTM) = RRtitle;    DTM[1:2,325:350]
#                                         副總 加盟 口頭 台北 台塑 品質 唸書 商品 四中 培訓 外包 多少錢 夢想 女同事 學分 客服 家族企業 家業 年終獎金 店家 廚房 廠區 廣告 想學 意願 房租
# 20歲到30歲沒專長的年輕人都在做甚麼工作    0    0    0    0    0    0    0    0    0    0    0      0    0      0    0    0        0    0        0    0    0    0    0    0    0    0
# 【問題】［保全］哪種固定日班類型比較好    0    0    0    0    0    0    0    0    0    0    0      0    0      0    0    0        0    0        0    0    0    0    0    0    0    0

#####===== (B5) (KDD4) (use B7 method) 以Seg$title找Dword2B ** ,求dtm,作文本聚類(C)求聚類group (Seg$title-->Dword2B-->dtm-->group) (GOOD!!!) =====#####
fit_hc = hclust(dist(as.matrix(dtm)), method="ward.D2" )
for (kk in 20:95) { group = cutree(fit_hc, k=kk);   print(table(group)) };  
Ncls=86;   group = cutree(fit_hc, k=Ncls);   print(table(group))  
#  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44 
# 17  10  29 397  14  24  40  27  19   9  39  20  15  37  16  32   8   6  12  17  13  14  27  46  16   6   8  25  24  27  21  21  10  32  19  17  16   9  15  15  29  24  13  23 
# 45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86 
# 20  15  18  10  15  10  35  24  10   7  39  18   9  14  14  17  11  21  15  29  40   6  17  10  26  11  10  13  27  21  11  24  23  10  17  20  10  24  11  22  22   9 


########## (C).文本分析中的知識架構:單類別知識分析 ##########

#####===== (C1) (KDD1) 類別選擇 (DTM--(選第1類)-->DTM1--(化簡)->DTM2)) =====#####
DTM1 = DTM[which(group %in% c(1)),];   dim(DTM1)   #-- [1]  17 682
length(which(colSums(as.matrix(DTM1))>0))    #-- [1] 13
DTM2 = DTM1[,which(colSums(as.matrix(DTM1))>1)];   dim(DTM2)   #-- [1] 17  4
colSums(DTM2)   #-- river (3)  鹿港 (3)  釣魚 (8)  youtu (17) 

#####===== (C2) (KDD3) 數據轉換 (matrix DTM2-->list L2-->set S2) =====#####
library(kst)
L2 = list();   for (i in 1:dim(DTM2)[1]) { L2[[i]] = as.set(colnames(DTM2)[which(DTM2[i,]>0)]) };   L2
S2 = as.set(unique(L2));  S2   #-- {{"youtu"}, {"river", "youtu"}, {"youtu", "釣魚"}, {"youtu", "鹿港", "釣魚"}}

#####===== (C3) (KDD4) 數據模型 (S2-->kst2-->ksp2) =====#####
kst2 = kstructure(S2);   kst2   #-- {{}, {"youtu"}, {"river", "youtu"}, {"youtu", "釣魚"}, {"youtu", "鹿港", "釣魚"}, {"river", "youtu", "鹿港", "釣魚"}}
kdomain(kst2)   #-- {"river", "youtu", "鹿港", "釣魚"}
ksp2 = kspace(kst2);   ksp2     #-- {{}, {"youtu"}, {"river", "youtu"}, {"youtu", "釣魚"}, {"river", "youtu", "釣魚"}, {"youtu", "鹿港", "釣魚"}, {"river", "youtu", "鹿港", "釣魚"}}
katoms(kst2, items=kdomain(kst2))
# $river {{"river", "youtu"}}
# $youtu {{"youtu"}}
# $鹿港  {{"youtu", "鹿港", "釣魚"}}
# $釣魚  {{"youtu", "釣魚"}}

#####===== (C4) (KDD5) 知識結構的應用之一 (ksp2) =====#####
lpath(ksp2)
# [[1]] {{}, {"youtu"}, {"river", "youtu"}, {"river", "youtu", "釣魚"}, {"river", "youtu", "鹿港", "釣魚"}}
# [[2]] {{}, {"youtu"}, {"youtu", "釣魚"}, {"river", "youtu", "釣魚"}, {"river", "youtu", "鹿港", "釣魚"}}
# [[3]] {{}, {"youtu"}, {"youtu", "釣魚"}, {"youtu", "鹿港", "釣魚"}, {"river", "youtu", "鹿港", "釣魚"}}

#####===== (C5) (KDD5) 數據繪圖 (kst2-->ksp2) =====#####
plot(kst2)
plot(kspace(kst2))

##---> 這兩個指令，一跑 RStudio 就當機...  怎麼辦?   
##---> 圖形的繪製 ---> igraph
