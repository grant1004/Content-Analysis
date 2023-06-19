
wkDir = "E:\\AllRepo\\R_Crawler\\文本分析\\" ;   setwd(wkDir);
dataDir = wkDir ;
library(jiebaR);     library(xlsx);    library(stringr);   library(text2vec);   library(data.table)

#####===== (1) (KDD1) 讀取數據 (RR....csv-->RR) =====#####
library(data.table)

RR = read.csv('new.csv', header = TRUE, sep = ',', fileEncoding = "utf-8" ) ; head( RR )

library(dplyr)
RR <- filter( RR, RR$sub.author != "" ); head( RR )

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

dim(RR);   head(RR,2)       #-- [1] 14959    11 
#   title      author                time gp bp
# 1 20歲到30歲沒專長的年輕人都在做甚麼工作 shueh201237 2022-07-30 04:18:52 12  -
# 2 20歲到30歲沒專長的年輕人都在做甚麼工作 shueh201237 2022-07-30 04:18:52 12  -
#   mainText
# 1 每年那麼多大學畢業生一些很專業的工作缺沒有那麼多也不是所有人都做相關科系的工作比較不需要門檻的像是很多工廠 工地也幾乎都沒有年輕人餐飲業也是店員幾乎都是老人的趨勢想問看看這種年紀區間沒特殊專長的人可以分享都在做甚麼樣的工作嗎
# 2 每年那麼多大學畢業生一些很專業的工作缺沒有那麼多也不是所有人都做相關科系的工作比較不需要門檻的像是很多工廠 工地也幾乎都沒有年輕人餐飲業也是店員幾乎都是老人的趨勢想問看看這種年紀區間沒特殊專長的人可以分享都在做甚麼樣的工作嗎
# sub.author            sub.time sub.gp sub.bp                                                                                           sub.Text
# 1  dh5987462 2022-07-30 04:50:36     20      - 輪班囉設備工程、產品工程、技術員。學校不會教你組裝業界機台、看良率分析啦！大多數文憑只是張面試門票
# 2  qbgeneral 2022-07-30 06:50:47     18      -         就大叔我的觀察有幾種外送員貨運電子廠輪班作業員餐飲超商派遣直銷賣車房仲藍藍路差不多就這樣了                                                


#####===== (2) (KDD3) 數據轉換 (RR$title/RR$text-->content/Ncontent) =====##### ---> 只就 RR$title 關鍵字太少，難以分類

RRtitle = unique(RR$title);   length(RRtitle)   #-- (1) 共有2821個討論主題

content = NULL;   Ncontent = NULL            

for (title in RRtitle) {                                   #-- (2) 同一討論主題的回文內容(RR$text),以"\n\n"串接
  content = c(content, paste(title, paste(RR$sub.Text[which(RR$title==title)],collapse="\n\n"),collapse="\n\n"))
  Ncontent = c(Ncontent, length(which(RR$title==title)))   #-- (3) length(which(條件)): 表示滿足條件的個數
}

length(content);   head(content,2)     #-- [1] 2214        #-- (4) content:討論主題的回文內容(RR$text)集合,
# [1] "20歲到30歲沒專長的年輕人都在做甚麼工作 輪班囉設備工程、產品工程、技術員。學校不會教你組裝業界機台、看良率分析啦！大多數文憑只是張面試門票\n\n就大叔我的觀察有幾種外送員貨運電子廠輪班作業員餐飲超商派遣直銷賣車房仲藍藍路差不多就這樣了\n\n車手宮廟跳8+9保險房仲保全\n\n導遊 特助 保全直銷造勢\n\n我自己身邊的小學、國中同學跳8+9（家裡開宮廟）輪班操作技術員賣車保險房仲木工水電開店（生意不錯）里長（最近要參選）全職主婦（嫁得好的那種）免洗作業員、品保（大宗）服務業店員酒促小姐CNC技術員混得越好的人越不想讓人知道他做什麼做業務這幾年真的很差的樣子\n\n服務業、夜班保全爽滑\n\n輪班星人啊...有新鮮的肝就可以做了\n\n做蝦皮代購  月薪十萬\n\n做露天代購  月薪二十萬\n\n做自己，月入O萬\n\n在家養粉絲賺流量錢...\n\n船員\n\n志願役 退伍後 一間小間的科技公司 後來又回役了\n\n修手機\n\n輪班技術員\n\n仲介、服務生、作業員、工廠技師(焊接、CNC等)很多人28~3x說好聽是刷工作經驗，說難聽點一直換工作資料來源:面試這年齡層很容易看到光履歷就有一頁半到二頁的\n\n經營網路賣場賺仲介費自家有家業幫忙做\n\n我做文字編輯，還在想下一步要幹嘛\n\n保全吧，或者警衛做到退休"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
# [2] "【問題】［保全］哪種固定日班類型比較好 ※ 引述《qwer50123 (ㄚ肥)》之銘言> 目前y25做過傳產一段時間 > 發現又熱又累又危險錢也沒多少 > 目前想找日班保全做一輩子 > 目前有科技業保全 > 豪宅保全 > 社區保全 > 工廠保全 > 請問哪個比較推？ 做一輩子，好像想了太遠了些，目前退休年限65歲，也就是還得經歷40年。保全基本上還是「看點」，商辦的夜班保全基本上都是睡得很開心，而且一般上班族也比較不會管保全，倘若是「社區保全」，一個社區來個幾戶腦子有病的住戶，整天都在找其他住戶、總幹事、保全的麻煩，自以為很厲害，碰到這種社區光上班就覺得煩。而「豪宅保全」除了一般門禁管制外，對於各住戶的習慣、喜好…等也要稍微知悉，搭配物管秘書一起合作服務住戶，初期踏入需要學習很多，薪水也是頗高的，而且三節、過年、宴會服務…等，住戶多少都會給紅包，不過「薪水、額外」多就表示「很容易畢業」倘若可以的話，去豪宅學習一下，再去一般的案場會覺得輕鬆很多，不過薪水最起碼少一萬以上，至於我說的豪宅，是北部一坪至少100萬的那種，室內坪數至少100坪起跳，有些高級住宅往往自抬身價也說是豪宅XDDD \n\n科技業保全豪宅保全這兩個比較硬，錢比較多，要求也比較多有的要求要軍警退休...這方面背景的門面(長相)也會有一定要求長期走動+緊迫盯人，安檢沒過可能就要被HL或者掰掰社區大樓保全工廠保全這兩個看點只能多找多看有哪個點適合你了\n\n每個保全類型都有其特定的工作環境和任務，無法給予明確的推薦。您需要先考慮自己的興趣和優勢，然後了解每種保全類型的工作內容，再考慮哪一個適合您。以下是各種保全類型的一些相關資訊：科技業保全：需要具備一定的技術知識和對資訊安全的了解，能夠保障公司設施的安全和穩定運作。豪宅保全：需要有良好的服務態度和溝通能力，能夠照顧居民的需求和維護豪宅的安全管理。社區保全：需要熟悉社區的各種設施和活動，能夠保持社區的安寧和維護社區秩序。工廠保全：需要具備安全知識和急救技能，能夠預防事故和處理突發事件。最終，選擇哪種保全類型需要評估您自己的能力和興趣，請依照自己個人情況作出選擇。\n\n還不如去找台塑企業自聘的警衛雖然要排班輪班但是一天也就8小時一個月幾天紅就休幾天我堂弟做9年，111年扣繳憑單97萬，加一堆禮券福利年薪超過100萬據說他有個老同事140幾萬但是沒什麼時間摸魚工作很精實\n\n去工地工廠混一段時間休息一下還可以，等你實際去當保全一段時間後，才能確定你是不是願意做這垃圾工作一輩子每天12小時，時薪低沒福利休假又少，同事基本上都是底層邏輯思維，住戶把你當狗在看……做久了真的會與社會脫節你還年輕建議再考慮，當兵都比當保全好，保全等當完兵四五十歲再做都來得及"

Ncontent[1:10]   #-- [1] 19  5  1  2  1  5  3  5 19 19     #-- (5) Ncontent: 每個討論主題的回文數

#####===== (3) (KDD4) (use B7 method) 以Seg$title找Dword2B ** ,求dtm,作文本聚類(A)取關鍵n詞 (Seg$title-->Dword2-->dtm-->group) (GOOD!!!) =====#####
removeKW <- function(KWlist,KWvec) { 
  OUTlist = list()
  for (kl in 1:length(KWlist)) {
    ind = NULL
    for (k in 1:length(KWvec)) 
    {
      ind = c( ind, as.vector(which(KWvec[k]==KWlist[[kl]])) )
    }
    OUTlist[[kl]] = KWlist[[kl]][setdiff( (1:length(KWlist[[kl]])), ind )]
  }
  return(OUTlist)
}

cutter = worker( stop_word = "stop.txt", type="tag" )   #-- 可以用 byliness=TRUE, 就可以轉成 list()
Dword = sapply(content, function(x)segment(tolower(as.character(x)),cutter));   length(Dword);   head(Dword[[1]],15)  #-- [1] 2821
# m       zg        v        m       zg        v        n       uj        n        d        p        v        r       vn        n 
# "20"     "歲"     "到"     "30"     "歲"     "沒"   "專長"     "的" "年輕人"     "都"     "在"     "做"   "甚麼"   "工作"   "輪班" 

nDword = lapply(Dword,nchar);    head(nDword[[1]],15)
# m zg  v  m zg  v  n uj  n  d  p  v  r vn  n 
# 2  1  1  2  1  1  2  1  3  1  1  1  2  2  2

Dword2B = lapply(Dword, function(x){  ## 刪除字數小於2的單字
                          nx=sapply(x,nchar); 
                          xA=x[nx>=2]; 
                          return(xA[setdiff((1:length(x)),grep("[[:digit:]]+",xA))])
                        } );   length(Dword2B);   head(Dword2B[[1]],15) 
# [1] 2821
# n        n        r       vn        n       vn        n        n        n        n        n        v        v        n        x 
# "專長" "年輕人"   "甚麼"   "工作"   "輪班"   "設備"   "工程"   "產品"   "工程" "技術員"   "學校"   "不會"   "組裝"   "業界"   "機台" 


## Dword2 -> Dword2B 名詞
Dword2 = lapply(Dword2B, function(xB) return(xB[grep(c("n","r"),names(xB))]));   length(Dword2);   Dword2[1:3]

## Dword3 -> "Dword2" 刪除 keyword : ( "https","http","www","tw","com","htm","wrote","vs","運動","如題","樓主","有點","謝謝","小弟","前輩","個人","我會","問題","時間","感覺","建議","時候","活動","東西","習慣" ) 
Dword3 = removeKW(Dword2, c("https","http","www","tw","com","htm","wrote","vs","運動","如題","樓主","有點","謝謝","小弟","前輩","個人","我會","問題","時間","感覺","建議","時候","活動","東西","習慣"))

## Dword3B -> "Dword2B" 刪除 keyword : ( "https","http","www","tw","com","htm","wrote","vs","運動","如題","樓主","有點","謝謝","小弟","前輩","個人","我會","問題","時間","感覺","建議","時候","活動","東西","習慣" ) 
Dword3B = removeKW(Dword2B, c("https","http","www","tw","com","htm","wrote","vs","運動","如題","樓主","有點","謝謝","小弟","前輩","個人","我會","問題","時間","感覺","建議","時候","活動","東西","習慣"))

keys = worker("keywords",topn=5)   #-- 上述應該可以用 stop_word=c("XX","XX",...) 來進行
Dword4B = lapply(Dword3B, FUN=function(x)vector_keywords(x,keys));   length(Dword4B);   head(Dword4B[[1]])  #-- 1993
Dword4 = lapply(Dword3, FUN=function(x)vector_keywords(x,keys));   length(Dword4);   head(Dword4[[1]])  #-- 1993


##########===== (B).詞語向量(vector_keywords())到文件詞語矩陣(dtm) =====##########

#####===== (B1) (KDD4) (use B7 method) 以Seg$title找Dword2B ** ,求dtm,作文本聚類(B)求dtm矩陣 (Seg$title-->Dword2B-->dtm-->group) (GOOD!!!) =====#####
tokens = Filter(function(x) !any( x == "NA"), Dword4)
length(tokens);   length(tokens[[1]]);   head(tokens[[1]])   
# [1] 2821
# [1] 5
# 58.696  46.9568  35.2176  23.4784  23.4784 
# "輪班" "技術員" "作業員"   "保險"    "cnc"
it = itoken(tokens, progressbar = FALSE)  #-- 可在此處加上參數 preprocessor = tolower 可以轉換成小寫，也可以在此處自行設置 tokenizer 
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 3L);   dim(vocab);   head(vocab,-1)   #-- [1] 787   3
# term term_count doc_count
# 1:     ae          3         3
# 2:    bsn          3         3
# 3: coding          3         3

vectorizer = vocab_vectorizer(vocab)
dtm = create_dtm(it, vectorizer);   dim(dtm);    #-- [1] 2821  789

#####===== (B2) (KDD4) (use B7 method) 以Seg$title找Dword2B ** ,求dtm,作文本聚類(C)求聚類group (Seg$title-->Dword2B-->dtm-->group) (GOOD!!!) =====#####

#####===== (C1) (KDD4) 文件的關鍵字向量化 到 文件距離(dist) =====#####
dtm[60:70,150:200]
# 60 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# 61 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# 62 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# 63 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# 64 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# 65 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 1
# 66 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# 67 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# 68 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# 69 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# 70 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 1 . . . . . . . .
as.matrix(dtm[60:70,150:200])
# 臨時工 蓋章 藉口 裁員 補貨 製造業 西點 規則 規模 觀念 訂單 計程車 記帳 論文 諮詢 證件 議題 變化 豆花 負債 貨櫃 車子 車輛 輪機 農業 連鎖 運輸 違規 邏輯 配電 酒店
# 60      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 61      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 62      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 63      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 64      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 65      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 66      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 67      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 68      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 69      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 70      0    0    0    0    0      0    0    0    0    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0
# 醫師 醫生 金融業 銷售 錄音 除草 階段 集團 電信 額度 顧客 馬達 體驗 高鐵 點數 eat forum gov qq rd
# 60    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 61    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 62    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 63    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 64    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 65    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  1
# 66    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 67    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 68    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 69    0    0      0    0    0    0    0    0    0    0    0    0    0    0    0   0     0   0  0  0
# 70    0    0      0    0    0    0    0    0    0    0    0    1    0    0    0   0     0   0  0  0
dist(as.matrix(dtm[60:70,150:200]))
# 60       61       62       63       64       65       66       67       68       69
# 61 0.000000                                                                                 
# 62 0.000000 0.000000                                                                        
# 63 0.000000 0.000000 0.000000                                                               
# 64 0.000000 0.000000 0.000000 0.000000                                                      
# 65 1.000000 1.000000 1.000000 1.000000 1.000000                                             
# 66 0.000000 0.000000 0.000000 0.000000 0.000000 1.000000                                    
# 67 0.000000 0.000000 0.000000 0.000000 0.000000 1.000000 0.000000                           
# 68 0.000000 0.000000 0.000000 0.000000 0.000000 1.000000 0.000000 0.000000                  
# 69 0.000000 0.000000 0.000000 0.000000 0.000000 1.000000 0.000000 0.000000 0.000000         
# 70 1.000000 1.000000 1.000000 1.000000 1.000000 1.414214 1.000000 1.000000 1.000000 1.000000

## fit_hc = hclust(dist(as(  dtm, "CsparseMatrix")), method="ward.D2" )
fit_hc = hclust(dist(as.matrix(dtm)), method="ward.D2" ) 
for (kk in 20:150) { group = cutree(fit_hc, k=kk);   print(table(group)) };  
Ncls=86;   group = cutree(fit_hc, k=Ncls);   print(table(group))  
# group
# 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28 
# 12  20  19  32  47  10  48  17  62  49   8  15  25  53  14 376  56  18  33  19  28  22  17  18  19  12  31  16 

# 29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56 
# 25  10  38  35  18  18  19  29  18  24   7  24  29  27  21  24  21  21  29  17  26  13  29   9  20  14  19  14 

# 57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84 
# 24  17  13  50  16  29  23  10  10  20  17  12  12  16  22  16  35   8  18  17   9  17  11   6  28  22   5  14 

# 85  86 
# 8  11 
RRtitle[group==11]    
# [1] "【討論】最近正考慮換工作"                                        
# [2] "【問題】禮拜五離職，離職日壓禮拜天會不會很哭?"                   
# [3] "【問題】請教有無認識或正在東南亞做博弈類工作的朋友（想了解現況）"
# [4] "【心得】營造業工作3年的看法(抱怨)"                               
# [5] "【問題】偉康科技這間公司如何？"                                  
# [6] "【問題】請問有人可以推薦一下好用的2D工業設計繪圖軟體嗎？"        
# [7] "【心得】台灣固定薪資被越南超車！"                                
# [8] "【情報】熊貓、Uber Eats 4月全台齊大改制 外送員哀嚎質疑變相減薪"  

#####===== (B3) (KDD4) 文本關鍵字分類數據框 (dtm/group-->KWgroup.df$k,KW,count,nContent,ind) =====##### (5/14 end???)
DTM = as.matrix(dtm)
KWgroup = NULL
for (k in 1:Ncls) {   #-- k = 63
  ind = as.vector(which(group==k))   #--  [1]  112  242  332  399  534  931 1558
  nContent = sum(Ncontent[ind])
  if (length(ind)==1) { colDTM0 = DTM[ind,] } else { colDTM0 = colSums(DTM[ind,]) };  colDTM0
  colDTM = colDTM0[which(colDTM0>0)];   colDTM
  sortDTM = colDTM[order(colDTM,decreasing=TRUE)];   sortDTM[1:20]

  max2 = as.integer(sortDTM[2]);  selDTM = sortDTM[which(sortDTM>=max2)];   selDTM

  KWgroup = rbind( KWgroup, c(k, paste(names(selDTM),collapse="/"), length(ind), nContent, paste(ind,collapse="/")) )
}
KWgroup.df = as.data.frame(KWgroup);   colnames(KWgroup.df) = c("k","KW","count","nContent","ind")

dim(KWgroup.df);   KWgroup.df[c(22,37,45),]   #-- [1] 148   5
# k          KW count nContent                                                                                                 ind
# 22 22   興趣/薪水    22      129 30/86/201/338/584/701/732/739/1073/1084/1115/1269/1500/1561/1653/1687/1874/1923/1946/2085/2136/2208
# 37 37 技術/工程師    18       91                          54/62/295/515/538/543/578/612/646/821/853/856/896/1140/1727/1860/2018/2049
# 45 45   內場/辦法    21      173          69/79/117/356/445/579/654/671/757/834/965/969/1018/1149/1357/1407/1501/1560/1940/2017/2163                                                                                                                                                                                                                                                                                                                                                 

paste0(KWgroup.df$KW,"(",KWgroup.df$count,"/",KWgroup.df$nContent,")")
write.xlsx(KWgroup.df,"./KWgroup1.xlsx")

#####===== (B4) (KDD4) 文本關鍵字分類數據框之大分類 (KWgroup.df$KW-->$KWkind) =====#####
kwKind = list()
kwKind$"老闆" = c("老闆","機車","主管","政府","業主","廠商","店長","客戶","朋友")
kwKind$"同事" = c("老鳥","新人","朋友","學長","人員","同事","機車","員工","人員","對方","部門")
kwKind$"薪水工時" = c("彈性","業績","價值","全額","薪水","天數","年資","月薪","夜班","薪資","加班費","輪班","日班","全職","行業")
kwKind$"請假休假" = c("事假","天數","小時","mueller","tomtom","puma","nike")
kwKind$"員工福利" = c("保險","投保","國民","年金","合約","契約","勞保","規定","勞基法","勞工","失業","獎金","福利")
kwKind$"工作機會" = c("公職","人生","待業","offer","創業","跳槽","運氣","機會","工作","大運","大學","內容","賽","球員","網站")
kwKind$"工作環境" = c("內場","單位","餐廳","中國","社區","業界","市場","疫苗","職場","環境","壓力","企業","發展","機構","國營","醫院","工廠","辦公室","加油站","疫情","日本","現場","銀行")
kwKind$"器材設備" = c("cnc","軟體","裝潢","電腦","手機","設備","檢查","系統","品管")
kwKind$"面試" = c("面試","面試官","課程","會員","次數","姿勢","強度","感覺")
kwKind$"學校科系" = c("學歷","老師","學生","學校","科系","老師","碩士","大學","畢業","感覺","興趣","經歷","讀書","當兵")
kwKind$"證照專長" = c("堆高機","程式","證照","乙級","課程","專長","考試")
# 公職/產業/行業/職業
kwKind$"職業" = c("清潔隊","職業","產業","遊戲","公務員","船員","郵差","計程車","設計師","助理","外勤","職業","作業員","行銷","企劃","工程師",
                  "司機","業務","uber","eat","技師","電機","熊貓","工程師","餐飲","line")
kwKind$"語文" = c("英文","基礎","語言","日文" )
kwKind$"學徒" = c("經驗","師傅","基礎","學徒","年輕人" )
kwKind$"健康" = c("身體","狀況" )
kwKind$"程式語言" = c("java","php", "game" )
kwKind$"投資" = c("投資","股票", "保險","情況","規劃" )

length(kwKind)  #-- 17
KWgroup.df$KWkind = ""
for (k in 1:length(kwKind)) {
  for (j in 1:length(kwKind[[k]])) 
    KWgroup.df$KWkind[ grep(kwKind[[k]][j],KWgroup.df$KW) ] = names(kwKind)[k]
}
KWgroup.df$KWkind

KWgroup.df[c(3,15,17,20,26:27),]
# k             KW count nContent                                                                                                         ind   KWkind
# 3   3      主管/員工    24      227  3/30/33/154/382/604/636/640/863/1171/1210/1221/1265/1305/1847/1897/2075/2308/2362/2547/2627/2707/2765/2795     同事
# 15 15      部門/主管    10       79                                                                 19/141/315/621/895/1167/1331/1539/1777/2122     同事
# 17 17 待業/帳號/文章    14       80                                            21/218/431/803/1473/1543/1831/1843/1922/2087/2140/2242/2298/2672 工作機會
# 20 20      客戶/老闆    21      116                26/271/391/441/443/493/602/807/822/852/855/1010/1243/1431/1452/1820/2171/2240/2292/2414/2440     老闆
# 26 26      興趣/工作    24      171 36/64/246/419/426/628/867/907/915/982/1238/1407/1743/1828/1908/1932/1942/2105/2154/2243/2389/2459/2486/2818 學校科系
# 27 27      薪水/老闆    16      169                                   37/139/716/874/943/1118/1550/1683/1952/1954/2041/2190/2320/2445/2625/2772 薪水工時

write.xlsx(KWgroup.df,"KWgroup2.xlsx")



########## (C).文本分析中的知識架構:單類別知識分析 ##########

#####===== (C1) (KDD1) 類別選擇 (DTM--(選第1類)-->DTM1--(化簡)->DTM2)) =====#####
DTM1 = DTM[which(group %in% c(6)),];   dim(DTM1)   #-- [1]  17 798
length(which(colSums(as.matrix(DTM1))>0))    #-- [1] 32
DTM2 = DTM1[,which(colSums(as.matrix(DTM1))>1)];   dim(DTM2)   #-- [1] 17  8
colSums(DTM2)     
# 職員 副業 資訊 國營 考試 單位 薪水 
# 3    2    2   10    5    5    5 

#####===== (C2) (KDD3) 數據轉換 (matrix DTM2-->list L2-->set S2) =====#####
library(kst)
L2 = list();   for (i in 1:dim(DTM2)[1]) { L2[[i]] = as.set(colnames(DTM2)[which(DTM2[i,]>0)]) };   L2

S2 = as.set(unique(L2));  S2  
# {{"國營", "薪水"}, {"副業", "國營", "薪水"}, {"單位", "國營", "考試"}, {"國營", "考試", "薪水"}, {"單位",
#   "國營", "考試", "職員"}, {"單位", "國營", "職員", "資訊"}}

#####===== (C3) (KDD4) 數據模型 (S2-->kst2-->ksp2) =====#####
kst2 = kstructure(S2);   kst2   
# { {}, 
#   {"國營", "薪水"}, 
#   {"副業", "國營", "薪水"}, 
#   {"單位", "國營", "考試"}, 
#   {"國營", "考試", "薪水"},
#   {"單位", "國營", "考試", "職員"}, 
#   {"單位", "國營", "職員", "資訊"}, 
#   <<set(7)>>
# }

kdomain(kst2)   #-- {"副業", "單位", "國營", "考試", "職員", "薪水", "資訊"}
ksp2 = kspace(kst2);   ksp2     
# {{}, {"國營", "薪水"}, {"副業", "國營", "薪水"}, {"單位", "國營", "考試"}, {"國營", "考試", "薪水"},
#   {"副業", "國營", "考試", "薪水"}, {"單位", "國營", "考試", "職員"}, {"單位", "國營", "考試", "薪水"},
#   {"單位", "國營", "職員", "資訊"}, {"副業", "單位", "國營", "考試", "薪水"}, {"單位", "國營", "考試",
#     "職員", "薪水"}, {"單位", "國營", "考試", "職員", "資訊"}, {"單位", "國營", "職員", "薪水", "資訊"},
#   <<set(6)>>, <<set(6)>>, <<set(6)>>, <<set(7)>>}

katoms(kst2, items=kdomain(kst2))
# $副業
# {{"副業", "國營", "薪水"}}
# $單位
# {{"單位", "國營", "考試"}, {"單位", "國營", "職員", "資訊"}}
# $國營
# {{"國營", "薪水"}, {"單位", "國營", "考試"}, {"單位", "國營", "職員", "資訊"}}
# $考試
# {{"單位", "國營", "考試"}, {"國營", "考試", "薪水"}}
# $職員
# {{"單位", "國營", "考試", "職員"}, {"單位", "國營", "職員", "資訊"}}
# $薪水
# {{"國營", "薪水"}}
# $資訊
# {{"單位", "國營", "職員", "資訊"}}


#####===== (C4) (KDD5) 知識結構的應用之一 (ksp2) =====#####
lpath(ksp2)

kst2_relation = as.relation_ensemble( ksp2 ); plot( kst2_relation ) 
  # { {}, {"國營", "薪水"}, {"副業", "國營", "薪水"}, {"單位", "國營", "考試"}, {"國營", "考試", "薪水"},
  #   {"副業", "國營", "考試", "薪水"}, {"單位", "國營", "考試", "職員"}, {"單位", "國營", "考試", "薪水"},
  #   {"單位", "國營", "職員", "資訊"}, {"副業", "單位", "國營", "考試", "薪水"}, {"單位", "國營", "考試",
  #     "職員", "薪水"}, {"單位", "國營", "考試", "職員", "資訊"}, {"單位", "國營", "職員", "薪水", "資訊"},
  #   <<set(6)>>, <<set(6)>>, <<set(6)>>, <<set(7)>>
  # }

gg = make_graph( edges = c( c("國營"),
                            c("國營", "薪水"),
                            c("副業", "國營", "薪水"),
                            c("單位", "國營", "考試"),
                            c("國營", "考試", "薪水"),
                            c("副業", "國營", "考試", "薪水"),
                            c("單位", "國營", "考試", "職員"),
                            c("單位", "國營", "考試", "薪水"),
                            c("單位", "國營", "職員", "資訊"),
                            c("副業", "單位", "國營", "考試", "薪水"),
                            c("單位", "國營", "考試", "職員", "薪水")
                            # c("單位", "國營", "考試", "職員", "資訊")
                            # c("單位", "國營", "職員", "薪水", "資訊"),
                            # c("副業", "單位", "國營", "考試", "薪水", "職員"),
                            # c("副業", "單位", "國營", "考試", "職員", "薪水"),
                            # c("副業", "單位", "國營", "考試", "職員", "資訊"),
                            # c("副業", "單位", "國營", "職員", "薪水", "資訊")
                            ) )

gg = make_graph( edges = c(
                            c("國營", "薪水"),
                            c("副業", "國營", "薪水"),
                            c("單位", "國營", "考試"),
                            c("國營", "考試", "薪水"),
                            c("單位", "國營", "考試", "職員"),
                            c("單位", "國營", "職員", "資訊"),
                            c("單位", "國營", "考試", "職員", "資訊")
                          ) )

#   {}, 
#   {"國營", "薪水"}, 
#   {"副業", "國營", "薪水"}, 
#   {"單位", "國營", "考試"}, 
#   {"國營", "考試", "薪水"},
#   {"單位", "國營", "考試", "職員"}, 
#   {"單位", "國營", "職員", "資訊"}, 
#   <<set(7)>>
plot( gg )
A10 <- get.adjacency(gg, sparse=FALSE);   gg10 <- network::as.network.matrix(A10) ; gg10
cores10 = coreness(gg);  cores10
sna::gplot.target( main="中心度圖", gg10, cores10, circ.lab=FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=cores10, edge.col="darkgray", displaylabels = TRUE )

#####===== (C5) (KDD5) 數據繪圖 (kst2-->ksp2) =====#####
plot(kst2, main = "知識結構 group = 6 " )
plot(ksp2, main = "知識空間 group = 6 " )

##---> 這兩個指令，一跑 RStudio 就當機...  怎麼辦?   
##---> 圖形的繪製 ---> igraph

##== (2) k核(k-core): 節點度為k的最大子圖 (節點度至少為 k, 且不包含於其他子圖中)
##                    the largest subgraph such that each vertex is connected to at least k others in subset
##=- (2A) 一個簡單易懂的例子
library(sna)
# g10 = add_edges(g10, c(1,3, 1,4, 2,4, 1,5, 4,10, 2,5, 3,5, 2,10, 6,8, 7,10));   
g10 = make_ring(10);  g10 = add_edges(g10, c(1,3, 1,4, 2,4, 1,5, 4,10, 2,5, 3,5, 2,10, 6,8, 7,10));   
par(mfrow=c(1,2));     plot(g10)
A10 <- get.adjacency(g10, sparse=FALSE);   gg10 <- network::as.network.matrix(A10)
plot(gg10, displaylabels = TRUE, label.col = "black", label.cex = 0.8, vertex.col = cores10, edge.col = "darkgray")
cores10 = coreness(g10);  cores10   #--  [1] 4 4 4 4 4 2 2 2 2 3
sna::gplot.target(gg10, cores10, circ.lab=FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=cores10, edge.col="darkgray", displaylabels = TRUE )
##=- (2B) 空手道網絡的例子
A = get.adjacency(karate, sparse=FALSE);      g = network::as.network.matrix(A);   plot(karate)
cores = graph.coreness(karate);   cores[order(cores,decreasing=T)]
# Mr Hi  Actor 2  Actor 3  Actor 4  Actor 8  Actor 9 Actor 14 Actor 31 Actor 33   John A  Actor 5  Actor 6  Actor 7 Actor 11 Actor 20 Actor 24 Actor 25 
#     4        4        4        4        4        4        4        4        4        4        3        3        3        3        3        3        3 
# Actor 26 Actor 28 Actor 29 Actor 30 Actor 32 Actor 10 Actor 13 Actor 15 Actor 16 Actor 17 Actor 18 Actor 19 Actor 21 Actor 22 Actor 23 Actor 27 Actor 12 
#        3        3        3        3        3        2        2        2        2        2        2        2        2        2        2        2        1 
sna::gplot.target(g, cores, circ.lab=FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=cores, edge.col="darkgray")
par(mfrow=c(1,1))
### detach("package:network");   detach("package:sna")


#Generate a random graph
g<-rgraph(15)

#Produce a target diagram, centering by betweenness
gplot.target(g,betweenness(g))


