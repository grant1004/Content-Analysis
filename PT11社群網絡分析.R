# PT11社群網絡分析.R: R program for Social Network Analysis
# Jia-Sheng Heh, 06/03/2022, revised from S907.R/S908.R/S909.R/S910.R
# Ref:  Chap. 4, 網絡圖特徵的描述性分析 of 
#      Eric D. Kolaczyk and Gabor Csardi. (2014). Statistical Analysis of Network Data with R. NY: Springer

wkDir = "E:\\AllRepo\\R_Crawler\\文本分析\\";   setwd(wkDir);
dataDir = wkDir;
# install.packages("igraph") #-- needs only to be done once
library(igraph)  #-- needs to be called for every new session
library(data.table);    


########## <A>.圖形的度量 ##########

#####===== 4.1 概述：網絡圖的結構分析 =====#####

#####===== 4.2 節點和邊的特徵 (Vertex and Edge Characteristics) =====#####
#== graph G(V,E)
#--   node/vertex node set V = {vi}: symbol as points, circles and rectangles

##======== 4.2.1 節點度(degree) ========##
#== (1) 節點度(degree) d_v = degree(v): 與 v 相連的 邊的數量
library(sand);   data(karate);   par(mfrow=c(2,1))
hist(degree(karate), col="lightblue", xlim=c(0,50), xlab="Vertex Degree", ylab="Frequency", main="")  #-- 度分布(degree distribution)

#== (2) 節點強度(strength) : 與 v 相連的 邊的權重之和
hist(graph.strength(karate), col="pink", xlab="Vertex Strength", ylab="Frequency", main="") #-- vertex strength distribution

#----   節點強度的範圍 要遠大於 節點度
#=- (3) 另一個例子：酵母菌蛋白質相互作用的網路
library(igraphdata);   data(yeast);   yeast   #-- (3A)數據樣貌
# IGRAPH 65c41bb UN-- 2617 11855 -- Yeast protein interactions, von Mering et al.
# + attr: name (g/c), Citation (g/c), Author (g/c), URL (g/c), Classes (g/x), name (v/c), Class (v/c), Description (v/c), Confidence (e/c)
# + edges from 65c41bb (vertex names):
#  [1] YLR197W--YDL014W YOR039W--YOR061W YDR473C--YPR178W YOR332W--YLR447C YER090W--YKL211C YDR394W--YGR232W YER021W--YPR108W YPR029C--YKL135C YIL106W--YGR092W
# [10] YKL166C--YIL033C YGL026C--YKL211C YOR061W--YGL019W YGL115W--YER027C YGL049C--YGR162W YDR394W--YOR117W YDL140C--YML010W YLR291C--YKR026C YGR158C--YDL111C
# + ... omitted several edges
ecount(yeast)   #-- [1] 11855
vcount(yeast)   #-- [1] 2617
d.yeast = degree(yeast);   head(d.yeast,15)   #-- (3B)節點度
# YLR197W YOR039W YDR473C YOR332W YER090W YDR394W YER021W YPR029C YIL106W YKL166C YGL026C YOR061W YGL115W YGL049C YDL140C 
#      40      19       9      13      21      37      21       5       6       2      15      23       5      24      43 
hist(d.yeast,col="blue", xlab="Degree", ylab="Frequency", main="Degree Distribution")           #-- (3C)節點度分布圖
#== (4) 度指數(degree exponent): 取對數後的頻率 隨著取對數後的度變化，呈現的下降趨勢(線性)
dd.yeast = degree.distribution(yeast);   length(dd.yeast);   head(dd.yeast,10)   #-- 119個元素  #-- (3D)節點度分布機率圖
# [1] 0.00000000 0.26518915 0.12877340 0.09247230 0.06763470 0.05502484 0.04241498 0.03668323 0.02483760 0.02292702
d = 1:max(d.yeast)-1;   ind = (dd.yeast!=0);   length(dd.yeast[ind]);   head(dd.yeast[ind],10)   #-- 79個非零元素
# [1] 0.26518915 0.12877340 0.09247230 0.06763470 0.05502484 0.04241498 0.03668323 0.02483760 0.02292702 0.02751242
plot(d[ind], dd.yeast[ind], log="xy", col="blue", xlab=c("Log-Degree"), ylab=c("Log-Intensity"), main="Log-Log Degree Distribution")
par(mfrow=c(1,1))
#==（5) 節點度／平均節點度 的關係：度值不同的節點，以何種方式彼此連接 ==> 高度值節點相互連接，低度值節點與 高/低度值節點 連接
a.nn.deg.yeast = graph.knn(yeast,V(yeast))$knn
plot(d.yeast, a.nn.deg.yeast, log="xy", col="goldenrod", xlab=c("Log Vertex Degree"), ylab=c("Log Average Neighbor Degree"))

##======== 4.2.2 節點中心度(centrality)：節點在網絡中的重要性 ========##
A = get.adjacency(karate, sparse=FALSE);   library(network);   library(sna);   A[1:3,1:15]
#         Mr Hi Actor 2 Actor 3 Actor 4 Actor 5 Actor 6 Actor 7 Actor 8 Actor 9 Actor 10 Actor 11 Actor 12 Actor 13 Actor 14 Actor 15
# Mr Hi       0       1       1       1       1       1       1       1       1        0        1        1        1        1        0
# Actor 2     1       0       1       1       0       0       0       1       0        0        0        0        0        1        0
# Actor 3     1       1       0       1       0       0       0       1       1        1        0        0        0        1        0
g = network::as.network.matrix(A);   par(mfrow=c(2,2))

##== (0) 節點度(degree): 與 v 相連的 邊的數量
sort(degree(g),decreasing=T)
# [1] 34 32 24 20 18 12 12 10 10 10  8  8  8  8  8  8  6  6  6  6  6  6  4  4  4  4  4  4  4  4  4  4  4  2
sna::gplot.target(g, degree(g), main="Degree", circ.lab=FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=c("blue",rep("red",32),"yellow"), edge.col="darkgray")
##== (1) 接近中心度(closeness centrality): 節點到其他所有節點距離之和 的倒數
sna::gplot.target(g, closeness(g), main="Closeness", circ.lab=FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=c("blue",rep("red",32),"yellow"), edge.col="darkgray")
sort(closeness(g),decreasing=T)
# [1]  0.5689655 0.5593220 0.5500000 0.5409836 0.5156250 0.5156250 0.5156250 0.5000000 0.4852941 0.4647887 0.4583333 0.4583333 0.4520548 0.4400000 0.4342105 0.3928571 0.3837209
# [18] 0.3837209 0.3837209 0.3793103 0.3793103 0.3750000 0.3750000 0.3750000 0.3750000 0.3707865 0.3707865 0.3707865 0.3707865 0.3707865 0.3707865 0.3666667 0.3626374 0.2844828
##== (2) 介數中心性(betweenness centrality): 通過節點之最短路徑數量 與最短路徑總數 比值之和
sna::gplot.target(g, betweenness(g), main="Betweenness", circ.lab=FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=c("blue",rep("red",32),"yellow"), edge.col="darkgray")
sort(betweenness(g),decreasing=T)
# [1]  462.1428571 321.1031746 153.3809524 151.7015873 146.0190476  59.0587302  56.9571429  48.4317460  34.2936508  31.6666667  31.6666667  23.5841270  18.6000000  15.2190476  12.5761905   4.0555556   3.0857143
# [18]   2.3333333   1.8952381   0.8952381   0.6666667   0.6666667   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000   0.0000000
##== (3) 特徵向量中心性(eigenvector centrality): 特徵向量元素的絕對值 之和 (不是 evcent(g)$vector )
sna::gplot.target(g, evcent(g), main="Evcent", circ.lab=FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=c("blue",rep("red",32),"yellow"), edge.col="darkgray")
sort(evcent(g),decreasing=T)
# [1]  0.37336347 0.35549144 0.31719250 0.30864422 0.26595992 0.22740391 0.22647272 0.21117972 0.19103384 0.17475830 0.17095975 0.15011857 0.14791251 0.13496082 0.13347715 0.13107782 0.10267425
# [18] 0.10140326 0.10140326 0.10140326 0.10140326 0.10140326 0.09239954 0.09239954 0.08425463 0.07948305 0.07948305 0.07596882 0.07596882 0.07557941 0.05920647 0.05705244 0.05285570 0.02363563
l = layout.kamada.kawai(aidsblog);   par(mfrow=c(1,2))
##== (4) 樞紐(hub)節點: 指向它的節點數量，為 A*A' 的特徵向量中心性 ==> 6個樞妞
plot(aidsblog, layout=l, main="Hubs", vertex.label="", vertex.size=10 * sqrt(hub.score(aidsblog)$vector), edge.arrow.size=0.1)
##== (5) 權威(authority)節點: 指向它的樞紐節點數量，為 A'*A 的特徵向量中心性 ==> 多數節點(含樞紐)起了權威的作用
plot(aidsblog, layout=l, main="Authorities", vertex.label="", vertex.size=10 * sqrt(authority.score(aidsblog)$vector), edge.arrow.size=0.1)
par(mfrow=c(1,1))

##########====> pause (5/29) ##########



##======== 4.2.3 邊的特徵 ========##

##== (1) 邊介數中心性: 通過它的最短路徑數量
eb = edge.betweenness(karate);   eb[order(eb,decreasing=T)][1:6]
# [1] 142.56667 110.53333  62.53333  58.83333  44.00000  44.00000
E(karate)[order(eb, decreasing=T)[1:6]]
# + 6/78 edges from 4b458a1 (vertex names):
# [1] Actor 20--John A    Mr Hi--Actor 20   Mr Hi--Actor 32   Actor 25--Actor 32    Mr Hi--Actor 6  Mr Hi--Actor 7 
#--> Actor 20 在 教練(Mr Hi) 和 主管(John A)間，維繫重要的信息流
##== (2) 線圖(line graph): G(V',E'): V'--原來的邊, E'--兩邊通過一個節點相連
LG.karate = line.graph(karate);   LG.karate
# IGRAPH 0cfd293 U--- 78 528 -- Line graph
# + attr: name (g/c)
# + edges from 0cfd293:
#  [1]  1-- 2  1-- 3  2-- 3  1-- 4  2-- 4  3-- 4  1-- 5  2-- 5  3-- 5  4-- 5  1-- 6  2-- 6  3-- 6  4-- 6  5-- 6  1-- 7  2-- 7
# [18]  3-- 7  4-- 7  5-- 7  6-- 7  1-- 8  2-- 8  3-- 8  4-- 8  5-- 8  6-- 8  7-- 8  1-- 9  2-- 9  3-- 9  4-- 9  5-- 9  6-- 9
# + ... omitted several edges

#####===== edges between entities (Eij: Ei->Ej) =====#####
##== (1) relationships/interaction
##== (2) correlation: co-occurrences -- 條件制約(CONDITIONING): Hebb's rule in neural network: d wij = alpha * Ei * Ej
##== (3) (directed) production rules: IF Ei THEN Ej ...... AI rule base: R1: if A1 then B1, R2: if A2 then B2, ...
##== (4) (directed, homogeneous) association: set Ei --> set Ej ...... data association
##== (5) (directed, heterogeneous) inference: condition Ei --> action Fj (bipartite) ...... decision tree



########## <B>.圖形的部件 ##########

#####===== 4.3 網絡的凝聚性特徵 (Characterizing Network Cohesion): 節點的子集(亞結構)與相應的邊，以何種程度聚合在一起 =====#####

##======== 4.3.1 子圖與普查(census): 團(cliques),k-核(core),二/三元組(dyad/triad),模體(motif) ========##

##== (1) 團(clique): 完全子圖，即完全凝聚的節點子集
clique.number(karate)  #-- [1] 5        #== (1A) 團數(clique number): 最大團尺寸
table(sapply(cliques(karate), length))  #== (1B) 團的普查(census)
#  1  2  3  4  5 
# 34 78 45 11  2
cliques(karate)[sapply(cliques(karate), length) == 5]   #== (1C) 某尺寸的團 cliques()
# [[1]]  [1]  1 2 3 4 8   [1] Mr Hi   Actor 2  Actor 3  Actor 4  Actor 8   maxCLQ1 #== (1D) 最大團(maximum cliques): 尺寸最大的團
# [[2]]  [1]  1 2 3 4 14  [1] Mr Hi   Actor 2  Actor 3  Actor 4  Actor 14  maxCLQ2
cliques(karate)[sapply(cliques(karate), length) == 4]
# [[1]]  [1] Mr Hi   Actor 2 Actor 3 Actor 4  < maxCLQ1,maxCLQ2
# [[2]]  [1] Actor 24 Actor 30 Actor 33 John A  ... (1E)
# [[3]]  [1] Actor 9  Actor 31 Actor 33 John A  ... (1E)
# [[4]]  [1] Actor 2  Actor 3  Actor 4  Actor 14  < maxCLQ2
# [[5]]  [1] Mr Hi    Actor 3  Actor 4  Actor 14  < maxCLQ2
# [[6]]  [1] Mr Hi    Actor 2  Actor 4  Actor 14  < maxCLQ2
# [[7]]  [1] Mr Hi    Actor 2  Actor 3  Actor 14  < maxCLQ2
# [[8]]  [1] Actor 2 Actor 3 Actor 4 Actor 8  < maxCLQ1
# [[9]]  [1] Mr Hi   Actor 3 Actor 4 Actor 8  < maxCLQ1
# [[10]] [1] Mr Hi   Actor 2 Actor 4 Actor 8  < maxCLQ1
# [[11]] [1] Mr Hi   Actor 2 Actor 3 Actor 8  < maxCLQ1
table(sapply(maximal.cliques(karate), length))  #== (1E) 極大團(maximal clique): 不被任何更大的團包含的團
#  2  3  4  5 -- 大小為5者，即是上述兩個
# 11 21  2  2
maximal.cliques(karate)[which(sapply(maximal.cliques(karate),length)==4)]
# [[1]]  [1] John A   Actor 24   Actor 30   Actor 33
# [[2]]  [1] John A   Actor 9    Actor 31   Actor 33 
clique.number(yeast)  #-- [1] 23

##== (2) k核(k-core): 節點度為k的最大子圖 (節點度至少為 k, 且不包含於其他子圖中)
##                    the largest subgraph such that each vertex is connected to at least k others in subset
##=- (2A) 一個簡單易懂的例子
library(sna)
# g10 = add_edges(g10, c(1,3, 1,4, 2,4, 1,5, 4,10, 2,5, 3,5, 2,10, 6,8, 7,10));   
g10 = make_ring(10);  g10 = add_edges(g10, c(1,3, 1,4, 2,4, 1,5, 4,10, 2,5, 3,5, 2,10, 6,8, 7,10));   
par(mfrow=c(1,2));     plot(g10)
A10 <- get.adjacency(g10, sparse=FALSE);   gg10 <- network::as.network.matrix(A10)
cores10 = coreness(g10);  cores10   #--  [1] 4 4 4 4 4 2 2 2 2 3
sna::gplot.target(gg10, cores10, circ.lab=FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=cores10, edge.col="darkgray")
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

##== (3) 二元組(dyad)與三元組(triad)
##== (3A) 二元組(dyad): 在有向圖中有三種可能狀態 -- 空(null,不存在有向邊), 非對稱(asymmetric,存在一條有向邊),雙向(mutual,兩條有向邊)
aidsblog = simplify(aidsblog)
dyad.census(get.adjacency(aidsblog,sparse=FALSE))  #-- 注意: 和書本上的不同
#      Mut Asym  Null
# [1,]   3  177 10405 #-- 有 173 + 3*2 = 183 個邊
dyad.census(get.adjacency(g10,sparse=FALSE)) 
#      Mut Asym Null
# [1,]  20    0   25  #-- 有 20 個邊
##== (3B) 三元組(triad): 在有向圖中有16種可能狀態
triad.census(get.adjacency(aidsblog,sparse=FALSE))  
#         003   012 102 021D 021U 021C 111D 111U 030T 030C 201 120D 120U 120C 210 300
# [1,] 484621 20717 300 2195   39   74    1  112    4    0   2    0   15    0   0   0
triad.census(get.adjacency(g10,sparse=FALSE));   plot(g10)
#      003 012 102 021D 021U 021C 111D 111U 030T 030C 201 120D 120U 120C 210 300
# [1,]  12   0  70    0    0    0    0    0    0    0  24    0    0    0   0  14
cliques(g10,min=3,max=3)   #-- 即 上述 300 的 14 種
# [[1]]  [1] 3 4 5   [[2]]  [1] 6 7 8   [[3]]  [1] 2 3 4   [[4]]  [1] 2 3 5   [[5]]  [1] 2 4 5   [[6]]  [1] 2 4 10   [[7]]  [1] 1 2 3   [[8]]  [1] 1 2 4   
# [[9]]  [1] 1 2 5   [[10]] [1] 1 2 10  [[11]] [1] 1 3 4   [[12]] [1] 1 3 5   [[13]] [1] 1 4 5   [[14]] [1] 1 4 10

##== (4A) 三節點模體(motifs): 包括扇形結構，前饋環結構等
graph.motifs(g10, size=3)  #-- [1] NA NA 24 14  --> 即上述triad的 201 與 300    #-- 也可用 motifs()
##== (4B) 四節點模體(motifs): 包括扇形結構，前饋環結構等
motifs4 = graph.motifs(g10, size=4)  #-- (4B1) 各類型motifs數目,但找不到說明 [1] NA NA NA NA  3 NA 36 18  1  6  6
count_motifs(g10,size=4)   #-- 70    #-- (4B2) motifs 數目 (但與上述總和不合)
pattern = graph.isocreate(size=4,number=50) #-- (4B3) 一個 motifs [1] 1->3 4->2 1->2 4->1 3->1 2->1
plot(graph.isocreate(size=4,number=50))      
iso = subgraph_isomorphisms(as.undirected(pattern), g10)  #-- (4B4) 找出所有的 motifs
motifs = lapply(iso, function (x) { induced_subgraph(g10, x) })
plot(motifs[[2]])

##======== 4.3.2 密度與相對頻率 ========##

##== (1) 密度(density): 實際出現的邊 與可能的邊的頻數 之比
##--     den(H) = |EH| / |VH|(|VH|-1)/2 (無向圖) 或 |EH| / |VH|(|VH|-1) (有向圖)
##--     H=G: 整個圖G的密度，    
graph.density(karate)   #-- [1] 0.1390374
##--     H＝Hv: 節點v直接相鄰鄰居的密度
# ego.instr = induced.subgraph(karate, graph.neighborhood(karate, 1, 1)[[1]]);   graph.density(ego.instr) 應改為下式
graph.density( graph.neighborhood(karate, 1, 1)[[1]] )    #-- [1] 0.25  --> 較整個網絡密度為高
# ego.admin = induced.subgraph(karate, graph.neighborhood(karate, 1, 34)[[1]]);   graph.density(ego.admin) 應改為下式
graph.density( graph.neighborhood(karate, 1, 34)[[1]] )   #-- [1] 0.2091503  --> 較整個網絡密度為高
neighbors(karate, "Mr Hi", "all")
##== (2) 相對頻率：定義聚集性(clustering)
##== (2A) 聚類係數(clustering coefficient)/傳遞性(transitivity): 傳遞性三元組的比例 
transitivity(karate)   #-- [1] 0.2556818   ##== (2A1) 全局聚集性的度量： clT(G) = 3 tau_delta(G)/tau_3(G)
transitivity(karate, "local", vids=c(1,34))  #-- [1] 0.1500000 0.1102941 --> 約為整體聚類係數的 50-60%
##== (2A2) 局部聚類係數：clT(v) = 3 tau_delta(v)/tau_3(v)

##== (3) 互惠性(reciprocity): 有向網絡中的邊的互惠狀況
reciprocity(aidsblog, mode="default")  #-- [1] 0.03278689  #-- 互惠(雙向)有向邊的二元組的數量比例
reciprocity(aidsblog, mode="ratio")    #-- [1] 0.01666667  #-- 互惠(雙向)有向邊的總數  的數量比例

##======== 4.3.3 連通性，割與流 ========##  

#== (1) 連通性(connectedness) -- 連通組件
#== (1A) 可達的(reachable): 對於任意兩個節點，至少存在一條通路
#== (1B) 連通的(connected): 當每個節點從任何其他節點都可達
igraph::is.connected(yeast)   #-- [1] FALSE   --> 注意 和課本上的指令略有出入，因為 masked by sna package
#== (1C) 連通組件(connected component): 最大化的連通子圖
comps = decompose.graph(yeast);   comps[[2]]
# IGRAPH 1eba287 UN-- 3 3 -- Yeast protein interactions, von Mering et al.
# + attr: name (g/c), Citation (g/c), Author (g/c), URL (g/c), Classes (g/x), name (v/c), Class (v/c),
# | Description (v/c), Confidence (e/c)
# + edges from 1eba287 (vertex names):
# [1] YBL046W--YNL201C YBL046W--YDR075W YNL201C--YDR075W
sapply(comps, vcount)
# [1]  2375    3    5    5    7    6    4    3    5    2    7    2    2    7    3    2    2    4    2    2    2    2    2    2    2    2    2    2    2    2    2
# [32]    4    2    2    2    2    2    5    2    2    3    2    2    3    2    2    5    3    3    2    2    2    2    3    3    2    2    2    2    2    2    2
# [63]    2    2    2    2    2    2    2    2    2    3    5    3    3    4    2    2    2    2    2    3    2    2    4    2    2    2    2    2    2    2
table(sapply(comps, vcount))
#    2    3    4    5    6    7 2375 
#   63   13    5    6    1    3    1  --> 2375 個節點的連通組件，即 comps[[1]]
#== (1D) 巨型組件(giant component): 規模上明顯佔優的連通組件
yeast.gc = decompose.graph(yeast)[[1]];   yeast.gc   #-- 占 2375/2617=90% 個節點，即為巨型組件
# IGRAPH 1f83f55 UN-- 2375 11693 -- Yeast protein interactions, von Mering et al.
# + attr: name (g/c), Citation (g/c), Author (g/c), URL (g/c), Classes (g/x), name (v/c), Class (v/c),
# | Description (v/c), Confidence (e/c)
# + edges from 1f83f55 (vertex names):
# [1] YLR197W--YDL014W YOR039W--YOR061W YDR473C--YPR178W YOR332W--YLR447C YER090W--YKL211C YDR394W--YGR232W
# [7] YER021W--YPR108W YPR029C--YKL135C YIL106W--YGR092W YKL166C--YIL033C YGL026C--YKL211C YOR061W--YGL019W
# [13] YGL115W--YER027C YGL049C--YGR162W YDR394W--YOR117W YDL140C--YML010W YLR291C--YKR026C YGR158C--YDL111C
# + ... omitted several edges
#== (1E) 巨型組件具小世界性(small world property): 
#=-     指(1)節點間最短路徑的長度(path.length)通常很小,
average.path.length(yeast.gc)   #-- [1] 5.09597   --> 平均的路徑長度
diameter(yeast.gc)              #-- [1] 15        --> 最長的路徑也不是太大
#=-       (2)網絡的聚集性(transitivity)很高
transitivity(yeast.gc)          #-- [1] 0.4686663 --> 近 50% 的連通三元組閉合形成了三角形

#== (2) 連通性 -- 節點／邊連通
#== (2A) k節點連通(k-vertex-connected): (1)節點數>k, (2)移除少於k的任意節點,其子圖仍為連通的
#== (2B) 圖的節點連通度(vertex connectivity): 圖是k節點連通的
vertex.connectivity(yeast.gc)  #-- [1] 1
#== (2C) k節點連通(k-vertex-connected): (1)邊數>k, (2)移除少於k的任意邊,其子圖仍為連通的
#== (2D) 圖的邊連通度(edge connectivity): 圖是k邊連通的
edge.connectivity(yeast.gc)    #-- [1] 1

#== (3) 割集(cut)
comps[[5]]
# IGRAPH 9f434e0 UN-- 7 7 -- Yeast protein interactions, von Mering et al.
# + attr: name (g/c), Citation (g/c), Author (g/c), URL (g/c), Classes (g/x), name (v/c), Class (v/c), Description (v/c), Confidence (e/c)
# + edges from 9f434e0 (vertex names):
# [1] YNL056W--YNL099C YOR043W--YLR019W YNL099C--YCR095C YNL056W--YNL032W YNL099C--YNL032W YOR043W--YNL032W YOR043W--YLL010C
plot(comps[[5]])
#== (3A) 點割集(vertex-cut): 移除某些節點，會破壞圖形的連通性
#== (3B) 割點(cut vertex)/關節點(articulation point): 能破壞圖連通性的單個節點
yeast.cut.vertices = articulation.points(yeast.gc)
length(yeast.cut.vertices)  #-- 350  --> 接近 15% (350/2375) 節點屬於割點，可用以認識網絡的脆弱之處
articulation.points(comps[[5]])  #-- [1] YNL099C YOR043W YNL032W  --> 比照上述的 plot(comps[[5]]) 來觀察
#== (3C) 邊割集(edge-cut): 移除某些邊，會破壞圖形的連通性
min_cut(comps[[5]], value.only=FALSE)$cut   #-- [1] YOR043W--YLR019W  --> 與 graph.mincut 相同

#== (4) 路徑(path)與流(flow) ... 圖形連通性的魯棒性(robustness)
#== (4A) Menger定理: 當且僅當所有不同的節點對，可以被k條內部不相交的節點(邊)路徑連接時,非平凡圖G是k節點連續的
shortest.paths(comps[[5]])  
#         YNL056W YNL099C YOR043W YLR019W YCR095C YNL032W YLL010C
# YNL056W       0       1       2       3       2       1       3
# YNL099C       1       0       2       3       1       1       3
# YOR043W       2       2       0       1       3       1       1
# YLR019W       3       3       1       0       4       2       2
# YCR095C       2       1       3       4       0       2       4
# YNL032W       1       1       1       2       2       0       2
# YLL010C       3       3       1       2       4       2       0
graph.mincut(comps[[5]], value.only=FALSE)$cut   #-- [1] YOR043W--YLR019W
graph.maxflow(comps[[5]], source="YNL056W", target="YLR019W")$cut   #-- 2
#== (4B) 弱連通的：有向圖G的基礎圖是連通的 
#== (4C) 弱連通的：即每個節點均可以從任一節點通過有向通路到達 --> 強連接組件/強連通度
igraph::is.connected(aidsblog, mode=c("weak"))    #-- [1] TRUE
igraph::is.connected(aidsblog, mode=c("strong"))  #-- [1] FALSE
aidsblog.scc = clusters(aidsblog, mode=c("strong"))
table(aidsblog.scc$csize)
#   1   4 
# 142   1
which(aidsblog.scc$csize==4)        #-- [1] 28 --> 第 28 類的大小為 4
which(aidsblog.scc$membership==28)  #-- [1] 7 134 142 143 --> 第 28 類的元素索引
aidsblog[which(aidsblog.scc$membership==28)]
# 4 x 146 sparse Matrix of class "dgCMatrix"
# [1,] 1 . . 1 . 1 . . . 1 . . 1 1 . . . . 1 . . . . . 1 1 . 1 1 . . . . . . . . . 1 1 . 1 . . . . 1 . 1 . 1 . . . . . . 1 . . . . . 1 . . . . . . . . .
# [2,] . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
# [3,] . . . . . . . . . . . . . . . . 1 . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 1 .
# [4,] . . . . . . 1 . . . . 1 . . . . . . . . . . 1 1 . . . . . . 1 . 1 . 1 . . . . . . . 1 . . . . 1 . 1 . . . . . 1 . . 1 1 . . . . . 1 . . . . . . 1
#                   
# [1,] . 1 . . 1 . 1 1 . . . . . . . . . . 1 . . . . 1 1 . . . . . 1 . . 1 . . . . . 1 1 . . . . . 1 . . . 1 . 1 1 1 1 . . . . . . 1 . 1 1 . 1 . 1 . 1 .
# [2,] . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 1 . 1 1 1 . 1 1 . 1 1 . 1 . . 1 . . 1 1 1 1 1 1
# [3,] . . . . . 1 . . . . . . . . . . 1 1 . . . . . . . . . . . 1 . . . . . . . . . . . . . . . . . . 1 . . . 1 1 . 1 . 1 . 1 1 . . . . . 1 . . . . . .
# [4,] . . . . . . . . . . 1 . 1 . . . . . . 1 . . 1 . . . . . . . . . 1 . . . . . . . . . 1 . . . . . . 1 . 1 1 1 . . 1 1 1 . 1 1 . . . . 1 . . . . 1 .
plot(induced_subgraph(aidsblog, which(aidsblog.scc$membership==28)), edge.curved=0.3)


########## <C>.圖形的分解 ##########

#####===== 4.4 圖分割 (Graph Partitioning): 將元素的集合劃分到"自然的"子集之中的過程 =====##### ...#-- [S711.R]

##======== 4.4.1 層次聚集(hierarchical clustering) ========##

##== (1) 層次聚集方法：凝聚(agglomerative)算法，分裂(divisive)方法 ==> 社團(community)
##== (2) 模塊度(modularity): sum[fkk-fkk*]^2
##== (3) 貪婪優化法(fastgreedy): 凝聚形式的層次聚類算法
kc = fastgreedy.community(karate)
length(kc)   #--- [1] 3  --> 3個社團(community)
sizes(kc)
# Community sizes
#  1  2  3 
# 18 11  5
membership(kc)
# Mr Hi  Actor 2  Actor 3  Actor 4  Actor 5  Actor 6  Actor 7  Actor 8  Actor 9 Actor 10 Actor 11 Actor 12 Actor 13 Actor 14 Actor 15 Actor 16 Actor 17 
#     2        2        2        2        3        3        3        2        1        1        3        2        2        2        1        1        3 
# Actor 18 Actor 19 Actor 20 Actor 21 Actor 22 Actor 23 Actor 24 Actor 25 Actor 26 Actor 27 Actor 28 Actor 29 Actor 30 Actor 31 Actor 32 Actor 33   John A 
#        2        1        2        1        2        1        1        1        1        1        1        1        1        1        1        1        1 
par(mfrow=c(1,2))
plot(kc,karate);      library(ape)
dendPlot(kc, mode="phylo")   #-- (4) 樹狀圖(dendrogram) -- 系統發生樹(phylogenetic tree)
par(mfrow=c(1,1))

##======== 4.4.2 譜分割 ========##

##== (1) 譜圖理論(spectral graph theory): 聯繫 圖的連通性 與 特定矩陣的特徵值分析
##== (2) 拉普拉斯矩陣(Laplacian) L = D(度序列的對角矩陣) - A, lambda1=..=lambdaK = 0, lambda(K+1) > 0
##== (3) Fiedler向量／值, x2/lambda2: x2(v)>=0, x2(v)<0
k.lap = graph.laplacian(karate);   head(k.lap,2)   ##=- (3A) 拉氏矩陣 L
# 34 x 34 sparse Matrix of class "dgCMatrix"
# [[ suppressing 34 column names ‘Mr Hi’, ‘Actor 2’, ‘Actor 3’ ... ]]
# Mr Hi   42 -4 -5 -3 -3 -3 -3 -2 -2 . -2 -3 -1 -3 . . . -2 . -2 . -2 . . . . . . . .  . -2 . .
# Actor 2 -4 29 -6 -3  .  .  . -4  . .  .  .  . -5 . . . -1 . -2 . -2 . . . . . . . . -2  . . .
eig.anal = eigen(k.lap);   eig.anal$values         ##=- (3B) 特徵值
#  [1]  5.206534e+01  4.599077e+01  4.183286e+01  3.799276e+01  2.968838e+01  2.555550e+01  2.278496e+01  2.135865e+01  1.932382e+01
# [10]  1.703380e+01  1.488858e+01  1.438135e+01  1.207540e+01  1.187442e+01  1.022181e+01  1.001070e+01  9.180967e+00  6.977880e+00
# [19]  6.446055e+00  5.594812e+00  5.168555e+00  4.978179e+00  4.697275e+00  4.544698e+00  4.161606e+00  3.800944e+00  3.706065e+00
# [28]  3.121263e+00  3.061041e+00  2.968302e+00  2.931820e+00  2.394319e+00  1.187107e+00 -1.078498e-15
head(eig.anal$vectors,2)                           ##=- (3C) 特徵向量
#            [,1]        [,2]       [,3]       [,4]       [,5]        [,6]         [,7]         [,8]          [,9]       [,10]       [,11]       [,12]
# [1,] 0.04112292  0.89148386  0.2819538  0.1585127 -0.1611964  0.01781463 -0.049011143 -0.007020226  0.0119094748 -0.04913501 -0.05599514 -0.01363859
# [2,] 0.00213939 -0.06001719 -0.1419185 -0.5650314 -0.7049861 -0.06416738  0.007308493  0.089214580 -0.0007935979  0.08076833 -0.10009931 -0.01530854
#            [,13]      [,14]       [,15]      [,16]       [,17]       [,18]      [,19]       [,20]       [,21]        [,22]       [,23]       [,24]
# [1,]  0.05510929 0.02861101 -0.01004615 0.03257824 -0.03137094 -0.07179482 0.01519708 -0.01200874 -0.03832430 -0.003592481 -0.03800291 -0.01739536
# [2,] -0.02513231 0.02638965  0.01029684 0.03003850 -0.08175557 -0.21354901 0.05493901 -0.04982970  0.01562152 -0.004604166 -0.07615169 -0.04619059
#             [,25]        [,26]      [,27]       [,28]       [,29]        [,30]       [,31]       [,32]      [,33]      [,34]
# [1,] -0.004893161 -0.001739837 0.02813140 0.009147148 -0.00145275  0.006386155 -0.01404568 -0.06155852 -0.1233171 -0.1714986
# [2,] -0.001990694 -0.015266031 0.08723272 0.052408570 -0.01099092 -0.030114297  0.03395736 -0.13116707 -0.0580066 -0.1714986
par(mfrow=c(1,2))                                  ##=- (3D) 特徵值/向量作圖
plot(eig.anal$values, col="blue", ylab="Eigenvalues of Graph Laplacian")
f.vec = eig.anal$vectors[, 33];   f.vec
# [1]  -0.12331710 -0.05800660 -0.01368446 -0.07445083 -0.26733837 -0.29881441 -0.29744558 -0.06406144  0.05300520
# [10]  0.12925543 -0.28550172 -0.20406685 -0.12324309 -0.03493189  0.17156602  0.15520216 -0.37166422 -0.16804128
# [19]  0.21148231 -0.06258820  0.18847412 -0.12892330  0.16858175  0.14801794  0.15762212  0.14632034  0.19249255
# [28]  0.12982346  0.09449019  0.16960883  0.08368282  0.11706470  0.13538405  0.12400534
faction = V(karate)$Faction     # 修改 faction = get.vertex.attribute(karate, "Faction")
f.colors = as.character(length(faction));   f.colors[faction == 1] = "red";   f.colors[faction == 2] = "cyan"
plot(f.vec, pch=16, xlab="Actor Number", ylab="Fiedler Vector Entry", col=f.colors)
abline(0, 0, lwd=2, col="lightgray")
par(mfrow=c(1,1))

##======== 4.4.3 圖分割的驗證 ========##
func.class = V(yeast.gc)$Class  # 修改 func.class = get.vertex.attribute(yeast.gc, "Class")
table(func.class)
#   A   B   C   D   E   F   G   M   O   P   R   T   U  --> 內定蛋白質功能的分類
#  51  98 122 238  95 171  96 278 171 248  45 240 483
yc = fastgreedy.community(yeast.gc)
c.m = membership(yc);   length(c.m);   head(c.m)   #-- [1] 2375
# YLR197W YOR039W YDR473C YOR332W YER090W YDR394W 
#       5       5       3       7       4       4
table(c.m)                                           # --> 依蛋白質相互作用的社團聚類
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31 
# 182  98 255 733 148  82 186 363  55  25  30  22  34  24  15  15  11  15   6  10   8   9   4   5   6   7   8   7   5   4   3 
head( table(c.m, func.class, useNA=c("no")), 6 )     # --> 內定分類 vs. 相互作用的社團聚類
#     func.class
# c.m    A   B   C   D   E   F   G   M   O   P   R   T   U
#   1    0   0   0   1   3   7   0   6   3 110   2  35  14
#   2    0   2   2   7   1   1   1   4  39   5   0   4  27
#   3    1   9   7  18   4   8   4  20  10  23   8  74  64
#   4   25  11  10  22  72  84  81 168  14  75  16  27 121
#   5    1   7   5  14   0   4   0   2   3   6   1  34  68
#   6    1  24   1   4   1   4   0   7   0   1   0  19  16
yeast.group = leading.eigenvector.community(yeast.gc);   yeast.group
# IGRAPH clustering leading eigenvector, groups: 8, mod: 0.63
# + groups:
#   $`1`
# [1] "YLR197W" "YOR039W" "YOR061W" "YGL049C" "YPL093W" "YER006W" "YHR052W" "YGR162W" "YOL041C" "YHR197W" "YLL008W"
# [12] "YPL043W" "YOR310C" "YNL002C" "YKL014C" "YPL012W" "YMR049C" "YLR409C" "YLR222C" "YOL077C" "YPL126W" "YDL060W"
# [23] "YFR001W" "YKR081C" "YDR496C" "YDR087C" "YGR145W" "YMR309C" "YIR001C" "YDR091C" "YGR103W" "YGL019W" "YPR016C"
# ...
### lec = leading.eigenvector.community.step(yeast.gc)  #-- 找不到 .step函數,無法執行,網路上2014年後亦未解答
### lec$membership
### # Try one more split
### leading.eigenvector.community.step(yeast, fromhere=lec, community=0)

#####===== 4.5 同配性(Assortativity) 與混合(Mixing): XXXX =====#####

##== (1) 同配混合(assortative mixing): 節點間依據某些特徵(可是分類/有序/連續變量)進行選擇性連接
##== (2) 同配係數(assortativity coefficient) ra: 量化給定網絡中同配程度的指標
##-- (3) ra 類似皮爾遜相關係數：介於 -1 與 1 之間, -1/0/1 各有意義
assortativity.nominal(yeast, (V(yeast)$Class=="P")+1, directed=FALSE)   #-- [1] 0.4965229
assortativity.degree(yeast) #-- [1] 0.4610798
#### A=matrix(get.adjacency(yeast),nrow=2617); cor(A[1,],A[2,])   #-- [1] 0.39291
