# PT09圖形繪製.R: R program for Graph
# Jia-Sheng Heh, 05/27/2022, revised from S905/906
# Ref: Chap. 2, 操作網絡數據 & Chap. 3, 網絡數據可視化 of 
#      Eric D. Kolaczyk and Gabor Csardi. (2014). Statistical Analysis of Network Data with R. NY: Springer

wkDir = "D:/CYCU/course_data/";   setwd(wkDir);
dataDir = wkDir;
# install.packages("igraph") #-- needs only to be done once
library(igraph)  #-- needs to be called for every new session
library(data.table);    

########## <A>.基本圖形(graph) ##########

#####===== 2.1 概述 =====#####

#####===== 2.2 創建網絡圖 =====#####
##======== <A1> 2.2.1 無向圖和有向圖 ========##
#== (1) Undirected graphs use - to indicate an edge
g.undirected = graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)
#Note: Any vertex name can be used here. We don't just have to use numbers.
#== (2) Directed graphs require -+ where + is the ending of the edge
g.directed = graph.formula(1-+2, 1-+3, 2-+3, 2-+4, 3++5, 4-+5, 4-+6, 4-+7, 5-+6, 6-+7)
#== (3) vertex names of a graph
V(g.undirected)   #--Vertex sequence: [1] "1" "2" "3" "4" "5" "6" "7"
#== (4) edgelist contained in the graph
E(g.undirected)   #--  [1] 1--2 1--3 2--3 2--4 3--5 4--5 4--6 4--7 5--6 6--7
E(g.directed)  #-- Edge sequence:  [1] 1->2 1->3 2->3 2->4 3->5 4->5 4->6 4->7 5->6 6->7
#== (5) basic idea of the data structure
print_all(g.undirected)  #-- str(g.undirected)
print_all(g.directed)    #-- str(g.directed)
# IGRAPH UN-- 7 10 -- 
# + attr: name (v/c)
# + edges (vertex names):
# 1 -- 2, 3
# 2 -- 1, 3, 4
# 3 -- 1, 2, 5
# 4 -- 2, 5, 6, 7
# 5 -- 3, 4, 6
# 6 -- 4, 5, 7
# 7 -- 4, 6
#== (6) simple plot first
plot(g.undirected)
dev.new()
plot(g.directed)

#####===== 2.4 關於圖 =====#####  略--請參閱課本 
##======== <A2> 2.4.1 圖的基本概念 ========##
#== (1) simple graph
g = graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)
is.simple(g)
mg = g+edge(2,3)
str(mg)
is.simple(mg)
E(mg)$weight = 1
wg2 = simplify(mg)
is.simple(wg2)
str(wg2)
E(wg2)$weight
neighbors(g,5)    #== (2) neighbors
dg = g.directed;   plot(dg)   #== (3) degree
degree(dg)
degree(dg, mode="in")
degree(dg, mode="out")
#== (4) connectedness
is.connected(g) 
is.connected(dg, mode="weak") 
is.connected(dg, mode="strong") 
clusters(g)  #== (5) cluster
diameter(g, weights=NA)  #== (6) diameter

##======== <A3> 2.4.2 特殊類型的圖 ========##
#== (1) 一般特殊圖
g.full = graph.full(7)
g.ring = graph.ring(7)
g.tree = graph.tree(7,children=2, mode="undirected")
g.star = graph.star(7, mode="undirected")
par(mfrow=c(2,2));   plot(g.full);  plot(g.ring);  plot(g.tree);  plot(g.star);   par(mfrow=c(1,1))
#== (2) bipartite graph
g.bip = graph.formula(actor1:actor2:actor3, movie1:movie2, actor1:actor2-movie1, actor2:actor3-movie2 )
V(g.bip)$type = grepl("movie", V(g.bip)$name)
str(g.bip)  #-- str() 函數在此有問題
plot(g.bip)

proj = bipartite.projection(g.bip)
str(proj[[1]])  #-- str() 函數在此有問題
plot(proj[[1]])
str(proj[[2]])  #-- str() 函數在此有問題
plot(proj[[2]])


########## <B>.圖形性質 ##########

#####===== <B1> 2.2.2 圖的表示 =====#####
#== (1) graph.formula()
dg = graph.formula(1-+2, 1-+3, 2++3, 3-+4);   plot(dg)
dg = graph.formula(Sam-+Mary, Sam-+Tom, Mary++Tom)
str(dg)
#== (2) edgelist: get.edgelist() -- Get the edgelist of the above graph
get.edgelist(dg)
# IGRAPH DN-- 3 4 -- 
# + attr: name (v/c)
# + edges (vertex names):
# [1] Sam ->Mary Sam ->Tom  Mary->Tom  Tom ->Mary
V(dg)$name = c("Sam", "Mary", "Tom")
E(dg)
# Edge sequence:
# [1] Sam  -> Mary
# [2] Sam  -> Tom 
# [3] Mary -> Tom 
# [4] Tom  -> Mary
#== (3) adjacency matrix: get.adjacency -- get the adjacency matrix representation of the graph above
Adj.1 = get.adjacency(dg);   Adj.1
# 3 x 3 sparse Matrix of class "dgCMatrix"
#   1 2 3
# 1 . 1 1
# 2 . . 1
# 3 . 1 . --> this gives a sparse representation of the adjacency matrix
library(Matrix);   image(Matrix(Adj.1))
Adj.2 = get.adjacency(g.undirected);   Adj.2
image(Matrix(Adj.2))  #--> Using image(Matrix()) for analyzing community structure (more on this later)

#####===== <B2> 2.2.3 圖的操作 =====#####
#== (1) induced graph: the subgraph with the first 5 vertices (and all edges among them)
par(mfrow=c(1,2))
plot(g.undirected)
h = induced.subgraph(g.undirected, 2:6);   plot(h)
par(mfrow=c(1,1))
str(h)
Adj.3 = get.adjacency(h)  #-- ensuring that this is the same as subsetting the original adjacency matrix
Adj.4 = Adj.2[2:6, 2:6]   #-- grabbing the subnetwork for the 1st 5 nodes directly from the original adjacency matrix
image(Matrix(Adj.3));   
dev.new()
image(Matrix(Adj.4))  #--> 節點的編號,會改為1-5
#== (2) delete vertices: give subgraph of everything excluding vertices 6 and 7
h = g.undirected - vertices(c(6,7))
#== (3) add vertices: add back the vertices 6 and 7
h = h + vertices(c(6,7))
V(h)
g = h + edges(c(4,6),c(4,7),c(5,6),c(6,7))
#== (4) graph operations:
h1 = h;      h2 = graph.formula(4-6, 4-7, 5-6, 6-7)
g = graph.union(h1,h2)
par(mfrow=c(1,3));   plot(h1);   plot(h2);   plot(g);   par(mfrow=c(1,1))

########## <C>.圖形定義 ##########

#####===== <C1>補充教材--圖形定義法 =====#####
#-- (1)圖形公式
g6 <- graph.formula(Alice-Bob-Cecil-Alice, Daniel-Cecil-Eugene, Cecil-Gordon);  plot(g6)
g7 <- graph.formula(Alice-Bob:Cecil:Alice, Daniel:Cecil-Eugene:Gordon);         plot(g7)
g8 <- graph.formula(Alice+-+Bob--+Cecil+--Daniel, Eugene--+Gordon:Helen);       plot(g8)
g9 <- graph.formula(Alice--Bob--Daniel, Cecil:Gordon:Helen);                    plot(g9)
g10 <- graph.formula(Alice+---------+Bob);                                      plot(g10)

#-- (2)邊公式與(3)毗鄰矩陣
e11 <- cbind( c(0,0,1,2), c(1,2,2,4) );   g11 <- graph.edgelist(e11);   
e12 <- cbind( c(1,1,2,3), c(2,3,3,5) );   g12 <- graph.edgelist(e12);   g12;   plot(g12) 
e13 <- cbind(c("Alice","Alice","Bob","Cecil"), c("Bob", "Cecil", "Cecil", "Ed"));    
g13 <- graph.edgelist(e13);                                             g13;   plot(g13)
summary(g13)
A14 <- matrix(sample(0:1,100,rep=TRUE),10,10);   A14
g14 <- graph.adjacency(A14);                     plot(g14) 
#-- (4)框架圖形
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David", "Esmeralda"),       
                     age=c(48,33,45,34,21), gender=c("F","M","F","M","F")) 
actors 
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David", "David", "Esmeralda"), 
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),  
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),     
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3)) 
relations 
g15 <- graph.data.frame(relations, directed=TRUE, vertices=actors) 
plot(g15) 
g15 

#####===== <C2> 圖形繪製: 2.3 網絡圖的修飾 =====#####

##======== 2.3.1 節點,邊和圖的屬性 ========##
#== (1) 一般無向圖的屬性
dg = graph.formula(Sam-+Mary, Sam-+Tom, Mary++Tom)
V(dg)$name  #-- [1] "Sam"  "Mary" "Tom"
V(dg)$gender = c("M","F","M")
V(dg)$color = "red"
#== (2) create a weighted graph with random numbers between 0 and 1 on each edge
is.weighted(dg)  #-- [1] FALSE
wg = g.undirected
ecount(wg)  #-- 10: counts the number of edges in the graph
E(wg)$weight = runif(ecount(wg))  #-- weights = runif - random uniform number generator
is.weighted(wg)  #-- [1] TRUE
#== (3) 圖形的屬性: 如, 圖形名稱
dg$name = "Toy Graph"

##======== 2.3.2 使用數據框 ========##
#-- install.packages("sand")
library(sand)
dim(elist.lazega);   head(elist.lazega,3)  #-- 115  2: 律師數據
#   V1 V17
#   V2  V7
#   V2 V16
dim(v.attr.lazega);   head(v.attr.lazega,3)   #-- 36 9: vertex attribute data frame
#   Name Seniority Status Gender Office Years Age Practice School
# 1   V1         1      1      1      1    31  64        1      1
# 2   V2         2      1      1      1    32  62        2      1
# 3   V3         3      1      1      2    13  67        1      1
#-- Note: the first column must always be "Name" and the entries correspond to the vertex names in the edge list provided
g.lazega = graph.data.frame(elist.lazega, directed="FALSE", vertices=v.attr.lazega)
g.lazega$name = "Lazega Lawyers"
vcount(g.lazega)   #-- [1] 36
ecount(g.lazega)   #-- [1] 115
list.vertex.attributes(g.lazega)  #-- [1] "name"      "Seniority" "Status"    "Gender"    "Office"    "Years"     "Age"       "Practice"  "School"   
plot(lazega)


########## <D>.圖形的可視化 ##########

#####===== 3.1 概述：網絡的展現技術是圖的繪制(graph drawing)或說圖形可視化(graph visualization)領域的核心問題 =====#####

#####===== <D1> 3.2 圖可視化的基本元素 =====#####

#== graph G(V,E)
#--   node/vertex node set V = {vi}: symbol as points, circles and rectangles
#--   edge set E = {eij}: 組合結構的幾何表示
#== di Battista, Eades, Tamassia and Tollis (1999). Graph Drawing. Prentice-Hall [46], Kolaczyk (2009) [91]
#== igraph::plot

######===== <D2> 3.3 圖的布局(layout) =====#####

#== (1) 代表性案例
library(sand)
g.l = graph.lattice(c(5, 5, 5))   #-- 案例1：5*5*5 的網絡
data(aidsblog)
summary(upgrade_graph(aidsblog))  #-- 案例2：博客網絡: IGRAPH 146獨立博客 187引用關係  AIDS有關的博客(blog)文章
# IGRAPH 8944001 D--- 146 187 -- 

#== (2) 環狀布局(circular layout)
#-- 所有的節點(通常是等距地)分布在一個圓的圓周上 -- 節點順序很重要
igraph.options(vertex.size=3, vertex.label=NA, edge.arrow.size=0.5)
par(mfrow=c(1, 2)) 
plot(g.l, layout=layout.circle);       title("5x5x5 Lattice")  #-- 案例1
plot(aidsblog, layout=layout.circle, edge.arrow.size=0.1);  title("Blog Network")   #-- 案例2
#== (3) both the fruchterman reingold and kamada kawai layouts are called "Force Directed Layouts" and used to visualize community structure in graphs
#-- 彈簧模型(spring-embedder method): 節點定義一種力的作用，力的大小取決於節點對的位置與距離，迭代到節點間的淨作用力收歛
plot(g.l,layout=layout.fruchterman.reingold);        title("5x5x5 Lattice")
plot(aidsblog,layout=layout.fruchterman.reingold);   title("Blog Network")
#-- 能量布局算法(energy-placement methods): 模仿物理中常見的能量，選擇使系統總能量最小的節點位置
plot(g.l, layout=layout.kamada.kawai);       title("5x5x5 Lattice")
plot(aidsblog, layout=layout.kamada.kawai);  title("Blog Network")

#== (3) 樹狀布局(tree layout): 包括輻射布局(radial layout)與分層布局(layered layout)
#compare three layouts side-by-side using a tree structure
g.tree = graph.formula(1-+2,1-+3,1-+4,2-+5,2-+6,2-+7,3-+8,3-+9,4-+10)
par(mfrow=c(1, 3))
igraph.options(vertex.size=30, edge.arrow.size=0.5,vertex.label=NULL)
plot(g.tree, layout=layout.circle)  #-- regular circular grid
plot(g.tree, layout=layout.reingold.tilford(g.tree,circular=T)) #-- 輻射布局: places root node in the CENTER of graph
plot(g.tree, layout=layout.reingold.tilford)                    #-- 分層布局: places root node at the TOP of the graph
par(mfrow = c(1,1)) 

#== (4) 二元圖(bipartite graph): The layout.bipartite layout will stack nodes according to specified type
E(g.bip)  #-- [1] actor1--movie1 actor2--movie1 actor2--movie2 actor3--movie2
V(g.bip)$name  #-- [1] "actor1" "actor2" "actor3" "movie1" "movie2"
V(g.bip)$type  #-- [1] FALSE FALSE FALSE  TRUE  TRUE
plot(g.bip, layout=-layout.bipartite(g.bip)[,2:1], vertex.size=30, 
     vertex.shape=ifelse(V(g.bip)$type,"rectangle","circle"),
     vertex.color=ifelse(V(g.bip)$type, "red", "cyan"))

#####===== <D3> 3.4 修飾圖的布局(layout) =====#####

#== (1) karate (karate club network, 空手道俱樂部網絡) (Zachary, 1970年代約兩年): V成員,E成員間社交關係,已分成兩派系(faction)
library(igraphdata);    library(igraph);    data(karate)   #-- (1A)內存數據集  
V(karate)$name 
# [1 ] "Mr Hi"    "Actor 2"  "Actor 3"  "Actor 4"  "Actor 5"  "Actor 6"  "Actor 7"  "Actor 8"  "Actor 9"  "Actor 10" "Actor 11" "Actor 12"
# [13] "Actor 13" "Actor 14" "Actor 15" "Actor 16" "Actor 17" "Actor 18" "Actor 19" "Actor 20" "Actor 21" "Actor 22" "Actor 23" "Actor 24"
# [25] "Actor 25" "Actor 26" "Actor 27" "Actor 28" "Actor 29" "Actor 30" "Actor 31" "Actor 32" "Actor 33" "John A"  
V(karate)$Faction  #-- [1] 1 1 1 1 1 1 1 1 2 2 1 1 1 1 2 2 1 1 2 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2
V(karate)[Faction]
# + 34/34 vertices, named, from 4b458a1:
#  [1] Mr Hi   Mr Hi   Mr Hi   Mr Hi   Mr Hi   Mr Hi   Mr Hi   Mr Hi   Actor 2 Actor 2 Mr Hi   Mr Hi   Mr Hi   Mr Hi   Actor 2 Actor 2 Mr Hi   Mr Hi  
#  [19] Actor 2 Mr Hi   Actor 2 Mr Hi   Actor 2 Actor 2 Actor 2 Actor 2 Actor 2 Actor 2 Actor 2 Actor 2 Actor 2 Actor 2 Actor 2 Actor 2
E(karate)$weight
# [1] 4 5 3 3 3 3 2 2 2 3 1 3 2 2 2 2 6 3 4 5 1 2 2 2 3 4 5 1 3 2 2 2 3 3 3 2 3 5 3 3 3 3 3 4 2 3 3 2 3 4 1 2 1 3 1 2 3 5 4 3 5 4 2 3 2 7 4 2 4 2 2 4 2
# [74] 3 3 4 4 5
head(E(karate))
# + 6/78 edges from 4b458a1 (vertex names):
#   [1] Mr Hi--Actor 2 Mr Hi--Actor 3 Mr Hi--Actor 4 Mr Hi--Actor 5 Mr Hi--Actor 6 Mr Hi--Actor 7
set.seed(42);   l = layout.kamada.kawai(karate);   plot(karate, layout=l, vertex.label=NA)    #-- (1B) 預設的繪圖方式
V(karate)$label = sub("Actor ", "", V(karate)$name)    #-- (1C) 修飾過後的繪圖方式 (decorate plotting) #-- 節點標籤 代表 行動者簡稱
V(karate)$shape               = "circle";     V(karate)[c("Mr Hi", "John A")]$shape = "rectangle"      #-- 節點形狀 代表 派系領導者
V(karate)[Faction == 1]$color = "red";        V(karate)[Faction == 2]$color         = "dodgerblue"     #-- 節點顏色 代表 派系
graph.strength(karate)  #-- vertex.strength: total weight of incident edges
V(karate)$size = 4*sqrt(graph.strength(karate));    V(karate)$size2 = V(karate)$size * .5              #-- 節點大小 代表 加權的degree(度), size2用於矩形
V(karate)$label.dist = ifelse(V(karate)$size >= 10, 0, 0.75);   #-- 給比較小的節點 予以節點labels偏移量
E(karate)$width = E(karate)$weight                                                                     #-- 邊寬度 代表 加權的degree (互動頻率)
F1 = V(karate)[Faction==1];               F2 = V(karate)[Faction==2]                                   #-- 邊顏色 代表 區分成員的派系
E(karate)[ F1 %--% F1 ]$color = "pink";   E(karate)[ F2 %--% F2 ]$color = "lightblue";   E(karate)[ F1 %--% F2 ]$color = "yellow" 
plot(karate, layout=l)

#== (2) lazega (律師網絡)   #-- library(sand)
data(lazega) 
V(upgrade_graph(lazega))
# + 36/36 vertices, named, from 17aa530:
#   [1] V1  V2  V3  V4  V5  V6  V7  V8  V9  V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22 V23 V24 V25 V26 V27 V28 V29 V30 V31 V32 V33 V34 V35 V36
V(upgrade_graph(lazega))$Office    #-- 辦公室地點 [1] 1 1 2 1 2 2 2 1 1 1 1 1 1 2 3 1 1 2 1 1 1 1 1 1 2 1 1 2 1 2 2 2 2 1 2 1  
V(upgrade_graph(lazega))$Practice  #-- 執業類型   [1] 1 2 1 2 1 1 2 1 2 2 1 2 1 2 2 2 2 1 2 1 1 1 1 1 2 1 1 2 2 1 1 1 1 2 2 1  
V(upgrade_graph(lazega))$Years     #-- 工作年份   [1] 31 32 13 31 31 29 29 28 25 25 23 24 22  1 21 20 23 18 19 19 17  9 16 15 15 15 13 11 10  7  8  8  8  8  8  5
head(E(upgrade_graph(lazega)))
# + 6/115 edges from dc8345a (vertex names):
#   [1] V1--V17 V2--V7  V2--V16 V2--V17 V2--V22 V2--V26
colbar   = c("red", "dodgerblue", "goldenrod");   v.colors <- colbar[V(lazega)$Office]  #-- 節點顏色: 辦公室地點
v.shapes = c("circle", "square")[V(lazega)$Practice]  #-- 節點形狀: 執業類型 
v.size   = 3.5*sqrt(V(lazega)$Years)                  #-- 節點大小: 工作年份
set.seed(42);   l = layout.fruchterman.reingold(lazega);  plot(lazega, layout = l, vertex.color=v.colors, vertex.shape=v.shapes, vertex.size=v.size)

#####===== <D4>3.5 大型網絡可視化 =====#####

##== (1) 大型網絡的粗糙化：法國政治性博客(Observatoire Presidentielle, 2006/10/某日)   # library(sand)
summary(fblog)   #-- (1A)內存數據集
# IGRAPH UN-- 192 1431 -- 192博客 1431條連接
# attr: name (v/c), PolParty (v/c)
party.names = sort(unique(V(fblog)$PolParty));   party.names  #-- 各博客 分屬 9個政黨
# [1] " Cap21"          " Commentateurs Analystes"  " Les Verts"        " liberaux"  " Parti Radical de Gauche"             
# [6] " PCF - LCR"      " PS"                       " UDF"              " UMP"
party.nums.f = as.factor(V(fblog)$PolParty);   party.nums = as.numeric(party.nums.f);   party.nums
# [1]   3 3 3 3 3 3 3 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 8 9 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
# [71]  8 8 8 8 8 8 8 8 8 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 5 5 5 5
# [141] 5 5 5 5 5 5 5 6 6 6 6 6 6 4 4 4 4 4 4 4 4 4 4 6 2 2 2 2 2 2 2 2 2 2 2 1 1 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
set.seed(42);   l=layout.kamada.kawai(fblog);   #-- (1B)標準能量布局法
plot(fblog, layout=l, vertex.label=NA, vertex.color=party.nums, vertex.size=3)  
set.seed(42);   l=layout.drl(fblog);            #-- (1C)DrL聚類，基於VxOrd算法，複雜度為O(Nv)
plot(fblog, layout=l, vertex.size=5, vertex.label=NA, vertex.color=party.nums)
#-- (1D)圖分割(graph partitioning): 將(192)節點粗糙化為9個政黨的元節點(meta-vertices)   
party.size = as.vector(table(V(fblog)$PolParty))          #---- (1D1)原圖(fblog)
table(V(fblog)$PolParty)
# Cap21  Commentateurs Analystes  Les Verts   liberaux  Parti Radical de Gauche   PCF - LCR     PS    UDF    UMP
#     2                       11          7         25                       11           7     57     32     40 
fblog.c = contract.vertices(fblog, party.nums);  fblog.c  #---- (1D2)節點濃縮圖(fblog.c)
# IGRAPH a3c6a71 UN-- 9 1431 -- 
# + attr: name (v/x)
# + edges from a3c6a71 (vertex names):
#   [1]  jeunesverts.org/bordeaux         ,  bix.enix.org/                    ,  www.arnaudcaron.net/             ,  dominiquevoynet.net/blog         ,  blogs.lesverts.fr/               ,  emilien.net/                     ,  lipietz.net/blog.php3?id_breve=63-- jeunesverts.org/bordeaux         ,  bix.enix.org/                    ,  www.arnaudcaron.net/             ,  dominiquevoynet.net/blog         ,  blogs.lesverts.fr/               ,  emilien.net/                     ,  lipietz.net/blog.php3?id_breve=63
#   [2]  jeunesverts.org/bordeaux         ,  bix.enix.org/                    ,  www.arnaudcaron.net/             ,  dominiquevoynet.net/blog         ,  blogs.lesverts.fr/               ,  emilien.net/                     ,  lipietz.net/blog.php3?id_breve=63-- jeunesverts.org/bordeaux         
# ...
unlist(lapply(V(fblog.c)$name, FUN=length))   #-- [1]  2 11  7 25 11  7 57 32 40  各政黨的博客個數
E(fblog.c)$weight = 1
fblog.c2 = simplify(fblog.c);   fblog.c2                  #---- (1D3)邊濃縮圖(fblog.c2)
# IGRAPH 0da97f5 UN-- 9 25 -- 
# + attr: name (v/x)
# + edges from 0da97f5 (vertex names):
#   [1]  www.cap21.net                ,  corinnelepage.hautetfort.com/-- hugues.blogs.com               ,  sarkostique.over-blog.com      ,  www.placedelademocratie.net    ,  www.page2007.com/              ,  blog.netpolitique.net/         ,  www.election-presidentielle.fr/,  www.debat2007.fr/              ,  www.politique20.info           ,  ceteris-paribus.blogspot.com/  ,  vanb.typepad.com/versac/       ,  presidentielle.2007.free.fr/   
#   + ... omitted several edges
E(fblog.c2)$weight   #--  [1]  1  1  2 10 27 11  7 77 52 42 14  2  1  7  3  7  1 20  1  3 18  2 27 18 43
plot(fblog.c2, vertex.size=5*sqrt(party.size), vertex.label=party.names, vertex.color=V(fblog.c2), vertex.label.dist=1.5, 
     edge.width=sqrt(E(fblog.c2)$weight), edge.arrow.size=0)

##== (2) 子網絡的提取：karate
data(karate)
k.nbhds <- graph.neighborhood(karate, order=1)  #-- (2A) 各節點的鄰居(neighborhood) --> 具最大鄰居者為第 1/34 節點
sapply(k.nbhds, vcount)  #-- [1] 17 10 11  7  4  5  5  5  6  3  4  2  3  6  3  3  3  3  3  4  3  3  3  6  4  4  3  5  4  5  5  7 13 18
k.1 <- k.nbhds[[1]];    k.34 <- k.nbhds[[34]]   #-- (2B) 提取子網絡並繪圖
par(mfrow=c(1,2))
plot(k.1, vertex.label=NA, vertex.color=c("red", rep("lightblue", 16)))
plot(k.34, vertex.label=NA, vertex.color=c(rep("lightblue", 17), "red"))
par(mfrow=c(1,1))

#####===== <D5> 3.6 使用R之外的可視化工具 =====#####
##== (1) GraphViz: 經典的圖布局和繪制工具，可通過 Rgraphvix 擴展包使用，亦可以 write.graph將圖輸出為 dot文件
##== (2) Pajek: 最早出現的網絡分析與可視化工具之一，可以 write.graph / read.graph 處理
##== (3) Cytoscape: 現代的多平台網絡分析與可視化工具，可以通過 GML格式文件讀寫
##== (4) Gephi: 多平台的網絡分析和可視化工具，文件格是為 gexf, 可以 rgexf擴展包轉換 
##== (5) 其他: 可以 GraphGML, GML, CSV 檔案進行轉換



########## (E) 實務文本數據的繪圖 (RR) ##########

#####===== (E1) 含主回文的爬文數據 (KDD->RR) =====#####
Rlist = c("\\RR1_500r6810","\\RR501_1000r6217","\\RR1001_1500r7707","\\RR1501_2000r8262")
for (k in 1:length(Rlist)) {
  print(paste0(">> reading file - ",Rlist[k],".csv..."))
  RRk = fread(paste0(dataDir,Rlist[k],".csv"), encoding="UTF-8" )
  if (k==1) { RR = RRk }   else { RR = rbind(RR,RRk) }
}
dim(RR);   head(RR,2)       #-- 28996 / 464636 
#     X ktitle                title replyNo            Tdate      Tauthor       author authorclass             date rPostNo authorcredit
# 1:  1      1 自己的腰痠背痛自己救       0 2017-11-16 16:32 vivavida7749 vivavida7749    進階會員 2017-11-16 16:32      #1           29
# 2:  2      1 自己的腰痠背痛自己救       0 2017-11-16 16:32 vivavida7749          ujm    資深會員 2017-11-16 19:34      #2           32
# text
# 1: 板上偶爾有人在討論背痛或是腰痛問題\r\n然而上班族會腰痠背痛的原因不外乎：坐姿不正確 (我就是)、搬重物\r\n姊爬文看了一下，腰酸背痛的主因是肌肉緊繃，可透過『肌肉伸展』改善下背疼痛症狀。\r\n肌肉伸展最重要的部分是：核心肌群 和 大腿後肌群。\r\n講這麼多，還是來看教學影片最實在，感謝姊夫和翻譯者呀!只能幫分享回報了
# 2:                                                              \n\nvivavida7749 wrote:\r\n板上偶爾有人在討論背...(恕刪)\n\r\n感覺是貼錯了⋯⋯應該是下背痛那幾篇...Jeff和譯者都佛心來著\r\nhttps://youtu.be/Z3-mYrVjBEo\r\nhttps://youtu.be/gYsXOKAfxTs\r\nhttps://youtu.be/vw-LIovZ_Os\r\nhttps://youtu.be/oXj3O0quR0E
length(unique(RR$title))    #-- 1993 主題
length(unique(RR$author))   #-- 9018 作者

#####===== (E2) 作者數據框到作者價值模型(Author Value Model) (KDD3:RR->Cv) =====#####
RR$Tdate = as.Date(RR$Tdate)
RR$date = as.Date(RR$date)
RR$text = as.character(RR$text)
RR$isTitle = as.integer(RR$Tdate==RR$date)
RR$nch     = nchar(RR$text);               dim(RR)   #-- 28996 x 14
setDT(RR,key="author")
Cv = RR[, .(D0=min(date), Df=max(date),Tcount=sum(isTitle), TRcount=length(date), chCount=sum(nch)), by=author]
Cv$Rcount = Cv$TRcount - Cv$Tcount
Cv$TRcount0 = cut(Cv$TRcount,breaks=c(-1,0,1,9,99,999,9999)); 
Cv$chCount0 = cut(Cv$chCount, breaks=c(-1,0,9,99,999,9999,99999,999999));   table(Cv$chCount0)
Cv$nDay = as.Date(Cv$Df)-as.Date(Cv$D0);   range(Cv$nDay)   #-- 0 3503
Cv$nDay0 = cut(as.numeric(Cv$nDay), breaks=c(-1,0,7,30,100,400,1200,4600));   table(Cv$nDay0)
dim(Cv);   head(Cv,3)    #-- 9018 x 7
#     author         D0         Df Tcount TRcount chCount Rcount TRcount0 chCount0    nDay  nDay0
# 1: -Azure- 2010-01-03 2010-02-01      0       2     150      2    (1,9] (99,999] 29 days (7,30]
# 2: -James- 2017-09-01 2017-09-01      2       2     415      0    (1,9] (99,999]  0 days (-1,0]
# 3:   cmh19 2012-03-22 2012-03-22      0       1      59      1    (0,1]   (9,99]  0 days (-1,0]

#####===== (E3) 主題數據框到主題價值模型(Topic Value Model) (Pv->Pv$chCount0,Pv$nDay0) =====#####  
setDT(RR, key = "ktitle") 
Pv = RR[, .(title=unique(title), postNo=length(date), D0=min(date), Df=max(date), Nauthor=length(unique(author)), 
            chCount=sum(nch,na.rm=T)), by=ktitle];   
Pv$postNo0 = cut(Pv$postNo, breaks=c(0,1,9,99,999,7999));   table(Pv$postNo0)
Pv$chCount0 = cut(Pv$chCount, breaks=c(-1,0,9,99,999,9999,99999,999999));    #-- table(Pv$chCount0)
Pv$nDay = as.Date(Pv$Df)-as.Date(Pv$D0);   range(Pv$nDay)   #-- 0 4253
Pv$nDay0 = cut(as.numeric(Pv$nDay), breaks=c(-1,0,7,30,100,400,1200,4300));  #-- table(Pv$nDay0)
dim(Pv);   head(Pv,3)   #-- 2000 x 11

#####===== (E4) 作者/主題數據框 投射回 貼文數據框 (Cv/Pv->RR) =====#####  
RR$indXC = match(RR$author,Cv$author)   #-- project RR by Cv
RR$indXP = match(RR$ktitle,Pv$ktitle)   #-- project RR by Pv
head(RR[,-12],3)
#     X ktitle                                           title replyNo      Tdate      Tauthor         author authorclass
# 1:  2      1                            自己的腰痠背痛自己救       0 2017-11-16 vivavida7749            ujm    資深會員
# 2:  1      1                            自己的腰痠背痛自己救       0 2017-11-16 vivavida7749   vivavida7749    進階會員
# 3: 21      2 德國坦克Dirk Nowitzki - 我可以得分，我可以創...      33 2011-06-02 kevin0921185 131529125David    資深會員
#          date rPostNo authorcredit isTitle nch indXC indXP
# 1: 2017-11-16      #2           32       1 203  6177     1
# 2: 2017-11-16      #1           29       1 152  6288     1
# 3: 2011-06-03     #19            3       0  61    94     2

#####===== (E5) PC model (RR-->PC00) =====#####  
PC00 = table( RR$indXP, RR$indXC );   dim(PC00)   #-- [1] 2000 9018
rownames(PC00) = Pv$title;    colnames(PC00) = Cv$author
PC0 = PC00[ order(rowSums(PC00),decreasing=TRUE), order(colSums(PC00),decreasing=TRUE) ]
PC0[1:4,1:10]
#                                         CP03 clairee_wen 克羅旺來 goodversion wonwonba HD101 拯救世界維護和平 dodoyo diken0955 鬼州
# 2016，巴西奧運，Final，Brazil Olympic      1           0        0          64        0   112              139    205         0    0
# WWE美國職業摔角                            0           0        0          41        0    35                0      0         0    0
# Kobe VS Jordan(果然Kobe 要追上Jordan...    0           0        0           2        0     0                0      0         0    0
# 由飼料雞往仿土雞之路邁進                   7         190       36           0        0     0                0      0         0    0
sum(PC0)   #-- [1] 28996

#####===== (E6) Customer undirected graph (PC-->CC-->gCC) =====#####  
library(igraph)
PC = PC0[1:6,1:8];   sum(PC);   PC   #-- 863
#                                                      CP03 clairee_wen 克羅旺來 goodversion wonwonba HD101 拯救世界維護和平 dodoyo
# 2016，巴西奧運，Final，Brazil Olympic                   1           0        0          64        0   112              139    205   --5奧
# WWE美國職業摔角                                         0           0        0          41        0    35                0      0   --2摔
# Kobe VS Jordan(果然Kobe 要追上Jordan...                 0           0        0           2        0     0                0      0   --1
# 由飼料雞往仿土雞之路邁進                                7         190       36           0        0     0                0      0   --2雞
# 【林書豪】T-shirt 女款大深V！閃閃惹人愛、3/18 300...    0           0        0           1        0     0                1      0   --2林
# 何謂女生的好身材??                                     29           0        0           0        0     0                0      0   --1
CC = t( round(PC>0) )%*%(round(PC>0));   CC   #-- undirected graph, symmetric matrix
#                  CP03 clairee_wen 克羅旺來 goodversion wonwonba HD101 拯救世界維護和平 dodoyo
# CP03                3           1雞      1雞         1奧      0     1奧              1奧    1奧
# clairee_wen         1           1        1雞         0        0     0                0      0
# 克羅旺來            1           1        1           0        0     0                0      0
# goodversion         1           0        0           4        0     2摔奧            2奧林  1奧
# wonwonba            0           0        0           0        0     0                0      0
# HD101               1           0        0           2        0     2                1奧    1奧
# 拯救世界維護和平    1           0        0           2        0     1                2      1奧
# dodoyo              1           0        0           1        0     1                1      1
### gCC = graph_from_adjacency_matrix(CC[-5,-5])
gCC = graph.adjacency(CC[-5,-5], mode="undirected", weighted=TRUE)
par(family="STKaiti")
plot(gCC, vertex.label.family="STKaiti", edge.color=c("black","blue","green","orange")[E(gCC)$weight],
     edge.label=E(gCC)$weight, vertex.label=V(gCC)$name)                 




library(arules)
data(Groceries)
inspect(Groceries[1:3])
arG = apriori( Groceries, parameter=list(support=0.05, confidence=0.2), control=list(verbose=FALSE));  
length(arG);  inspect(arG)    #-- [1] 7 association rules
graph.arG = graph.edgelist( cbind(inspect(arG)$lhs, inspect(arG)$rhs) )
plot(graph.arG, edge.curved=0.3)


