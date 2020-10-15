library(tidyverse)
library(ggcorrplot)
library(genalg)
library(gridExtra)
library(ggExtra)
library(EBImage)
library(reshape2)
library(future)
library(repr)

options(repr.plot.width=15, repr.plot.height = 10)

library(ggmap)


train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")

test%>%group_by(delta)%>%summarise(n=n())%>%ggplot()+geom_line(aes(delta,n))
delta <- train %>% select(grep("delta", colnames(train))) %>% as.matrix()
start <- train %>% select(grep("start_", colnames(train))) %>% as.matrix()
stop <- train %>% select(grep("stop_", colnames(train))) %>% as.matrix()

head(which(delta==2))
id=23
start_plot <- start[id,]
stop_plot <- stop[id,]

dim(start_plot) <- c(1,625)
dim(stop_plot) <- c(1,625)

img_start <- t(matrix(start_plot, nrow=25L, byrow=T))
image(img_start,col=gray.colors(33),axes=F,main=paste0('start'))

img_stop <- t(matrix(stop_plot, nrow=25L, byrow=T))
image(img_stop,col=gray.colors(33),axes=F,main=paste0('stop, depth:',delta[id]))



matrixoverlap <- array(rep(NA,625), c( 25,25))
for(i in 1:25){
  for(j in 1:25){
    if(img_start[i,j]==0 & img_stop[i,j]==0) matrixoverlap[j,i] <- 0
    if(img_start[i,j]>0  & img_stop[i,j]==0) matrixoverlap[j,i] <- 1
    if(img_start[i,j]==0 & img_stop[i,j]>0)  matrixoverlap[j,i] <- 2
    if(img_start[i,j]>0  & img_stop[i,j]>0)  matrixoverlap[j,i] <- 3
    
  }
}
image(matrixoverlap,col=c('black','white','blue','red'),axes=F)


#---------------------------------------

mask <- matrix(c(1/8,1/8,1/8,1/8,0,1/8,1/8,1/8,1/8),3,3)
mask
help(filter2)
id=100
x2 <- stop[id,]%>%matrix(25,25)%>%filter2(filter=mask)
x2
image(matrix(stop[id,],25,25))
image(matrix(x2,25,25))

x <- 1*(x>((0.5)^2))

x <- x2
x <- x%>%matrix(25,25)%>%filter2(filter=mask)
image(matrix(x,25,25))
x <- sqrt(abs(x*x2))
image(matrix(x,25,25))
x <- 1*(x>((0.5)^2))
image(matrix(x,25,25))

zoneInv <- function(x, n = 1, umbral = TRUE){
  mask <- matrix(c(1/8,1/8,1/8,1/8,0,1/8,1/8,1/8,1/8),3,3)
  # deja guardado el primer filtro
  x2 <- x %>% filter2(filter =  mask)
  # itera la convoluvion
  for (k in c(1:n)) {x <- x %>% filter2(filter =  mask)}
  x <- sqrt(abs(x*x2)) # suaviza el valor resultante, cuando n muy grande tiende a abarcar todo o nada
  if(umbral){x <- 1*(x>((0.5)^n))} # umbral para seleccionar la zona
  return(x)
}



zoneInvD <- function(x, n = 2, umbral = T, r = 1){
  # calcula posiciones de mascaras (combinatoria n entre 8)
  if(n>0){maskn <- combn(8,n) %>% apply(2,function(x) ifelse(x>=5, x+1, x))}
  if(n==0){d_maskn <- 1}
  if(n==1){d_maskn <- 8}
  if(n>1){d_maskn <- dim(maskn)[2]}
  # itera la convolucion mascaras n vecinos
  for (j in 1:d_maskn) {
    cm <- rep.int(0,9)
    if(n==1){cm[maskn[j]] <- 1/n}
    if(n>1){cm[maskn[,j]] <- 1/n}
    cm <- cm %>% matrix(nrow = 3, ncol = 3)
    xn <- x %>% filter2(filter = cm)
    if(j==1){xxn <- xn %>% matrix(nrow = 1)}
    if(j>1){xxn <- rbind(xxn, xn %>% matrix(nrow = 1))}
  }
  xxn <- c(1:d_maskn) %>% sapply(function(z){xxn[z,]*1}) %>% t() %>% colMeans()
  if(umbral) {xxn <- 1*(xxn>=(r*sum(x)/625))}
  return(xxn)
}

set.seed(12)
t = sample.int(50000,1)
delta[t]
sugg <- stop[t,] %>% matrix(nrow = 25, ncol = 25) %>% zoneInv(n=1, umbral = F) %>% matrix(nrow = 1)
image(matrix(stop[t,],25,25))
image(matrix(sugg,25,25))
ggcorrplot(matrix(sugg,25,25))
sugg <- rbind(sugg, start[t,], stop[t,], 0.5*stop[t,] + start[t,])



grid.arrange(
  ggcorrplot(sugg[1,] %>% matrix(nrow = 25, ncol = 25), show.legend = F, title = "Conv(stop) delta = 1"),
  ggcorrplot(sugg[2,] %>% matrix(nrow = 25, ncol = 25), show.legend = F, title = "Start Original"),
  ggcorrplot(sugg[3,] %>% matrix(nrow = 25, ncol = 25), show.legend = F, title = "Stop Original"),
  ggcorrplot(sugg[4,] %>% matrix(nrow = 25, ncol = 25), show.legend = F, title = "Stop - Start"),
  nrow = 2,
  top = paste0("Posibles zonas de busqueda") 
)


im <- m
matrix(1,nrow = 25,ncol = 25)
x <- rbind(cbind(im[19:25,19:25],im[19:25,],im[19:25,1:6]),
           cbind(im[,19:25],im,im[,1:6]),
           cbind(im[1:6,19:25],im[1:6,],im[1:6,1:6]))


image(im)
image(x)

# funcion del game of life hacia adelante
gameLifeStep <- function(im, n=1){
  for (k in c(1:n)) {
    x <- rbind(cbind(im,im,im),
               cbind(im,im,im),
               cbind(im,im,im))
    x2 <- matrix(0, ncol = ncol(im), nrow = nrow(im))
    for (i in (nrow(im)+1):(2*nrow(im))) {
      for (j in (ncol(im)+1):(2*ncol(im))) {# sobrepoblacion y subpoblacion por descarte
        if(x[i,j]==0){
          if(sum(x[(i-1):(i+1), (j-1):(j+1)])==3){
            x2[(i-nrow(im)),(j-ncol(im))] = 1 # reproduccion
          }
        }
        if(x[i,j]==1){
          if((sum(x[(i-1):(i+1), (j-1):(j+1)])==3)|(sum(x[(i-1):(i+1), (j-1):(j+1)])==4)){
            x2[(i-nrow(im)),(j-ncol(im))] = 1 # Estasis
          }
        }
      }
    }
    im <- x2
  }
  return(x2)
}

funcObj <- function(x, y, n=1){
  x <- x %>% matrix(nrow = 25, ncol = 25) %>% gameLifeStep(n = n) %>% matrix(nrow = 1)
  return(sum(x!=y)/625)
}



testAG <- function(x, delta, popSizeFactor = 1.25, iters = 10, 
                   mutationChance = 0.005, elitism = NA, verbose = T, suggN = NULL){
  num = dim(x)[1]
  vars = dim(x)[2]
  pop <- matrix(nrow = 1, ncol = vars)
  evalHist <- matrix(nrow = 1, ncol = vars)
  eval <- c()
  model <- list()
  if(verbose){pb <- txtProgressBar(min = 0, max = num, style = 3)}
  
  for (k in 1:num) {
    z <-  x[k,]
    sugg <- z
    sugg <- rbind(sugg, z %>% matrix(nrow = 25, ncol = 25) %>% zoneInvD(n=2, umbral = T)) #2
    sugg <- rbind(sugg, z %>% matrix(nrow = 25, ncol = 25) %>% zoneInvD(n=3, umbral = T)) #3
    for (kk in 1:5) {
      sugg <- rbind(sugg, z %>% matrix(nrow = 25, ncol = 25) %>% zoneInv(n=kk, umbral = T)
                    %>% matrix(nrow = 1)) # 4-8
    }
    if(!is.null(suggN)){sugg <- rbind(sugg, suggN[k,])}
    
    pSize <- round(popSizeFactor*dim(sugg)[1])
    if(pSize<=5){pSize <- 6}
    
    myProb <-rbga.bin(size=vars, 
                      evalFunc = function(x, n=delta[k]){funcObj(x, y = z, n = n)}, 
                      showSettings = F, 
                      popSize = pSize, 
                      iters = ceiling(1.5*sqrt(iters)*sqrt(delta[k]+iters)*log1p(sum(z)*iters)),
                      suggestions = sugg,
                      mutationChance = mutationChance,
                      elitism = elitism,
                      verbose = F)
    
    model[[k]] <- myProb
    pop <-rbind(pop, 
                myProb$population[order(myProb$evaluations)[1],])
    
    if(k<=1){evalHist <- matrix(myProb$evaluations, nrow = 1)}
    if(k>1){evalHist <- rbind(evalHist, myProb$evaluations)}
    
    eval <- c(eval, 
              funcObj(x = matrix(myProb$population[order(myProb$evaluations)[1],], nrow = 25, ncol = 25),
                      y = z,
                      n = delta[k]))
    if(verbose){setTxtProgressBar(pb, k)}
  }
  pop <- pop[-1,]
  if(verbose){close(pb)}
  return(list(eval = eval, pop = pop, evalHist = evalHist, model = model))
}

set.seed(2)
id <- sample.int(50000,11)
p <- testAG(x = stop[id,], delta = delta[id], popSizeFactor = 1.1,
            mutationChance = 0.03, iters = 5*3, elitism = T, verbose = F)
summary(p$eval)


x = stop[id,]
delta = delta[id]

if(!dir.exists("AG test 2/")){dir.create("AG test 2/")}

it <- 5*3
nrow(test)
nrow(train)
seqk <- seq.int(131,250,5)
head(stop)

id <- 1:101
seqk <- seq.int(1,101,by = 5)
stop <- stop[id,]
delta <- delta[id,]
library(future)


delta <- train %>% select(grep("delta", colnames(test)))
stop <- train %>% select(grep("stop_", colnames(test)))

delta <- train %>% select(grep("delta", colnames(train))) %>% as.matrix()
start <- train %>% select(grep("start_", colnames(train))) %>% as.matrix()
stop <- train %>% select(grep("stop_", colnames(train))) %>% as.matrix()

it=15
k=seqk[1]
for (k in seqk) {# debe demorar 15 hrs aprox
   if(k==seqk[1]){cat("================= START =================\n")}
   t1 <- Sys.time()

   id1 <- c(1:20)+20*(k-1)
   id2 <- c(1:20)+20*(k)
   id3 <- c(1:20)+20*(k+1)
   id4 <- c(1:20)+20*(k+2)
   id5 <- c(1:20)+20*(k+3)
   
#    
#    id1 <- c(1:200) + 200*(k-1) # debe demorar alrededor de 5 mins
#    print(paste0('id1:',min(id1),'-',max(id1)))
#    id2 <- c(1:200) + 200*(k) # debe demorar alrededor de 5 mins
#    print(paste0('id2:',min(id2),'-',max(id2)))
#    id3 <- c(1:200) + 200*(k+1) # debe demorar alrededor de 5 mins
#    print(paste0('id3:',min(id3),'-',max(id3)))
#    id4 <- c(1:200) + 200*(k+2) # debe demorar alrededor de 5 mins
#    id5 <- c(1:200) + 200*(k+3) # debe demorar alrededor de 5 mins
# }
   
   p1 <- future({ testAG(x = stop[id1,], delta = delta[id1], popSizeFactor = 1.25,
                         mutationChance = 0.03, iters = it, verbose = F) }) %plan% multiprocess
   
   p2 <- future({ testAG(x = stop[id2,], delta = delta[id2], popSizeFactor = 1.25,
                         mutationChance = 0.03, iters = it, verbose = F) }) %plan% multiprocess
   
   p3 <- future({ testAG(x = stop[id3,], delta = delta[id3], popSizeFactor = 1.25,
                         mutationChance = 0.03, iters = it, verbose = F) }) %plan% multiprocess
   p4 <- future({ testAG(x = stop[id4,], delta = delta[id4], popSizeFactor = 1.25,
                         mutationChance = 0.03, iters = it, verbose = F) }) %plan% multiprocess
   p5 <- future({ testAG(x = stop[id5,], delta = delta[id5], popSizeFactor = 1.25,
                         mutationChance = 0.03, iters = it, verbose = F) }) %plan% multiprocess
   print('end1')

   pp1 <- value(p1)
   pp2 <- value(p2)
   pp3 <- value(p3)
   pp4 <- value(p4)
   pp5 <- value(p5)
   print('end2')
   saveRDS(pp1, paste0("AG test 2/AG_test_", k, ".rds"))
   saveRDS(pp2, paste0("AG test 2/AG_test_", k+1, ".rds"))
   saveRDS(pp3, paste0("AG test 2/AG_test_", k+2, ".rds"))
   saveRDS(pp4, paste0("AG test 2/AG_test_", k+3, ".rds"))
   saveRDS(pp5, paste0("AG test 2/AG_test_", k+4, ".rds"))
   print('c')
   t2 <- Sys.time()

   cat(paste0("---- iter ", k," ----", "\n"))
   print(t2 -t1)
   cat("                                \n")
   cat(paste0("loss ",k,": ", summary(pp1$eval)[4]), "\n")
   cat(paste0("loss ",k+1,": ", summary(pp2$eval)[4]), "\n")
   cat(paste0("loss ",k+2,": ", summary(pp3$eval)[4]), "\n")
   cat(paste0("loss ",k+3,": ", summary(pp4$eval)[4]), "\n")
   cat(paste0("loss ",k+4,": ", summary(pp5$eval)[4]), "\n")
   cat("=========================================\n")

}
