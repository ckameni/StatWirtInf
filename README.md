# StatWirtInf
statistik Kurs ostfalia WirtschaftsInformatik

Code für Simulationen
################### 5 Wuerfel #########################

k <- 100000
s5 <- c()
for (i in 1:k){
  x <- sample(6,5,replace=T)
  s5 <- c(s5,sum(x)) 
}

length(subset(s5,s5<=6))/k




################# Wei�e Wuerfel #########################


k <- 10000
ball1 <- c()
ball2 <- c()
for (i in 1:k){
  draw2 <- sample(c("w","w","b","b"),2,replace=F)
  ball1 <- c(ball1,draw2[1])
  ball2 <- c(ball2,draw2[2])
}

b1gb2w <- subset(ball1,ball2=="w")

nob2w <- length(b1gb2w)

length(subset(b1gb2w,b1gb2w=="w"))/nob2w




################ Seltene Krankheit ########################


k <- 100000
person <- sample(c("g","k"),k,replace=T,prob=c(0.995,0.005)) 
testerg <- c()
for (i in 1:k){
  if (person[i]=="k"){
    p.pos <- 0.99
  }else{
    p.pos <- 0.02
  }
  test.i <- sample(c("p","n"),1,prob=c(p.pos,1-p.pos)) 
  testerg <- c(testerg,test.i)
}

ppostest <- subset(person,testerg=="p")
ppostest.k <- subset(person,testerg=="p" & person=="k")

length(ppostest.k)/length(ppostest)




########## Monte-Carlo-Integration ##########################

k <- 100000
x <- runif(k,min=0,max=2)
y <- runif(k,min=0,max=4)

pucx <- subset(x,x^2>y)
8*length(pucx)/k
