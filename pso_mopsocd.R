library(mopsocd)
library(stringr)

set.seed(1234)
RAW_MATERIAL <- read.csv("Table 1.csv")
PRODUCT_PROFIT <- read.csv("Table 2.csv")
PRODUCT_MATERIAL <- read.csv("Table 3.csv")

fab_x <- PRODUCT_MATERIAL$fabric
yar_x <- PRODUCT_MATERIAL$yarn
rub_x <- PRODUCT_MATERIAL$rubber
zip_x <- PRODUCT_MATERIAL$zipper
rop_x <- PRODUCT_MATERIAL$rope
sil_x <- PRODUCT_MATERIAL$silicon

real_fab <- RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="fabric","stock"]
real_yar <- RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="yarn","stock"]
real_rub <- RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="rubber","stock"]
real_zip <- RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="zipper","stock"]
real_rop <- RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="rope","stock"]
real_sil <- RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="silicon","stock"]


# for(i in 1:20){
#   cat(paste("(xn[",i,"] * ",zip_x[i],")",sep=" "),end="+ ")
# }



fungsi_utama <- function(xn){
  total = ((xn[1] *   7000 ) + (xn[2] *   15000 ) + (xn[3] *   8000 ) + (xn[4] *   0 ) + (xn[5] *   10000 ) + (xn[6] *   10000 ) + (xn[7] *   7000 ) + (xn[8] *   25000 ) + (xn[9] *   20000 ) + (xn[10] *   15000 ) + (xn[11] *   10000 ) + (xn[12] *   7000 ) + (xn[13] *   5000 ) + (xn[14] *   21000 ) + (xn[15] *   14000 ) + (xn[16] *   24000 ) + (xn[17] *   24000 ) + (xn[18] *   11000 ) + (xn[19] *   8000 ) + (xn[20] *   26000))
  production = sum(xn)
    
  if(production > 600){
      minus = 500 * (production - 600)
      total = total - minus
  }
  return(total)
}

fungsi_objektif <- function(xn){
  f1 <- fungsi_utama(xn)
  return(c(f1))
}


fungsi_batas<- function(xn){
  totalku = 0
  nilai_min = min(xn)
  if(xn[1] >= nilai_min){
    xn[1] = 0
  }
  totalku = totalku + xn[1]
  
  if(xn[2] >= nilai_min){
    xn[2] = 0
  }
  totalku = totalku + xn[2]
  
  if(xn[3] >= nilai_min){
    xn[3] = 0
  }
  totalku = totalku + xn[3]
  
  if(xn[4] >= nilai_min){
    xn[4] = 0
  }
  totalku = totalku + xn[4]
  
  if(xn[5] >= nilai_min){
    xn[5] = 0
  }
  totalku = totalku + xn[5]
  
  if(xn[6] >= nilai_min){
    xn[6] = 0
  }
  totalku = totalku + xn[6]
  
  if(xn[7] >= nilai_min){
    xn[7] = 0
  }
  totalku = totalku + xn[7]
  
  if(xn[8] >= nilai_min){
    xn[8] = 0
  }
  totalku = totalku + xn[8]
  
  if(xn[9] >= nilai_min){
    xn[9] = 0
  }
  totalku = totalku + xn[9]
  
  if(xn[10] >= nilai_min){
    xn[10] = 0
  }
  totalku = totalku + xn[10]
  
  if(xn[11] >= nilai_min){
    xn[11] = 0
  }
  totalku = totalku + xn[11]
  
  if(xn[12] >= nilai_min){
    xn[12] = 0
  }
  totalku = totalku + xn[12]
  
  if(xn[13] >= nilai_min){
    xn[13] = 0
  }
  totalku = totalku + xn[13]
  
  if(xn[14] >= nilai_min){
    xn[14] = 0
  }
  totalku = totalku + xn[14]
  
  if(xn[15] >= nilai_min){
    xn[15] = 0
  }
  totalku = totalku + xn[15]
  
  if(xn[16] >= nilai_min){
    xn[16] = 0
  }
  totalku = totalku + xn[16]
  
  if(xn[17] >= nilai_min){
    xn[17] = 0
  }
  totalku = totalku + xn[17]
  
  if(xn[18] >= nilai_min){
    xn[18] = 0
  }
  totalku = totalku + xn[18]
  
  if(xn[19] >= nilai_min){
    xn[19] = 0
  }
  totalku = totalku + xn[19]
  
  if(xn[20] >= nilai_min){
    xn[20] = 0
  }
  totalku = totalku + xn[20]
  
  return(100*totalku)
}

fungsi_cons <- function(xn){
  # fabric
  g1 <- ((xn[1] * 0.629) + (xn[2] * 0.441) + (xn[3] * 0.911) + (xn[4] * 0.897) + (xn[5] * 0.629) + (xn[6] * 0.288) + (xn[7] * 0.75) + (xn[8] * 3.96) + (xn[9] * 3.52) + (xn[10] * 3.3) + (xn[11] * 2.64) + (xn[12] * 2.2) + (xn[13] * 1.98) + (xn[14] * 0.48) + (xn[15] * 0.32) + (xn[16] * 0.72) + (xn[17] * 0.72) + (xn[18] * 0.24) + (xn[19] * 0.24) + (xn[20] * 0.665)) <= 2000.0
  
  # yarn
  g2 <- ((xn[1] * 3.867) + (xn[2] * 3.437) + (xn[3] * 4.297) + (xn[4] * 4.297) + (xn[5] * 3.291) + (xn[6] * 3.916) + (xn[7] * 4.99) + (xn[8] * 8) + (xn[9] * 7.6) + (xn[10] * 7.4) + (xn[11] * 6.8) + (xn[12] * 6.4) + (xn[13] * 6.2) + (xn[14] * 3.683) + (xn[15] * 3.291) + (xn[16] * 4.071) + (xn[17] * 4.071) + (xn[18] * 3.157) + (xn[19] * 3.727) + (xn[20] * 4.687)) <= 9282.0
  
  # rubber
  g3 <- ((xn[1] * 0) + (xn[2] * 0) + (xn[3] * 0) + (xn[4] * 0) + (xn[5] * 0) + (xn[6] * 0) + (xn[7] * 0) + (xn[8] * 200) + (xn[9] * 180) + (xn[10] * 160) + (xn[11] * 140) + (xn[12] * 120) + (xn[13] * 120) + (xn[14] * 0) + (xn[15] * 0) + (xn[16] * 0) + (xn[17] * 0) + (xn[18] * 0) + (xn[19] * 0) + (xn[20] * 0)) <= 8000.0
  
  # zipper
  g4 <- ((xn[1] * 50) + (xn[2] * 50) + (xn[3] * 50) + (xn[4] * 70) + (xn[5] * 40) + (xn[6] * 0) + (xn[7] * 0) + (xn[8] * 0) + (xn[9] * 0) + (xn[10] * 0) + (xn[11] * 0) + (xn[12] * 0) + (xn[13] * 0) + (xn[14] * 20) + (xn[15] * 25) + (xn[16] * 30) + (xn[17] * 25) + (xn[18] * 15) + (xn[19] * 10) + (xn[20] * 15)) <= 9000.0
  
  # rope
  g5 <- ((xn[1] * 0) + (xn[2] * 0) + (xn[3] * 0) + (xn[4] * 0) + (xn[5] * 0) + (xn[6] * 96) + (xn[7] * 150) + (xn[8] * 0) + (xn[9] * 0) + (xn[10] * 0) + (xn[11] * 0) + (xn[12] * 0) + (xn[13] * 0) + (xn[14] * 0) + (xn[15] * 0) + (xn[16] * 0) + (xn[17] * 0) + (xn[18] * 0) + (xn[19] * 0) + (xn[20] * 0)) <= 6500.0
  
  # silicon
  g6 <- ((xn[1] * 0) + (xn[2] * 0) + (xn[3] * 0) + (xn[4] * 0) + (xn[5] * 0) + (xn[6] * 0) + (xn[7] * 0) + (xn[8] * 0) + (xn[9] * 0) + (xn[10] * 0) + (xn[11] * 0) + (xn[12] * 0) + (xn[13] * 0) + (xn[14] * 1) + (xn[15] * 0.65) + (xn[16] * 1.5) + (xn[17] * 1.5) + (xn[18] * 0.25) + (xn[19] * 0.6) + (xn[20] * 1.5)) <= 200.0
  # g7 <- fungsi_batas(xn) <= 10000
  # g7 <- xn[1] %% 1 == 0 
  # g8 <- xn[2] %% 1 == 0 
  # g9 <- xn[3] %% 1 == 0 
  # g10 <- xn[4] %% 1 == 0 
  # g11 <- xn[5] %% 1 == 0 
  # g12 <- xn[6] %% 1 == 0 
  # g13 <- xn[7] %% 1 == 0 
  # g14 <- xn[8] %% 1 == 0 
  # g15 <- xn[9] %% 1 == 0 
  # g16 <- xn[10] %% 1 == 0 
  # g17 <- xn[11] %% 1 == 0 
  # g18 <- xn[12] %% 1 == 0 
  # g19 <- xn[13] %% 1 == 0 
  # g20 <- xn[14] %% 1 == 0 
  # g21 <- xn[15] %% 1 == 0 
  # g22 <- xn[16] %% 1 == 0 
  # g23 <- xn[17] %% 1 == 0 
  # g24 <- xn[18] %% 1 == 0 
  # g25 <- xn[19] %% 1 == 0 
  # g26 <- xn[20] %% 1 == 0 

  # return(c(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,
  #          g20,g21,g22,g23,g24,g25,g26))
  return(c(g1,g2,g3,g4,g5,g6))
}


# uband <- c(180,180,180,128,225,67,43,40,44,50,
#            57,66,66,200,307,133,133,600,333,133)

# for(up_item in 50:30){
#   lband <- rep(0,20)
#   uband <- rep(up_item,20)
#   set.seed(1234)
#   data_pso <- mopsocd(fn=fungsi_objektif,gn=fungsi_cons,varcnt=20,fncnt=1 ,
#                       lowerbound=lband,upperbound=uband,opt=1,
#                       popsize = 200,verbosity=0,w=0.729,c1=1.49445,
#                       c2=1.49445,maxgen=10000,pMut=0)
# 
#   pred_fab <- sum(data_pso$paramvalues * fab_x)
#   pred_yar <- sum(data_pso$paramvalues * yar_x)
#   pred_rub <- sum(data_pso$paramvalues * rub_x)
#   pred_zip <- sum(data_pso$paramvalues * zip_x)
#   pred_rop <- sum(data_pso$paramvalues * rop_x)
#   pred_sil <- sum(data_pso$paramvalues * sil_x)
# 
#   kond_fab <- pred_fab <= real_fab
#   kond_yar <- pred_yar <= real_yar
#   kond_rub <- pred_rub <= real_rub
#   kond_zip <- pred_zip <= real_zip
#   kond_rop <- pred_rop <= real_rop
#   kond_sil <- pred_sil <= real_sil
# 
#   if(kond_fab && kond_yar && kond_rub && kond_zip  && kond_rop && kond_sil){
#     print(up_item)
#   }
# }

lband <- rep(0,20)
uband <- rep(25,20)

data_pso <- mopsocd(fn=fungsi_objektif,gn=fungsi_cons,varcnt=20,fncnt=1 ,
                      lowerbound=lband,upperbound=uband,opt=1,
                      popsize = 200,verbosity=0,w=0.729,c1=1.49445,
                      c2=1.49445,maxgen=10000,pMut=0)

data_pso_v <- capture.output(mopsocd(fn=fungsi_objektif,gn=fungsi_cons,varcnt=20,fncnt=1 ,
                                     lowerbound=lband,upperbound=uband,opt=1,
                                     popsize = 200,verbosity=3,w=0.729,c1=1.49445,
                                     c2=1.49445,maxgen=10000,pMut=0))

print(data_pso$paramvalues)
print(data_pso$objfnvalues)

pred_fab <- sum(data_pso$paramvalues * fab_x)
pred_yar <- sum(data_pso$paramvalues * yar_x)
pred_rub <- sum(data_pso$paramvalues * rub_x)
pred_zip <- sum(data_pso$paramvalues * zip_x)
pred_rop <- sum(data_pso$paramvalues * rop_x)
pred_sil <- sum(data_pso$paramvalues * sil_x)

kond_fab <- pred_fab <= real_fab
kond_yar <- pred_yar <= real_yar
kond_rub <- pred_rub <= real_rub
kond_zip <- pred_zip <= real_zip
kond_rop <- pred_rop <= real_rop
kond_sil <- pred_sil <= real_sil

# fab_x <- PRODUCT_MATERIAL$fabric
# yar_x <- PRODUCT_MATERIAL$yarn
# rub_x <- PRODUCT_MATERIAL$rubber
# zip_x <- PRODUCT_MATERIAL$rubber
# rop_x <- PRODUCT_MATERIAL$rope
# sil_x <- PRODUCT_MATERIAL$silicon

# cat(paste("Fabric",sum(data_pso$paramvalues * fab_x),"Batas Fabric",
#           RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="fabric","stock"],"",sep="\n"))
# 
# cat(paste("Yarn",sum(data_pso$paramvalues * yar_x),"Batas Yarn",
#           RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="yarn","stock"],"",sep="\n"))
# 
# cat(paste("Rubber",sum(data_pso$paramvalues * rub_x),"Batas Rubber",
#           RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="rubber","stock"],"",sep="\n"))
# 
# cat(paste("Zipper",sum(data_pso$paramvalues * zip_x),"Batas Zipper",
#           RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="zipper","stock"],"",sep="\n"))
# 
# cat(paste("Rope",sum(data_pso$paramvalues * rop_x),"Batas Rope",
#           RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="rope","stock"],"",sep="\n"))
# 
# cat(paste("Silicon",sum(data_pso$paramvalues * sil_x),"Batas Silicon",
#           RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="silicon","stock"],"",sep="\n"))

# lak <- c()
# for(i in 1:20){
#   lak <- c(lak,(min(RAW_MATERIAL$stock/unname(unlist(c(PRODUCT_MATERIAL[i,2:7]))))))
# }
# print(floor(lak))

# for(i in 1:20){
# cat(paste("  if(xn[",i,"] >= nilai_min){
#       xn[",i,"] = 0
#     }
#   totalku = totalku + xn[",i,"]",sep="",end="\n\n"))
# }

# for(i in 1:20){
# cat(paste("g",i+6," <- xn[",i,"] %% 1 == 0",sep=""),end="\n")}

# for(i in 1:20){
#   cat(paste("g",i+6,sep=""),end=",",sep="")}

write(data_pso_v,file="pso_verbose.txt")
if(kond_fab && kond_yar && kond_rub && kond_zip  && kond_rop && kond_sil){
  print("Benar")
}

data_penulis <- readLines("pso_verbose.txt")

letak_value <- which(str_detect(data_penulis,"Objective Function Values:")==TRUE)
letak_value <- letak_value + 1
val_func <- data_penulis[letak_value]
val_func_has <- lapply(val_func,substr,start=5,stop=1000)
val_func_has <- lapply(val_func_has,strtoi,base=10)
val_func_has <- unlist(val_func_has)

plot(val_func_has,main="Graphic Fitness Function",type="l",col="blue")

pred_fab <- sum(floor(data_pso$paramvalues) * fab_x)
pred_yar <- sum(floor(data_pso$paramvalues) * yar_x)
pred_rub <- sum(floor(data_pso$paramvalues) * rub_x)
pred_zip <- sum(floor(data_pso$paramvalues) * zip_x)
pred_rop <- sum(floor(data_pso$paramvalues) * rop_x)
pred_sil <- sum(floor(data_pso$paramvalues) * sil_x)


print(floor(data_pso$paramvalues))
print(paste("pred_fab",pred_fab))
print(paste("pred_yar",pred_yar))
print(paste("pred_rub",pred_rub))
print(paste("pred_zip",pred_zip))
print(paste("pred_rop",pred_rop))
print(paste("pred_sil",pred_sil))