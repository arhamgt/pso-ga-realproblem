library(mopsocd)

RAW_MATERIAL <- read.csv("Table 1.csv")
PRODUCT_PROFIT <- read.csv("Table 2.csv")
PRODUCT_MATERIAL <- read.csv("Table 3.csv")

fab_x <- PRODUCT_MATERIAL$fabric
yar_x <- PRODUCT_MATERIAL$yarn
rub_x <- PRODUCT_MATERIAL$rubber
zip_x <- PRODUCT_MATERIAL$rubber
rop_x <- PRODUCT_MATERIAL$rope
sil_x <- PRODUCT_MATERIAL$silicon

for(i in 1:20){
  cat(paste("(xn[",i,"] * PRODUCT_PROFIT[",i,",\"price\"])",sep=""),end="+ ")
}



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

fungsi_cons <- function(xn){
  g1 <- ((xn[1] *  0.629 ) + (xn[2] *  0.441 ) + (xn[3] *  0.911 ) + (xn[4] *  0.897 ) + (xn[5] *  0.629 ) + (xn[6] *  0.288 ) + (xn[7] *  0.75 ) + (xn[8] *  3.96 ) + (xn[9] *  3.52 ) + (xn[10] *  3.3 ) + (xn[11] *  2.64 ) + (xn[12] *  2.2 ) + (xn[13] *  1.98 ) + (xn[14] *  0.48 ) + (xn[15] *  0.32 ) + (xn[16] *  0.72 ) + (xn[17] *  0.72 ) + (xn[18] *  0.24 ) + (xn[19] *  0.24 ) + (xn[20] *  0.665 )) <= 2000.0
  g2 <- ((xn[1] *  3.867 ) + (xn[2] *  3.437 ) + (xn[3] *  4.297 ) + (xn[4] *  4.297 ) + (xn[5] *  3.291 ) + (xn[6] *  3.916 ) + (xn[7] *  4.99 ) + (xn[8] *  8 ) + (xn[9] *  7.6 ) + (xn[10] *  7.4 ) + (xn[11] *  6.8 ) + (xn[12] *  6.4 ) + (xn[13] *  6.2 ) + (xn[14] *  3.683 ) + (xn[15] *  3.291 ) + (xn[16] *  4.071 ) + (xn[17] *  4.071 ) + (xn[18] *  3.157 ) + (xn[19] *  3.727 ) + (xn[20] *  4.687 )) <= 9282.0
  g3 <- ((xn[1] *  0 ) + (xn[2] *  0 ) + (xn[3] *  0 ) + (xn[4] *  0 ) + (xn[5] *  0 ) + (xn[6] *  0 ) + (xn[7] *  0 ) + (xn[8] *  200 ) + (xn[9] *  180 ) + (xn[10] *  160 ) + (xn[11] *  140 ) + (xn[12] *  120 ) + (xn[13] *  120 ) + (xn[14] *  0 ) + (xn[15] *  0 ) + (xn[16] *  0 ) + (xn[17] *  0 ) + (xn[18] *  0 ) + (xn[19] *  0 ) + (xn[20] *  0 )) <= 8000.0
  g4 <- ((xn[1] *  0 ) + (xn[2] *  0 ) + (xn[3] *  0 ) + (xn[4] *  0 ) + (xn[5] *  0 ) + (xn[6] *  0 ) + (xn[7] *  0 ) + (xn[8] *  200 ) + (xn[9] *  180 ) + (xn[10] *  160 ) + (xn[11] *  140 ) + (xn[12] *  120 ) + (xn[13] *  120 ) + (xn[14] *  0 ) + (xn[15] *  0 ) + (xn[16] *  0 ) + (xn[17] *  0 ) + (xn[18] *  0 ) + (xn[19] *  0 ) + (xn[20] *  0 )) <= 9000.0
  g5 <- ((xn[1] *  0 ) + (xn[2] *  0 ) + (xn[3] *  0 ) + (xn[4] *  0 ) + (xn[5] *  0 ) + (xn[6] *  96 ) + (xn[7] *  150 ) + (xn[8] *  0 ) + (xn[9] *  0 ) + (xn[10] *  0 ) + (xn[11] *  0 ) + (xn[12] *  0 ) + (xn[13] *  0 ) + (xn[14] *  0 ) + (xn[15] *  0 ) + (xn[16] *  0 ) + (xn[17] *  0 ) + (xn[18] *  0 ) + (xn[19] *  0 ) + (xn[20] *  0 )) <= 6500.0
  g6 <- ((xn[1] *  0 ) + (xn[2] *  0 ) + (xn[3] *  0 ) + (xn[4] *  0 ) + (xn[5] *  0 ) + (xn[6] *  0 ) + (xn[7] *  0 ) + (xn[8] *  0 ) + (xn[9] *  0 ) + (xn[10] *  0 ) + (xn[11] *  0 ) + (xn[12] *  0 ) + (xn[13] *  0 ) + (xn[14] *  1 ) + (xn[15] *  0.65 ) + (xn[16] *  1.5 ) + (xn[17] *  1.5 ) + (xn[18] *  0.25 ) + (xn[19] *  0.6 ) + (xn[20] *  1.5 )) <= 200.0
  return(c(g1,g2,g3,g4,g5,g6))
}

lband <- rep(0,20)
uband <- rep(100,20)

data_pso <- mopsocd(fn=fungsi_objektif,gn=fungsi_cons,varcnt=20,fncnt=1 ,
                    lowerbound=lband,upperbound=uband,opt=1,
                    popsize = 200,verbosity=0,w=0.729,c1=1.49445,
                    c2=1.49445,maxgen=10000,pMut=0,archivesize = 250)
print(data_pso$paramvalues)
print(data_pso$objfnvalues)

# fab_x <- PRODUCT_MATERIAL$fabric
# yar_x <- PRODUCT_MATERIAL$yarn
# rub_x <- PRODUCT_MATERIAL$rubber
# zip_x <- PRODUCT_MATERIAL$rubber
# rop_x <- PRODUCT_MATERIAL$rope
# sil_x <- PRODUCT_MATERIAL$silicon


cat(paste("Fabric",sum(data_pso$paramvalues * fab_x),"Batas Fabric",
          RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="fabric","stock"],"",sep="\n"))
cat(paste("Yarn",sum(data_pso$paramvalues * yar_x),"Batas Yarn",
          RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="yarn","stock"],"",sep="\n"))
cat(paste("Rubber",sum(data_pso$paramvalues * rub_x),"Batas Rubber",
          RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="rubber","stock"],"",sep="\n"))
cat(paste("Zipper",sum(data_pso$paramvalues * zip_x),"Batas Zipper",
          RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="zipper","stock"],"",sep="\n"))
cat(paste("Rope",sum(data_pso$paramvalues * rop_x),"Batas Rope",
          RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="rope","stock"],"",sep="\n"))
cat(paste("Silicon",sum(data_pso$paramvalues * sil_x),"Batas Silicon",
          RAW_MATERIAL[RAW_MATERIAL["raw.materials"]=="silicon","stock"],"",sep="\n"))

