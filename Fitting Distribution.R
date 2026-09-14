library(fitdistrplus)
library(MASS)
library(GLDEX)
#Import data return hasil prediksi BPNN
ret.prediksi <-read_excel("MAPA.xlsx",sheet="MAPA",col_names=T)
ret.prediksi <- var_estimates2
ret.prediksi <- as.numeric(as.vector(ret.prediksi))
#Pendugaan sesuai distribusi umum
par(mfrow=c(1,1))
A <- descdist(ret.prediksi)
#Pendugaan Distribusi Menggunakan Generalize Lambda
#Estimasi parameter
dist.pre <- fun.data.fit.mm(ret.prediksi)
dist.pre
#Menggunakan 1000000 sampel acak untuk menentukan metode distribusi
# generalized lambda
# GLD RS
set.seed(99)
rs_sample.pre <- rgl(n = 1000000, lambda1=dist.pre[1,1], lambda2 =
                       dist.pre[2,1],lambda3 = dist.pre[3,1],lambda4 = dist.pre[4,1],param
                     = "rs")
# GLD FMKL
set.seed(99)
fmkl_sample.pre <- rgl(n = 1000000, lambda1=dist.pre[1,2],
                       lambda2=dist.pre[2,2], lambda3 = dist.pre[3,2],lambda4 =
                         dist.pre[4,2],param = "fmkl")
fun.moments.r(rs_sample.pre, normalise="Y")
fun.moments.r(fmkl_sample.pre, normalise="Y")
fun.moments.r(ret.prediksi, normalise="Y")
#Mendapatkan tingkat akurasi
accuracy(rs_sample.pre,ret.prediksi)
accuracy(fmkl_sample.pre,ret.prediksi)
#Visualisasi pendugaan distribusi Generalized Lambda
fun.plot.fit(fit.obj = dist.pre, data = ret.prediksi, nclass =
               100,param = c("rs", "fmkl"), xlab ="Returns")
#Pemilihan parameter distribusi yang akan digunakan adalah dengan
#meninjau nilai akurasi dengan tingkat error yang paling kecil
#GLD FMKL yang dipilih
