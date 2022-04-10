#No.1 A

x1a = 3
p1a = 0.8
q1a = 1 - p1a
P1a = dgeom(x1a, p1a)
p1a

#No.1 b
mean(rgeom(n = 10000, prob = p1a) == 3)
mean

#No.1 c
#Dari situ, dapat disimpulkan bahwa P(X=x) distribusi geometrik dengan peluang p dapat diestimasi dengan
#menghitung rerata kemunculan angka x pada serangkaian bilangan random berdistribusi geometrik dengan peluang p.

#No.1 d
n = 10000
hist(rgeom(n, prob = p1a))

#No.1 e
varian.1e = 1-p1a/p1a
print(varian.1e)


#No. 2 a
comb = function(n, x){
  temp = factorial(n) / factorial(n-x)/factorial(x)
  return (temp)
}

n_2a = 20
x_2a = 4
p_2a = 0.2

peluang_2a = (comb(n_2a, x_2a)*(p_2a^x_2a)*((p_2a-1)^(n_2a-x_2a)))
peluan_2a

#No.2 b
plot(0:20, dbinom(0:20, n_2a, p_2a),type='h')
hist(rbinom(4, n_2a, prob = p_2a), ylab = "Frequency", main = "Binomial Histogram")

#No.2 c
#mean
mean_2c = (n_2a * p_2a)
print(mean_2c)

#varian
varian_2c = (n_2a * p_2a * (1-p_2a))
print(varian_2c)

#No.3 a
lambda_3a = 4.5
dpois(6, lambda_3a)

#No.3 b
hist(rpois(365, lambda_3a), main = "Poisson Histogram")

#No.3 d
lambda_3d = lambda_3a
varian_3d = lambda_3a
print(lambda_3d)
print (varian_3d)

#No.4 a
x_4a = 2
v_4a = 10
dchisq(x_4a, v_4a)

#No.4 b
set.seed(0)
x_4b = 1:100
y_4b <- rchisq(x_4b, 10)

hist(y_4b)

#No.4 c
mean_4c = v_4a
varian_4c = 2 * v_4a
print (mean_4c)
print (varian_4c)

#No.5 a
lambda_5a = 3
dexp(lambda_5a)


#No.5 b
set.seed(1)
hist(rexp(10, rate=lambda_5a))

set.seed(1)
hist(rexp(100, rate=lambda_5a))

set.seed(1)
hist(rexp(1000, rate=lambda_5a))

set.seed(1)
hist(rexp(10000, rate=lambda_5a))

#No.5 c
n_5c = 100
rataan_5c = mean(rexp(n_5c, rate = lambda_5a))
varian_5c = (sd(rexp(n_5c, rate = lambda_5a))) ^ 2

rataan_5c
varian_5c

#No.6 a

n_6a = 100
mean_6a = 50
sd_6a = 8

data_6a = rnorm(n_6a, mean_6a, sd_6a)
dataMean_6a = mean(data_6a)
dataSD_6a = sd(data_6a)

floor_6a = floor(dataMean_6a)
ceil_6a = ceiling(dataMean_6a)

z1_6a = ((floor_6a - dataMean_6a)/dataSD_6a )
z2_6a = 1 - ((ceil_6a - dataMean_6a)/dataSD_6a)
z_6a = z1_6a + z2_6a
z_6a

plot(data_6a, dnorm(data_6a, dataMean_6a, dataSD_6a))


#No.6 b
h = hist(rnorm(100, 50, 8), breaks = 50, main="5025201239_Kadek Ari Dharmika_Probstat_D_DNhistogram")

#6c
varian_6c = sd_6a * sd_6a
Varian_6c
