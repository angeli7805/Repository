j=24
k=55
m=8
x <- sample(1:m-1,k,replace=T)
y <- numeric(length(x))
for(i in 1:length(x)){
  y[k+1-i]=x[i]
}
rep = 200
while (rep > 0){
  y[k] = (y[k]+y[j])%%m
  print(y[k])
  k = k-1
  j = j-1
  if(j==0) j <- 55
  if(k==0) k <- 55
  rep = rep - 1
}