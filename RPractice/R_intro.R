is_prime <- function(n){
  if(n == 1 || n == 2){
    return(TRUE)
  }
  for(i in 2 : sqrt(n)){
    if(n %% i == 0){
      return(FALSE)
    }
  }
  return(TRUE)
}

fact <- function(n){
  if(n == 1){
    return(1)
  }
  return(fact(n-1)*n)
}

fact_iter <- function(n){
  return(Reduce('*', 1 : n))
}

is_palin <- function(s){
  for(i in 1:nchar(s)){
    if(substring(s,i,i) != substring(s, nchar(s)-i+1, nchar(s)-i+1)){
      return(FALSE)
    }
  }
  return(TRUE)
}

pyramid <- function(n){
  for(i in 1:n){
    s = ""
    for(j in 1:i){
      s = paste(s, "*", sep = "")
    }
    print(s)
  }
}

armstrong <- function(n){
  len = nchar(toString(n))
  temp = n
  sum<-0
  for(i in 1:len){
    sum = sum + ((n%%10)^len)
    n = n %/% 10
  }
  if(sum == temp){
    return(TRUE)
  }
  else{
  return(FALSE)
  }
}

reverseNumber <- function(n){
  rev = 0
  i = nchar(toString(n)) - 1
  while(n > 0){
    rev = rev + ((n%%10)*(10^i))
    n = n %/% 10
    i = i - 1
  }
  return(rev)
}

fib <- function(n){
  f <- 1 : n
  for(i in 3:n){
    f[i] = f[i-1] + f[i-2]
  }
  return(f[n])
}

num_vow <- function(s){
  vow = c('a', 'e', 'i', 'o','u')
  cou = 0
  for(i in strsplit(s, "")[[1]]){
    if(i %in% vow){
      cou = cou + 1
    }
  }
  return(cou)
}