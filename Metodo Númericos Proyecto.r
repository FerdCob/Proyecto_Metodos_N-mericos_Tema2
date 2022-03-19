#Definimos la función
f <- function(x){
    return(x-cos(x))
}
#Definimos el MÉTODO DE BISECCIÓN
bisection <- function(a,b){
    if(a>b){
        return(bisection(b,a))
    }
    c=0.5*(a+b)
    if (abs (f(c))<=1E-10){
        return(c)
    }
    if(f(a)*f(c)<0){
        return (bisection(a,c))
    }
    if(f(c)*f(b)<0){
        return(bisection(c,b))
    }
}
#Calculamos la raíz de f
raíz1=bisection(0,1)
#Mostramos resultados
cat("Método de bisección\t")
cat("Raíz:")
raíz1

#Definimos la derivada de la función
df <- function(x){
    return(1+sin(x))
}
#Definimos el MÉTODO DE NEWTON
tol=0.001
g <- 1.5
newton <- function (f,df,g,tol){
    x=g
    while(abs(f(x))>tol){
        x=x-f(x)/df(x)
    }
    x
}
cat("Método de Newton\t")
cat("Raíz:")
newton(f,df,g,tol)

#Definimos el MÉTODO DE SECANTE
secant <- function(fun,x0,x1,tol=1e-07,niter=5){
    for(i in 1:niter){
    x2<-x1-fun(x1)*(x1-x0)/(fun(x1)-f(x0))
    if (abs(fun(x2))<tol)
    return(x2)
    x0 <- x1
    x1 <- x2
    }
    stop("Excede el número de interaciones")
}
cat("Método de Secante\t")
cat("Raíz:")
 secant(f,x0=1,x1=2)