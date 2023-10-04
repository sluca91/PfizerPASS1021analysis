


m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
m

n <- cbind(m, 8:14) # insert a column
n <- n[,c(1,3,2)]
n
m <- cbind(m, 8:14)[, c(1, 3, 2)] # insert a column
m


cbind(1:7, diag(3)) # vector is subset -> warning
cbind(1:7, diag(7)) # no warning
cbind(1:7) # first part
cbind(diag(7)) #second part



cbind(0, rbind(1, 1:3))
rbind(1,1:3)
cbind(0,1:3)


cbind(I = 0, X = rbind(a = 1, b = 1:3))  # use some names

X = rbind(a = 1, b = 1:3)
X
x

xx <- data.frame(I = rep(0,2))
xx
yy <- data.frame(I = rep(0,5))
yy

cbind(xx, X = rbind(a = 1, b = 1:3))   # named differently

cbind(xx,X)

cbind(0, matrix(1, nrow = 0, ncol = 4)) #> Warning (making sense)
dim(cbind(0, matrix(1, nrow = 2, ncol = 0))) #-> 2 x 1

## deparse.level
dd <- 10
rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 0) # middle 2 rownames
rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 1) # 3 rownames (default)
rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 2) # 4 rownames

## cheap row names:
b0 <- gl(3,4, labels=letters[1:3])
bf <- setNames(b0, paste0("o", seq_along(b0)))
df  <- data.frame(a = 1, B = b0, f = gl(4,3))
df. <- data.frame(a = 1, B = bf, f = gl(4,3))
new <- data.frame(a = 8, B ="B", f = "1")
(df1  <- rbind(df , new))
(df.1 <- rbind(df., new))
stopifnot(identical(df1, rbind(df,  new, make.row.names=FALSE)),
          identical(df1, rbind(df., new, make.row.names=FALSE)))