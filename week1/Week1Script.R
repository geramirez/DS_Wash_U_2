# use # for comments

help(table)
?help
??help
apropos("tab")


a = "c"
b = 7

d = c(a,b)
typeof(d)
d[2]
as.numeric(d[2])

sizes = c("s", "m", "m", "l", "s")

d = factor(sizes)
d[2]
as.numeric(d[2])
d[2] > d[1]

d = ordered(sizes)
d[2] > d[1]
d = factor(sizes, levels=c("s", "m", "l"), ordered=TRUE)
d[2] > d[1]

colors = c("red", "blue", "green", "blue", "red")
shirts = data.frame(sizes, colors)
shirts
shirts[2,]
shirts$colors
shirts[shirts$colors == "blue",]
shirts$sizes = factor(shirts$sizes, levels=c("s", "m", "l"), ordered=TRUE)
shirts$id = c(1,2,3,4,5)
shirts = shirts[with(shirts, order(sizes)),] # Go to the Workspace tab to examine the result

table(shirts$sizes, shirts$colors)
table(shirts)
ftable(shirts)

size.col = table(shirts$sizes, shirts$colors)
margin.table(size.col)
margin.table(size.col,1)
prop.table(size.col)
prop.table(size.col,1)

my.addition = function(a,b) {
  return(a+b)
}

my.addition(1,2)

my.multiplication = function(a,b) a*b

my.multiplication(4,5)

