# Joint Probability Exercise first steps...

# Read in the data
x = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=FALSE)
names(x) = c("Sex","Length", "Diameter", "Height", "WholeWeight", "SchuckedWeight", "VisceraWeight", "ShellWeight", "Rings")

# Select number of buckets for quantiles
qn = 3

# Map Height to quantiles
q = quantile(x$Height, probs=seq(0,1,1/qn))
H = diff(range(x$Height))
q[[1]] = q[[1]] - 0.1*H
q[[length(q)]] = q[[length(q)]] + 0.1*H
x$Height3 = as.numeric(cut(x$Height, breaks=q))

# Map WholeWeight to quantiles
q = quantile(x$WholeWeight, probs=seq(0,1,1/qn))
H = diff(range(x$WholeWeight))
q[[1]] = q[[1]] - 0.1*H
q[[length(q)]] = q[[length(q)]] + 0.1*H
x$WholeWeight3 = as.numeric(cut(x$WholeWeight, breaks=q))

# Exercise: compute the joint distribution of the two discretized variables


# Exercise: does the joint distribution indicate that the variables are independnet?




# Regular Expressions, simple examples
str = "data science 350"

pattern1 = "a sc"
pattern2 = "nce+[[:space:]]+[[:digit:]]+"

pattern = pattern1

grepl(pattern, str)
sub(pattern, "****", str)
m = regexpr(pattern, str)
regmatches(str, m)

pattern <- pattern2

grepl(pattern, str)
sub(pattern, "****", str)
m = regexpr(pattern, str)
regmatches(str, m)

# Twitter example
path = "/Users/rsharp/PROJECTS/Twitter Analysis/TweetAnalysis/"

# A short aside: working with JSON data in R
require(rjson)

file.name = paste(path, "tweets.json", sep="")
tweet.con = file(file.name)
open(tweet.con)
x = readLines(tweet.con)

time.series = c()

for(line in x) {
  y = fromJSON(line)
  t = strptime(y$created_at, format="%a %b %d %H:%M:%S +0000 %Y")
  time.series = c(time.series, as.numeric(t))
}

close(tweet.con)

time.series = time.series - time.series[1]

table(time.series)

y$created_at
y$id
y$text

# Now back to the SINAI example

# read in the labeled example data 
labeled.data.file.name <- "appleTweetsLabeled.tsv"
labeled.data <- read.csv(paste(path, labeled.data.file.name, sep=''), sep='\t', header=TRUE)

# add an ID feature
labeled.data$id = as.numeric(row.names(labeled.data))
names(labeled.data) <- c("created_at", "text", "label", "id")

# reverse the order of the columns
labeled.data = labeled.data[,c(4,1,2,3)]

# read the annotated file
annotated.data.file.name <- "AppleText.ann"
annotated.data.file.full.name <- paste(path, annotated.data.file.name, sep='') 
annotated.data <- read.csv(annotated.data.file.full.name, sep='\n', header=FALSE)

# add an ID feature
annotated.data$id = as.numeric(row.names(annotated.data))
names(annotated.data) = c("annotatedText", "id")

# reverse the order of the columns
annotated.data = annotated.data[,c(2,1)]

# function that first breaks a line into tweet.id and tweet.ann.text.
# Next it strips the entities from the text and returns a table with
# three columns: tweet.id, ent.type, ent.val 
# function that breaks a line into tweet.id and tweet.ann.text, then strips the Orgs from the text
getEnts = function(line) { 
    
  # define regular expressions to be used later
  p1  = "[^<]*<[^<]*>"       # a tag
  p1a = "[^<]*<"             # open tag
  p1b = "[[:alnum:]]+"       # tag type
  p1c = "[[:space:]]?[^<]*>" # rest of tag
  p2 = "[^<]*"               # between tags
  p3 = "[^<]*</[[:alnum:]]+[[:space:]]?[^<]*>" # close tag

  id = line[[1]]
  s  = line[[2]]

  tweet.id = c()
  ent.type = c()
  ent = c()
  while(grepl(p1,s)) {
    # there is at least one tag left so create a new row for it...
    # write the tweet id
    tweet.id = c(tweet.id, id)
    # strip the ent tag
    s = sub(p1a,"",s)  # strip out the leading material and open tag, '<', to expose the entity type
    m = regexpr(p1b,s) # match the entity type    
    ent.type = c(ent.type, regmatches(s, m)) # write the entity type
    s = sub(p1c, "", s) # remove the remainder of the tag
    # match the entity value
    m = regexpr(p2, s) # match the org name
    # write the entity value
    #print(s)
    #print(regmatches(s, m))
    ent = c(ent,regmatches(s, m))
    # strip the close tag
    s = sub(p3,"",s) # strip out the org and close </Org...> tag
  } 
  
  if(length(tweet.id) == 0) {
    empty.df = data.frame(numeric(), character(), character())
    names(empty.df) = c("tweet.id", "ent.type", "ent")
    return(empty.df)  
  } else {
    return(data.frame(tweet.id,ent.type,ent))
  }

}

# this is a bit more efficient than using the equivalent "merge"
entities.data = Reduce(rbind, apply(annotated.data, 1, getEnts))
entities.data$tweet.id = as.numeric(as.character(entities.data$tweet.id))

# siani_2: Company name recognized as Organization in tweet
s2 = apply(entities.data, 1, function(row) grepl("apple", tolower(row[[3]])))
s2[which(entities.data$ent.type != "Organization")] = FALSE
s2 = data.frame(entities.data$tweet.id,s2)
names(s2) = c("id","s2Pass")

# tweets with multiple entities
id = c()
pass = c()
for(i in unique(s2$id)) {
  id = c(id, i)
  # print(paste(i, length(which(s2$id == i)), min(sum(s2[which(s2$id == i),]$s2Pass),1)))
  if(min(sum(s2[which(s2$id == i),]$s2Pass),1) == 1) {
    pass = c(pass, TRUE)
  } else {
    pass = c(pass, FALSE)
  }
}

s2 = data.frame(id,pass)

labeled.data$s2 = rep(FALSE, nrow(labeled.data))

for(i in seq(nrow(labeled.data))) {
  idx = which(s2$id == labeled.data[i,]$id)
  if(length(idx) > 0) {
    labeled.data[i,]$s2 = s2[idx,]$pass
  }
}


# Exercise: What is the probability that a tweet reffers to Apple Inc. 
# using the entire sample of tweets?


# Exercise: What is the probability that a tweet passes the s2 test
# using the entire sample of tweets?


# Exercise: What is the probability that a tweet reffers to Apple Inc. 
# given than it has passed the s2 test?


# Exercise: What is the probability that a tweet passes the s2 test 
# given that it reffers to Apple Inc.? Use Bayes' approach.


# Exercise: Compute the accuracy, precision, and recall of s2
# To simplify things, let's treat the case s2==TRUE and "not sure" 
# as a false positive and s2==FALSE and "not sure" as a false negative.

# Exercise: Read in the abalone data. Use the function t.test to perform 
# a t test on a sample of 100 abalone to check the hypothesis that the 
# mean diameter of abalone is 0.4. Interpret the results.
x = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=FALSE)
names(x) = c("Sex","Length", "Diameter", "Height", "WholeWeight", "SchuckedWeight", "VisceraWeight", "ShellWeight", "Rings")

hist(x$Diameter)

