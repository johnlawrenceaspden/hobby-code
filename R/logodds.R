
## Log Odds
logit<-function(x) log(x/(1-x))
expit<-function(x) exp(x)/(1+exp(x))

## These two functions are inverses
logit(expit(10))  # -> 10
expit(logit(0.9)) # -> 0.9

## Probability 0.5, or 1:1 odds

logit(0.5) # -> 0

## Probability 0.1, or 1:9 odds

logit(0.1) # -> -2.197225

2*logit(0.1) # -> -4.394449

expit(2*logit(0.1))

0.01219512


# Multiplying odds 1:9 * odds 1:9 gives odds 1:81
# and logits add to do the same thing

1/(81+1) # 0.01234568

# Why would multiplying odds ratios be a nice thing to do?

# Classic example:

# There's a rare cancer, 1 person in a million gets it

# You've got a 99% accurate test,
# i.e. of 100 people with cancer, it says 'cancer' for 99 of them
# of 100 people without cancer, it only says 'cancer' for 1 of them

# If you give a random person your cancer test, and it says 'cancer', What's the probability of them actually having the rare cancer?

# Initial odds 999999:1, Test Odds Ratio 1:99, Final Ratio 999999:99

# Therefore the person has 99:999999 odds of being fine. (1:10101)

# In probability terms it's
99/(999999+99)
0.0000989

# This is a bit harder to work out in terms of probabilities



## Log Loss is a totally different thing:

logloss<-function(p,y) y*log(p)+(1-y)*log(1-p)

# handwavy is bad
logloss(0.5,1) # -0.69
logloss(0.5,0) # -0.69
# confident and correct is good
logloss(0.0005,0) # -0.000500125
# confident and wrong is very bad
logloss(0.0005,1) # -7.600902


sum(logloss(0.9,c(1,1,0,0,0,0,0,0,0,0)))    # -18.6314
sum(logloss(0.5,c(1,1,0,0,0,0,0,0,0,0)))    # -6.931472
sum(logloss(0.18,c(1,1,0,0,0,0,0,0,0,0)))   # -5.017204
sum(logloss(0.2,c(1,1,0,0,0,0,0,0,0,0)))    # -5.004024
sum(logloss(0.19,c(1,1,0,0,0,0,0,0,0,0)))   # -5.007231
sum(logloss(0.1,c(1,1,0,0,0,0,0,0,0,0)))    # -5.448054


fluffy<-function(x) {sum(logloss(x,c(1,1,0,0,0,0,0,0,0,0)))}


fluffy(0.9)  # -18.6314
fluffy(0.8)  # -13.32179
fluffy(0.7)  # -10.34513
fluffy(0.6)  # -8.351977
fluffy(0.5)  # -6.931472
fluffy(0.4)  # -5.919186
fluffy(0.3)  # -5.261345
fluffy(0.2)  # -5.004024
fluffy(0.1)  # -5.448054
fluffy(0.05) # -6.401811




