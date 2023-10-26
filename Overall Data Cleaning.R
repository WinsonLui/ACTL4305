X <- read.table("./Data/Cleaned Data/X.csv", sep=",", header=TRUE)
y <- read.table("./Data/Cleaned Data/y.csv", sep=",", header=TRUE)

# X <- X %>% 
#   select(-State.Code)

df <- X %>% 
  left_join(y, join_by(date==Date,state==State))

write.csv(df, "./Data/Cleaned Data/df.csv", row.names=FALSE)
