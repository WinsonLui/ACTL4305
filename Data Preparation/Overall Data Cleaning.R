X <- read.table("./Data/Cleaned Data/X.csv", sep=",", header=TRUE)
y <- read.table("./Data/Cleaned Data/y.csv", sep=",", header=TRUE)

# X <- X %>% 
#   select(-State.Code)

df <- X %>% 
  left_join(y, join_by(Date==Date,State==State))


check <- y %>% 
  filter(Date=="1983-02-16")
  # group_by(Date, State) %>% 
  # summarise(Count=n()) %>% 
  # filter(Count>1)

# Exploratory Data Analysis


write.csv(df, "./Data/Cleaned Data/df.csv", row.names=FALSE)
