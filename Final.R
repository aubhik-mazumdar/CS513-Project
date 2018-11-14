# Importing dataset
df <- read.csv('https://raw.githubusercontent.com/aubhik-mazumdar/CS513-Project/master/datasets/summer_new.csv')
levels(df$Country)

# Replacing west germany + east germany to Germany
df$Country <- gsub('FRG','GER',df$Country)
df$Country <- gsub('GDR','GER',df$Country)
df$Country <- factor(df$Country)
levels(df$Country)

# Removing countries which split into multiple countries
# Method 1
df <- df[!grepl('YUG',df$Country) & !grepl('SCG',df$Country) &!grepl('TCH',df$Country),] 
# Will remove columns with country as YUG, SCG and TCH but will still show it in levels

# Convert these countries into NA and remove them
df$Country <- gsub('YUG', NA, df$Country)
df$Country <- gsub('TCH', NA, df$Country)
df$Country <- gsub('SCH', NA, df$Country)
df <- na.omit(df)
df$Country <- factor(df$Country)
levels(df$Country)

# Missing values
which(df$Country == '')
df[12778,6] <- 'BRN'
df[14247,6] <- 'CAN'
df[14266,6] <- 'KOR'
df[14285,6] <- 'RUS'
