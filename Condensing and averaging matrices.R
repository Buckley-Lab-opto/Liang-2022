#### BEFORE YOU START ####
# crop your image into squares and save the pixel intensity data as text files


#### IMPORTANT - SET WORKING DIRECTORY TO THE FOLDER CONTAINING YOUR FILES ####
# setwd()


#### setting up the functions ####

# function to read all .csv files

readall <- function(data) {
  
  # extract csv and telling it to NOT read the first column as header
  X <- read.csv(data, header = FALSE, skip = 1)
  
  return(X)
}

# function to trim data into X*X matrix where X is divisible by 10
# for example, a 555*555 matrix will become 560*560

trimall <- function(X) {
  # add NA rows or trim unwanted rows using if else logic and while loop 
  if (round(ncol(X) / 10) >= nrow(X) / 10) {
    
    while (nrow(X) %% 10 != 0)  {
      X[nrow(X)+1,] <- NA
      X[,ncol(X)+1] <- NA
    }
    
  } else {
    
    while (nrow(X) %% 10 != 0)  {
      X <- X[-nrow(X),]
      X <- X[,-ncol(X)]
    }
  }

  # decide n, which is the number of rows and columns you're averaging to end up with a 10*10 matric
  # for example, for a 560*560 matrix n will be 56
  n <- length(X) / 10 
   
  # make it into a matrix to use the tapply function
  matrixtrial1 <- data.matrix(X, rownames.force = NA)
  
  # averaging the values every n columns for every row
  # this function conveniently (though unintentionally) transposes the matrix
  condensed <- apply(matrixtrial1, 1, function(x) tapply(x, rep(seq(1, length(x), n), each=n), mean, na.rm = TRUE))
  
  # rows are now columns and columns are now rows
  # get mean for every n again
  finalcondensed <- apply(condensed, 1, function(x) tapply(x, rep(seq(1, length(x), n), each=n), mean, na.rm = TRUE))
  
  # convert the condensed files into matrix
  xmat2 <- as.matrix(finalcondensed)
}




#### converting txt files into csv ####

# Create a vector of txt files to read
files.to.read <- list.files(pattern="txt")

# Read each file and write it to csv
lapply(files.to.read, function(f) {
  df = read.table(f, header = FALSE)
  write.csv(df, gsub("txt", "csv", f), row.names=FALSE)
})




#### running the automation ####

# make all files with names containing specific characters into a list
# you'll need to change the pattern to suit your data
myfiles <- list.files(pattern = "csv")

# or alternatively read all files in a folder
# file_list <- list.files(path="C:/Users/Luke/Documents/NBA")

# apply a function to everything in the list
# in this case read all csv files
csvinlist <- lapply(myfiles, FUN = readall)   

# apply the trimall to all csv in the list
condensedmat <- lapply(csvinlist, FUN = trimall)  

# Reduce function complies everything together by adding each cell together
# then divide it by the length of the list
averagedmat <- Reduce("+", condensedmat)/length(condensedmat)




#### saving files ####

# write individual condensed dataframe into individual csv files 

# change the list of file names to a new name you want
# gsub("what you want replaced", "what you want to replace it with", "list of original file names")
new.file.names <- gsub(".txt", " condensed.csv", files.to.read)

Map(write.table, condensedmat, new.file.names, sep = ",", row.names = F, col.names = F)

# write the averaged matrix into a single csv
# change file title here
write.table(averagedmat, "averagedmat.csv", sep=",", row.names = F, col.names=F)


# you should end up with individual 10*10 matrices as csv
# and also a single file containing a 10*10 matrix averaged from all other matrices