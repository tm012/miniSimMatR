miniSimMatR <- function(path) {
  library(R.matlab)
  cross_dsads <- readMat(path) #add your mat file

  cross_dsads_1 <- cross_dsads$elemDataI

  # df_driving <- data.frame(Accel= cross_dsads_1[1][[1]],
  #                          Auto = cross_dsads_1[2][[1]])
  #
  #
  # Accel <- cross_dsads_1[9][[1]]
  #
  # Accel[,1]
  #
  # df_driving$Accel_x <- Accel[,1]


  RowNum <- c(1:length(cross_dsads_1[1][[1]][,1]))
  df <- data.frame(RowNum)
  p<-unlist(attributes(cross_dsads_1))
  i <- 1

  while (i <= length(cross_dsads_1)) {
    j <- 1
    alphabets <- c( "X", "Y", "Z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
                    "T", "U", "V", "W", 1:ncol(cross_dsads_1[i][[1]]))
    #print(alphabets)
    while (j <= ncol(cross_dsads_1[i][[1]])) {

      dim_name <- paste(as.character('dimnames'), as.character(i), sep="")
      dim_name <- ( as.character(p[dim_name][1]))
      if(ncol(cross_dsads_1[i][[1]]) > 1){



        col_name <- paste(as.character(dim_name), as.character(alphabets[j]), sep="-")
        # print(col_name)
        df[col_name] <- cross_dsads_1[i][[1]][,j]
      }else{

        col_name <-dim_name
        # print(col_name)
        df[col_name]  <- cross_dsads_1[i][[1]]
      }

      # if(j == 1){
      #   col_name <-as.character(i)
      #   print(col_name)
      #    df[col_name]  <- cross_dsads_1[i][[1]][,j]
      # }else{
      #   col_name <- paste(as.character(i), as.character(j), sep="-")
      #   print(col_name)
      #     df[col_name] <- cross_dsads_1[i][[1]][,j]
      # }



      j = j+1
    }

    i = i+1


  }

  df$RowNum <- NULL
  return(df)
}
