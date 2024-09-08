#表の作成
#index = t(c("id", "average"))
id = c()
average = c()

for (j in 0:1) {
  for (i in 0:4){
    for(k in 0:5){
      filename = paste("Group0", as.character(k), "_Block0", as.character(j), "_Session0", as.character(i), '.tsv', sep = "")
      data = read.table(file = filename, sep="\t", header=T)
      ave = mean(data$lap[5:20])
      if (k %% 2 == 0 && j == 0){
      average[length(average) + 1] = ave
      id[length(id) + 1] = filename
      }
    }
  }
}

for (j in 0:1) {
  for (i in 0:4){
    for(k in 0:5){
      filename = paste("Group0", as.character(k), "_Block0", as.character(j), "_Session0", as.character(i), '.tsv', sep = "")
      data = read.table(file = filename, sep="\t", header=T)
      ave = mean(data$lap[5:20])
      if (k %% 2 == 1 && j == 1){
        average[length(average) + 1] = ave
        id[length(id) + 1] = filename
      }
    }
  }
}

for (j in 0:1) {
  for (i in 0:4){
    for(k in 0:5){
      filename = paste("Group0", as.character(k), "_Block0", as.character(j), "_Session0", as.character(i), '.tsv', sep = "")
      data = read.table(file = filename, sep="\t", header=T)
      ave = mean(data$lap[5:20])
      if (k %% 2 == 0 && j == 1){
        average[length(average) + 1] = ave
        id[length(id) + 1] = filename
      }
    }
  }
}

for (j in 0:1) {
  for (i in 0:4){
    for(k in 0:5){
      filename = paste("Group0", as.character(k), "_Block0", as.character(j), "_Session0", as.character(i), '.tsv', sep = "")
      data = read.table(file = filename, sep="\t", header=T)
      ave = mean(data$lap[5:20])
      if (k %% 2 == 1 && j == 0){
        average[length(average) + 1] = ave
        id[length(id) + 1] = filename
      }
    }
  }
}


datafile = data.frame(id, average)
write.csv(datafile, file = "result.csv", row.names = FALSE)
#data <-read.table(file = "Group00_Block00_Session00.tsv",sep="\t",header=T)
#ave = mean(data$lap[5:20])

#i = as.character(1)
#paste("Group0", as.character(k), "_Block0", as.character(j), "_Session", as.character(i), '.tsv', sep = "")