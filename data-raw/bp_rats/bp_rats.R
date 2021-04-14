
# ****** Dahl Rats *******#
ss_hs <- read.table('data-raw\\bp_rats\\SS_HS.txt')
ss_ls <- read.table('data-raw\\bp_rats\\SS_LS.txt')
ssbn13_hs <- read.table('data-raw\\bp_rats\\SSBN13_HS.txt')
ssbn13_ls <- read.table('data-raw\\bp_rats\\SSBN13_LS.txt')

colnames(ss_hs) <- c("rat_01", "rat_02", "rat_03", "rat_04", "rat_05", "rat_06", "rat_07", "rat_08", "rat_09")
colnames(ss_ls) <- c("rat_01", "rat_02", "rat_03", "rat_04", "rat_05", "rat_06", "rat_07", "rat_08", "rat_09")
colnames(ssbn13_hs) <- c("rat_01", "rat_02", "rat_03", "rat_04", "rat_05", "rat_06")
colnames(ssbn13_ls) <- c("rat_01", "rat_02", "rat_03", "rat_04", "rat_05", "rat_06")

rat_files <- list(ss_hs, ss_ls, ssbn13_hs, ssbn13_ls)
rat_files_names <- c("ss_hs",
                     "ss_ls",
                     "ssbn13_hs",
                     "ssbn13_ls")

rm(ss_hs, ss_ls, ssbn13_hs, ssbn13_ls)

out <- NULL

for(i in 1:length(rat_files)){

  rat_names <- names(rat_files[[i]])

  tmp1 <- NULL

  for(j in 1:length(rat_names)){

    tmp2 <- cbind(rat_files_names[i], rat_names[j], rat_files[[i]][,j])

    tmp1 <- rbind(tmp1, tmp2)

  }

  out <- rbind(out, tmp1)

}

rm(rat_files, tmp1, tmp2)

out <- as.data.frame(out)

out$V1 <- as.factor(out$V1)
out$V2 <- as.factor(out$V2)
out$V3 <- as.numeric(out$V3)

colnames(out) <- c("rat_type", "rat_ID", "ABP")

# Add columns for elapsed time
out <- out %>% group_by(rat_type, rat_ID) %>% mutate(time_sec = row_number()/100, time_min = row_number()/100/60) # 100hz = 0.01 seconds

bp_rats <- out
rm(out)

tmpx <- bp_rats %>% select("rat_type", "rat_ID", "ABP") %>% tidyr::pivot_wider(names_from = rat_ID, values_from = ABP)

ss_hs <- list(rat_01 = as.vector(tmpx[1,2][[1]]),
              rat_02 = as.vector(tmpx[1,3][[1]]),
              rat_03 = as.vector(tmpx[1,4][[1]]),
              rat_04 = as.vector(tmpx[1,5][[1]]),
              rat_05 = as.vector(tmpx[1,6][[1]]),
              rat_06 = as.vector(tmpx[1,7][[1]]),
              rat_07 = as.vector(tmpx[1,8][[1]]),
              rat_08 = as.vector(tmpx[1,9][[1]]),
              rat_09 = as.vector(tmpx[1,10][[1]]))

ss_ls <- list(rat_01 = as.vector(tmpx[2,2][[1]]),
              rat_02 = as.vector(tmpx[2,3][[1]]),
              rat_03 = as.vector(tmpx[2,4][[1]]),
              rat_04 = as.vector(tmpx[2,5][[1]]),
              rat_05 = as.vector(tmpx[2,6][[1]]),
              rat_06 = as.vector(tmpx[2,7][[1]]),
              rat_07 = as.vector(tmpx[2,8][[1]]),
              rat_08 = as.vector(tmpx[2,9][[1]]),
              rat_09 = as.vector(tmpx[2,10][[1]]))

ssbn13_hs <- list(rat_01 = as.vector(tmpx[3,2][[1]]),
                  rat_02 = as.vector(tmpx[3,3][[1]]),
                  rat_03 = as.vector(tmpx[3,4][[1]]),
                  rat_04 = as.vector(tmpx[3,5][[1]]),
                  rat_05 = as.vector(tmpx[3,6][[1]]),
                  rat_06 = as.vector(tmpx[3,7][[1]]),
                  rat_07 = as.vector(tmpx[3,8][[1]]),
                  rat_08 = as.vector(tmpx[3,9][[1]]),
                  rat_09 = as.vector(tmpx[3,10][[1]]))

ssbn13_ls <- list(rat_01 = as.vector(tmpx[4,2][[1]]),
                  rat_02 = as.vector(tmpx[4,3][[1]]),
                  rat_03 = as.vector(tmpx[4,4][[1]]),
                  rat_04 = as.vector(tmpx[4,5][[1]]),
                  rat_05 = as.vector(tmpx[4,6][[1]]),
                  rat_06 = as.vector(tmpx[4,7][[1]]),
                  rat_07 = as.vector(tmpx[4,8][[1]]),
                  rat_08 = as.vector(tmpx[4,9][[1]]),
                  rat_09 = as.vector(tmpx[4,10][[1]]))

bp_rats_list <- list("ss_hs" = ss_hs, "ss_ls" = ss_ls, "ssbn13_hs" = ssbn13_hs, "ssbn13_ls" = ssbn13_ls)
rm(tmpx)


usethis::use_data(bp_rats, overwrite = TRUE)
usethis::use_data(bp_rats_list, overwrite = TRUE)
