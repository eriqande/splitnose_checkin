library(tidyverse)
library(stringr)


# first get slg_pipe
source("./R/splitnose_checkin_r_funcs.R")
get_slg_pipe()


#### Now read in the data and combine them into an slg_pipe file ####

# get the data
rec <- read_delim(file = "data/splitnose_recruits.txt", delim = "\t")
ad <- read_delim(file = "data/splitnose_adults.txt", delim = "\t")

# make short names 
rec2 <- rec %>%
  mutate(short_name = paste("rec", sprintf("%03d", 1:n()), sep = "")) %>%
  select(short_name, Sample_Name, everything())

ad2 <- ad %>%
  mutate(short_name = paste("adu", sprintf("%03d", 1:n()), sep = "")) %>%
  select(short_name, Sample_Name, everything())


# save the short names so we can get back to the sample names with them
dir.create("outputs")

saveRDS(bind_rows(recruit_names = rec2 %>% select(short_name, Sample_Name),
             adult_names = ad2 %>% select(short_name, Sample_Name)),
        file = "outputs/short_names.rds")


# write out to an slg_pipe file
fish <- bind_rows(rec2, ad2) %>%
  select(-Sample_Name)

names(fish)[1] <- ""
names(fish)[-1][c(F,T)] <- names(fish)[-1][c(T,F)]

dir.create("slg_pipe/arena/splitnose_inputs")
write.table(fish, row.names = F, col.names = TRUE, quote = F, sep = "\t", file = "slg_pipe/arena/splitnose_inputs/splitnose.txt")

cat(names(fish)[-1][c(T,F)], sep = "\n", file = "slg_pipe/arena/splitnose_inputs/locs.txt")

cat("adu\nrec\n", file = "slg_pipe/arena/splitnose_inputs/pops.txt")


# run pipeline like this:
# cd slg_pipe/arena/
# ../script/Do_standard_analyses.sh splitnose_inputs/splitnose.txt splitnose_inputs/pops.txt splitnose_inputs/locs.txt SPLITNOSE_STRUCTURE ../../inputs/splitnose_structure_runs.sh > SPLITNOSE_STRUCTURE.log 2>&1 &
# cd SPLITNOSE_STRUCTURE/StructureArea/arena/
# nohup ../script/ExecuteStructureRuns.sh  6  > BIG_LOG.txt  2>&1 &
# rm StdoutStruct_genos_slg_pipe.txt_dat001_k00*  # get rid of the large-ish stdout output files.
# cd ../clump_and_distruct/
# ./script/ClumpAndDistructAll.sh 6
# ./script/LaTeXify.sh -b ./final_pdf "2 3 4" > splitnose_struct.tex


# and, from that, it is clear that the recruits are composed of two different groups,
# which I suspect are different species.

# It would be nice to actually see how different those individuals are, in terms of 
# Fst, say.  So, we are going to pull them out and do another pipe run.
struct_out <- readLines("slg_pipe/arena/SPLITNOSE_STRUCTURE/StructureArea/arena/StructOuput_genos_slg_pipe.txt_dat001_k002_Rep001.txt_f")
top <- which(str_detect(struct_out, "^Inferred ancestry of individuals")) + 2
bottom <- which(str_detect(struct_out, "^Estimated Allele Frequencies in each cluster")) - 3

qvals <- str_split_fixed(str_trim(struct_out[top:bottom], side = "both"), pattern = "  *", n = 7)[,-c(1, 4, 5)] %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  tbl_df() %>%
  setNames(c("id", "miss", "q1", "q2")) %>%
  mutate(q1 = as.numeric(q1),
         q2 = as.numeric(q2))

# now make a "splitnose_q" column which could depend on structure's
# labeling...
adu_means <- qvals %>%
  filter(str_detect(id, "adu")) %>%
  summarise(q1mean = mean(q1),
            q2mean = mean(q2))

if(adu_means$q1mean[1] < adu_means$q2mean[1]) {
  qvals2 <- qvals %>%
    mutate(splitnose_q = q2)
} else {
  qvals2 <- qvals %>%
    mutate(splitnose_q = q1)
}


# now see how many are in the non-splitnose category
qvals2 %>%
  filter(splitnose_q < 0.5)

# that is 58 of them.  Which is pretty close to the 57 related individuals
# that Ottmann et al. report.


# now, we are going to want to get a list of the individuals that
# are splitnose and which ones are clearly not, and we want to re-associate
# the original ID to them so that we can hand those off for mtDNA sampling.
qv3 <- qvals2 %>%
  mutate(miss = parse_number(miss) / 100)

# and get stuff to join back with them
orig_ids <- bind_rows(ad2, rec2) %>%
  select(short_name, Sample_Name) %>%
  rename(id = short_name) %>%
  left_join(qv3, .) %>%
  select(Sample_Name, everything())

# and now we are going to take the 24 juveniles of each
# group with the least missing data and prepare them for 
# mtDNA sequencing
mtDNA_splitnoses_24 <- orig_ids %>%
  filter(splitnose_q > .9, str_detect(id, "^rec")) %>%
  arrange(miss, desc(splitnose_q)) %>%
  slice(1:24)
mtDNA_other_24 <- orig_ids %>%
  filter(splitnose_q > .9, str_detect(id, "^rec")) %>%
  arrange(miss, desc(splitnose_q)) %>%
  slice(1:24)

# now, let's pull them out and rename them and keep track of who is who
other <- qvals2 %>%
  filter(splitnose_q < 0.5) %>%
  mutate(new_name = sprintf("other%03d", 1:n()))

fish2 <- as.data.frame(fish)

rownames(fish2) <- fish2[,1] 
fish2[other$id, 1] <- other$new_name

short_names <- readRDS("outputs/short_names.rds")
other_short_names <- left_join(other, short_names, by = c("id" = "short_name"))

saveRDS(other_short_names, "outputs/other_short_names.rds")


# write that file out
write.table(fish2, row.names = F, col.names = TRUE, quote = F, sep = "\t", file = "slg_pipe/arena/splitnose_inputs/split_and_other.txt")
cat("adu\nrec\nother\n", file = "slg_pipe/arena/splitnose_inputs/other_pops.txt")

# then run that through the pipeline, and do more of the analyses
#  ../script/Do_standard_analyses.sh splitnose_inputs/split_and_other.txt splitnose_inputs/other_pops.txt splitnose_inputs/locs.txt SPLITNOSE_OTHER ../../inputs/splitnose_run2.sh > SPLITNOSE_OTHER.log 2>&1 &
#../script/SummarizeAll.sh  SPLITNOSE_OTHER

# Also, in Genepop you can compute the Fst value and it comes out looking like this:
#
# Estimates for all loci (diploid):
#   =========================
#   pop      1       2       
# 2      0.0071 
# 3      0.1322  0.1363 
#
# pop1 = adult.  pop2 = recruit. pop3 = "other"
# So, you are looking at at Fst of about .13, which could certainly be a different species.
# Certainly if these were really just related splitnose, they wouldn't have such a dramatically
# different Fst.

# That pretty much settles it.



#### Run the different groups through colony ####
# We can use that same file and run everyone through colony in three different groups:
# adult, recruit (splitnose) and "other" for the other cluster.
# Here we go:
# 2017-03-22 13:07 /arena/--% (coho-working-branch) pwd
# /Users/eriq/Documents/git-repos/splitnose_checkin/slg_pipe/arena
# 2017-03-22 13:07 /arena/--% (coho-working-branch) ../script/Do_standard_analyses.sh splitnose_inputs/split_and_other.txt splitnose_inputs/other_pops.txt splitnose_inputs/locs.txt SPLITNOSE_COLONY ../../inputs/splitnose_colony_runs.sh > SPLITNOSE_COLONY.log 2>&1 


# and then:
# 2017-03-22 13:10 /ColonyArea/--% (coho-working-branch) pwd
# /Users/eriq/Documents/git-repos/splitnose_checkin/slg_pipe/arena/SPLITNOSE_COLONY/ColonyArea
# 2017-03-22 13:10 /ColonyArea/--% (coho-working-branch) ./script/RunAllColony.sh Run1 0 3

