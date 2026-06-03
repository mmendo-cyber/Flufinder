# Function 1
upload_fasta <- function(testfasta.txt) {
  
  # Open library and reading FASTA files
  library(seqinr)
  #Reading the fasta file
  read.fasta(testfasta.txt, seqtype = "AA", as.string = TRUE,
             set.attributes = FALSE)
}



# Function 3
# Open library and split peptides into individual amino acids
split_peptides <- function(peptides) {
  library(stringr)
  lapply(peptides, str_split, pattern="")
}



# Function 5
count_matching_masses <- function(protein_masses, sample) {
  
  df <- as.data.frame(sapply(protein_masses, function (x)
    sum(as.character(sample) %in% as.character(x))))
  
  names(df) <- "peptide_counts"
  return(df)
}



#Function 2
trypsinize <- function(proteins) {
  #Opening stringr for simple string manipulation
  library(stringr)
  
  #Using str_split_1 to split proteins after R or K amino acids; "(?<=R|K)" is a regular expression for splitting after "?<=" R or K "R|K"
  lapply(proteins, str_split_1, pattern="(?<=R|K)")
}



#Function 4
splitpeptides_to_masses <- function(aa) {
  # Generating a vector of masses for each amino acid
  aa_masses <- c(A=71.037, R=156.101, N=114.042, D=115.026, C=103.009,
                 Q=128.058, E=129.042, G=57.021, H=137.058, I=113.084, L=113.084,
                 K=128.094, M=131.040, F=147.068, P=97.052, S=87.032, T=101.047,
                 W=186.079, Y=163.063, V=99.068)
  
  # Looping over each list of peptides and mapping amino acids to masses,returning the sum as peptide masses
  peptide_masses <- aa
  
  for(i in 1:length(aa)) {
    peptide_masses[[i]] <- lapply(aa[[i]],
                                  function(x) sum(aa_masses[x]))
  }
  
  # Unlisting the inner lists to generate a list of vectors of masses for each protein
  lapply(peptide_masses, unlist)
}



#Function 6
ggbarplot <- function(peptide_counts_table) {
  library(ggplot2)
  # Generating a barplot from the peptide counts dataframe
  ggplot(peptide_counts_table) +
    aes(rownames(peptide_counts_table), peptide_counts) +
    geom_col(fill="blue", width=0.5) +
    theme_bw() +
    labs(x="Flu Strain", y="Peptide Counts")
}









