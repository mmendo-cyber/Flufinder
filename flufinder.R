upload_fasta <- function(testfasta.txt) {
  
  # Open library and reading FASTA files
  library(seqinr)
  #Reading the fasta file
  read.fasta(testfasta.txt, seqtype = "AA", as.string = TRUE,
             set.attributes = FALSE)
}


# Open library and split peptides into individual amino acids
split_peptides <- function(peptides) {
  library(stringr)
  lapply(peptides, str_split, pattern="")
}


count_matching_masses <- function(protein_masses, sample) {
  
  df <- as.data.frame(sapply(protein_masses, function (x)
    sum(as.character(sample) %in% as.character(x))))
  
  names(df) <- "peptide_counts"
  return(df)
}











