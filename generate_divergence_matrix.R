library(Biostrings)
library(dplyr)

# Read the FASTA file
fasta_file <- "/Users/richardsalvato/Documents/rabv_paper/tree/aln_sequences_paper.aligned.fasta"  # Change to the path of your file
sequences <- readDNAStringSet(fasta_file)

# Convert sequences into a matrix
seq_matrix <- as.matrix(sequences)

# Function to calculate divergence as a percentage
calculate_divergence_percentage <- function(seq1, seq2) {
  # Ignore positions with gaps "-"
  valid_positions <- (seq1 != "-") & (seq2 != "-")
  
  # Count the number of divergences
  divergences <- sum(seq1[valid_positions] != seq2[valid_positions])
  
  # Count the total number of valid positions
  total_valid_positions <- sum(valid_positions)
  
  # Calculate divergence percentage
  if (total_valid_positions == 0) {
    return(NA)  # Avoid division by zero
  } else {
    return((divergences / total_valid_positions) * 100)
  }
}

# Create a divergence matrix
n <- length(sequences)
divergence_matrix <- matrix(0, nrow = n, ncol = n)
rownames(divergence_matrix) <- names(sequences)
colnames(divergence_matrix) <- names(sequences)

# Fill the divergence matrix
for (i in 1:n) {
  for (j in 1:n) {
    divergence_matrix[i, j] <- calculate_divergence_percentage(seq_matrix[i, ], seq_matrix[j, ])
  }
}

# Convert the matrix to a data frame
divergence_df <- as.data.frame(divergence_matrix)

# Save as CSV
write.csv(divergence_df, "/Users/richardsalvato/Documents/rabv_paper/tree/divergence_matrix.csv", row.names = TRUE)

# Load the libraries
library(Biostrings)
library(dplyr)

# Read the FASTA file
fasta_file <- "/Users/richardsalvato/Documents/rabv_paper/tree/aln_sequences_paper.aligned.fasta"  # Change to the path of your file
sequences <- readDNAStringSet(fasta_file)

# Convert sequences into a matrix
seq_matrix <- as.matrix(sequences)

# Function to calculate divergence as a percentage and round to an integer
calculate_divergence_percentage <- function(seq1, seq2) {
  # Ignore positions with gaps "-"
  valid_positions <- (seq1 != "-") & (seq2 != "-")
  
  # Count the number of divergences
  divergences <- sum(seq1[valid_positions] != seq2[valid_positions])
  
  # Count the total number of valid positions
  total_valid_positions <- sum(valid_positions)
  
  # Calculate divergence percentage and round it
  if (total_valid_positions == 0) {
    return(NA)  # Avoid division by zero
  } else {
    return(round((divergences / total_valid_positions) * 100))  # Round to integer
  }
}

# Create a divergence matrix
n <- length(sequences)
divergence_matrix <- matrix(0, nrow = n, ncol = n)
rownames(divergence_matrix) <- names(sequences)
colnames(divergence_matrix) <- names(sequences)

# Fill the divergence matrix
for (i in 1:n) {
  for (j in 1:n) {
    divergence_matrix[i, j] <- calculate_divergence_percentage(seq_matrix[i, ], seq_matrix[j, ])
  }
}

# Convert the matrix to a data frame
divergence_df <- as.data.frame(divergence_matrix)

# Save as CSV
write.csv(divergence_df, "/Users/richardsalvato/Documents/rabv_paper/tree/divergence_matrix.csv", row.names = TRUE)
