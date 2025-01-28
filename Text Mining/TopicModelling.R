# Load necessary libraries
library(tm)
library(topicmodels)
library(stopwords)

# Specify the folder containing text files
folder_path <- "C:/Users/Thech/OneDrive/الماجستير/S2/Data Analytics نال 6114 تحليل البيانات/Assignments and Projects/Project/Text mining/v2. Topic modelling use text mining/"

# Read all text files from the folder
files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)

# Combine all text files into one character vector
articles <- sapply(files, function(file) {
  paste(readLines(file, encoding = "UTF-8"), collapse = " ")
})

# Check the raw text
cat("Raw Text:\n")
print(articles)

# Convert the text data into a Corpus
corpus <- VCorpus(VectorSource(articles))

# Text cleaning
corpus <- tm_map(corpus, content_transformer(tolower))             # Convert to lowercase
corpus <- tm_map(corpus, content_transformer(removePunctuation))   # Remove punctuation
corpus <- tm_map(corpus, content_transformer(removeNumbers))       # Remove numbers

# Load Arabic stopwords and add custom stopwords
custom_stopwords <- c(stopwords("ar", source = "stopwords-iso"), "الـ")

# Remove Arabic stopwords
corpus <- tm_map(corpus, removeWords, custom_stopwords)

# Remove extra spaces
corpus <- tm_map(corpus, stripWhitespace)

# Create a Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# Check DTM before removing sparse terms
cat("DTM before removing sparse terms:\n")
print(dtm)

# Remove sparse terms (words that appear in less than 1% of documents)
dtm <- removeSparseTerms(dtm, 0.99)

# Remove empty rows
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]

# Check if DTM is not empty
if (nrow(dtm) > 0 && ncol(dtm) > 0) {
  # Set the number of topics
  num_topics <- 3
  
  # Apply LDA (Latent Dirichlet Allocation)
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
  
  # Check if topics were generated
  if (length(lda_model@beta) > 0) {
    # Extract top 5 words for each topic
    topics <- terms(lda_model, 5)
    
    # Format topics for display
    topics_df <- data.frame(matrix(ncol = 5, nrow = num_topics))
    for (i in 1:num_topics) {
      topics_df[i, ] <- topics[, i]
    }
    colnames(topics_df) <- paste0("Word ", 1:5)
    rownames(topics_df) <- paste("Topic", 1:num_topics)
    
    # Display topics in a tabular format
    cat("\nTop words for each topic (each row is a topic):\n")
    print(topics_df)
  } else {
    cat("No topics were generated. Please check your data or preprocessing steps.\n")
  }
} else {
  cat("The Document-Term Matrix is empty. Please review the preprocessing steps.\n")
}
