# Directory where the png files are located
dir_name <- "output-eth-to-plots"

# Sub-directory where the png files will be copied
dest_dir_name <- "output-eth-to-plots/scatterplots"

# Create dest_dir_name if not exists
if (!dir.exists(dest_dir_name)){
  dir.create(dest_dir_name)
}

# Get all directories in dir_name
dirs <- list.dirs(path = dir_name, recursive = FALSE)

# Sort directories by date created
dirs_ordered <- dirs[order(file.info(dirs)$mtime)]

# Iterate over each directory
for(dir in dirs_ordered) {
  # Get all png file paths with 'scatter' in their name from the current directory
  filepaths <- list.files(path = dir, pattern = "group_scatter.png", full.names = TRUE)
  
  # Copy each file to dest_dir_name and rename it with its parent directory name
  sapply(filepaths, function(x) {
    # Extract the parent directory name from the filepath
    parent_dir_name <- basename(dirname(x))
    
    # Define the new filename with the parent directory name
    new_filename <- paste0(parent_dir_name, ".png")
    
    # Define the destination path for copying and renaming the file
    dest_path <- file.path(dest_dir_name, new_filename)
    
    # Copy and rename the file to dest_path
    file.copy(from = x, to = dest_path)
  })
}