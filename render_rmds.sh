#!/bin/bash

# This script renders R Markdown files in specified subdirectories,
# excluding directories like ./renv or any other non-project directories.

echo "Starting to render R Markdown files..."

# Navigate to the script's directory (root directory of project)
cd "$(dirname "$0")" || { echo "Failed to change directory"; exit 1; }

# Define directories to exclude from the search, using -prune to exclude them
EXCLUDE_DIRS="( -path ./renv -o -path ./path_to_exclude )"

# Find and render all Rmd files in subdirectories, excluding unwanted paths
find . \( $EXCLUDE_DIRS \) -prune -o -type f -name "*.Rmd" -print | while read -r rmdfile; do
    echo "Rendering: $rmdfile"
    # Ensure environment is properly set if using renv or similar
    Rscript -e "rmarkdown::render('$rmdfile')"
done

echo "Rendering complete."
