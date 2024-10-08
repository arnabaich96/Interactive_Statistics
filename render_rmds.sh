#!/bin/bash

# This script renders R Markdown files in specified subdirectories,
# excluding directories like ./renv or any other non-project directories.

echo "Starting to render R Markdown files..."

# Navigate to the script's directory (root directory of project)
cd "$(dirname "$0")"

# Define directories to exclude from the search
EXCLUDE_DIRS="( -path ./renv -o -path ./path_to_exclude )"

# Find and render all Rmd files in subdirectories, excluding unwanted paths
find . -type d ! -path '*/\.*' $EXCLUDE_DIRS -prune -o -type f -name "*.Rmd" -print | while read rmdfile; do
    echo "Rendering: $rmdfile"
    # Ensures environment is properly set if using renv or similar
    Rscript -e "rmarkdown::render('$rmdfile')"
done

echo "Rendering complete."
