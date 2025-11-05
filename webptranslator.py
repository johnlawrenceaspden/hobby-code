!/bin/bash

# Tries to replace .webp files from reddit with pngs, renames any .webp with a .png version to .webp.old 

# Use locate to find all .webp files and put them in webpfiles
head -n 100 webpfiles | while read webp_file; do
  echo Trying $webp_file
  # Extract the base filename (without extension)
  base_filename=$(basename "$webp_file" .webp)
  
  # Construct the URL for the .png file (assuming it follows the same pattern)
  png_url="https://i.redd.it/${base_filename}.png"

  # Define the output .png file path (same location as the .webp file)
  png_file="${webp_file%.webp}.png"

  # Check if the .png file already exists
  if [ -f "$png_file" ]; then
    echo "PNG file already exists: $png_file. Skipping..."
    continue  # Skip to the next .webp file if the .png already exists
  fi

  # Check if the PNG file exists at the URL
  if curl --head --fail "$png_url" 2>/dev/null; then
    # Download the .png file next to the .webp file
    echo "Downloading $png_url"
    curl -o "$png_file" "$png_url"

    # Rename the original .webp file to .webp.old
    mv "$webp_file" "${webp_file}.old"
    echo "Renamed $webp_file to ${webp_file}.old"
  else
    echo "No PNG found for $base_filename"
  fi
done
