# Original file
INPUT="04-02-01C_Sussman.rm"

# 1️⃣ Lossless x264
ffmpeg -i "$INPUT" -c:v libx264 -preset veryslow -crf 0 -c:a aac -b:a 320k "04-02-01C_Sussman_lossless.mp4"

# 2️⃣ Near-lossless visually (CRF 18)
ffmpeg -i "$INPUT" -c:v libx264 -preset slow -crf 18 -c:a aac -b:a 192k "04-02-01C_Sussman_crf18.mp4"

# 3️⃣ Moderate quality (CRF 23, default preset)
ffmpeg -i "$INPUT" -c:v libx264 -preset medium -crf 23 -c:a aac -b:a 128k "04-02-01C_Sussman_crf23.mp4"

# 4️⃣ Fast encode, slightly larger file (CRF 18, ultrafast preset)
ffmpeg -i "$INPUT" -c:v libx264 -preset ultrafast -crf 18 -c:a aac -b:a 192k "04-02-01C_Sussman_ultrafast.mp4"

# 5️⃣ High-bitrate audio test (CRF 18, 320k audio)
ffmpeg -i "$INPUT" -c:v libx264 -preset slow -crf 18 -c:a aac -b:a 320k "04-02-01C_Sussman_crf18_320k.mp4"






#!/bin/bash
# find ~/ -type f -iname '*.rm' | while read -r f; do
#     outfile="${f%.rm}.mp4"
#     echo "Converting '$f' → '$outfile'"
#     ffmpeg -i "$f" -c:v libx264 -preset slow -crf 23 -c:a aac -b:a 192k "$outfile"
# done



