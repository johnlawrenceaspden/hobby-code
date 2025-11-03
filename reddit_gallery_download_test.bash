#!/bin/bash

# run doctests first
python -m doctest reddit_gallery_download.py -v


# Two different links to the same image 
python reddit_gallery_download.py https://www.reddit.com/media?url=https%3A%2F%2Fi.redd.it%2Fo9fu9uw82rvf1.png
python reddit_gallery_download.py https://i.redd.it/o9fu9uw82rvf1.png
# Two different links to the same gallery
python reddit_gallery_download.py "https://www.reddit.com/gallery/1occk95"
python reddit_gallery_download.py "https://www.reddit.com/r/dalle2/comments/1occk95/youre_exploring_a_lonely_asteroid_in_the_middle/"
# specify download directory manually
python reddit_gallery_download.py "https://www.reddit.com/r/dalle2/comments/1occk95/youre_exploring_a_lonely_asteroid_in_the_middle/" dalle2/asteroid_images
# another gallery, just for paranoia
python reddit_gallery_download.py https://www.reddit.com/r/dalle2/comments/1mqp46m/gloomy_winter_scenes_from_berlin_and_iceland/
