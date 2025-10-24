#!/usr/bin/env python3

# example usage
# python ~/hobby-code/reddit_gallery_download.py "https://www.reddit.com/r/dalle2/comments/1occk95/youre_exploring_a_lonely_asteroid_in_the_middle/" asteroid_images




import requests
import os
import sys
import urllib.parse

def get_gallery_image_urls(reddit_post_url):
    """
    Given a Reddit post URL, fetch JSON and extract full image URLs if it’s a gallery.
    """
    # Normalize URL to JSON endpoint
    if not reddit_post_url.endswith("/"):
        reddit_post_url += "/"
    json_url = reddit_post_url + ".json"
    headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.5993.90 Safari/537.36"
    }
    resp = requests.get(json_url, headers=headers)
    resp.raise_for_status()
    data = resp.json()
    # Navigate JSON structure
    post = data[0]["data"]["children"][0]["data"]
    if "media_metadata" not in post:
        print("No gallery metadata found — maybe it’s not a gallery post.")
        return []
    media = post["media_metadata"]
    image_urls = []
    for media_id, meta in media.items():
        # meta["m"] is MIME type like "image/png" or "image/jpg"
        mime = meta.get("m", "")
        ext = mime.split("/")[-1]  # e.g. 'png' or 'jpeg'
        # The “i.redd.it” host is standard for Reddit-hosted images
        url = f"https://i.redd.it/{media_id}.{ext}"
        image_urls.append(url)
    return image_urls

def download_images(urls, outdir="images"):
    os.makedirs(outdir, exist_ok=True)
    for url in urls:
        fname = url.split("/")[-1]
        print("Downloading", url)
        r = requests.get(url, stream=True)
        if r.status_code == 200:
            path = os.path.join(outdir, fname)
            with open(path, "wb") as f:
                for chunk in r.iter_content(1024):
                    f.write(chunk)
        else:
            print("Failed:", url, "status", r.status_code)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: reddit_gallery_download.py <reddit_post_url> [output_folder]")
        sys.exit(1)
    post_url = sys.argv[1]
    # outdir = sys.argv[2] if len(sys.argv) >= 3 else "images"
    if len(sys.argv) >= 3:
        outdir = sys.argv[2]
    else:
        parsed = urllib.parse.urlparse(post_url)
        # Get last non-empty part of the path
        parts = [p for p in parsed.path.split("/") if p]
        outdir = parts[-1] if parts else "images"

    urls = get_gallery_image_urls(post_url)
    if not urls:
        print("No images found.")
    else:
        download_images(urls, outdir)
        print("Done. Downloaded", len(urls), "images to", outdir)



