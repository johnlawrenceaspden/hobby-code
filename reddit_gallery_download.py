#!/usr/bin/env python3
import requests
import os
import sys

def get_gallery_image_urls(reddit_post_url):
    """
    Given a Reddit post URL, fetch JSON and extract full image URLs if it’s a gallery.
    """
    # Normalize URL to JSON endpoint
    if not reddit_post_url.endswith("/"):
        reddit_post_url += "/"
    json_url = reddit_post_url + ".json"
    headers = {
        "User-Agent": "gallery-downloader-script/0.1"
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
    outdir = sys.argv[2] if len(sys.argv) >= 3 else "images"
    urls = get_gallery_image_urls(post_url)
    if not urls:
        print("No images found.")
    else:
        download_images(urls, outdir)
        print("Done. Downloaded", len(urls), "images to", outdir)
