#!/usr/bin/env python3

# example usage
# python ~/hobby-code/reddit_gallery_download.py "https://www.reddit.com/r/dalle2/comments/1occk95/youre_exploring_a_lonely_asteroid_in_the_middle/" asteroid_images

import requests
import os
import sys
import urllib.parse
import time

def get_gallery_image_urls(reddit_post_url):
    """
    Given a Reddit post URL, fetch JSON and extract full image URLs if it’s a gallery.
    """
    if not reddit_post_url.endswith("/"):
        reddit_post_url += "/"
    json_url = reddit_post_url + ".json"
    headers = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                      "AppleWebKit/537.36 (KHTML, like Gecko) "
                      "Chrome/118.0.5993.90 Safari/537.36"
    }

    try:
        resp = requests.get(json_url, headers=headers, timeout=10)
        resp.raise_for_status()
        data = resp.json()
    except Exception as e:
        print("❌ Error fetching Reddit JSON:", e)
        return [], None, None

    try:
        post = data[0]["data"]["children"][0]["data"]
    except (KeyError, IndexError, TypeError):
        print("❌ Unexpected JSON structure — cannot find post data.")
        return [], None, None

    subreddit = post.get("subreddit", "unknown_subreddit")
    post_name = post.get("name") or post.get("id")
    post_title_slug = (
        post.get("title", "")
        .strip()
        .replace(" ", "_")
        .replace("/", "_")
        .replace("\\", "_")
        [:80]
    )

    # Handle galleries
    if "media_metadata" in post:
        media = post["media_metadata"]
        image_urls = []
        for media_id, meta in media.items():
            mime = meta.get("m", "")
            ext = mime.split("/")[-1].replace("jpeg", "jpg")
            url = f"https://i.redd.it/{media_id}.{ext}"
            image_urls.append(url)
        return image_urls, subreddit, post_name or post_title_slug

    # Handle single-image posts
    if "url_overridden_by_dest" in post and post["url_overridden_by_dest"].startswith("https://i.redd.it/"):
        return [post["url_overridden_by_dest"]], subreddit, post_name or post_title_slug

    print("ℹ️ No gallery or image found in post.")
    return [], subreddit, post_name or post_title_slug

def safe_download(url, outdir, retries=3):
    """Download a single image, skipping if already complete."""
    os.makedirs(outdir, exist_ok=True)
    fname = url.split("/")[-1]
    path = os.path.join(outdir, fname)

    headers = {"User-Agent": "RedditDownloader/1.0"}

    # Check existing file
    if os.path.exists(path):
        try:
            head = requests.head(url, headers=headers, timeout=10)
            if head.status_code == 200 and "Content-Length" in head.headers:
                expected_size = int(head.headers["Content-Length"])
                actual_size = os.path.getsize(path)
                if actual_size == expected_size:
                    print(f"✅ Skipping {fname} (already downloaded, {actual_size} bytes).")
                    return True
                else:
                    print(f"⚠️  File {fname} incomplete ({actual_size}/{expected_size}), re-downloading.")
        except Exception as e:
            print(f"⚠️  Could not verify existing file {fname}: {e}")

    # Attempt download with retries
    for attempt in range(1, retries + 1):
        try:
            print(f"⬇️  Downloading {fname} (attempt {attempt})...")
            with requests.get(url, headers=headers, stream=True, timeout=30) as r:
                r.raise_for_status()
                total_size = int(r.headers.get("Content-Length", 0))
                tmp_path = path + ".part"
                with open(tmp_path, "wb") as f:
                    for chunk in r.iter_content(chunk_size=8192):
                        if chunk:
                            f.write(chunk)
                # Verify file size
                if total_size > 0 and os.path.getsize(tmp_path) != total_size:
                    print(f"❌ Incomplete download for {fname}, retrying...")
                    continue
                os.rename(tmp_path, path)
                print(f"✅ Finished {fname} ({os.path.getsize(path)} bytes)")
                return True
        except KeyboardInterrupt:
            print("\n🛑 Interrupted by user — exiting cleanly.")
            sys.exit(0)
        except Exception as e:
            print(f"❌ Error downloading {fname}: {e}")
            time.sleep(2)
    print(f"❌ Failed to download {fname} after {retries} attempts.")
    return False

def download_images(urls, outdir="images"):
    print(f"📁 Downloading {len(urls)} image(s) to '{outdir}'...")
    success = 0
    for url in urls:
        if safe_download(url, outdir):
            success += 1
    print(f"✅ Done. Successfully downloaded {success}/{len(urls)} images.")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: reddit_gallery_download.py <reddit_post_url> [output_folder]")
        sys.exit(1)

    post_url = sys.argv[1]
    urls, subreddit, _ = get_gallery_image_urls(post_url)

    # Determine post_name as the last non-empty part of the path
    parsed = urllib.parse.urlparse(post_url)
    path_parts = [p for p in parsed.path.split("/") if p]
    post_name = path_parts[-1] if path_parts else "post"

    if len(sys.argv) >= 3:
        outdir = sys.argv[2]
    else:
        outdir = os.path.join(subreddit, post_name)

    if not urls:
        print("No images found.")
        sys.exit(1)

    download_images(urls, outdir)




