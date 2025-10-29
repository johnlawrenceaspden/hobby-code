#!/usr/bin/env python3
"""
Reddit Gallery Downloader — robust, resumable, and user-friendly.

Usage:
    python reddit_gallery_download.py "https://www.reddit.com/gallery/1occk95"
    python reddit_gallery_download.py "https://www.reddit.com/r/dalle2/comments/1occk95/youre_exploring_a_lonely_asteroid_in_the_middle/" asteroid_images
"""

import requests
import os
import sys
import urllib.parse
import time


# ----------------------------- Core Logic -----------------------------------

def get_gallery_image_urls(reddit_post_url):
    """
    Given a Reddit post URL, fetch JSON and extract full image URLs if it’s a gallery.
    Handles both /r/... and /gallery/... URLs.
    """
    headers = {
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 (KHTML, like Gecko) "
            "Chrome/118.0.5993.90 Safari/537.36"
        )
    }

    # --- Handle /gallery/<id> shortcuts ---
    if "/gallery/" in reddit_post_url:
        try:
            # Extract post ID (e.g., 1occk95)
            parts = reddit_post_url.strip("/").split("/")
            post_id = parts[-1] if parts[-1] else parts[-2]
            api_url = f"https://api.reddit.com/api/info/?id=t3_{post_id}"
            resp = requests.get(api_url, headers=headers, timeout=10)
            resp.raise_for_status()
            data = resp.json()
            post_data = data["data"]["children"][0]["data"]
            permalink = post_data.get("permalink")
            if permalink:
                reddit_post_url = urllib.parse.urljoin("https://www.reddit.com", permalink)
                print(f"↪️  Resolved gallery link to: {reddit_post_url}")
            else:
                print("⚠️  Could not resolve permalink, continuing with original URL.")
        except Exception as e:
            print(f"⚠️  Error resolving /gallery/ link via API: {e}")

    if not reddit_post_url.endswith("/"):
        reddit_post_url += "/"

    json_url = reddit_post_url + ".json"

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
    post_title_slug = (
        post.get("title", "")
        .strip()
        .replace(" ", "_")
        .replace("/", "_")
        .replace("\\", "_")
        [:80]
    )

    # --- Handle galleries ---
    if "media_metadata" in post:
        media = post["media_metadata"]
        image_urls = []
        for media_id, meta in media.items():
            mime = meta.get("m", "")
            ext = mime.split("/")[-1].replace("jpeg", "jpg")
            url = f"https://i.redd.it/{media_id}.{ext}"
            image_urls.append(url)
        return image_urls, subreddit, post_title_slug

    # --- Handle single-image posts ---
    if "url_overridden_by_dest" in post and post["url_overridden_by_dest"].startswith("https://i.redd.it/"):
        return [post["url_overridden_by_dest"]], subreddit, post_title_slug

    print("ℹ️ No gallery or image found in post.")
    return [], subreddit, post_title_slug


# -------------------------- Download Helpers --------------------------------

def safe_download(url, outdir, retries=3):
    """Download a single image with resume support."""
    os.makedirs(outdir, exist_ok=True)
    fname = url.split("/")[-1]
    path = os.path.join(outdir, fname)
    tmp_path = path + ".part"

    headers = {"User-Agent": "RedditDownloader/1.0"}

    # Check if file already complete
    if os.path.exists(path):
        head = requests.head(url, headers=headers, timeout=10)
        if head.status_code == 200 and "Content-Length" in head.headers:
            expected_size = int(head.headers["Content-Length"])
            actual_size = os.path.getsize(path)
            if actual_size == expected_size:
                print(f"✅ Skipping {fname} (already complete, {actual_size} bytes).")
                return True
        print(f"⚠️  File {fname} exists but is incomplete, resuming...")

    # Determine how much we already have
    downloaded_size = os.path.getsize(tmp_path) if os.path.exists(tmp_path) else 0

    for attempt in range(1, retries + 1):
        try:
            if downloaded_size > 0:
                headers["Range"] = f"bytes={downloaded_size}-"
            else:
                headers.pop("Range", None)

            print(f"⬇️  Downloading {fname} (attempt {attempt})... starting at {downloaded_size} bytes")

            with requests.get(url, headers=headers, stream=True, timeout=30) as r:
                if r.status_code not in (200, 206):
                    print(f"❌ Server did not support resume (status {r.status_code}). Restarting download.")
                    downloaded_size = 0
                    open(tmp_path, "wb").close()  # reset
                    continue

                mode = "ab" if downloaded_size > 0 else "wb"
                total_size = int(r.headers.get("Content-Length", 0)) + downloaded_size
                with open(tmp_path, mode) as f:
                    for chunk in r.iter_content(chunk_size=8192):
                        if chunk:
                            f.write(chunk)

            final_size = os.path.getsize(tmp_path)
            if total_size > 0 and final_size < total_size:
                print(f"⚠️  Incomplete ({final_size}/{total_size}), retrying...")
                downloaded_size = final_size
                time.sleep(2)
                continue

            os.rename(tmp_path, path)
            print(f"✅ Finished {fname} ({final_size} bytes)")
            return True

        except KeyboardInterrupt:
            print("\n🛑 Interrupted by user — exiting cleanly.")
            sys.exit(0)
        except Exception as e:
            print(f"❌ Error downloading {fname}: {e}")
            downloaded_size = os.path.getsize(tmp_path) if os.path.exists(tmp_path) else 0
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


# ------------------------------ Entry Point ---------------------------------

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: reddit_gallery_download.py <reddit_post_url> [output_folder]")
        sys.exit(1)

    post_url = sys.argv[1]
    urls, subreddit, post_title_slug = get_gallery_image_urls(post_url)

    if not subreddit:
        subreddit = "unknown_subreddit"
    if not post_title_slug:
        post_title_slug = "post"

    if len(sys.argv) >= 3:
        outdir = sys.argv[2]
    else:
        outdir = os.path.join(subreddit, post_title_slug)

    if not urls:
        print("No images found.")
        sys.exit(1)

    download_images(urls, outdir)
