#!/usr/bin/env python3
"""
Reddit Gallery Downloader — robust, resumable, and user-friendly.

Usage:
    # Two different links to the same image
    # direct link
    python reddit_gallery_download.py https://i.redd.it/o9fu9uw82rvf1.png
    # media redirect
    python reddit_gallery_download.py https://www.reddit.com/media?url=https%3A%2F%2Fi.redd.it%2Fo9fu9uw82rvf1.png
    # In either case it should be able to find the original post
    # subreddit dalle2, title The_Dude_holding_a_Wandering_Dude, and put the image in an appropriate subdirectory

    # Two different links to the same gallery
    python reddit_gallery_download.py "https://www.reddit.com/gallery/1occk95"
    python reddit_gallery_download.py "https://www.reddit.com/r/dalle2/comments/1occk95/youre_exploring_a_lonely_asteroid_in_the_middle/"

    # specify download directory manually
    python reddit_gallery_download.py "https://www.reddit.com/r/dalle2/comments/1occk95/youre_exploring_a_lonely_asteroid_in_the_middle/" dalle2/asteroid_images

    # another gallery, just for paranoia
    python reddit_gallery_download.py https://www.reddit.com/r/dalle2/comments/1mqp46m/gloomy_winter_scenes_from_berlin_and_iceland/
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

    json_url = reddit_post_url + ".json"  # Fetch the JSON data for the Reddit post

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
    """Download a single image with resume support and speed estimation."""
    os.makedirs(outdir, exist_ok=True)  # Create the download directory if it doesn't exist
    fname = url.split("/")[-1]  # Get the image filename from the URL
    path = os.path.join(outdir, fname)  # Path where the image will be saved
    tmp_path = path + ".part"  # Temporary file used for resuming download

    headers = {"User-Agent": "RedditDownloader/1.0"}

    # Check if file already exists and is complete
    if os.path.exists(path):
        head = requests.head(url, headers=headers, timeout=10)  # Send a HEAD request to check file size
        if head.status_code == 200 and "Content-Length" in head.headers:
            expected_size = int(head.headers["Content-Length"])
            actual_size = os.path.getsize(path)
            if actual_size == expected_size:
                print(f"✅ Skipping {fname} (already complete, {actual_size} bytes).")
                return True
        print(f"⚠️  File {fname} exists but is incomplete, resuming...")

    # Determine how much has already been downloaded
    downloaded_size = os.path.getsize(tmp_path) if os.path.exists(tmp_path) else 0

    for attempt in range(1, retries + 1):  # Try up to 'retries' times
        try:
            if downloaded_size > 0:
                headers["Range"] = f"bytes={downloaded_size}-"  # Set the Range header to resume the download
            else:
                headers.pop("Range", None)

            print(f"⬇️  Downloading {fname} (attempt {attempt})... starting at {downloaded_size} bytes")

            start_time = time.time()  # Start the timer to calculate download speed
            with requests.get(url, headers=headers, stream=True, timeout=30) as r:
                if r.status_code not in (200, 206):  # If the server does not support resume, restart
                    print(f"❌ Server did not support resume (status {r.status_code}). Restarting download.")
                    downloaded_size = 0
                    open(tmp_path, "wb").close()  # reset the temporary file
                    continue

                mode = "ab" if downloaded_size > 0 else "wb"  # Open the file in append mode if resuming
                total_size = int(r.headers.get("Content-Length", 0)) + downloaded_size
                bytes_downloaded = 0

                with open(tmp_path, mode) as f:
                    for chunk in r.iter_content(chunk_size=8192):
                        if chunk:
                            f.write(chunk)
                            bytes_downloaded += len(chunk)

                            # Calculate elapsed time and download speed
                            elapsed_time = time.time() - start_time
                            if elapsed_time > 0:
                                download_speed = bytes_downloaded / elapsed_time
                                download_speed_kbps = download_speed / 1024  # Convert to KB/s
                                download_speed_mbps = download_speed_kbps / 1024  # Convert to MB/s

                                # Print download progress and speed
                                progress = f"{bytes_downloaded}/{total_size} bytes"
                                print(f"\r⬇️  Downloading {fname} — {progress} {download_speed_mbps:.2f} MB/s", end="")

            final_size = os.path.getsize(tmp_path)
            if total_size > 0 and final_size < total_size:
                print(f"⚠️  Incomplete ({final_size}/{total_size}), retrying...")
                downloaded_size = final_size
                time.sleep(2)
                continue

            os.rename(tmp_path, path)  # Rename the temporary file to the final path
            print(f"\n✅ Finished {fname} ({final_size} bytes)")

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
    """
    Given a list of URLs, download all the images to the specified directory.
    Prints the download progress and success rate.
    """
    print(f"📁 Downloading {len(urls)} image(s) to '{outdir}'...")
    success = 0
    for url in urls:
        if safe_download(url, outdir):  # Call safe_download for each image URL
            success += 1
    print(f"✅ Done. Successfully downloaded {success}/{len(urls)} images.")


# ------------------------------ Entry Point ---------------------------------

def resolve_reddit_post_from_image(image_url):
    """
    Given an image URL, try to find the Reddit post that contains it.
    If successful, return the subreddit and post title slug.
    If unsuccessful, return 'direct_links' as subreddit and image ID as the title.
    """
    try:
        image_id = os.path.splitext(os.path.basename(image_url))[0]  # Extract image ID from the URL
        headers = {"User-Agent": "RedditDownloader/1.0"}

        # Search Reddit API to find the post referencing the image
        search_url = f"https://api.reddit.com/search/?q=url:{image_id}&restrict_sr=0"
        resp = requests.get(search_url, headers=headers, timeout=10)
        resp.raise_for_status()
        data = resp.json()

        children = data.get("data", {}).get("children", [])
        if not children:
            return "direct_links", image_id

        post = children[0]["data"]
        subreddit = post.get("subreddit", "direct_links")
        title_slug = (
            post.get("title", "")
            .strip()
            .replace(" ", "_")
            .replace("/", "_")
            .replace("\\", "_")
            [:80]
        )
        print(f"resolved image url {image_url} to subreddit {subreddit}, title {title_slug}")

        return subreddit, title_slug
    except Exception as e:
        print(f"⚠️  Could not resolve subreddit/title for {image_url}: {e}")
        image_id = os.path.splitext(os.path.basename(image_url))[0]
        return "direct_links", image_id

from urllib.parse import urlparse, parse_qs, unquote

def normalize_media_redirect(url):
    """
    If the URL is a Reddit media redirect (/media?url=...), extract the real image URL.
    e.g. 
    https://www.reddit.com/media?url=https%3A%2F%2Fi.redd.it%2Fo9fu9uw82rvf1.png
    should resolve to:
    https://i.redd.it/o9fu9uw82rvf1.png

    For example:
    
    >>> normalize_media_redirect('https://www.reddit.com/media?url=https%3A%2F%2Fi.redd.it%2Fo9fu9uw82rvf1.png')
    ↪️  Resolved media redirect to: https://i.redd.it/o9fu9uw82rvf1.png
    'https://i.redd.it/o9fu9uw82rvf1.png'

    >>> normalize_media_redirect('https://www.reddit.com/media?url=https%3A%2F%2Fi.redd.it%2F12345.png')
    ↪️  Resolved media redirect to: https://i.redd.it/12345.png
    'https://i.redd.it/12345.png'

    >>> normalize_media_redirect('https://www.reddit.com/r/test/comments/12345/test_image/')
    'https://www.reddit.com/r/test/comments/12345/test_image/'
    
    """
    parsed = urlparse(url)
    if "reddit.com" in parsed.netloc and parsed.path == "/media":
        qs = parse_qs(parsed.query)
        if "url" in qs:
            real_url = unquote(qs["url"][0])
            print(f"↪️  Resolved media redirect to: {real_url}")
            return real_url
    return url


# -------------------------- Main Logic --------------------------------------

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: reddit_gallery_download.py <reddit_post_url> [output_folder]")
        sys.exit(1)

    # --- Always normalize first ---
    # https://i.redd.it/o9fu9uw82rvf1.png
    # can also be expressed as a 'media redirect'
    # https://www.reddit.com/media?url=https%3A%2F%2Fi.redd.it%2Fo9fu9uw82rvf1.png

    post_url = normalize_media_redirect(sys.argv[1])

    # --- Handle direct image URLs (i.redd.it) ---
    if post_url.startswith("https://i.redd.it/"):
        urls = [post_url]
        subreddit, post_title_slug = resolve_reddit_post_from_image(post_url)

    # --- Handle normal Reddit posts or galleries ---
    else:
        urls, subreddit, post_title_slug = get_gallery_image_urls(post_url)

    if not subreddit:
        subreddit = "unknown_subreddit"
    if not post_title_slug:
        post_title_slug = "post"

    # --- Define output directory ---
    if len(sys.argv) >= 3:
        outdir = sys.argv[2]
    else:
        outdir = os.path.join(subreddit, post_title_slug)

    if not urls:
        print("No images found.")
        sys.exit(1)

    download_images(urls, outdir)  # Download the images to the specified directory
