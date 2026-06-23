#!/usr/bin/env python3


# Needs beautifulsoup4, which is a debian package
# sudo apt install python3-bs4


"""
reddit image downloader

This script downloads images from Reddit posts, galleries, and direct image URLs.

SUPPORTED INPUTS:
- Direct image links (i.redd.it, preview.redd.it)
- Reddit post links (/comments/)
- Reddit gallery links (/gallery/)
- reddit.com/media wrapper URLs

FEATURES:
- Uses Firefox cookies (if available) for authenticated access
- Falls back from Reddit JSON API → HTML scraping
- Resumable downloads (.part files)
- Organised folder structure by subreddit + post title
"""





# Example cases of various reddit image links
# broken (video links not supported)
# r https://v.redd.it/yu7m1sqb658h1

# working
# r https://i.redd.it/628wzjftft1h1.png
# r https://i.redd.it/hrc30eeqqosg1.jpeg
# r https://i.redd.it/o9fu9uw82rvf1.png

# r https://www.reddit.com/gallery/1ubp2ao
# r https://www.reddit.com/gallery/1tu4cki
# r https://www.reddit.com/gallery/1tyw4xv

# r https://www.reddit.com/media?url=https%3A%2F%2Fi.redd.it%2Fmnjzwo59eg1h1.png

# r https://www.reddit.com/r/dalle2/comments/1temwv1/dalle2_revival/
# r https://www.reddit.com/r/dalle2/comments/1tevijp/any_suggestions_for_a_model_more_like_the/
# r https://www.reddit.com/r/dalle2/comments/1tgaphy/i_made_an_ai_image_that_anyone_can_add_to/
# r https://www.reddit.com/r/dalle2/comments/1tyw4xv/my_dalle_images_from_2022_havent_used_it_since/









# ======================================================
# ARCHITECTURE
# ======================================================
#
# URL
#  ↓
# build_image_list()
#  ├─ direct image
#  └─ Reddit post/gallery
#         ↓
#     JSON parser
#         ↓
#     HTML fallback
#         ↓
# image list
#         ↓
# process()
#         ↓
# download()
#
# ======================================================

import re
import sys
import json
import time
import hashlib
from pathlib import Path
from urllib.parse import urlparse, parse_qs, unquote

import requests
import browser_cookie3
from bs4 import BeautifulSoup


# ======================================================
# USER AGENT (pretend to be a browser)
# ======================================================
HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (X11; Linux x86_64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0 Safari/537.36"
    )
}


# ======================================================
# SESSION SETUP
# ======================================================
def get_session():
    """
    Create a requests session with:
    - Browser-like headers
    - Optional Firefox cookies (for logged-in access)

    This improves access to restricted or rate-limited content.
    """
    s = requests.Session()
    s.headers.update(HEADERS)

    try:
        s.cookies.update(browser_cookie3.firefox())
        print("[+] Firefox cookies loaded")
    except Exception:
        print("[!] No Firefox cookies found (anonymous mode)")

    return s


# ======================================================
# SMALL UTILITIES
# ======================================================
def file_exists(path: Path):
    """
    Check if a file already exists and is non-empty.
    Used to avoid re-downloading.
    """
    return path.exists() and path.stat().st_size > 0


def unwrap_media_url(url):
    """
    Reddit sometimes wraps media URLs like:

    reddit.com/media?url=<encoded>

    This extracts the real image URL.
    """
    if "reddit.com/media" not in url:
        return url
    try:
        qs = parse_qs(urlparse(url).query)
        if "url" in qs:
            return unquote(qs["url"][0])
    except Exception:
        pass
    return url


def safe_name(text):
    """
    Convert Reddit titles into safe directory names.

    Example:
        "My awesome image post!"
        -> "my-awesome-image-post"
    """
    text = text or "untitled"

    # Remove characters we don't want in filenames
    text = re.sub(r"[^a-zA-Z0-9-_ ]", "", text)

    # Collapse multiple spaces into one
    text = re.sub(r"\s+", " ", text).strip()

    # lowercase only
    text = text.lower()

    # Replace spaces with dashes
    text = text.replace(" ", "-")

    return text[:80].rstrip(".-")

# ======================================================
# IMAGE ID EXTRACTION
# ======================================================
def extract_reddit_image_id(url):
    """
    Try to extract a stable ID from Reddit image URLs.

    Examples:
        i.redd.it/abc123.png → abc123
        preview.redd.it/abc123-LQ.jpg → abc123

    Used to generate consistent filenames.

    Falls back to a hash if parsing fails.
    """
    path = urlparse(url).path
    filename = path.split("/")[-1]

    base = filename.split(".")[0]
    base = base.split("-")[0]

    if re.match(r"^[a-zA-Z0-9]+$", base):
        return base

    return hashlib.sha1(url.encode()).hexdigest()[:10]

# ======================================================
# REDDIT JSON API
# ======================================================
def fetch_json(session, post_id):
    """
    Fetch Reddit post data in JSON format.

    This is the primary (preferred) data source.
    """
    url = f"https://www.reddit.com/comments/{post_id}/.json"
    r = session.get(url, timeout=30)
    if r.status_code != 200:
        return None
    return r.json()


def parse_json(data):
    """
    Extract metadata + image URLs from Reddit JSON.

    Supports:
    - gallery posts
    - single-image posts

    Returns:
        meta: {title, subreddit}
        images: [url, url, url]
    """
    post = data[0]["data"]["children"][0]["data"]

    meta = {
        "title": post.get("title"),
        "subreddit": post.get("subreddit"),
    }

    images = []

    # Gallery support
    for item in post.get("gallery_data", {}).get("items", []):
        mid = item.get("media_id")
        m = post.get("media_metadata", {}).get(mid, {})
        if "s" in m:
            images.append(m["s"]["u"].replace("&amp;", "&"))

    # Fallback: single image post
    if not images and post.get("url"):
        images.append(post["url"])

    return meta, images


# ======================================================
# HTML FALLBACK (when JSON fails)
# ======================================================
def extract_images(html):
    """
    Regex-based extraction of image URLs from HTML.
    """
    imgs = set()

    imgs.update(re.findall(r"https://i\.redd\.it/[^\s\"']+", html))
    imgs.update(re.findall(r"https://preview\.redd\.it/[^\s\"']+", html))
    imgs.update(re.findall(r"https://external-preview\.redd\.it/[^\s\"']+", html))

    return list(imgs)


def parse_html(session, post_id):
    """
    Backup parser when Reddit JSON is unavailable.

    Extracts:
    - metadata from __NEXT_DATA__
    - images from raw HTML
    """
    url = f"https://www.reddit.com/comments/{post_id}"
    r = session.get(url, timeout=30)
    html = r.text

    meta = {"title": None, "subreddit": None}

    soup = BeautifulSoup(html, "html.parser")

    script = soup.find("script", id="__NEXT_DATA__")
    if script:
        try:
            data = json.loads(script.string)
            post = (
                data.get("props", {})
                    .get("pageProps", {})
                    .get("post", {})
                    .get("post", {})
            ) or {}

            meta = {
                "title": post.get("title"),
                "subreddit": post.get("subredditName"),
            }
        except Exception:
            pass

    return meta, extract_images(html)


# ======================================================
# DOWNLOAD ENGINE (supports resume + progress)
# ======================================================
def download(session, url, path: Path):
    """
    Download a file with:
    - Resume support (.part files)
    - Progress display
    - Speed calculation
    """

    tmp = path.with_suffix(path.suffix + ".part")

    # check if partial download exists
    existing = tmp.stat().st_size if tmp.exists() else 0

    headers = {"User-Agent": HEADERS["User-Agent"]}

    # resume download if partial exists
    if existing > 0:
        headers["Range"] = f"bytes={existing}-"
        print(f"[↻] Resuming {path.name}")

    r = session.get(url, stream=True, headers=headers, timeout=60)

    if r.status_code not in (200, 206):
        r.raise_for_status()

    total = r.headers.get("Content-Length")
    if total:
        total = int(total) + existing

    start = time.time()
    downloaded = existing
    last = time.time()

    with open(tmp, "ab" if existing else "wb") as f:
        for chunk in r.iter_content(1024 * 64):
            if not chunk:
                continue

            f.write(chunk)
            downloaded += len(chunk)

            # update progress every ~0.4 seconds
            now = time.time()

            if now - last > 0.4:
                speed = (downloaded - existing) / (now - start) if start else 0

                if total:
                    pct = downloaded / total * 100
                    msg = (
                        f"[↓] {downloaded/1024/1024:.1f}MB/"
                        f"{total/1024/1024:.1f}MB "
                        f"({pct:.1f}%) @ {speed/1024:.1f}KB/s"
                    )
                else:
                    msg = f"[↓] {downloaded/1024/1024:.1f}MB @ {speed/1024:.1f}KB/s"

                print("\r" + msg, end="", flush=True)
                last = now

    print()

    # finalise file
    tmp.rename(path)


# ======================================================
# IMAGE LIST BUILDER (core logic)
# ======================================================
def build_image_list(url, session):
    """
    Convert any supported Reddit URL into a list of images.

    Handles:
    - direct image links
    - gallery links
    - post links

    Uses:
    1. Reddit JSON API (preferred)
    2. HTML fallback (if JSON fails)
    """

    url = unwrap_media_url(url)

    # -------------------------
    # DIRECT IMAGE MODE
    # -------------------------
    if "i.redd.it" in url or "preview.redd.it" in url:
        img_id = extract_reddit_image_id(url)
        ext = urlparse(url).path.split(".")[-1].lower()
        if ext not in {"jpg", "jpeg", "png", "webp", "gif"}:
            ext = "jpg"

        meta = {"title": img_id, "subreddit": "direct"}
        return meta, [(url, img_id, ext)]

    # -------------------------
    # POST / GALLERY MODE
    # -------------------------
    post_id = re.search(r"(?:gallery|comments)/([a-zA-Z0-9]+)", url)
    if not post_id:
        return {"title": "unknown", "subreddit": "unknown"}, []

    post_id = post_id.group(1)

    meta = {"title": post_id, "subreddit": "unknown"}
    images = []

    data = fetch_json(session, post_id)

    if data:
        try:
            m, imgs = parse_json(data)
            meta.update({k: v for k, v in m.items() if v})
            images = imgs
        except Exception:
            pass

    # fallback if JSON fails
    if not images:
        print("[*] HTML fallback")
        m, images = parse_html(session, post_id)
        meta.update({k: v for k, v in m.items() if v})

    # normalise images into (url, id, ext)
    out = []
    for img in images:
        img_id = extract_reddit_image_id(img)
        ext = urlparse(img).path.split(".")[-1].lower()
        if ext not in {"jpg", "jpeg", "png", "webp", "gif"}:
            ext = "jpg"
        out.append((img, img_id, ext))

    return meta, out


# ======================================================
# MAIN PROCESSOR
# ======================================================
def process(url, session):
    """
    Main orchestration function:

    - Build image list
    - Create output folders
    - Download images
    - Skip existing files
    """

    meta, images = build_image_list(url, session)
    print(f"{meta=}")
    print(f"[meta] {meta}")
    print(images)

    if not images:
        print("[-] No images found")
        sys.exit(1)

    title = safe_name(meta["title"])
    subreddit = meta["subreddit"] or "unknown"

    out_dir = Path(subreddit) / title
    out_dir.mkdir(parents=True, exist_ok=True)

    print(f"[URL] {url}")
    print(f"[r/{subreddit}] {meta['title']}")
    print(f"[+] Downloading {len(images)} file(s)")
    print(f"[→] Output directory: {out_dir}")


    for img, img_id, ext in images:

        # direct images go into flat folder
        if meta["subreddit"] == "direct":
            out_dir = Path("direct")
            out_dir.mkdir(parents=True, exist_ok=True)
            out_file = out_dir / f"{img_id}.{ext}"

        # posts/galleries keep structured folders
        else:
            out_file = out_dir / f"{img_id}.{ext}"

        # skip already downloaded files
        if file_exists(out_file):
            print(f"[=] Skipping {out_file.name}")
            continue

        try:
            download(session, img, out_file)
        except Exception as e:
            print(f"[!] Failed {img}: {e}")

    print(f"[✓] Done → {out_dir}")


# ======================================================
# CLI ENTRY POINT
# ======================================================
def main():
    """
    Command line interface:

        python redditdl.py <url>
    """
    if len(sys.argv) != 2:
        print("Usage: redditdl <url>")
        sys.exit(1)

    session = get_session()
    process(sys.argv[1], session)


if __name__ == "__main__":
    main()
