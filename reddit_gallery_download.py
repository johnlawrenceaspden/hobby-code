#!/usr/bin/env python3


# python3 -m venv venv
# source venv/bin/activate
# pip install requests browser-cookie3 beautifulsoup4

# or

# pip install beautifulsoup4 --break-system-packages


import re
import sys
import json
from pathlib import Path

import requests
import browser_cookie3
from bs4 import BeautifulSoup


HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (X11; Linux x86_64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0 Safari/537.36"
    ),
    "Accept-Language": "en-US,en;q=0.9",
}


def get_session():
    """Create session with Firefox cookies if available."""
    session = requests.Session()
    session.headers.update(HEADERS)

    try:
        cookies = browser_cookie3.firefox()
        session.cookies.update(cookies)
        print("[+] Loaded Firefox cookies")
    except Exception as e:
        print("[!] Could not load Firefox cookies:", e)

    return session


def extract_post_id(url):
    m = re.search(r"gallery/([a-zA-Z0-9]+)", url)
    if not m:
        m = re.search(r"comments/([a-zA-Z0-9]+)", url)
    if not m:
        raise ValueError("Invalid Reddit URL")
    return m.group(1)


def try_json(session, post_id):
    url = f"https://www.reddit.com/comments/{post_id}/.json"
    r = session.get(url, timeout=30)

    if r.status_code != 200:
        print(f"[!] JSON blocked ({r.status_code})")
        return None

    return r.json()


def parse_gallery_from_json(data):
    post = data[0]["data"]["children"][0]["data"]

    gallery = post.get("gallery_data", {}).get("items", [])
    media = post.get("media_metadata", {})

    images = []

    for item in gallery:
        mid = item["media_id"]
        meta = media.get(mid, {})
        if "s" not in meta:
            continue

        url = meta["s"]["u"].replace("&amp;", "&")
        images.append(url)

    return images


def extract_from_html(session, post_id):
    """Fallback: parse HTML for i.redd.it links."""
    url = f"https://www.reddit.com/gallery/{post_id}"
    r = session.get(url, timeout=30)

    soup = BeautifulSoup(r.text, "html.parser")

    text = soup.get_text(" ")

    # crude but effective fallback
    urls = re.findall(r"https://i\.redd\.it/[a-zA-Z0-9]+\.(jpg|png|jpeg)", r.text)

    # rebuild full matches (regex only captured extension)
    full_urls = re.findall(r"https://i\.redd\.it/[^\s\"']+", r.text)

    # dedupe
    seen = set()
    cleaned = []
    for u in full_urls:
        if u not in seen:
            seen.add(u)
            cleaned.append(u)

    return cleaned


def download(session, url, out_path):
    r = session.get(url, stream=True, timeout=60)
    r.raise_for_status()

    with open(out_path, "wb") as f:
        for chunk in r.iter_content(1024 * 64):
            f.write(chunk)


def main():
    if len(sys.argv) != 2:
        print("Usage: python reddit_gallery.py <url>")
        sys.exit(1)

    url = sys.argv[1]
    post_id = extract_post_id(url)

    session = get_session()

    images = None

    # 1. Try JSON (best case)
    data = try_json(session, post_id)
    if data:
        try:
            images = parse_gallery_from_json(data)
        except Exception as e:
            print("[!] JSON parse failed:", e)

    # 2. Fallback HTML scraping
    if not images:
        print("[*] Falling back to HTML extraction...")
        images = extract_from_html(session, post_id)

    if not images:
        print("[-] No images found")
        sys.exit(1)

    out_dir = Path(post_id)
    out_dir.mkdir(exist_ok=True)

    print(f"[+] Found {len(images)} images")

    for i, img in enumerate(images, 1):
        ext = img.split("?")[0].split(".")[-1]
        if len(ext) > 5:
            ext = "jpg"

        out_file = out_dir / f"{i:02d}.{ext}"

        print(f"[+] Downloading {out_file}")
        download(session, img, out_file)

    print("[✓] Done")


if __name__ == "__main__":
    main()
