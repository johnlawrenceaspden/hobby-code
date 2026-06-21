#!/usr/bin/env python3


# python3 -m venv venv
# source venv/bin/activate
# pip install requests browser-cookie3 beautifulsoup4

# or

# pip install beautifulsoup4 --break-system-packages


#!/usr/bin/env python3

import re
import sys
import json
import hashlib
from pathlib import Path

import requests
import browser_cookie3
from bs4 import BeautifulSoup


HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (X11; Linux x86_64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0 Safari/537.36"
    )
}


# ----------------------------
# Session
# ----------------------------
def get_session():
    s = requests.Session()
    s.headers.update(HEADERS)

    try:
        s.cookies.update(browser_cookie3.firefox())
        print("[+] Firefox cookies loaded")
    except Exception:
        print("[!] No Firefox cookies found (continuing anonymous)")

    return s


# ----------------------------
# Parsing helpers
# ----------------------------
def extract_post_id(url):
    m = re.search(r"(?:gallery|comments)/([a-zA-Z0-9]+)", url)
    if not m:
        raise ValueError("Invalid Reddit URL")
    return m.group(1)


def safe_name(text):
    text = text or "untitled"
    text = re.sub(r"[^a-zA-Z0-9-_ ]", "", text)
    return text.strip()[:80]


# ----------------------------
# JSON mode (best)
# ----------------------------
def fetch_json(session, post_id):
    url = f"https://www.reddit.com/comments/{post_id}/.json"
    r = session.get(url, timeout=30)
    if r.status_code != 200:
        return None
    return r.json()


def parse_json(data):
    post = data[0]["data"]["children"][0]["data"]

    meta = {
        "title": post.get("title"),
        "subreddit": post.get("subreddit"),
    }

    gallery = post.get("gallery_data", {}).get("items", [])
    media = post.get("media_metadata", {})

    images = []

    for item in gallery:
        mid = item["media_id"]
        m = media.get(mid, {})
        if "s" not in m:
            continue
        url = m["s"]["u"].replace("&amp;", "&")
        images.append(url)

    return meta, images


# ----------------------------
# HTML fallback
# ----------------------------
def fetch_html(session, post_id):
    url = f"https://www.reddit.com/comments/{post_id}"
    r = session.get(url, timeout=30)
    return r.text


def parse_html_meta(html):
    soup = BeautifulSoup(html, "html.parser")

    script = soup.find("script", id="__NEXT_DATA__")
    if not script:
        return {"title": None, "subreddit": None}

    data = json.loads(script.string)

    post = (
        data.get("props", {})
            .get("pageProps", {})
            .get("post", {})
            .get("post", {})
    )
    
    return {
        "title": post.get("title"),
        "subreddit": post.get("subredditName"),
    }


def parse_html_images(html):
    # capture i.redd.it assets
    urls = re.findall(r"https://i\.redd\.it/[^\s\"']+", html)

    # dedupe preserving order
    seen = set()
    out = []
    for u in urls:
        if u not in seen:
            seen.add(u)
            out.append(u)

    return out


# ----------------------------
# Downloading
# ----------------------------
def download(session, url, path):
    r = session.get(url, stream=True, timeout=60)
    r.raise_for_status()

    h = hashlib.sha1()

    with open(path, "wb") as f:
        for chunk in r.iter_content(1024 * 64):
            f.write(chunk)
            h.update(chunk)

    return h.hexdigest()


# ----------------------------
# Main logic
# ----------------------------
def main():
    if len(sys.argv) != 2:
        print("Usage: redditdl <url>")
        sys.exit(1)

    url = sys.argv[1]
    post_id = extract_post_id(url)

    session = get_session()

    meta = {"title": post_id, "subreddit": "unknown"}
    images = None

    # --- JSON attempt ---
    data = fetch_json(session, post_id)

    if data:
        try:
            meta, images = parse_json(data)
        except Exception as e:
            print("[!] JSON parse failed:", e)

    # --- HTML fallback ---
    if not images:
        print("[*] Falling back to HTML...")
        html = fetch_html(session, post_id)

        meta = parse_html_meta(html)
        images = parse_html_images(html)

    if not images:
        print("[-] No images found")
        sys.exit(1)

    title = safe_name(meta["title"])
    subreddit = meta["subreddit"] or "unknown"

    out_dir = Path(subreddit) / f"{title}_{post_id}"
    
    out_dir.mkdir(parents=True, exist_ok=True)

    print(f"[r/{subreddit}] {meta['title']}")
    print(f"[+] Downloading {len(images)} files")

    hashes = set()

    for i, img in enumerate(images, 1):
        ext = img.split("?")[0].split(".")[-1]
        if len(ext) > 5:
            ext = "jpg"

        out_file = out_dir / f"{i:02d}.{ext}"

        try:
            h = download(session, img, out_file)

            # simple duplicate detection
            if h in hashes:
                print(f"[!] Duplicate skipped {out_file.name}")
                out_file.unlink()
                continue

            hashes.add(h)
            print(f"[+] {out_file.name}")

        except Exception as e:
            print(f"[!] Failed {img}: {e}")

    print(f"[✓] Done → {out_dir}")


if __name__ == "__main__":
    main()


