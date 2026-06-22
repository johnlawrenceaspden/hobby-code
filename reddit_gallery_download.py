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
from urllib.parse import urlparse

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


# ======================================================
# SESSION
# ======================================================
def get_session():
    s = requests.Session()
    s.headers.update(HEADERS)

    try:
        s.cookies.update(browser_cookie3.firefox())
        print("[+] Firefox cookies loaded")
    except Exception:
        print("[!] No Firefox cookies found (anonymous mode)")

    return s


# ======================================================
# CLASSIFICATION
# ======================================================
def classify_url(url):
    if "i.redd.it" in url:
        return "image"
    if "reddit.com/gallery" in url:
        return "gallery"
    if "reddit.com/comments" in url:
        return "post"
    return "unknown"


def extract_post_id(url):
    m = re.search(r"(?:gallery|comments)/([a-zA-Z0-9]+)", url)
    if not m:
        raise ValueError("Invalid Reddit post URL")
    return m.group(1)


# ======================================================
# UTILITIES
# ======================================================
def safe_name(text):
    text = text or "untitled"
    text = re.sub(r"[^a-zA-Z0-9-_ ]", "", text)
    text = re.sub(r"\s+", " ", text).strip()
    return text[:80].rstrip(". ")


def guess_from_url(url, fallback_id):
    subreddit = "unknown"
    if "/r/" in url:
        try:
            subreddit = url.split("/r/")[1].split("/")[0]
        except Exception:
            pass

    return {
        "title": fallback_id,
        "subreddit": subreddit,
    }


# ======================================================
# IMAGE → POST RESOLUTION (best-effort)
# ======================================================
def resolve_image_to_post(session, url):
    headers = {
        "User-Agent": HEADERS["User-Agent"],
        "Accept": "text/html,application/xhtml+xml",
        "Referer": "https://www.reddit.com/",
    }

    try:
        r = session.get(url, headers=headers, allow_redirects=True, timeout=30)
    except Exception:
        return None

    # If redirected directly to Reddit post
    if "reddit.com/r/" in r.url or "reddit.com/comments/" in r.url:
        return r.url

    if "text/html" in r.headers.get("Content-Type", ""):
        soup = BeautifulSoup(r.text, "html.parser")

        tag = soup.find("meta", property="og:url")
        if tag and tag.get("content"):
            return tag["content"]

        m = re.search(r"https://www\.reddit\.com/r/[^\"'\s]+", r.text)
        if m:
            return m.group(0)

    return None


# ======================================================
# FETCH POST DATA
# ======================================================
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

    images = []

    gallery = post.get("gallery_data", {}).get("items", [])
    media = post.get("media_metadata", {})

    for item in gallery:
        mid = item["media_id"]
        m = media.get(mid, {})
        if "s" not in m:
            continue
        images.append(m["s"]["u"].replace("&amp;", "&"))

    return meta, images


def fetch_html(session, post_id):
    url = f"https://www.reddit.com/comments/{post_id}"
    r = session.get(url, timeout=30)
    return r.text


def parse_html(html):
    soup = BeautifulSoup(html, "html.parser")

    script = soup.find("script", id="__NEXT_DATA__")
    if not script:
        return {"title": None, "subreddit": None}, []

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

    images = re.findall(
        r"https://i\.redd\.it/[a-zA-Z0-9_/.-]+\.(?:jpg|jpeg|png|webp|gif)",
        html
    )

    return meta, images


# ======================================================
# DOWNLOAD
# ======================================================
def download(session, url, path):
    r = session.get(url, stream=True, timeout=60)
    r.raise_for_status()

    h = hashlib.sha1()

    with open(path, "wb") as f:
        for chunk in r.iter_content(1024 * 64):
            f.write(chunk)
            h.update(chunk)

    return h.hexdigest()


# ======================================================
# MAIN PIPELINE
# ======================================================
def main():
    if len(sys.argv) != 2:
        print("Usage: redditdl <url>")
        sys.exit(1)

    url = sys.argv[1]
    session = get_session()

    kind = classify_url(url)
    meta = None
    images = None

    # ==================================================
    # STEP 1: IMAGE INPUT
    # ==================================================
    if kind == "image":
        print("[*] Direct image detected")

        resolved = resolve_image_to_post(session, url)

        if resolved and "reddit.com" in resolved:
            print(f"[+] Resolved → {resolved}")
            url = resolved
            kind = classify_url(url)
        else:
            print("[!] No post found → direct mode")

            img_id = url.split("/")[-1].split(".")[0]
            meta = guess_from_url(url, img_id)
            images = [url]

    # ==================================================
    # STEP 2: DIRECT MODE
    # ==================================================
    if images is not None:
        title = safe_name(meta["title"])
        subreddit = meta["subreddit"]

        out_dir = Path("direct") / subreddit / title
        out_dir.mkdir(parents=True, exist_ok=True)

        out_file = out_dir / "01.jpg"

        print("[+] Downloading direct image")
        download(session, url, out_file)

        print(f"[✓] Done → {out_dir}")
        return

    # ==================================================
    # STEP 3: REDDIT POST MODE
    # ==================================================
    post_id = extract_post_id(url)
    meta = guess_from_url(url, post_id)

    data = fetch_json(session, post_id)

    if data:
        try:
            meta_json, images = parse_json(data)
            meta.update({k: v for k, v in meta_json.items() if v})
        except Exception:
            pass

    if not images:
        print("[*] Falling back to HTML")
        html = fetch_html(session, post_id)
        meta_html, images = parse_html(html)
        meta.update({k: v for k, v in meta_html.items() if v})

    if not images:
        print("[-] No images found")
        sys.exit(1)

    # ==================================================
    # OUTPUT
    # ==================================================
    title = safe_name(meta.get("title"))
    subreddit = meta.get("subreddit") or "unknown"

    out_dir = Path(subreddit) / title
    out_dir.mkdir(parents=True, exist_ok=True)

    print(f"[r/{subreddit}] {meta.get('title')}")
    print(f"[+] Downloading {len(images)} file(s)")

    hashes = set()

    for i, img in enumerate(images, 1):
        ext = urlparse(img).path.split(".")[-1].lower()
        if ext not in {"jpg", "jpeg", "png", "webp", "gif"}:
            ext = "jpg"

        out_file = out_dir / f"{i:02d}.{ext}"

        try:
            h = download(session, img, out_file)

            if h in hashes:
                out_file.unlink()
                continue

            hashes.add(h)
            print(f"[+] {out_file.name}")

        except Exception as e:
            print(f"[!] Failed {img}: {e}")

    print(f"[✓] Done → {out_dir}")


if __name__ == "__main__":
    main()
