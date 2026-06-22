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
# URL helpers
# ----------------------------
def extract_post_id(url):
    m = re.search(r"(?:gallery|comments)/([a-zA-Z0-9]+)", url)
    if not m:
        raise ValueError("Invalid Reddit URL")
    return m.group(1)


def is_direct_image_url(url):
    return "i.redd.it/" in url


def safe_name(text):
    text = text or "untitled"
    text = re.sub(r"[^a-zA-Z0-9-_ ]", "", text)
    text = re.sub(r"\s+", " ", text).strip()
    return text[:80].rstrip(". ")


def guess_from_url(url, post_id):
    parts = url.split("/")
    subreddit = "unknown"

    if "/r/" in url:
        try:
            subreddit = parts[parts.index("r") + 1]
        except Exception:
            pass

    return {
        "title": post_id,
        "subreddit": subreddit,
    }


# ----------------------------
# Resolve i.redd.it → Reddit post
# ----------------------------
def resolve_reddit_post_from_image(session, url):
    try:
        r = session.get(url, timeout=30)
    except Exception:
        return None

    html = r.text
    soup = BeautifulSoup(html, "html.parser")

    # 1. OG canonical URL
    tag = soup.find("meta", property="og:url")
    if tag and tag.get("content"):
        return tag["content"]

    # 2. direct reddit link in HTML
    m = re.search(r"https://www\.reddit\.com/r/[^\"'\s]+", html)
    if m:
        return m.group(0)

    # 3. embedded permalink JSON
    m = re.search(r"\"permalink\":\"(\/r\/[^\"']+)\"", html)
    if m:
        return "https://www.reddit.com" + m.group(1)

    return None


# ----------------------------
# JSON mode
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
    ) or {}

    return {
        "title": post.get("title"),
        "subreddit": post.get("subredditName"),
    }


def parse_html_images(html):
    urls = re.findall(r"https://i\.redd\.it/[a-zA-Z0-9_/.-]+\.(?:jpg|jpeg|png|webp|gif)", html)

    seen = set()
    out = []
    for u in urls:
        if u not in seen:
            seen.add(u)
            out.append(u)

    return out


# ----------------------------
# Download
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
# Main
# ----------------------------
def main():
    if len(sys.argv) != 2:
        print("Usage: redditdl <url>")
        sys.exit(1)

    url = sys.argv[1]
    session = get_session()

    images = None
    meta = None

    # ----------------------------
    # CASE 1: direct image URL
    # ----------------------------
    if is_direct_image_url(url):
        print("[*] Direct i.redd.it image detected")

        resolved = resolve_reddit_post_from_image(session, url)

        if resolved:
            print(f"[+] Resolved to: {resolved}")
            url = resolved
        else:
            print("[!] Could not resolve post — downloading directly")

            meta = guess_from_url(url, url.split("/")[-1].split(".")[0])
            images = [url]

    # ----------------------------
    # CASE 2: normal Reddit post
    # ----------------------------
    if images is None:
        post_id = extract_post_id(url)

        meta = guess_from_url(url, post_id)

        data = fetch_json(session, post_id)

        if data:
            try:
                meta_json, images = parse_json(data)
                meta.update({k: v for k, v in meta_json.items() if v})
            except Exception as e:
                print("[!] JSON failed:", e)

        if not images:
            print("[*] Falling back to HTML...")
            html = fetch_html(session, post_id)

            meta_html = parse_html_meta(html)
            images = parse_html_images(html)

            meta.update({k: v for k, v in meta_html.items() if v})

    if not images:
        print("[-] No images found")
        sys.exit(1)

    # ----------------------------
    # Output folder
    # ----------------------------
    title = safe_name(meta.get("title"))
    subreddit = meta.get("subreddit") or "unknown"

    out_dir = Path(subreddit) / f"{title}"
    out_dir.mkdir(parents=True, exist_ok=True)

    print(f"[r/{subreddit}] {meta.get('title')}")
    print(f"[+] Downloading {len(images)} file(s)")

    hashes = set()

    # ----------------------------
    # Download loop
    # ----------------------------
    for i, img in enumerate(images, 1):

        path_part = img.split("?")[0].split(".")[-1].lower()
        if path_part not in {"jpg", "jpeg", "png", "webp", "gif"}:
            path_part = "jpg"

        out_file = out_dir / f"{i:02d}.{path_part}"

        try:
            h = download(session, img, out_file)

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

