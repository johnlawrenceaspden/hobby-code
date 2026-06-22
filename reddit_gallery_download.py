#!/usr/bin/env python3


# python3 -m venv venv
# source venv/bin/activate
# pip install requests browser-cookie3 beautifulsoup4

# or

# pip install beautifulsoup4 --break-system-packages

"""
broken
r https://v.redd.it/yu7m1sqb658h1


working



r https://i.redd.it/628wzjftft1h1.png
r https://i.redd.it/hrc30eeqqosg1.jpeg
r https://i.redd.it/o9fu9uw82rvf1.png

r https://www.reddit.com/gallery/1ubp2ao
r https://www.reddit.com/gallery/1tu4cki
r https://www.reddit.com/gallery/1tyw4xv

r https://www.reddit.com/media?url=https%3A%2F%2Fi.redd.it%2Fmnjzwo59eg1h1.png

r https://www.reddit.com/r/dalle2/comments/1temwv1/dalle2_revival/
r https://www.reddit.com/r/dalle2/comments/1tevijp/any_suggestions_for_a_model_more_like_the/
r https://www.reddit.com/r/dalle2/comments/1tgaphy/i_made_an_ai_image_that_anyone_can_add_to/
r https://www.reddit.com/r/dalle2/comments/1tyw4xv/my_dalle_images_from_2022_havent_used_it_since/
"""

#!/usr/bin/env python3

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
# HELPERS
# ======================================================
def file_exists(path: Path):
    return path.exists() and path.stat().st_size > 0


def unwrap_media_url(url):
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
    text = text or "untitled"
    text = re.sub(r"[^a-zA-Z0-9-_ ]", "", text)
    text = re.sub(r"\s+", " ", text).strip()
    return text[:80].rstrip(". ")


# ======================================================
# CANONICAL IMAGE ID RESOLUTION
# ======================================================
def extract_reddit_image_id(url):
    """
    Try to extract stable ID from Reddit CDN URLs.
    Works for:
    - i.redd.it/<id>.png
    - preview.redd.it/<id>-...jpg
    - external-preview.redd.it/<id>-...
    """

    path = urlparse(url).path
    filename = path.split("/")[-1]

    # strip extension
    base = filename.split(".")[0]

    # preview/external often have suffixes like:
    # abc123-LQ.jpg → abc123
    base = base.split("-")[0]

    # sanity check: reddit media IDs are usually alphanumeric
    if re.match(r"^[a-zA-Z0-9]+$", base):
        return base

    return hashlib.sha1(url.encode()).hexdigest()[:10]


# ======================================================
# RESOLVE IMAGE → POST
# ======================================================
def resolve_image(session, url):
    headers = {
        "User-Agent": HEADERS["User-Agent"],
        "Accept": "text/html,application/xhtml+xml",
        "Referer": "https://www.reddit.com/",
    }

    try:
        r = session.get(url, headers=headers, allow_redirects=True, timeout=30)
    except Exception:
        return None

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
# JSON PARSER
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

    for item in post.get("gallery_data", {}).get("items", []):
        mid = item.get("media_id")
        m = post.get("media_metadata", {}).get(mid, {})
        if "s" in m:
            images.append(m["s"]["u"].replace("&amp;", "&"))

    if not images and post.get("url"):
        images.append(post["url"])

    return meta, images


# ======================================================
# HTML FALLBACK
# ======================================================
def extract_images(html):
    imgs = set()

    imgs.update(re.findall(r"https://i\.redd\.it/[^\s\"']+", html))
    imgs.update(re.findall(r"https://preview\.redd\.it/[^\s\"']+", html))
    imgs.update(re.findall(r"https://external-preview\.redd\.it/[^\s\"']+", html))

    return list(imgs)


def parse_html(session, post_id):
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
# DOWNLOAD (RESUME + PROGRESS)
# ======================================================
def download(session, url, path: Path):
    tmp = path.with_suffix(path.suffix + ".part")

    existing = tmp.stat().st_size if tmp.exists() else 0

    headers = {"User-Agent": HEADERS["User-Agent"]}

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

    tmp.rename(path)


# ======================================================
# PIPELINE
# ======================================================
def build_image_list(url, session):
    url = unwrap_media_url(url)

    # -------------------------
    # DIRECT IMAGE
    # -------------------------
    if "i.redd.it" in url or "preview.redd.it" in url:
        img_id = extract_reddit_image_id(url)
        ext = urlparse(url).path.split(".")[-1].lower()
        if ext not in {"jpg", "jpeg", "png", "webp", "gif"}:
            ext = "jpg"

        meta = {"title": img_id, "subreddit": "direct"}
        return meta, [(url, img_id, ext)]

    # -------------------------
    # POST / GALLERY
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

    if not images:
        print("[*] HTML fallback")
        m, images = parse_html(session, post_id)
        meta.update({k: v for k, v in m.items() if v})

    # normalize images
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
    meta, images = build_image_list(url, session)

    if not images:
        print("[-] No images found")
        sys.exit(1)

    title = safe_name(meta["title"])
    subreddit = meta["subreddit"] or "unknown"

    out_dir = Path(subreddit) / title
    out_dir.mkdir(parents=True, exist_ok=True)

    print(f"[r/{subreddit}] {meta['title']}")
    print(f"[+] Downloading {len(images)} file(s)")


    for img, img_id, ext in images:

        # ==================================================
        # DIRECT MODE → FLAT OUTPUT
        # ==================================================
        if meta["subreddit"] == "direct":
            out_dir = Path("direct")
            out_dir.mkdir(parents=True, exist_ok=True)
            out_file = out_dir / f"{img_id}.{ext}"

        # ==================================================
        # NORMAL MODE → STRUCTURED OUTPUT
        # ==================================================
        else:
            out_file = out_dir / f"{img_id}.{ext}"

        # skip existing
        if file_exists(out_file):
            print(f"[=] Skipping {out_file.name}")
            continue

        try:
            download(session, img, out_file)
        except Exception as e:
            print(f"[!] Failed {img}: {e}")
    
    print(f"[✓] Done → {out_dir}")


# ======================================================
# MAIN
# ======================================================
def main():
    if len(sys.argv) != 2:
        print("Usage: redditdl <url>")
        sys.exit(1)

    session = get_session()
    process(sys.argv[1], session)


if __name__ == "__main__":
    main()
