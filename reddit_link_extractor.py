#!/usr/bin/env python3
import requests
import time
import sys

SUBREDDIT = sys.argv[1] if len(sys.argv) > 1 else "dalle2"

LIMIT = 100   # posts per page (max 100)
after = None
count = 0

headers = {"User-Agent": "LinkExtractor/1.0"}

while True:
    params = {"limit": LIMIT}
    if after:
        params["after"] = after

    url = f"https://www.reddit.com/r/{SUBREDDIT}/new.json"
    try:
        resp = requests.get(url, headers=headers, params=params, timeout=10)
        resp.raise_for_status()
        data = resp.json()
    except Exception as e:
        print(f"❌ Error fetching data: {e}", file=sys.stderr)
        break

    posts = data["data"]["children"]
    if not posts:
        break

    for post in posts:
        permalink = post["data"].get("permalink")
        if permalink:
            print(f"https://www.reddit.com{permalink}")
            count += 1

    after = data["data"].get("after")
    if not after:
        break

    time.sleep(1)  # be polite to Reddit’s API

print(f"✅ Extracted {count} links", file=sys.stderr)
