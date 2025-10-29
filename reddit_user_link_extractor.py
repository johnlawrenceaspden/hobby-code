#!/usr/bin/env python3
import requests
import sys
import time
import io

# Force stdout to handle UTF-8
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

if len(sys.argv) < 2:
    print("Usage: reddit_user_link_extractor.py <reddit_username> [limit_pages]", file=sys.stderr)
    sys.exit(1)

username = sys.argv[1]
limit_pages = int(sys.argv[2]) if len(sys.argv) >= 3 else None
after = None
count = 0
headers = {"User-Agent": "RedditUserLinkExtractor/1.0"}

while True:
    params = {"limit": 100}
    if after:
        params["after"] = after

    url = f"https://www.reddit.com/user/{username}/submitted.json"
    try:
        resp = requests.get(url, headers=headers, params=params, timeout=10)
        resp.raise_for_status()
        data = resp.json()
    except Exception as e:
        print(f"❌ Error fetching data for user {username}: {e}", file=sys.stderr)
        break

    children = data.get("data", {}).get("children", [])
    if not children:
        break

    for child in children:
        post = child.get("data", {})
        permalink = post.get("permalink")
        if permalink:
            print("https://www.reddit.com" + permalink)
            count += 1

    after = data.get("data", {}).get("after")
    if not after:
        break

    if limit_pages is not None:
        limit_pages -= 1
        if limit_pages <= 0:
            break

    time.sleep(1)  # polite delay

print(f"✅ Extracted {count} links from user {username}", file=sys.stderr)
