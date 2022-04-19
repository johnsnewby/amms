#!/usr/bin/env python3

import json, requests, sys

url = f"https://api.tzkt.io/v1/operations/originations?sender={sys.argv[1]}"

response = requests.get(url)

data = json.loads(response.content.decode())

for contract in data:
    originated = contract['originatedContract']
    alias = originated.get('alias')
    if alias:
        print("""
- name: \"""" + alias + """\"
  address: \"""" + originated['address'] + """\"
""")
    
