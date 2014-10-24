states=["Alabama", "Hawaii", "Massachusetts", "New Mexico",
"South Dakota", "Alaska", "Idaho", "Michigan", "New York",
"Tennessee", "Arizona", "Illinois", "Minnesota", "North Carolina",
"Texas", "Arkansas", "Indiana", "Mississippi", "North Dakota",
"Utah", "California", "Iowa", "Missouri", "Ohio",
"Vermont", "Colorado", "Kansas", "Montana", "Oklahoma",
"Virginia", "Connecticut", "Kentucky", "Nebraska", "Oregon",
"Washington", "Delaware", "Louisiana", "Nevada", "Pennsylvania",
"West Virginia", "Florida", "Maine", "New Hampshire", "Rhode Island",
"Wisconsin", "Georgia", "Maryland", "New Jersey", "South Carolina",
"Wyoming"]

anagrams={}

for i in states:
    for j in states:
        if i<j: continue
        anagram=tuple(sorted(i+j))
        if anagram in anagrams:
            print (anagrams[anagram], i,j)
        else:
            anagrams[anagram]=[i,j]





