#!/usr/bin/env python3

def check(input, stack):
    if input == "":
        return ("ok" if stack == "" else "incomplete", stack)
    
    if input == "" and stack != "":
        return ""
    
    if input[0] == "(":
        return check(input[1:], ")" + stack)
    
    elif input[0] == "[":
        return check(input[1:], "]" + stack)

    elif input[0] == "{":
        return check(input[1:], "}" + stack)
    
    elif input[0] == "<":
        return check(input[1:], ">" + stack)
    
    elif input[0] == stack[0]:
        return check(input[1:], stack[1:])
    
    return ("mismatch", input[0])


points1 = { ")" : 3
          , "]" : 57
          , "}" : 1197
          , ">" : 25137
          }

points2 = { ")" : 1
          , "]" : 2
          , "}" : 3
          , ">" : 4
          }

def scoreCompletion(seq):
    res = 0
    for paren in seq:
        res = res * 5 + points2[paren]
    return res

with open("input.txt") as fh:
    lines = fh.readlines()
lines = [line.strip() for line in lines]


results = [ check(line, "") for line in lines ]

mismatches = [ rest for (msg, rest) in results if msg == "mismatch" ]
print(f"Part one: {sum([ points1[mismatch] for mismatch in mismatches ])}")

incompletes = [ rest for (msg, rest) in results if msg == "incomplete"]
print(f"Part two: {sorted([ scoreCompletion(incomplete) for incomplete in incompletes ])[len(incompletes) // 2]}")