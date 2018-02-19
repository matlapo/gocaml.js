import os
import sys
import math
from functools import reduce

if os.system("ocamlbuild comp.native -use-ocamlfind") != 0:
    exit(1)
print("COMPILATION: OK\n")

TEST_DIR = "tests"

modes = ['all', 'scanner', 'parser']

report = {}

def successRate(results):
    total = 0
    for result in results:
        if result[1]:
            total += 1
    return total / len(results)

def checkfile(filename, expect_success):
    out = os.system("./comp.native tokens " + filename)
    success = (expect_success and out == 0) or ((not expect_success) and out != 0)
    return (os.path.basename(filename), success, expect_success)

for mode in modes:
    directory = TEST_DIR + "/" + mode
    validpath = directory + "/valid"
    invalidpath = directory + "/invalid"
    validfiles = os.listdir(path=validpath)
    invalidfiles = os.listdir(path=invalidpath)
    report[mode] = []
    for file in validfiles:
        if file.endswith('.min'):
            report[mode].append(checkfile(validpath + "/" + file, True))
    for file in invalidfiles:
        if file.endswith('.min'):
            report[mode].append(checkfile(invalidpath + "/" + file, False))

print()
print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
print()

for mode in modes:
    results = report[mode]
    print("###########################")
    print("########## TESTS ##########")
    print("###########################")
    print("Step: " + str(mode) + "")
    if len(results) > 0:
        print("Success: " + str(math.floor(successRate(results)*100)) + "%")
        print("---------------------------")
        for result in results:
            if result[1]:
                print(result[0] + ": GOOD")
            else:
                print(result[0] + ": FAIL 💔 (expected " + ("success" if result[2] else "failure") + ")")
    else:
        print("No tests found.")
    print("")