#!/usr/bin/env python3
import subprocess
import shutil
import os
import readline, rlcompleter
import re

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

def fail(text):
    return bcolors.FAIL + bcolors.BOLD + text + bcolors.ENDC

def success(text):
    return bcolors.OKGREEN + bcolors.BOLD + text + bcolors.ENDC

def prepare_test_dir(cnet, a):
    prefix = "[Hw{a[0]}Ex{a[1]}, {cnet}] ".format(a=a, cnet=cnet)

    module_name = "Hw{a[0]}Ex{a[1]}".format(a=a)
    test_module_name = "Test" + module_name

    orig_submission_path = "../assignments/{cnet}/hw{a[0]:02d}/ex{a[0]}-{a[1]}.hs".format(cnet=cnet, a=a)
    new_submission_path =  "{}.hs".format(module_name)

    orig_test_path = "../tests/{}.hs".format(test_module_name)
    new_test_path =  "{}.hs".format(test_module_name)

    if not os.path.isfile(orig_submission_path):
        print(prefix + fail("No submission found!"))
        # TODO add a thing to look for files in the relevant directory
        return False

    # Filter out any module declaration in the submission and add our own
    #  so the tests now what to import
    with open(orig_submission_path, "r") as f:
        contents = f.readlines()
        new_contents = ["module {} where".format(module_name)] + \
            [line for line in contents if not line.startswith("module")]

    with open(new_submission_path, "w") as f:
        f.write("\n".join(new_contents))

    # Copy the test into this directory
    shutil.copyfile(orig_test_path, new_test_path)
    return True

def compile_tests(module, prefix=""):
    test_module = "Test" + module

    # Compile the test
    while True:
        print(prefix + "Compiling...")

        compile_output = subprocess.run(
            ["sh", "../include/compile-script.sh", test_module]
        )

        if compile_output.returncode != 0:
            print(prefix + fail("Compilation failed"))
            should_edit = input("Edit the file and recompile (y|n)? ")

            cmd_output_plain = subprocess.run([
                "sed", r"s|\x1b\[[;0-9]*m||g", "compile-output.txt"],
                capture_output=True
            ).stdout

            with open("compile-output-plain.txt", "w") as f:
                f.write(cmd_output_plain.decode('utf-8'))

            if not re.match(r'[Nn]([Oo])*', should_edit):
                subprocess.run(['vim', '-O', module + '.hs', 'compile-output-plain.txt'])
            else:
                return False
        else:
            print(prefix + success("Compilation succeeded"))
            break

    return True


assignments = [(18, 1)]

cnets = [c for c in os.listdir("assignments/") if c != '.git']

def complete(text, state):
    try:
        v = [c for c in cnets if c.startswith(text)]
        return v[state]
    except IndexError:
        return None

readline.parse_and_bind("set enable-keypad on")
readline.set_completer(complete)
readline.set_completer_delims(' \t\n;,')
readline.parse_and_bind("tab: complete")

while True:
    inp_cnets = input("Student CNet(s): ")

    if inp_cnets == "q":
        print("Quitting...")
        break

    for cnet in re.split('[ \t\n;,]+', inp_cnets):

        try:
            os.mkdir("tmp")
        except FileExistsError:
            pass

        os.chdir("tmp")

        for a in assignments:

            # Prep the temp dir for testing
            if not prepare_test_dir(cnet, a):
                continue

            # Initialize appropriate names based on the assignment and cnet
            prefix = "[Hw{a[0]}Ex{a[1]}, {cnet}] ".format(a=a, cnet=cnet)

            module = "Hw{a[0]}Ex{a[1]}".format(a=a)
            test_module = "TestHw{a[0]}Ex{a[1]}".format(a=a)

            if not compile_tests(module, prefix=prefix):
                continue

            # Run the test
            print(prefix + "Running tests...".format(cnet))
            print(bcolors.OKBLUE + bcolors.BOLD + "{l}-- OUTPUT --{l}".format(l='-'*15) + bcolors.ENDC)
            test_output = subprocess.run("./" + test_module)
            print(bcolors.OKBLUE + bcolors.BOLD + "{l} END OUTPUT {l}".format(l='-'*15) + bcolors.ENDC)

            if test_output.returncode != 0:
                print(prefix + fail("Tests failed"))
            else:
                print(prefix + success("Tests passed"))

        os.chdir("..")
        shutil.rmtree("tmp")
