from string import Template
import os, glob
import csv

# ---------------------
hwnum = 17
filetype = [".hs",".txt"]
problems = [1,2]
# ---------------------

hw_folder = "hw" + str(hwnum) + r"/"
ex_names = ["ex" + str(hwnum) + "-" + str(x) for x in problems]
probstr = [ex_name + ext for ex_name in ex_names for ext in filetype]

# print(probstr)
# list(map(lambda x: "ex" + str(hwnum) + "-" + str(x), problems))

#change to path to hw folder
root_folder_path =  r"./assignments/"

with open('tex-template.tex') as f:
    template = f.read()

submission_tex_template = Template(r"""
\section*{$ex_name}
\begin{$texttype}
$code
\end{$texttype}""")

title_template = Template(r"""

\renewcommand{\cnet}{$cnet}
\renewcommand{\classsection}{$section}
\renewcommand{\homeworktitle}{$homework_title}
\renewcommand{\studentname}{$name}
\setcounter{page}{1}
""")


student_divider = r"\cleardoublepage"

# submissions = {
#     "cwinkler": {
#         "16.1": """
# module Hw16Ex1 where
# print :: String -> IO ()
# print hi = println "hi"
# """
#         ,"16.2": """
# module Hw16Ex2 where
# print :: String -> IO ()
# print stuff = println stuff
# """
#     },
#     "lhackworth": {
#         "16.1": """
# module Hw16Ex1 where
# print :: String -> IO ()
# print hi = println hi
# """
#     }
# }

with open("2019_students.csv", "r") as f:
    reader = csv.reader(f)
    rows = list(reader)[1:]

    cnetid_section1 = {
        row[3]: (row[1], row[2], row[0])
        for row in rows
        if int(row[0]) == 1
    }

    cnetid_section2 = {
        row[3]: (row[1], row[2], row[0])
        for row in rows
        if int(row[0]) == 2
    }


cnetid_section_test = {
    "yguan1" : ("Guan", "Yuxin"),
    "liuyifan" : ("Liu", "Yifan"),
    "cwinkler" : ("Winkler", "Chris"),
    "lhackworth" : ("Hackworth", "Lilly")
}

all_cnetids = cnetid_section2
all_cnetids.update(cnetid_section1)
# all_cnetids.update(cnetid_section_test)

def tex_submissions(s):
    submissions = []
    for student in all_cnetids:
        if student in s and s[student]:
            assignment_texs = []
            for assignment in s[student]:
                assignment_tex = submission_tex_template.substitute(
                    ex_name=assignment,
                    code=s[student][assignment].strip(),
                    texttype="haskell-code" if assignment.endswith(".hs")
                        else "plain-text"
                )
                assignment_texs.append(assignment_tex)

            submission = title_template.substitute(
                cnet=student,
                # name=all_cnetids[student][1] + all_cnetids[student][0],
                name=" ".join(reversed(all_cnetids[student][:2])),
                homework_title="Homework " + str(hwnum),
                section=all_cnetids[student][2]
            )

            submission += '\n{}\n'.format('').join(assignment_texs)
            submissions.append(submission)

    return '\n{}\n'.format(student_divider).join(submissions)

# print(template.substitute(code=tex_submissions(submissions)))

def get_submissions():
    submissions = {}
    for cnetid in next(os.walk(root_folder_path))[1]:
        if cnetid == '.git':
            continue
        # print(cnetid)
    # for cnetid in filter(lambda x: os.path.isdir(os.path.join(root_folder_path, x)), os.listdir(root_folder_path)):
        submissions[cnetid]={}
        for file_name in probstr:
            # print(file_name)
            # print(root_folder_path + cnetid + r"/")
            # print(os.listdir(root_folder_path + cnetid + r"/"))
            path = root_folder_path + cnetid + r"/" + hw_folder
            # for real_file in os.listdir(path):
            #     # print(real_file)
            #     if real_file.startswith(file_name):
            #         with open(path + real_file, 'r') as ans_file:
            #             submissions[cnetid][file_name] = ans_file.read()
            try:
                with open(path + file_name, 'r') as ans_file:
                    submissions[cnetid][file_name] = ans_file.read()
            except:
                pass


        for n in ex_names:
            if len([fname for fname in submissions[cnetid].keys() if fname.startswith(n)]) == 0:
                print("NO SUBMISSION: {} for {}".format(cnetid, n))


    return submissions

# print(get_submissions())
with open("output.tex", "w") as f:
    f.write(template.replace("CODE", tex_submissions(get_submissions())))
