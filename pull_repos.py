#!/usr/bin/env python3
import subprocess as sp
import errno
import os
import shutil
from datetime import datetime
import csv

# PARAMETERS
student_repo_folder = 'student-repos'
hw_repo_folder = 'assignments'

repo_url = 'https://{grader}@mit.cs.uchicago.edu/cmsc16100-aut-19/{student}.git'
# END PARAMETERS


def safe_makedir(dir_name):
    '''
    Makes a directory. If it already exists, don't freak out though.
    '''
    try:

        os.makedirs(dir_name)
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise

def get_cnets():
    '''
    Get the student CNETs from the CSV file
    '''
    with open("2019_students.csv", "r") as f:
        reader = csv.reader(f)
        output = [row[3] for row in reader][1:]
    return output

def pull_student_repo(grader_cnet, student_cnet):

    print("------ PULLING REPO FOR: {} ------".format(student_cnet))
    os.chdir(student_repo_folder)

    if os.path.isdir(student_cnet):
        os.chdir(student_cnet)
        sp.run(['git', 'pull', 'origin', 'master'])
        os.chdir('..')
    else:
        sp.run(['git', 'clone', repo_url.format(
            grader=grader_cnet,
            student=student_cnet
        )])

    os.chdir('..')

    copy_from = os.path.join(student_repo_folder, student_cnet, '')
    copy_to = os.path.join(hw_repo_folder, student_cnet)

    print("COPY FROM", copy_from)

    if os.path.isdir(copy_from):
        safe_makedir(copy_to)
        for name in os.listdir(copy_from):
            if name.startswith('hw'):
                sp.run(['cp', '-rf', os.path.join(copy_from, name), copy_to])
    else:
        print("------ NO REPOSITORY FOR: {} ------".format(student_cnet))


def commit_homeworks():
    os.chdir(hw_repo_folder)
    if not os.path.isdir('.git'):
        print("Initializing git repository")
        sp.run(['git', 'init'])

    sp.run(['git', 'add', '-A'])
    sp.run(['git', 'diff', '--no-pager', '--staged', '--stat'])
    sp.run(['git',
            '-c', 'user.name="CS161 Homework Bot"',
            '-c', 'user.email="CS161-HW-Bot@CSIL"',
            'commit', '-m',
            'Pulled submissions on {}'.format(datetime.now().strftime('%Y-%m-%d %H:%M:%S'))
           ])

    os.chdir('..')


def main():
    grader_cnet = input("Grader CNet ID: ")

    sp.run(['git', 'config', '--global', 'credential.helper', 'cache'])

    safe_makedir(student_repo_folder)
    safe_makedir(hw_repo_folder)

    for student_cnet in get_cnets():
        pull_student_repo(grader_cnet, student_cnet)


    commit_homeworks()


if __name__ == '__main__':
    main()
