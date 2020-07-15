from os import listdir, path
from subprocess import run


def handle_dir(dir):
    sub_dir = f"./{dir}/script/"
    if not path.isdir(sub_dir):
        print(f"{sub_dir} as not code")
        return
    # If there is currently not an existing r script generate one from a jupyter notebook. Else print out an error
    if not any(map(lambda x: x.split()[-1] == 'r', listdir(sub_dir))):
        notebooks = list(filter(lambda x: x.split()[-1] == 'irnb', listdir(sub_dir)))
        if len(notebooks) > 1:
            print(f"{dir} has more than one notebook")
            return
        if len(notebooks) == 0:
            print(f"{dir} has no notebooks or r script")
            return
        run(f"jupyter-nbconvert --to script {sub_dir}{notebooks[0]}")

    # At this point we have an r script in the dir

    # We are just going to assume there is one and only one r script
    r_file_name = list(filter(lambda x: x.split()[-1] == 'r', listdir(sub_dir)))[0]

    print(r_file_name)


def run_all():
    dirs = listdir()
    for dir in dirs:
        handle_dir(dir)


if __name__ == '__main__':
    run_all()
